# Script importing YouGov data
# Programme created on June 19th 2021
YouGov <- read.csv(file = "~/Desktop/Health innovation/Dissertation/Database/YouGov_cleanup.csv",header = TRUE, sep = ",")

# Install packages 
install.packages("table1")
install.packages("magrittr")
install.packages("dyplr")
install.packages("ldatuning")
install.packages("broom")
install.packages("Hmisc")
install.packages("stringr")
install.packages("tidyverse")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("textmineR")
install.packages("data.table")
install.packages("senitmentr")
install.packages("dbplyr")

# Load libraries
library(table1) 
library(Hmisc)
library(ggplot2)

# Packages for NLP
library(stringr)
library(dbplyr)
library(broom)
library(magrittr)
library(dbplyr)
library(tidyverse)
library(topicmodels)
library(tidytext)
library(textmineR)
library(data.table)
library(sentimentr)
library(ldatuning)
library(tm)

# Packages for wordclouds
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)

# Natural language processing (NLP) 
# Creating LDA function
# Set random seed
seed = 346962
set.seed(seed)

# Create function to test LDA models consisting of the dtm and modelN (maximum number of topics to test)
calculate_LDA_models <- function(create_dtm_benefits,modelN=25){
  output <- ldatuning::FindTopicsNumber(dtm = create_dtm_benefits,
                                        topics = seq(from = 2,to = modelN, by = 1),
                                        metrics = c("CaoJuan2009",
                                                    "Arun2010",
                                                    "Deveaud2014"),
                                        method = 'Gibbs',
                                        control = list(seed = seed),
                                        verbose = T,
                                        return_models = T)
  return(output)
}

# Visualize calculate_LDA_models function to decide on optimal number of topics 
better_plot_FindTopicNumber <- function (values){
  if ("LDA_model" %in% names(values)) {
    values <- values[!names(values) %in% c("LDA_model")]
  }
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns, 
                                                           2, function(column) {
                                                             scales::rescale(column, to = c(0, 1), from = range(column))
                                                           }))
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
  values$group <- base::factor(values$group, levels = c(FALSE, 
                                                        TRUE), labels = c("minimise", "maximise"))
  p <- ggplot(values, aes_string(x = "topics", y = "value", 
                                 group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = none, shape = guide_legend(title = "metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(), 
                                       panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour = "grey70"), 
                                       panel.grid.minor.x = element_blank(), legend.key = element_blank(), 
                                       strip.text.y = element_text(angle = 90))
  g <- ggplotGrob(p)
  g$layout[g$layout$name == "strip-right", c("l", "r")] <- 3
  grid::grid.newpage()
  grid::grid.draw(g)
  return(p)
}

# Benefits

# Part 1: Data processing
YouGov$Benefits <- str_to_lower(YouGov$Benefits) #turn all the text into lower case
# Replace necessary text
YouGov$Benefits  <- gsub(pattern = "don't|Don't", replacement = "do not", x = YouGov$Benefits)
YouGov$Benefits  <- gsub(pattern = "dk|DK|Dk|d/k|D/k|\\|i don’t know|i do not know?", replacement = "idonotknow", x = YouGov$Benefits)
YouGov$Benefits  <- gsub(pattern = "Na|N/A|n/a|N/a", replacement = "noanswer", x = YouGov$Benefits)
# Remove unnecessary newlines in text and multiple white spaces 
YouGov$Benefits  <- gsub(pattern = "^\n{1,}", replacement = "", x = YouGov$Benefits ) #Remove single newlines at beginning of text
YouGov$Benefits  <- gsub(pattern = "\n{1,}$", replacement = ".", x = YouGov$Benefits ) #change multiple newlines at end to colons
YouGov$Benefits  <- gsub(pattern = "\n{1,}", replacement = ". ", x = YouGov$Benefits ) #Change remaining newlines to colons
YouGov$Benefits  <- gsub(pattern = " {1,}", replacement = " ", x = YouGov$Benefits ) #Change double spaces to single
# Remove numbers and punctuation marks
YouGov$Benefits <- gsub('[[:digit:]]+', ' ', YouGov$Benefits)
YouGov$Benefits <- gsub('[[:punct:]]+', ' ', YouGov$Benefits)
# Remove stop words 
benefits_textfile <- YouGov %>%
  tidytext::unnest_tokens(output = word,input = Benefits,token = 'words') %>% 
  filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words, by="word")
#Stitch text back together, #tokens$text <- trimws(tokens$text)
tokens_benefits <- benefits_textfile  %>%
  mutate(ind = row_number()) 
tokens_benefits <- tokens_benefits %>% group_by(ID) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word) 
tokens_benefits [is.na(tokens_benefits)] <- ""
tokens_benefits <- tidyr::unite(tokens_benefits,Benefits,-c(ID,Country,Gender,Age_group,Ethnicity,General_health,eHEALs_Score, Low_literacy,High_literacy,Challenges), sep =" ") 
tokens_benefits = subset(tokens_benefits, select = -c(ID,Country,Gender,Age_group,Ethnicity,General_health,eHEALs_Score, Low_literacy,High_literacy,Challenges) )
View(tokens_benefits)

# Create document-term matrix (dtm) from textminr library > dtm,  n-grams, lemmatise words 
create_dtm_benefits <- function(YouGov){
  dtm <- CreateDtm(doc_vec = c(YouGov$Benefits),
                   doc_names = YouGov$ID,
                   ngram_window = c(1,3),verbose = T, #n-grams set 1-3
                   stem_lemma_function = function(x) textstem::lemmatize_strings(x))
  return(dtm)}
dtm_benefits <- create_dtm_benefits(tokens_benefits)

# Remove documents without rows
rowTotals_benefits <- apply(dtm_benefits , 1, sum)  #find the sum of words in each document
dtm_benefits   <- dtm_benefits[rowTotals_benefits> 0, ]  #remove all docs without words

# Part 2: find the best number of topics to split the documents into

models_benefits <- calculate_LDA_models(dtm_benefits,modelN = 25) 
# Plot the results from the previous function, change the xintercept parameter to change where the red line appears in the plot
sf1 <- better_plot_FindTopicNumber(models_benefits) +
  theme(legend.position = 'bottom') +
  geom_vline(xintercept = 8, linetype = 2, color = 'red')#the model with 8 topics is the element 17 in the list, #more important to minsimse than maximise?
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig1.1_metricsbenefits.png", sf1, width = 14, height = 10, units = "in", dpi = 600)


# Part 3: choose the LDA model with the 8 topics and plot the terms per topic

# Visualising terms in each topic 
plot_top_topic_words <- function(model,model_map=NULL){
  
  ldamod_topics <- tidy(model, matrix = "beta")
  
  ap_top_terms <- ldamod_topics %>%
    group_by(topic) %>%
    top_n(8, beta) %>% #showing n topics
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic = paste0("Topic",topic))
  
  if(!is.null(model_map)){
    ap_top_terms <- left_join(ap_top_terms,model_map,
                              by=c("topic"="id"))
    ap_top_terms$topic <- ap_top_terms$label
  }
  
  p <- ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    labs(x="Beta",y="Term") +
    theme_bw()
  
  return(p)
}

# Running LDA model with 8 topics 

models_benefits$LDA_model #show the list of LDA models with 8 topics
model_benefits <- models_benefits$LDA_model[[18]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_benefits,model_map = NULL) #Plot the terms per topic
model_map_benefits <- data.frame(id=paste0("Topic",1:8), #Label each topic based on what terms tend to appear
                                 label=c("T01: Primary care delivery" #1
                                         ,"T02: Infection control" #2
                                         ,"T03: Reducing contacts" #3
                                         ,"T04: Virtual care" #4
                                         ,"T05: No answer provided" #5
                                         ,"T06: Patient-doctor interaction" #6
                                         ,"T07: Convenience and safety" #7
                                         ,"T08: Timeliness" #8
                                 ))
f3 <- plot_top_topic_words(model_benefits,model_map_benefits) #Rerun the previous function with a model_map so that the new topic terms appear
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/fig3_8tbenefits.png", f3, width = 14, height = 10, units = "in", dpi = 600)

# Sensitivity analysis: running LDA model with 9 topics 
models_benefits$LDA_model
model_benefits <- models_benefits$LDA_model[[17]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_benefits,model_map = NULL) #Plot the terms per topic
model_map_benefits <- data.frame(id=paste0("Topic",1:9), #Label each topic based on what terms tend to appear
                                 label=c("T01: Primary care delivery" #1
                                         ,"T02: Convenience and safety" #2
                                         ,"T03: Reducing contacts" #3
                                         ,"T04: Virtual care" #4
                                         ,"T05: No answer provided" #5
                                         ,"T06: Healthcare services" #6
                                         ,"T07: Infection control" #7
                                         ,"T08: Patient-doctor interaction" #8
                                         ,"T09: Timeliness"#9
                                 ))
plot_top_topic_words(model_benefits,model_map_benefits) #Rerun the previous function with a model_map so that the new topic terms appear
sf4.1 <- plot_top_topic_words(model_benefits,model_map_benefits) #Rerun the previous function with a model_map so that the new topic terms appear
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig4.1_9tbenefits.png", sf4.1, width = 10, height = 10, units = "in", dpi = 600)

# Sensitivity analysis: running LDA model with 10 topics 
models_benefits$LDA_model
model_benefits <- models_benefits$LDA_model[[16]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_benefits,model_map = NULL) #Plot the terms per topic
model_map_benefits <- data.frame(
  id = reorder(paste0("Topic", 1:10), as.numeric(gsub("Topic", "", paste0("Topic", 1:10)))),
  label = c(
    "T01: Primary care delivery",
    "T02: Infection control",
    "T03: Keeping safe at home",
    "T04: Reducing contacts",
    "T05: Virtual care",
    "T06: No answer provided",
    "T07: Avoiding contact in surgery",
    "T08: Patient-doctor interaction",
    "T09: Convenience",
    "T10: Timeliness"
                                 ))
plot_top_topic_words(model_benefits,model_map_benefits) #Rerun the previous function with a model_map so that the new topic terms appear
sf4.2 <- plot_top_topic_words(model_benefits,model_map_benefits) 
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig4.2_10tbenefits.png", sf4.2, width = 14, height = 10, units = "in", dpi = 600)

#NLP for challenges 
YouGov$Challenges <- str_to_lower(YouGov$Challenges) #turn all the text into lower case
#Replace necessary text
YouGov$Challenges  <- gsub(pattern = "don't|Don't", replacement = "do not", x = YouGov$Challenges)
YouGov$Challenges  <- gsub(pattern = "dk|DK|Dk|d/k|D/k|\\|i don’t know|i do not know?", replacement = "idonotknow", x = YouGov$Challenges)
YouGov$Challenges   <- gsub(pattern = "Na|N/A|n/a|N/a", replacement = "noanswer", x = YouGov$Challenges)
#Remove unnecessary newlines in text and multiple white spaces 
YouGov$Challenges  <- gsub(pattern = "^\n{1,}", replacement = "", x = YouGov$Challenges ) 
YouGov$Challenges  <- gsub(pattern = "\n{1,}$", replacement = ".", x = YouGov$Challenges ) 
YouGov$Challenges  <- gsub(pattern = "\n{1,}", replacement = ". ", x = YouGov$Challenges ) 
YouGov$Challenges  <- gsub(pattern = " {1,}", replacement = " ", x = YouGov$Challenges )
#Remove numbers and punctuation marks
YouGov$Challenges  <- gsub('[[:digit:]]+', ' ', YouGov$Challenges)
YouGov$Challenges <- gsub('[[:punct:]]+', ' ', YouGov$Challenges)
#Remove stop words 
challenges_textfile <- YouGov %>%
  tidytext::unnest_tokens(output = word,input = Challenges,token = 'words') %>% 
  filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words, by="word")
#View(challenges_textfile)
#Stitch text back together
tokens_challenges <- challenges_textfile  %>% mutate(ind = row_number())
tokens_challenges <- tokens_challenges %>% group_by(ID) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens_challenges [is.na(tokens_challenges)] <- ""
tokens_challenges <- tidyr::unite(tokens_challenges, Challenges,-c(ID,Country,Gender,Age_group,Ethnicity,General_health,eHEALs_Score, Low_literacy,High_literacy,Benefits),sep =" ")
tokens_challenges = subset(tokens_challenges, select = -c(ID,Country,Gender,Age_group,Ethnicity,General_health,eHEALs_Score, Low_literacy,High_literacy,Benefits) )
View(tokens_challenges)
#create dtm
create_dtm_challenges <- function(YouGov){
  dtm <- CreateDtm(doc_vec = c(YouGov$Challenges),
                   doc_names = YouGov$ID,
                   ngram_window = c(1,3),verbose = T,
                   stem_lemma_function = function(x) textstem::lemmatize_strings(x))
  return(dtm)}
dtm_challenges <- create_dtm_challenges(tokens_challenges)
#remove documents without rows
rowTotals_challenges <- apply(dtm_challenges , 1, sum)  #find the sum of words in each document
dtm_challenges   <- dtm_challenges[rowTotals_challenges> 0, ]  #remove all docs without words
#find the best number of topics to split the documents into
models_challenges <- calculate_LDA_models(dtm_challenges,modelN = 25) 
#plot the results from the previous function, change the xintercept parameter to change where the red line appears in the plot
sf1.1 <- plot_dettopic_challenges <- better_plot_FindTopicNumber(models_challenges) +
  theme(legend.position = 'bottom') +
  geom_vline(xintercept = 8, linetype = 2, color = 'red')
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig1.1_metricschallenges.png", sf1.1, width = 14, height = 10, units = "in", dpi = 600)

#Run LDA model with 8 topics
models_challenges$LDA_model
model_challenges <- models_challenges$LDA_model[[18]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_challenges,model_map = NULL) #Plot the terms per topic
model_map_challenges <- data.frame(id=paste0("Topic",1:8), #Label each topic based on what terms tend to appear
                                   label=c("T1: Diagnostic difficulties" #1
                                           ,"T2: Physical examination" #2
                                           ,"T3: Digital health risks" #3
                                           ,"T4: Technical challenges" #4
                                           ,"T5: Virtual care" #5
                                           ,"T6: Data security and protection" #6
                                           ,"T7: No answer provided" #7
                                           ,"T8: Lack of personal contact" #8
                                   ))
plot_top_topic_words(model_challenges,model_map_challenges)
f4 <- plot_top_topic_words(model_challenges,model_map_challenges) #Rerun the previous function with a model_map so that the new topic terms appear
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/fig4_8tchallenges.png", f4, width = 14, height = 10, units = "in", dpi = 600)


# Sensitivity analysis: running LDA model with 9 topics 
models_challenges$LDA_model
model_challenges <- models_challenges$LDA_model[[17]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_challenges,model_map = NULL) #Plot the terms per topic
model_map_challenges <- data.frame(id=paste0("Topic",1:9), #Label each topic based on what terms tend to appear
                                   label=c("T01: No answer provided" #1
                                           ,"T02: Physical examination" #2
                                           ,"T03: Patient-doctor interaction" #3 former: Digital health risks
                                           ,"T04: Technical challenges" #4
                                           ,"T05: Diagnostic difficulties" #5
                                           ,"T06: Virtual care" #6
                                           ,"T07: Digital access" #7 former Data security and protection
                                           ,"T08: Data security and protection" #8
                                           ,"T09: Lack of personal contact" #9
                                   ))
plot_top_topic_words(model_challenges,model_map_challenges)
sp5 <- plot_top_topic_words(model_challenges,model_map_challenges) #Rerun the previous function with a model_map so that the new topic terms appear
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig5.1_9tchallenges.png", sp5, width = 10, height = 10, units = "in", dpi = 600)

# Sensitivity analysis: running LDA model with 10 topics 
models_challenges$LDA_model
model_challenges <- models_challenges$LDA_model[[16]] #extract the best model from the list by counting the models from 25 backwards till you get to 8
plot_top_topic_words(model = model_challenges,model_map = NULL) #Plot the terms per topic
model_map_challenges <- data.frame(id=paste0("Topic",1:10), #Label each topic based on what terms tend to appear
                                   label=c("T01: Physical examination" 
                                           ,"T02: Patient-doctor interaction"
                                           ,"T03: No answer provided" 
                                           ,"T04: Technical challenges"  
                                           ,"T05: Digital health risks" # back again
                                           ,"T06: Virtual care"
                                           ,"T07: Data security and protection" # aber: "noanswer"
                                           ,"T08: Digital access" # former Data security and protection
                                           ,"T09: Lack of personal contact"
                                           ,"T10: Diagnostic difficulties" 
                                   ))
plot_top_topic_words(model_challenges,model_map_challenges)
sp6 <- plot_top_topic_words(model_challenges,model_map_challenges) #Rerun the previous function with a model_map so that the new topic terms appear
# Save the plot as a high-resolution PNG file
ggsave("~/Desktop/Health innovation/Dissertation/Results/Figures/supfig5.2_10tchallenges.png", sp6, width = 14, height = 10, units = "in", dpi = 600)

#Creating wordclouds (https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a)
#https://www.pluralsight.com/guides/visualization-text-data-using-word-cloud-r

plot_lda_wordclouds <- function(Model,Topic,Output,MaxWords=50,multiplier=100000,...){
  mod_data <- tidy(Model,matrix='beta')
  
  token_benefits <- mod_data %>% 
    filter(topic == Topic) %>%
    select(word = term, freq = beta) %>%
    data.frame()
  
  tokens_benefits <- data.frame(word = token_benefits$word,freq=round(token_benefits$freq*multiplier,0))
  
  plot_data <- subset(tokens_benefits, tokens_benefits$freq>MaxWords) %>% arrange(desc(freq))
  
  p <- wordcloud2::wordcloud2(data = plot_data,
                              minRotation = 0,maxRotation = 0)
  saveWidget(p, "tmp.html", selfcontained = F)
  webshot("tmp.html", Output, vwidth = 700, vheight = 700)
  return(plot_data)
}

#plotting wordclouds 
#benefits
lda_b_t1 <- plot_lda_wordclouds(Model = model_benefits,Topic = 1,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t1$word, freq = lda_b_t1$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t2 <- plot_lda_wordclouds(Model = model_benefits,Topic = 2,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t2$word, freq = lda_b_t2$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t3 <- plot_lda_wordclouds(Model = model_benefits,Topic = 3,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t3$word, freq = lda_b_t3$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t4 <- plot_lda_wordclouds(Model = model_benefits,Topic = 4,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t4$word, freq = lda_b_t4$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t5 <- plot_lda_wordclouds(Model = model_benefits,Topic = 5,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t5$word, freq = lda_b_t5$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t6 <- plot_lda_wordclouds(Model = model_benefits,Topic = 6,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t6$word, freq = lda_b_t6$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t7 <- plot_lda_wordclouds(Model = model_benefits,Topic = 7,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t7$word, freq = lda_b_t7$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_b_t8 <- plot_lda_wordclouds(Model = model_benefits,Topic = 8,Output = "SampleWordcloud_benefits.png")
wordcloud(words = lda_b_t8$word, freq = lda_b_t8$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

#challenges
lda_c_t1 <- plot_lda_wordclouds(Model = model_challenges,Topic = 1,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t1$word, freq = lda_c_t1$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t2 <- plot_lda_wordclouds(Model = model_challenges,Topic = 2,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t2$word, freq = lda_c_t2$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t3 <- plot_lda_wordclouds(Model = model_challenges,Topic = 3,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t3$word, freq = lda_c_t3$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t4 <- plot_lda_wordclouds(Model = model_challenges,Topic = 4,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t4$word, freq = lda_c_t4$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t5 <- plot_lda_wordclouds(Model = model_challenges,Topic = 5,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t5$word, freq = lda_c_t5$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t6 <- plot_lda_wordclouds(Model = model_challenges,Topic = 6,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t6$word, freq = lda_c_t6$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t7 <- plot_lda_wordclouds(Model = model_challenges,Topic = 7,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t7$word, freq = lda_c_t7$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

lda_c_t8 <- plot_lda_wordclouds(Model = model_challenges,Topic = 8,Output = "SampleWordcloud_challenges.png")
wordcloud(words = lda_c_t8$word, freq = lda_c_t8$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

#Statistical analysis
#function to create plots and do statistical analysis
stats_LDA <- function(model,dtm,model_map=NULL,target_var='IC',map=demo_vars,labels)
{
  #Get the gamma probs from the model
  gamm <- tidy(model, matrix = "gamma")
  gamm$topic <- paste0("Topic",gamm$topic)
  gamm <- data.frame(gamm)
  gamm <- left_join(gamm,map,by=c("document"="ID"))
  
  #Add the target variable to the probs data
  gamm$target = gamm[,target_var]
  xx <- gamm %>%
    mutate(Label = target) %>% 
    filter(!is.na(Label))
  
  #If model map is specified, add the new topic names to the data
  if(!is.null(model_map)){
    xx <- left_join(xx,model_map,by=c("topic"="id"))
    xx$Topic <- xx$label
  }
  
  #Perform statistical analysis
  plot_means <- xx %>%
    group_by(Topic) %>%
    rstatix::wilcox_test(formula = gamma ~ Label,p.adjust.method = 'fdr') %>%
    rstatix::add_y_position(fun = 'mean_se') %>%
    mutate(p.signif = gtools::stars.pval(p)) %>%
    mutate(p.signif = ifelse(p.signif==" ","ns",p.signif))
  
  #Plot data plus stats significance
  p <- xx %>% group_by(Topic,Label) %>%
    summarise(prob = mean(gamma),
              sd = sd(gamma)/sqrt(n())) %>% 
    mutate(prob=round(prob, digits=3))%>%
    ggplot(aes(x=Label,y=prob)) +
    geom_bar(stat='identity',fill="grey") + 
    geom_text(aes(label = paste(format(prob, nsmall = 1))), vjust = 10, size = 4) +
    facet_wrap(~Topic) +
    labs(x="",y="Probability") +
    theme_bw() +
    theme(text = element_text(size = 13)) +
    scale_x_discrete(labels=labels) +
    ggpubr::stat_pvalue_manual(data = plot_means,hide.ns = T,label = "p.signif")
  
  return(list(stats = plot_means,plot = p))
}

#create a dataframe with demographic variables country, gender, age groups, ethnicity, high literacy
YouGov_demographics <- YouGov[,-c(6,7,8,10,11)] #delete column three 
#create character variables
YouGov_demographics$ID=as.character(YouGov_demographics$ID)
YouGov_demographics$Country=as.character(YouGov_demographics$Country)
YouGov_demographics$Gender=as.character(YouGov_demographics$Gender)
YouGov_demographics$Age_group=as.character(YouGov_demographics$Age_group)
YouGov_demographics$Ethnicity=as.character(YouGov_demographics$Ethnicity)
YouGov_demographics$High_literacy=as.character(YouGov_demographics$High_literacy)


#Benefits 

#statsanalysis for gender
stats_analysis <- stats_LDA(model = model_benefits, #LDA model create
                            dtm = dtm_benefits, #document-term matrix used for the model
                            model_map = model_map_benefits, #data frame mapping the topics to their new labels
                            target_var = "Gender", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("Male","Female")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig5.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for Age groups
stats_analysis <- stats_LDA(model = model_benefits, #LDA model create
                            dtm = dtm_benefits, #document-term matrix used for the model
                            model_map = model_map_benefits, #data frame mapping the topics to their new labels
                            target_var = "Age_group", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("18-24","25-34","35-44","45-54","55+")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig6.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for country 
stats_analysis <- stats_LDA(model = model_benefits, #LDA model create
                            dtm = dtm_benefits, #document-term matrix used for the model
                            model_map = model_map_benefits, #data frame mapping the topics to their new labels
                            target_var = "Country", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("UK","IT","SE","DE")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output

#save plot as jpg
ggsave(
  file = "fig7.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for literacy
stats_analysis <- stats_LDA(model = model_benefits, #LDA model create
                            dtm = dtm_benefits, #document-term matrix used for the model
                            model_map = model_map_benefits, #data frame mapping the topics to their new labels
                            target_var = "High_literacy", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("Low literacy","High literacy")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output

#save as jpg
ggsave(
  file = "fig8.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


#statsanalysis for ethnicity
stats_analysis <- stats_LDA(model = model_benefits, #LDA model create
                            dtm = dtm_benefits, #document-term matrix used for the model
                            model_map = model_map_benefits, #data frame mapping the topics to their new labels
                            target_var = "Ethnicity", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("White","Mixed","Asian","Black","Other","Skipped")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig9.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


#Challenges 
#statsanalysis for gender
stats_analysis <- stats_LDA(model = model_challenges, #LDA model create
                            dtm = dtm_challenges, #document-term matrix used for the model
                            model_map = model_map_challenges, #data frame mapping the topics to their new labels
                            target_var = "Gender", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("Male","Female")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig10.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for Age groups
stats_analysis <- stats_LDA(model = model_challenges, #LDA model create
                            dtm = dtm_challenges, #document-term matrix used for the model
                            model_map = model_map_challenges, #data frame mapping the topics to their new labels
                            target_var = "Age_group", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("18-24","25-34","35-44","45-54","55+")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
view(stats_analysis$stats)
#save as jpg
ggsave(
  file = "fig11.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for country 
stats_analysis <- stats_LDA(model = model_challenges, #LDA model create
                            dtm = dtm_challenges, #document-term matrix used for the model
                            model_map = model_map_challenges, #data frame mapping the topics to their new labels
                            target_var = "Country", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("UK","IT","SE","DE")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig12.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#statsanalysis for literacy
stats_analysis <- stats_LDA(model = model_challenges, #LDA model create
                            dtm = dtm_challenges, #document-term matrix used for the model
                            model_map = model_map_challenges, #data frame mapping the topics to their new labels
                            target_var = "High_literacy", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("Low literacy","High literacy")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
view(stats_analysis$stats)
#save as jpg
ggsave(
  file = "fig13.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


#statsanalysis for ethnicity
stats_analysis <- stats_LDA(model = model_challenges, #LDA model create
                            dtm = dtm_challenges, #document-term matrix used for the model
                            model_map = model_map_challenges, #data frame mapping the topics to their new labels
                            target_var = "Ethnicity", #Name of the variable that should be used to split data by, should be one of the columns in the dataframe demographic vars
                            map = YouGov_demographics, #data frame containing the demographic variables
                            labels = c("White","Mixed","Asian","Black","Other","Skipped")
)
stats_analysis$plot #View plot 
stats_analysis$stats #View statistical test output
#save as jpg
ggsave(
  file = "fig14.jpg",
  plot = last_plot(),
  device = NULL,
  path = "~/Desktop/Dissertation/Figures",
  scale = 1.1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  
  
  
