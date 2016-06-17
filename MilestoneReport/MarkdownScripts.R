setwd("C:/D/R/Capstone Project/Coursera-SwiftKey/final/en_US")
library(quanteda)
library(dplyr)

rm(list=ls())

#load files

con <- file("./en_US.blogs.txt","r")
en_US_blogs <- readLines(con)
close(con)

con <- file("./en_US.news.txt","r")
en_US_news <- readLines(con)
close(con)

con <- file("./en_US.twitter.txt","r")
en_US_twitter <- readLines(con)
close(con)

datasize<- data.frame(dataset=c("en_US.blogs","en_US.news","en_US.twitter"),instances=c(length(en_US_blogs),length(en_US_news),length(en_US_twitter)))


#data cleansering & transformation
# 1. remove foreign charactors
# 2. remove white space
# 3. change to lower case

data_clean <- function(x){
	x <- gsub("[^([:alnum:]|[:punct:])]"," ",x)
	x <- gsub("[âãð]"," ",x)
	x <- gsub("\\s+"," ",x)
	x <- gsub("^\\s+|\\s+$","",x)
	x <- tolower(x)
	x
}

en_US_blogs <- data_clean(en_US_blogs)
en_US_news <- data_clean(en_US_news)
en_US_twitter <- data_clean(en_US_twitter)


# 4. get profanity list

con <- file("../../../profanity_from_google.csv","r")
profanity_list <- readLines(con)
close(con)

profanity_list <- data_clean(profanity_list)

b_tokenized<- tokenize(en_US_blogs, removeNumbers = TRUE, removePunct = FALSE, removeTwitter = TRUE)
n_tokenized<- tokenize(en_US_news, removeNumbers = TRUE, removePunct = FALSE, removeTwitter = TRUE)
t_tokenized<- tokenize(en_US_twitter, removeNumbers = TRUE, removePunct = FALSE, removeTwitter = TRUE)


#word count, stop word count and profanity count


b_wordcount<- sum(ntoken(b_tokenized))
n_wordcount<- sum(ntoken(n_tokenized))
t_wordcount<- sum(ntoken(t_tokenized))

b_profanity<- selectFeatures(b_tokenized, features=profanity_list, selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()

n_profanity<- selectFeatures(n_tokenized, features=profanity_list, selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()

t_profanity<- selectFeatures(t_tokenized, features=profanity_list, selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()

b_stopword<- selectFeatures(b_tokenized, features=stopwords("english"), selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()

n_stopword<- selectFeatures(n_tokenized, features=stopwords("english"), selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()

t_stopword<- selectFeatures(t_tokenized, features=stopwords("english"), selection = "keep",valuetype="fixed") %>% ntoken() %>% sum()
  
datasize$word.count <- c(b_wordcount,n_wordcount,t_wordcount)

datasize$stopword <- c(paste("",round(b_stopword*100/b_wordcount,2),"%"),paste("",round(n_stopword*100/n_wordcount,2),"%"),paste("",round(t_stopword*100/t_wordcount,2),"%"))

datasize$profanity <- c(paste("",round(b_profanity*100/b_wordcount,2),"%"),paste("",round(n_profanity*100/n_wordcount,2),"%"),paste("",round(t_profanity*100/t_wordcount,2),"%"))

#build ngrams

S_en_US_blogs <- sample(en_US_blogs, length(en_US_blogs)/100)
S_en_US_news <- sample(en_US_news, length(en_US_news)/100)
S_en_US_twitter <- sample(en_US_twitter, length(en_US_twitter)/100)

S_combined <- c(S_en_US_blogs,S_en_US_news,S_en_US_twitter)

S_tokenized<- tokenize(S_combined, removeNumbers = TRUE, removePunct = TRUE, removeTwitter = TRUE)
S_tokenized<- selectFeatures(S_tokenized, features=profanity_list, selection = "remove",valuetype="fixed")
S_tokenized_nostop <- selectFeatures(S_tokenized, features=stopwords("english"), selection = "remove")

S_unigram <- unlist(ngrams(S_tokenized, n = 1L, skip = 0L, concatenator = " "))

S_unigram <- data.frame(Unigram=S_unigram)

S_unigram_agg <- S_unigram %>% group_by(Unigram) %>% summarise(count=n()) %>% arrange(desc(count))

S_unigram_ns <- unlist(ngrams(S_tokenized_nostop, n = 1L, skip = 0L, concatenator = " ",ignoredFeatures = stopwords("english")))

S_unigram_ns  <- data.frame(Unigram=S_unigram_ns)

S_unigram_ns_agg <- S_unigram_ns %>% group_by(Unigram) %>% summarise(count=n()) %>% arrange(desc(count))

S_bigram <- unlist(ngrams(S_tokenized, n = 2L, skip = 0L, concatenator = " "))

S_bigram <- data.frame(Bigram=S_bigram)

S_bigram_agg <- S_bigram %>% group_by(Bigram) %>% summarise(count=n()) %>% arrange(desc(count))

S_bigram_ns <- unlist(ngrams(S_tokenized_nostop, n = 2L, skip = 0L, concatenator = " ",ignoredFeatures = stopwords("english")))

S_bigram_ns  <- data.frame(Bigram=S_bigram_ns)

S_bigram_ns_agg <- S_bigram_ns %>% group_by(Bigram) %>% summarise(count=n()) %>% arrange(desc(count))


S_trigram <- unlist(ngrams(S_tokenized, n = 3L, skip = 0L, concatenator = " "))

S_trigram <- data.frame(Trigram=S_trigram)

S_trigram_agg <- S_trigram %>% group_by(Trigram) %>% summarise(count=n()) %>% arrange(desc(count))

S_trigram_ns <- unlist(ngrams(S_tokenized_nostop, n = 3L, skip = 0L, concatenator = " ",ignoredFeatures = stopwords("english")))

S_trigram_ns  <- data.frame(Trigram=S_trigram_ns)

S_trigram_ns_agg <- S_trigram_ns %>% group_by(Trigram) %>% summarise(count=n()) %>% arrange(desc(count))


#plotting

library(ggplot2)

plotTable <- within(S_unigram_agg[1:20,], 
                   Unigram <- reorder(Unigram,count))

ggplot(plotTable,aes(x=Unigram,y=count))+geom_bar(stat="identity")+coord_flip()

plotTable <- within(S_bigram_agg[1:20,], 
                   Bigram <- reorder(Bigram,count))

ggplot(plotTable,aes(x=Bigram,y=count))+geom_bar(stat="identity")+coord_flip()

plotTable <- within(S_trigram_agg[1:20,], 
                   Trigram <- reorder(Trigram,count))

ggplot(plotTable,aes(x=Trigram,y=count))+geom_bar(stat="identity")+coord_flip()

plotTable <- within(S_unigram_ns_agg[1:20,], 
                   Unigram <- reorder(Unigram,count))

ggplot(plotTable,aes(x=Unigram,y=count))+geom_bar(stat="identity")+coord_flip()

plotTable <- within(S_bigram_ns_agg[1:20,], 
                   Bigram <- reorder(Bigram,count))

ggplot(plotTable,aes(x=Bigram,y=count))+geom_bar(stat="identity")+coord_flip()

plotTable <- within(S_trigram_ns_agg[1:20,], 
                   Trigram <- reorder(Trigram,count))

ggplot(plotTable,aes(x=Trigram,y=count))+geom_bar(stat="identity")+coord_flip()

#statistics on the ngrams model

ngrem_prof <- data.frame(type=c("Unigram","Bigram","Trigram","Unigram_nostop","Bigram_nostop","Trigram_nostop"))
ngrem_prof$Tokens<- c(sum(S_unigram_agg$count),sum(S_bigram_agg$count),sum(S_trigram_agg$count),sum(S_unigram_ns_agg$count),sum(S_bigram_ns_agg$count),sum(S_trigram_ns_agg$count))
ngrem_prof$unique.terms<- c(nrow(S_unigram_agg),nrow(S_bigram_agg),nrow(S_trigram_agg),nrow(S_unigram_ns_agg),nrow(S_bigram_ns_agg),nrow(S_trigram_ns_agg))

findb <- function(x,p){
  csum <- cumsum(x[,2])
  
  total <- sum(x[,2])
  
  index <- min(which(csum*100/total>=p))
  
  index
  
}

ngrem_prof$coverage.50 <- c(findb(S_unigram_agg,50),findb(S_bigram_agg,50),findb(S_trigram_agg,50),findb(S_unigram_ns_agg,50),findb(S_bigram_ns_agg,50),findb(S_trigram_ns_agg,50))

ngrem_prof <- ngrem_prof %>% mutate( percent.50 =paste("",round(coverage.50*100/unique.terms,2),"%"))

ngrem_prof$coverage.90 <- c(findb(S_unigram_agg,90),findb(S_bigram_agg,90),findb(S_trigram_agg,90),findb(S_unigram_ns_agg,90),findb(S_bigram_ns_agg,90),findb(S_trigram_ns_agg,90))

ngrem_prof <- ngrem_prof %>% mutate( percent.90 =paste("",round(coverage.90*100/unique.terms,2),"%"))

