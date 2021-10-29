library(graphics)
library(purrr)
library(stringr) 
library(tm)
library(syuzhet)
library(textclean)
rm(list=ls())

##reading the data
database_15_1 <- read_csv("3.csv")


#sampling
samp <- sample(1:nrow(database_15_1), 500)
samp_database_15_1 <-  database_15_1[samp, ]

#write_xlsx(samp_database_15_1,"samp_database_15_1.xlsx")
#tarjome ro dasti kardam
#reading the samp_database_14_3.xlsx
newdata <- read_excel("samp_database_15_1.xlsx")

#data cleaning
new <- newdata %>% 
  dplyr::select(index, mention_Arthaud, mention_Asselineau, mention_Cheminade, 
                mention_Dupont_Aignan, mention_Fillon, mention_Hamon, mention_Lassalle, `mention_Le Pen` , mention_Macron, 
                mention_Melenchon , mention_Poutou, translation)

df_Arthaud <- new %>% filter(mention_Arthaud == 1)     

df_Asselineau <- new %>% filter(mention_Asselineau == 1)

df_Cheminade <- new %>% filter(mention_Cheminade == 1)   

df_Dupont_Aignan <- new %>% filter(mention_Dupont_Aignan == 1)

df_Fillon <- new %>% filter(mention_Fillon == 1)

df_Hamon <- new %>% filter(mention_Hamon == 1)

df_Lassalle <- new %>% filter(mention_Lassalle == 1)

df_LEPEN <- new %>% filter(`mention_Le Pen` == 1)

df_macron <- new %>% filter(mention_Macron == 1)

df_Melenchon <- new %>% filter(mention_Melenchon == 1)

df_Poutou <- new %>% filter(mention_Poutou == 1)

###############################   MACRON    ####################################
#data cleaning for df_macron
twitterCorpus_macron <-Corpus(VectorSource(df_macron$translation))
inspect(twitterCorpus_macron[1:501])
twitterCorpus_macron<- tm_map(twitterCorpus_macron, content_transformer(tolower))
twitterCorpus_macron<- tm_map(twitterCorpus_macron,removeWords,stopwords("en"))
twitterCorpus_macron<- tm_map( twitterCorpus_macron,removeNumbers)
twitterCorpus_macron<- tm_map( twitterCorpus_macron,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_macron<- tm_map(twitterCorpus_macron,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_macron<- tm_map(twitterCorpus_macron,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_macron<-tm_map(twitterCorpus_macron,content_transformer(removeNonAscii))

twitterCorpus_macron<- tm_map(twitterCorpus_macron,removeWords,c("amp","ufef",
                                                                 "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_macron<- tm_map(twitterCorpus_macron,stripWhitespace)

inspect(twitterCorpus_macron[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_macron<-get_nrc_sentiment(twitterCorpus_macron$content)
barplot(colSums(emotions_macron),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  MACRON    ####################################

###############################   LEPEN    ####################################
#data cleaning for df_LEPEN
twitterCorpus_lepen <-Corpus(VectorSource(df_LEPEN$translation))
inspect(twitterCorpus_lepen[1:501])
twitterCorpus_lepen<- tm_map(twitterCorpus_lepen, content_transformer(tolower))
twitterCorpus_lepen<- tm_map(twitterCorpus_lepen,removeWords,stopwords("en"))
twitterCorpus_lepen<- tm_map( twitterCorpus_lepen,removeNumbers)
twitterCorpus_lepen<- tm_map( twitterCorpus_lepen,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_lepen<- tm_map(twitterCorpus_lepen,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_lepen<- tm_map(twitterCorpus_lepen,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_lepen<-tm_map(twitterCorpus_lepen,content_transformer(removeNonAscii))

twitterCorpus_lepen<- tm_map(twitterCorpus_lepen,removeWords,c("amp","ufef",
                                                               "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_lepen<- tm_map(twitterCorpus_lepen,stripWhitespace)

inspect(twitterCorpus_lepen[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_lepen<-get_nrc_sentiment(twitterCorpus_lepen$content)
barplot(colSums(emotions_lepen),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)

###############################  END  --  LEPEN    ####################################

###############################   Asselineau    ####################################
#data cleaning for df_Asselineau
twitterCorpus_Asselineau <-Corpus(VectorSource(df_Asselineau$translation))
inspect(twitterCorpus_Asselineau[1:501])
twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau, content_transformer(tolower))
twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau,removeWords,stopwords("en"))
twitterCorpus_Asselineau<- tm_map( twitterCorpus_Asselineau,removeNumbers)
twitterCorpus_Asselineau<- tm_map( twitterCorpus_Asselineau,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Asselineau<-tm_map(twitterCorpus_Asselineau,content_transformer(removeNonAscii))

twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau,removeWords,c("amp","ufef",
                                                                         "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Asselineau<- tm_map(twitterCorpus_Asselineau,stripWhitespace)

inspect(twitterCorpus_Asselineau[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Asselineau<-get_nrc_sentiment(twitterCorpus_Asselineau$content)
barplot(colSums(emotions_Asselineau),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)

###############################  END  --  Asselineau    ####################################


###############################   Dupont_Aignan    ####################################
#data cleaning for df_Dupont_Aignan
twitterCorpus_Dupont <-Corpus(VectorSource(df_Dupont_Aignan$translation))
inspect(twitterCorpus_Dupont[1:501])
twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont, content_transformer(tolower))
twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont,removeWords,stopwords("en"))
twitterCorpus_Dupont<- tm_map( twitterCorpus_Dupont,removeNumbers)
twitterCorpus_Dupont<- tm_map( twitterCorpus_Dupont,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Dupont<-tm_map(twitterCorpus_Dupont,content_transformer(removeNonAscii))

twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont,removeWords,c("amp","ufef",
                                                                 "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Dupont<- tm_map(twitterCorpus_Dupont,stripWhitespace)

inspect(twitterCorpus_Dupont[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Dupont<-get_nrc_sentiment(twitterCorpus_Dupont$content)
barplot(colSums(emotions_Dupont),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)

###############################  END  --  Dupont_Aignan    ####################################

###############################   Melenchon    ####################################
#data cleaning for df_Melenchon
twitterCorpus_Melenchon <-Corpus(VectorSource(df_Melenchon$translation))
inspect(twitterCorpus_Melenchon[1:501])
twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon, content_transformer(tolower))
twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon,removeWords,stopwords("en"))
twitterCorpus_Melenchon<- tm_map( twitterCorpus_Melenchon,removeNumbers)
twitterCorpus_Melenchon<- tm_map( twitterCorpus_Melenchon,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Melenchon<-tm_map(twitterCorpus_Melenchon,content_transformer(removeNonAscii))

twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon,removeWords,c("amp","ufef",
                                                                       "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Melenchon<- tm_map(twitterCorpus_Melenchon,stripWhitespace)

inspect(twitterCorpus_Melenchon[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Melenchon<-get_nrc_sentiment(twitterCorpus_Melenchon$content)
barplot(colSums(emotions_Melenchon),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Melenchon   ####################################

###############################   Fillon    ####################################
#data cleaning for df_Fillon
twitterCorpus_Fillon <-Corpus(VectorSource(df_Fillon$translation))
inspect(twitterCorpus_Fillon[1:501])
twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon, content_transformer(tolower))
twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon,removeWords,stopwords("en"))
twitterCorpus_Fillon<- tm_map( twitterCorpus_Fillon,removeNumbers)
twitterCorpus_Fillon<- tm_map( twitterCorpus_Fillon,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Fillon<-tm_map(twitterCorpus_Fillon,content_transformer(removeNonAscii))

twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon,removeWords,c("amp","ufef",
                                                                 "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Fillon<- tm_map(twitterCorpus_Fillon,stripWhitespace)

inspect(twitterCorpus_Fillon[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Fillon<-get_nrc_sentiment(twitterCorpus_Fillon$content)
barplot(colSums(emotions_Fillon),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Fillon   ####################################

###############################   Lassalle    ####################################
#data cleaning for df_Lassalle
twitterCorpus_Lassalle <-Corpus(VectorSource(df_Lassalle$translation))
inspect(twitterCorpus_Lassalle[1:501])
twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle, content_transformer(tolower))
twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle,removeWords,stopwords("en"))
twitterCorpus_Lassalle<- tm_map( twitterCorpus_Lassalle,removeNumbers)
twitterCorpus_Lassalle<- tm_map( twitterCorpus_Lassalle,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Lassalle<-tm_map(twitterCorpus_Lassalle,content_transformer(removeNonAscii))

twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle,removeWords,c("amp","ufef",
                                                                     "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Lassalle<- tm_map(twitterCorpus_Lassalle,stripWhitespace)

inspect(twitterCorpus_Lassalle[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Lassalle<-get_nrc_sentiment(twitterCorpus_Lassalle$content)
barplot(colSums(emotions_Lassalle),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Lassalle   ####################################

###############################   Hamon    ####################################
#data cleaning for df_Hamon
twitterCorpus_Hamon <-Corpus(VectorSource(df_Hamon$translation))
inspect(twitterCorpus_Hamon[1:501])
twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon, content_transformer(tolower))
twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon,removeWords,stopwords("en"))
twitterCorpus_Hamon<- tm_map( twitterCorpus_Hamon,removeNumbers)
twitterCorpus_Hamon<- tm_map( twitterCorpus_Hamon,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Hamon<-tm_map(twitterCorpus_Hamon,content_transformer(removeNonAscii))

twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon,removeWords,c("amp","ufef",
                                                               "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Hamon<- tm_map(twitterCorpus_Hamon,stripWhitespace)

inspect(twitterCorpus_Hamon[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Hamon<-get_nrc_sentiment(twitterCorpus_Hamon$content)
barplot(colSums(emotions_Hamon),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Hamon   ####################################

###############################   Poutou    ####################################
#data cleaning for df_Poutou
twitterCorpus_Poutou <-Corpus(VectorSource(df_Poutou$translation))
inspect(twitterCorpus_Poutou[1:501])
twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou, content_transformer(tolower))
twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou,removeWords,stopwords("en"))
twitterCorpus_Poutou<- tm_map( twitterCorpus_Poutou,removeNumbers)
twitterCorpus_Poutou<- tm_map( twitterCorpus_Poutou,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Poutou<-tm_map(twitterCorpus_Poutou,content_transformer(removeNonAscii))

twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou,removeWords,c("amp","ufef",
                                                                 "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Poutou<- tm_map(twitterCorpus_Poutou,stripWhitespace)

inspect(twitterCorpus_Poutou[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Poutou<-get_nrc_sentiment(twitterCorpus_Poutou$content)
barplot(colSums(emotions_Poutou),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Poutou   ####################################

###############################   Cheminade    ####################################
#data cleaning for df_Cheminade
twitterCorpus_Cheminade <-Corpus(VectorSource(df_Cheminade$translation))
inspect(twitterCorpus_Cheminade[1:501])
twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade, content_transformer(tolower))
twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade,removeWords,stopwords("en"))
twitterCorpus_Cheminade<- tm_map( twitterCorpus_Cheminade,removeNumbers)
twitterCorpus_Cheminade<- tm_map( twitterCorpus_Cheminade,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade,content_transformer(removeURL))

removeURL<- function(x) gsub("edua[[:alnum:]]*", "", x)   
twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade,content_transformer(removeURL))

# remove non "American standard code for information interchange (curly quotes and ellipsis)"
#  using function from package "textclean"            

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus_Cheminade<-tm_map(twitterCorpus_Cheminade,content_transformer(removeNonAscii))

twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade,removeWords,c("amp","ufef",
                                                                 "ufeft","uufefuufefuufef","uufef","s"))  

twitterCorpus_Cheminade<- tm_map(twitterCorpus_Cheminade,stripWhitespace)

inspect(twitterCorpus_macron[1:501])



#Sentiment analysis
# find count of 8 emotional sentiments
emotions_Cheminade<-get_nrc_sentiment(twitterCorpus_Cheminade$content)
barplot(colSums(emotions_Cheminade),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)


###############################  END  --  Cheminade    ####################################
