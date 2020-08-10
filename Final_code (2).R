#################FInal Code###########################
library(xlsx)
Data<-read.xlsx('C&C.xlsx',as.data.frame = T,sheetName ="Stream")
Data<-Data[,1:35]
Data<-Data[,-c(19:22)]

#####################Remove duplicates###################
Data<-read.csv('C&C.csv',stringsAsFactors = F)
Data<-Data[!duplicated(Data$Sound.Bite.Text),]


sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
entity_annotator <- Maxent_Entity_Annotator(language="en",kind="person",probs=TRUE)
entity_annotator
while(len ==0){
  print(i)
  s=as.String(Data$Sound.Bite.Text[i])
  print(s)
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
annotate(s, entity_annotator, a2)

entity_annotator(s, a2)
persons = s[entity_annotator(s,a2)]
len=length(persons)
i=i+1
}
annotate(s, Maxent_Entity_Annotator(probs = TRUE), a2)



#######################preparing for sentimnet analysis####################
some_txt=Data$Sound.Bite.Text
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", " ", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", " ", some_txt)
some_txt = gsub("^\\s+|\\s+$", " ", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

length(some_txt)
size=822
system.time(sentiments_raw<-sapply(some_txt[1:size],function(x){
  return(c(calculate_total_presence_sentiment(x)[2,]))
}))

sentiments_DT=data.table(t(matrix(sentiments_raw,nrow=6)))
names(sentiments_DT)<-c("Sarcasm","Neutral","Negative","Positive","Very Negative","Very Positive")
for (nm in names(sentiments_DT)){
  sentiments_DT[[nm]]<-as.numeric(sentiments_DT[[nm]])
}
save(file="sentiments.RData",list=c("sentiments_DT"))

colSums(sentiments_DT)
stats=data.table()
Data$Total.Engagements<-as.numeric(Data$Total.Engagements)
Data$Total.Engagements[is.na(Data$Total.Engagements)]<-0
for (nm in names(sentiments_DT)){
nb=sum(sentiments_DT[[nm]])
stats=rbind(stats,data.table(name=nm,fav_average=sum(sentiments_DT[[nm]]*Data$Total.Engagements[1:822])/nb,
                               volume=nb))
}
stats=stats[stats$volume>0]
stats$name=as.factor(stats$name)

stats
plot(x=stats$name,y=stats$fav_average)

tweets_sample=cbind(Data[1:size,],sentiments_DT)
weights=c("sarcasm"=-2,"neutral"=0,"Negative"=-1,"positive"=1,"very_negative"=-2,"very_positive"=2)
weights

score=apply(sentiments_DT,1,function(x){
  sum(x*weights)
})
head(score)

tweets_sample$sentiment_score=score
tweets_sample$tweet_length=nchar(as.character(tweets_sample$Sound.Bite.Text))
names(tweets_sample)

modelling_var=c("Total.Engagements","tweet_length")

for (nm in modelling_var){
  tweets_sample[[nm]]=as.numeric(as.character(tweets_sample[[nm]]))
}

tweets_sample$Total.Engagements<-log(tweets_sample$Total.Engagements)

train_sample=sample(1:size,round(0.7*size))
gbm_params=c(shrinkage=0.002,nb_trees=500,depth=2)
summary(tweets_sample)



###############Buidling Model#####################
gbm_model=gbm(sentiment_score ~ Total.Engagements+tweet_length,
              data=tweets_sample[train_sample,],shrinkage=gbm_params[1],
              n.trees=gbm_params[2],interaction.depth=gbm_params[3],verbose=TRUE,
              train.fraction=0.7)
summary(gbm_model)

plot(gbm_model,i.var=1)

pred = predict(newdata=tweets_sample[-train_sample,],object=gbm_model)

pred_sign = pred
pred_extreme = abs(pred)

obs = tweets_sample[-train_sample]$sentiment_score
gini_sign = (roc.area(obs=1*(obs>0),pred=pred_sign)$A -1/2)*2
gini_extreme = (roc.area(obs=1*(abs(obs)>1),pred=pred_extreme)$A -1/2)*2
gini_sign
gini_extreme


strsplit_space_tokenizer=function(x)
  {unlist(strsplit(as.character(x), "[[:space:]]+"))}
ctrl=list(tokenize = strsplit_space_tokenizer,
          removePunctuation = list(preserve_intra_word_dashes = TRUE),
          stemming = TRUE,
          wordLengths = c(4, Inf),
          language="en")
TF_CTRL=termFreq(as.character(tweets_sample$Sound.Bite.Text), control = ctrl)
findMostFreqTerms(TF_CTRL,n = 300)

topics_number=10
topics = LDA(x = TF_CTRL,k = topics_number,method="Gibbs")
summary(topics)
library(tidytext)
library(ggplot2)

getTopicToClaimDistribution = function(ldaModel){
  ldaGamma <- tidytext::tidy(ldaModel, matrix = "gamma")
  ldaGamma
}
getTopicToClaimDistribution(topics)

lda_inf = posterior(topics,tweets_sample$Sound.Bite.Text)

#####Load Libraries##################
library(tm)
library("wordcloud")
library("RColorBrewer")
library(ggplot2)
library(qdap)
library(RWeka)
library(tm)
library("SnowballC")
library(textstem)
library(metricsgraphics)
library(magrittr)
library(broom)
library(tidyr)
library(RWeka)
library(tidytext)
library(dplyr)

#####Read CSV
Data<-read.csv('Coffee and beverages.csv',stringsAsFactors = F)
Data<-Food_BU
################################Unigram Analysis#############################################
Data_prepr<-VectorSource(Data1$CONTENT)
corpus<-VCorpus(Data_prepr)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(gsub), pattern ="[^[:alnum:][:space:]'#]",replacement= "")

#corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus<-lemmatize_words(corpus)

#myDf <- data.frame(text = sapply(corpus, paste, collapse = " "), stringsAsFactors = FALSE)
all_dtm <- TermDocumentMatrix(corpus)
all_dtm_s<-removeSparseTerms(all_dtm,0.98)
all_m <- as.matrix(all_dtm_s)
#write.csv(all_m,'DTM.csv')
v <- sort(rowSums(all_m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
write.csv(d,'frequenyw.csv')

#d$rank<-1:13447
par(bg="grey30")
png(file="WordCloud.png",width=1000,height=1000, bg="grey30")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
dev.off()

############################Bigram Analysis############################
tokenizer<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
bigram_dtm<-DocumentTermMatrix(corpus,control=list(tokenize=tokenizer))
all_dtm_s<-removeSparseTerms(bigram_dtm,0.98)

bigram_dtm_m<-as.matrix(all_dtm_s)
freq<-colSums(bigram_dtm_m)
bi_words<-names(freq)
bigramfreq <- data.frame(word = names(freq),freq=freq)

write.csv(bigramfreq,'bigramfrequenyw.csv')

tail(bi_words)
par(bg="grey30")
png(file="WordCloud_bi.png",width=1000,height=700, bg="grey30")
wordcloud(bi_words,freq,max.words = 300,col=terrain.colors(length(bi_words), alpha=0.9), random.order=FALSE, rot.per=0.3)
title(main = "Instagram most used words", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

###################################finding Polarity###############################

myDf <- data.frame(text = sapply(corpus, paste, collapse = " "), stringsAsFactors = FALSE)
#myDf<-cbind(myDf,Insta[,3:10])
#myDf<-myDf %$% polarity(text)
conversation_st <- myDf %$% polarity(text, Author.Location...State.Province.1)
conversation_ge <- myDf %$% polarity(text, Author.Gender)

counts(conversation_st)
plot(conversation_st)
plot(conversation_ge)


amplification.words<-qdapDictionaries::amplification.words
deamplification.words<-qdapDictionaries::deamplification.words
negation.words<-qdapDictionaries::negation.words
key.pol<-qdapDictionaries::key.pol
a<-polarity(text.var= myDf$text,
            grouping.var   = myDf$Author.Location...State.Province.1,
            polarity.frame = key.pol,
            negators       = negation.words,
            amplifiers     = amplification.words,
            deamplifiers   = deamplification.words
)
plot(a)

######### Sentiment R Polarity ##############
##############File read##################
data<-read.csv('Nutrition.csv')
library(lexicon)
library(sentimentr)
library(tm)
#corpus<-VectorSource(data$Sound.Bite.Text)
#corpus<-VCorpus(corpus)
#corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, content_transformer(gsub), pattern ="[^[:alnum:][:space:]'#]",replacement= "")

#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stripWhitespace)

sentence = gsub('[[:punct:]]', '', data$Sound.Bite.Text)
sentence = gsub('[[:cntrl:]]', '', sentence)

sentence = gsub('\\d+', '', sentence)
# and convert to lower case:
sentence = tolower(sentence)
sentence<-Corpus(VectorSource(sentence))
sentence <- tm_map(sentence, removeWords, stopwords("english"))

cleaned_content
myDf1 <- data.frame(text = sapply(Data$cleaned_content, paste, collapse = " "), stringsAsFactors = FALSE)
write.csv(myDf1,'bi.csv')
corpus<-lemmatize_words(corpus)

##########Dictionary Read Read#######################
Data<-read.csv('Cleaned1.3.csv',stringsAsFactors = F)
Data<-Data[!duplicated(Data$cleaned_content),]
a<-read.csv('Dictionary.csv')
Data<-Data[,1:25]
library(sentimentr)
new<-as_key(a,  sentiment = TRUE)
wechat_df$clean_D<-as.String(wechat_df$clean_D)

Senti <-  sentiment_by(wechat_df$clean_D,polarity_dt = new,by=NULL,
                       valence_shifters_dt = lexicon::hash_valence_shifters,hyphen = " ",
                       amplifier.weight = 0.6,  n.before = 4,n.after = 2,
                       question.weight = 0.7,adversative.weight = 0.5)
typ

Sentim<-cbind(data$Sound.Bite.Text,Senti)
## Rounding digits
## Scaling

Sentim$asentments <- sentimentr::general_rescale(Sentim$ave_sentiment,lower = -5,upper = 5,keep.zero = TRUE)


write.csv(Sentim,'Sentiments_file.csv')

#sentiment <- function(text.var, polarity_dt = lexicon::,
#                     valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
#                    amplifier.weight = .8, n.before = 5, n.after = 2, question.weight = 1,
#                   adversative.weight = .85, neutral.nonverb.like = FALSE, missing_value = 0, ...){

#  UseMethod('sentiment')

#}


###################LDA###############################
#General Analysis Insta
file<-read.csv('Coffee and beverages.csv',stringsAsFactors = F)
################################Unigram Analysis#############################################
corpus<-VectorSource(Data$Sound.Bite.Text)
library(stringr)

# corpus=str_replace_all(corpus,"[^[:graph:]]", " ")
corpus<-VectorSource(corpus)

corpus<-VCorpus(corpus)
corpus<-tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
#corpus = tm_map(corpus,removeWords,c(stopwords("english"),"maggi","nestle","#maggi","india","maggirodricks","priyabh","shownaj","dubeymouly","priya","like"))
myStopwords = c(stopwords('english'),"coffee","nescafe","classic","coffeetime","nestlé","nescafé")
# idx = which(myStopwords == "r")
# myStopwords = myStopwords[-idx]
corpus = tm_map(corpus, removeWords, myStopwords)
corpus <- tm_map(corpus, content_transformer(gsub), pattern ="[^[:alnum:][:space:]'#]",replacement= "")

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus<-lemmatize_words(corpus)

#myDf <- data.frame(text = sapply(corpus, paste, collapse = " "), stringsAsFactors = FALSE)
all_dtm <- DocumentTermMatrix(corpus)
# all_dtm_m<-as.matrix(all_dtm)
# sparse<-removeSparseTerms(all_dtm,0.98)
# all_dtm_s<-as.matrix(sparse)
# all_dtm_m_c<-as.data.frame(all_dtm_s)
# c<-colSums(all_dtm_s)
rowTotals <- apply(all_dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- all_dtm[rowTotals> 0, ]           #remove all docs without words

######Topic Extraction##########
library(topicmodels)

ap_lda <- LDA(dtm.new, k = 5, control = list(seed = 1234))
ap_lda
library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread


ap_documents <- tidy(ap_lda, matrix = "gamma")%>%arrange(desc(gamma))

ap_documents

tidy(all_dtm) %>%
  filter(document == 266) %>%
  arrange(desc(count))

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

gammaDF <- as.data.frame(ldaOut@gamma) 
names(gammaDF) <- c(1:5)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
top<-cbind(toptopics,Data$Sound.Bite.Text)
write.csv(top,'CandBtopics1.csv')
Data<-Data[1:20854,]
write.csv(Data,"final_d.csv")

###############################Machine Learning Analysis#####################

Data<-read.csv('Final_dump.csv',stringsAsFactors = F)

Data_prepr<-VectorSource(Data$Sound.Bite.Text)
corpus<-VCorpus(Data_prepr)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(gsub), pattern ="[^[:alnum:][:space:]'#]",replacement= "")

#corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus<-lemmatize_words(corpus)

#myDf <- data.frame(text = sapply(corpus, paste, collapse = " "), stringsAsFactors = FALSE)
all_dtm <- DocumentTermMatrix(corpus)

all_m <- as.matrix(all_dtm)
all_m_d<-data.frame(all_m)
all_dtm_s<-removeSparseTerms(all_dtm,0.98)
all_m <- as.matrix(all_dtm_s)
all_m_d<-data.frame(all_m)

#####Sample###############
library(caTools)
set.seed(123)
all_m_d_c<-cbind(all_m_d,Data$Sentiment.code)
names(all_m_d_c)[245]<-"Sentiment.code"
Sample<-sample.split(all_m_d,0.7)
Train<- subset(all_m_d_c,Sample==T)
Test<-subset(all_m_d_c,Sample==F)
Train$Sentiment.code<-as.factor(Train$Sentiment.code)
Test$Sentiment.code<-as.factor(Test$Sentiment.code)


##########Model Selection#######################
library(e1071)

Train<-lapply(Train[,-1], as.factor)
Model<-naiveBayes(Sentiment.code~.,Train[,-1])
class(Model)
summary(Model)
preds <- predict(Model, newdata = Test[,-1])
table(preds, Test$Sentiment.code)
library(caret)
confusionMatrix(table(preds, Test$Sentiment.code))
