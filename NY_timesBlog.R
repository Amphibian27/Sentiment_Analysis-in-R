# loading the data
Train = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
Test = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(Train)

#formatting date feature
Train$PubDate = strptime(Train$PubDate, "%Y-%m-%d %H:%M:%S")
Test$PubDate = strptime(Test$PubDate, "%Y-%m-%d %H:%M:%S")

# adding weekday feature
Train$Weekday = Train$PubDate$wday
Test$Weekday = Test$PubDate$wday

# coverting variables to factors
Train$NewsDesk=as.factor(Train$NewsDesk)
Train$SectionName=as.factor(Train$SectionName)
Train$SubsectionName=as.factor(Train$SubsectionName)
Train$Popular=as.factor(Train$Popular)
Train$Weekday=as.factor(Train$Weekday)

Test$NewsDesk=as.factor(Test$NewsDesk)
Test$SectionName=as.factor(Test$SectionName)
Test$SubsectionName=as.factor(Test$SubsectionName)
Test$Weekday=as.factor(Test$Weekday)

# realigning levels
levels(Test$NewsDesk) = levels(Train$NewsDesk)
levels(Test$SectionName)=levels(Train$SectionName)
levels(Test$SubsectionName)=levels(Train$SubsectionName)
levels(Test$Weekday)=levels(Train$Weekday)

# simple model without text variables
M1=glm(Popular~NewsDesk+SectionName+SubsectionName+Weekday,data=Train,family="binomial")
summary(M1)
PredM1 = predict(M1, newdata=Test, type="response")
#AUC=0.56293

# text analytics
library(tm)
library(SnowballC)

# headline corpus
CorpusH = Corpus(VectorSource(c(Train$Headline,Test$Headline)))
CorpusH = tm_map(CorpusH, tolower)
CorpusH = tm_map(CorpusH, PlainTextDocument)
CorpusH = tm_map(CorpusH, removePunctuation)
CorpusH = tm_map(CorpusH, removeWords,c(stopwords("english"),"will","new","york","times"))
CorpusH = tm_map(CorpusH, stemDocument)

dtmH = DocumentTermMatrix(CorpusH)
sparseH = removeSparseTerms(dtmH, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparseH))
colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))

HeadlineListTrain = head(HeadlineWords, nrow(Train))
HeadlineListTest = tail(HeadlineWords, nrow(Test))

#absract corpus
CorpusA = Corpus(VectorSource(c(Train$Abstract,Test$Abstract)))
CorpusA = tm_map(CorpusA, tolower)
CorpusA = tm_map(CorpusA, PlainTextDocument)
CorpusA = tm_map(CorpusA, removePunctuation)
CorpusA = tm_map(CorpusA, removeWords,c(stopwords("english"),"will","new","york","times"))
CorpusA = tm_map(CorpusA, stemDocument)

dtmA = DocumentTermMatrix(CorpusA)
sparseA = removeSparseTerms(dtmA, 0.99)
AbstractWords = as.data.frame(as.matrix(sparseA))
colnames(AbstractWords) = paste0("A", colnames(AbstractWords))

AbstractListTrain = head(AbstractWords, nrow(Train))
AbstractListTest = tail(AbstractWords, nrow(Test))

# word cloud
library(wordcloud)
library(RColorBrewer)
dtm=c(dtmA, dtmH)
m<- as.matrix(dtm)
v<- sort(colSums(m),decreasing=TRUE)
head(v,40)
words <- names(v)
d <- data.frame(word=words, freq=v)

col <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(6,.5),min.freq=5,max.words=250, color=col,random.order=F, rot.per=.15, vfont=c("sans serif","plain"))


# creating new train and test dataframe
TextTrain=cbind(HeadlineListTrain, AbstractListTrain,row.names=NULL)
TextTest=cbind(HeadlineListTest, AbstractListTest,row.names=NULL)

TextTrain$Popular<-Train$Popular
TextTrain$Weekday<-Train$Weekday
TextTrain$NewsDesk<-Train$NewsDesk
TextTrain$SectionName<-Train$SectionName
TextTrain$SubsectionName<-Train$SubsectionName
TextTrain$WordCount<-Train$WordCount

TextTest$Weekday<-Test$Weekday
TextTest$NewsDesk<-Test$NewsDesk
TextTest$SectionName<-Test$SectionName
TextTest$SubsectionName<-Test$SubsectionName
TextTest$WordCount<-Test$WordCount

#RandomForest model
library(randomForest)
M2=randomForest(Popular~.,data=TextTrain,nodesize=10,ntree=3500,importance=TRUE)
PredM2 = predict(M2, newdata=TextTest, type="prob")[,2]
# Test AUC=0.87627

importance(M2)
summary(importance(M2))

#selecting top quartile words
MeanDecreaseAccuracy<-as.table(importance(M2)[,3])
wordsAccuracy<-MeanDecreaseAccuracy[MeanDecreaseAccuracy>12.9942]
wordsAccuracy
wordsAccuracy<-as.table(sort(wordsAccuracy[wordsAccuracy<56.82428]))

