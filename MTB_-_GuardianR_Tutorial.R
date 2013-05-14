## GuardianR Tutorial ##
## MT Bastos 14/05/13 ##

# set working folder
# setwd("")

# install and load package
# install.packages("GuardianR")
library(GuardianR)

# check help files
?get_guardian

# search guardian
results <- get_guardian(keywords="thatcher", from.date="2013-04-09", to.date="2013-04-09")

# search guardian for three months 
results <- get_guardian(keywords="thatcher", from.date="2012-01-09", to.date="2013-04-09")

# try different keywords
results <- get_guardian(keywords="World+Cup+Brazil", from.date="2013-01-09", to.date="2013-04-09")

# search guardian for three months
results <- get_guardian(keywords="World+Cup+Brazil", from.date="2012-01-09", to.date="2013-04-09")

# retrieve full text of guardian articles
# Guardian API-key can be obtained by registering at http://guardian.mashery.com/
results <- get_guardian_full(keywords="World+Cup+Brazil", from.date="2012-01-09", to.date="2013-04-09", api.key=api.key)

# check full article of entry #7
results$body[7]

# search for corinthians
results <- get_guardian_full(keywords="corinthians+brazil", from.date="2012-01-09", to.date="2013-04-09", api.key=api.key)

# search for corinthians again (no full text)
results <- get_guardian(keywords="corinthians+brazil", from.date="2010-01-09", to.date="2013-04-09")

# read headlines
results[,10]

# DATA ANALYSIS
# install.packages("lubridate")
library(lubridate)

# convert ISO8601 format to POSIX
results$webPublicationDate <- ymd_hms(results$webPublicationDate, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)
results$lastModified <- ymd_hms(results$lastModified, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)
results$newspaperEditionDate <- ymd_hms(results$newspaperEditionDate, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)
results$commentCloseDate <- ymd_hms(results$commentCloseDate, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)

# save results in rda and csv
# save(results,file="guardian_data.rda")
# write.csv2(results, file="guardian_data.csv")

# exploratory plot - publication date
par(mfrow=c(1,2))
plot(results$webPublicationDate, as.numeric(as.character(results$wordcount)), col="blue",cex=1.5,xlab="Web Publication Time", ylab="Article Word Count")
plot(results$webPublicationDate, as.numeric(as.character(results$score)), col="blue",cex=1.5,xlab="Web Publication Time", ylab="Article Score")

# exploratory plot - newspaper sections
par(mfrow=c(1,2))
plot(results$sectionName, as.numeric(as.character(results$wordcount)), col="lightblue",cex=2.5,xlab="Newspaper Sections", ylab="Article Word Count")
barplot(as.numeric(as.character(results$wordcount, results$sectionName)),  horiz=TRUE, names.arg=results$sectionName)

# exploratory plot - comparison between sister papers
par(mfrow=c(1,2))
plot(as.numeric(as.character(results$wordcount))~results$publication, col="blue",xlab="Sister Papers", ylab="Article Word Count", main="Word Count per Sister Papers")
plot(table(results$publication), main="Number of Articles per Sister Papers", ylab="")

# exploratory plot - time series
# install.packages("ggplot2")
library(ggplot2)
results$wordcount <- as.numeric(as.character(results$wordcount))
ggplot(results)+geom_point(aes(x=webPublicationDate,y=wordcount)) + labs(x = "time", y = "wordcount", title="Time Series") + theme(axis.text.x = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=1))

# install.packages("wordcloud")
# install.packages("tm")
library(wordcloud)
library(tm)

# plot wordcloud
if(NROW(results)>5000) {
  results.sp<-sample(results, size=5000)
  results.corpus <- Corpus(VectorSource(results.sp$headline))
} else {
  results.corpus <- Corpus(VectorSource(results$headline))
}
results.corpus <- tm_map(results.corpus, tolower)
results.corpus <- tm_map(results.corpus, removePunctuation)
results.corpus <- tm_map(results.corpus, removeWords, stopwords("english"))
print(wordcloud(results.corpus,colors=brewer.pal(6,"Dark2"),random.order=FALSE))

# plot wordcloud of full text
results <- get_guardian_full(keywords="corinthians+brazil", from.date="2012-01-09", to.date="2013-04-09", api.key=api.key)
if(NROW(results)>5000) {
  results.sp<-sample(results, size=5000)
  results.corpus <- Corpus(VectorSource(results.sp$body))
} else {
  results.corpus <- Corpus(VectorSource(results$body))
}
results.corpus <- tm_map(results.corpus, tolower)
results.corpus <- tm_map(results.corpus, removePunctuation)
results.corpus <- tm_map(results.corpus, removeWords, stopwords("english"))
print(wordcloud(results.corpus,colors=brewer.pal(6,"Dark2"),random.order=FALSE))

# check results with maximum time frame
results <- get_guardian(keywords="corinthians+brazil", from.date="2000-01-09", to.date="2013-04-09")
results$webPublicationDate <- ymd_hms(results$webPublicationDate, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)
results$wordcount <- as.numeric(as.character(results$wordcount))
ggplot(results)+geom_point(aes(x=webPublicationDate,y=wordcount)) + labs(x = "time", y = "wordcount", title="Time Series") + theme(axis.text.x = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=1))
plot(results$webPublicationDate, results$wordcount)

# check results with maximum time frame
results <- get_guardian(keywords="sao+paulo+football", from.date="2000-01-09", to.date="2013-04-09")
results$webPublicationDate <- ymd_hms(results$webPublicationDate, quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), truncated = 0)
results$wordcount <- as.numeric(as.character(results$wordcount))
ggplot(results)+geom_point(aes(x=webPublicationDate,y=wordcount)) + labs(x = "time", y = "wordcount", title="Time Series") + theme(axis.text.x = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=1))
plot(results$webPublicationDate, results$wordcount)

# check summary of results
summary(results)

# cite source accordingly to avoid issues with Guardian copyright 
citation("GuardianR")
