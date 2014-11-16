library(RCurl) 
require(twitteR)
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#twitter API
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "HT2dG6WE9MUrsf88w7E81TczL"
consumerSecret <- "mLKJTIrP8hWTjWHL1G5i9aFJICv4r6RPenaB9JynSwmeNdU7mE"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
twitCred$handshake(cainfo="cacert.pem")
save(twitCred, file="twitter authentication.Rdata")
registerTwitterOAuth(twitCred)



#train
#load data
posTweet1 <- read.csv(file='C:/Users/Yu/Desktop/CApositive.csv', header=FALSE, stringsAsFactors=FALSE)
posTweet <- posTweet1$V1
negTweet1 <- read.csv(file='C:/Users/Yu/Desktop/CAnegative.csv', header=FALSE, stringsAsFactors=FALSE)
negTweet <- negTweet1$V1
neuTweet1 <- read.csv(file='C:/Users/Yu/Desktop/CAneutral.csv', header=FALSE, stringsAsFactors=FALSE)
neuTweet <- neuTweet1$V1
all_Tweet1 <- rbind(posTweet1, negTweet1, neuTweet1)
all_Tweet <-all_Tweet1$V1

# prior
pos_Tweet <- 97/(17+78+97)
neu_Tweet <- 78/(17+78+97)
neg_Tweet <- 17/(17+78+97) 
#pos_Tweet <- 0.45
#neu_Tweet <- 0.3
#neg_Tweet <- 1-neu_Tweet


token <- function(sentence){
  #remove unnecessary characters and split up by word 
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence <- gsub('[[:cntrl:]]', '', sentence)
  sentence <- gsub('\\d+', '', sentence)
  sentence <- tolower(sentence)
  wordList <- str_split(sentence, '\\s+')
  words <- unlist(wordList)
  return(words)
}  


post_count1<- as.data.frame(token(posTweet))
post_count <- table(post_count1)
neut_count1<- as.data.frame(token(neuTweet))
neut_count <- table(neut_count1)
negt_count1<- as.data.frame(token(negTweet))
negt_count <- table(post_count1)
allt_count1<- as.data.frame(token(all_Tweet))
allt_count <- table(allt_count1)


#get and process tweet data
#temp_tweet1=searchTwitter("captain america", n=2000, lang="en")
temp_tweet = read.csv(file='C:/Users/Yu/Desktop/tweetexploration.csv', header=FALSE, stringsAsFactors=FALSE)
#tweets_df = twListToDF(temp_tweet)
#tweets_df = tweets_df$text
#tweets_df = twListToDF(tweets_df)
tweets_df1 <- as.matrix(cbind(temp_tweet, 'predict'))


sim <- as.numeric(length(tweets_df))
for(i in 1:sim){
  process <- token(tweets_df[i])
  #  process_table <- table(process)
  iter <- as.numeric(length(process))
  pos_like <- 1
  neg_like <- 1
  neu_like <- 1
  vocal <- c("","captain","america","the","i")
  for(j in 1:iter){
    if(is.na(match(process[j],vocal))){
      count_pos <- as.numeric(length(grep(process[j],posTweet)))
      count_neg <- as.numeric(length(grep(process[j],negTweet)))
      count_neu <- as.numeric(length(grep(process[j],neuTweet)))
      pos_like <-as.numeric(pos_like*(count_pos+1)/(length(all_Tweet)+97))
      neg_like <-as.numeric(neg_like*(count_neg+1)/(length(all_Tweet)+17))
      neu_like <-as.numeric(neu_like*(count_neu+1)/(length(all_Tweet)+78))
    }
    pos_result <- pos_like * pos_Tweet
    neg_result <- neg_like * neg_Tweet
    neu_result <- neu_like * neu_Tweet
    max_pro <- max(pos_result, neg_result, neu_result)
    if(max_pro == pos_result){
      tweets_df1[i,3]="positive"
    }else if(max_pro == neg_result){
      tweets_df1[i,3]="negative"
    }else{
      tweets_df1[i,3]="neutral"
    }
  }
}


contablet <- table(tweets_df1[,2],tweets_df1[,3],dnn=list('actual','predicted'))
contablet

tempp<-as.data.frame(tweets_df1)
# plot distribution of emotions
ggplot(tempp, aes(x=tempp$V2)) +
  geom_bar(aes(y=..count.., fill=tempp$V2)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="sentiment categories", y="number of tweets") +
  opts(title = "Sentiment Analysis of Tweets about Captain America\n")