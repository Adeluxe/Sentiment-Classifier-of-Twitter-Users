
#load data
posText1 <- read.csv(file='C:/Users/Yu/Desktop/textpositive_train.csv', header=FALSE, stringsAsFactors=FALSE)
posText <- posText1$V2
negText1 <- read.csv(file='C:/Users/Yu/Desktop/textnegative_train.csv', header=FALSE, stringsAsFactors=FALSE)
negText <- negText1$V2
neuText1 <- read.csv(file='C:/Users/Yu/Desktop/textneutral_train.csv', header=FALSE, stringsAsFactors=FALSE)
neuText <- neuText1$V2
all_data1 <- rbind(posText1, negText1, neuText1)
all_data <-all_data1$V2

# prior
pos_prior <- 1200/(1200+1000+1200)
neu_prior <- 1200/(2000+600+800)
neg_prior <- 1000/(2000+600+800) 
#pos_prior <- 0.2
#neu_prior <- 0.4
#neg_prior <- 1-neu_prior - pos_prior


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


pos_count1<- as.data.frame(token(posText))
pos_count <- table(pos_count1)
neu_count1<- as.data.frame(token(neuText))
neu_count <- table(neu_count1)
neg_count1<- as.data.frame(token(negText))
neg_count <- table(pos_count1)
all_count1<- as.data.frame(token(all_data))
all_count <- table(all_count1)

testText1 <- read.csv(file='C:/Users/Yu/Desktop/exploration1.csv', header=FALSE, stringsAsFactors=FALSE)
testText <- testText1$V2
testText_df <- cbind(testText1$V2, testText1$V4)
testText_df1 <- as.matrix(cbind(testText_df, 'predict'))
colnames(testText_df1) <- c("sentence", "result", "predict")


point_pos <- as.numeric(length(posText))
point_neg <- as.numeric(length(negText))
point_neu <- as.numeric(length(neuText))

sim <- as.numeric(length(testText))
for(i in 1:sim){
  process <- token(testText[i])
  #  process_table <- table(process)
  iter <- as.numeric(length(process))
  pos_like <- 1
  neg_like <- 1
  neu_like <- 1
  vocal <- c("","the","a","and","of","to","is","s","it","in","that","as","with","its","for","this","an","nt","on","about","at","by","i","his","lrb","rrb","up","can","director","would","thier","us","way","which","through","was","her","from","be","are","were","all","into","he","she","me","does","we","makes","our","my","story","if","your","not","no","some","what","have","has","had","just","also","both","still","make")
  for(j in 1:iter){
    if(is.na(match(process[j],vocal))){
      count_pos <- as.numeric(length(grep(process[j],posText)))
      count_neg <- as.numeric(length(grep(process[j],negText)))
      count_neu <- as.numeric(length(grep(process[j],neuText)))
      pos_like <-as.numeric(pos_like*(count_pos+1)/(length(all_count)/20+1200))
      neg_like <-as.numeric(neg_like*(count_neg+1)/(length(all_count)/20+1000))
      neu_like <-as.numeric(neu_like*(count_neu+1)/(length(all_count)/20+1200))
    }
    pos_result <- pos_like * pos_prior
    neg_result <- neg_like * neg_prior
    neu_result <- neu_like * neu_prior
    max_pro <- max(pos_result, neg_result, neu_result)
    if(max_pro == pos_result){
      testText_df1[i,3]="positive"
    }else if(max_pro == neg_result){
      testText_df1[i,3]="negative"
    }else{
      testText_df1[i,3]="neutral"
    }
  }
}

contable <- table(testText_df1[,2],testText_df1[,3],dnn=list('actual','predicted'))
contable


tempp1<-as.data.frame(testText_df1)
# plot distribution of emotions
ggplot(tempp1, aes(x=tempp1$predict)) +
  geom_bar(aes(y=..count.., fill=tempp1$predict)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="sentiment categories", y="frequency") +
  opts(title = "Sentiment Analysis of movie reviews\n")


tempp2<-as.data.frame(testText_df1)
# plot distribution of emotions
ggplot(tempp1, aes(x=tempp1$result)) +
  geom_bar(aes(y=..count.., fill=tempp1$result)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="sentiment categories", y="frequency") +
  opts(title = "Sentiment Analysis of movie reviews\n")