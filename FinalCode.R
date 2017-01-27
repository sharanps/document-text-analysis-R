EA_URL <- c("http://www.sec.gov/Archives/edgar/data/712515/000119312509129576/ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312510142277/ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312511162968/ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312512265133/d354736ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312513259589/d510167ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312514236264/d740192ddef14a.htm","http://www.sec.gov/Archives/edgar/data/712515/000119312515237104/d939178ddef14a.htm")
QA_Start <- "Why am I receiving these materials?"
QA_End <- "www.wellsfargo.com/shareownerservices."

Final_QA_Consolidation <- NULL

for(i in 1:length(EA_URL))
{
  raw_Data = readLines(EA_URL[i]);
  start_Line = grep(QA_Start,raw_Data,ignore.case = FALSE);
  end_Line = grep(QA_End,raw_Data,ignore.case = FALSE);
  QA_Section <- raw_Data[start_Line:end_Line];
  
  fileName <- paste(i,'.txt');
  print(start_Line);
  print(end_Line);
  cat(QA_Section, file=fileName, append = TRUE);
  QA_Section_Clean <- htmlToText(fileName)
  
  cat(QA_Section_Clean, file=fileName, append = FALSE);
  
  QA_Section_Latin <- readChar(fileName, file.info(fileName)$size);
  
  QA_ASCII <- iconv(x = QA_Section_Latin, from = "latin1", to = "ASCII", "");
  
  Final_QA_Consolidation[i] <- QA_ASCII ;
  
}

summary(Final_QA_Consolidation)

QA_Vector <- VectorSource(Final_QA_Consolidation)

QA_Corpus <- Corpus(QA_Vector)

inspect(QA_Corpus)

QA_Corpus <- tm_map(QA_Corpus, tolower)
QA_Corpus <- tm_map(QA_Corpus, removePunctuation)
QA_Corpus <- tm_map(QA_Corpus, removeNumbers)
QA_Corpus <- tm_map(QA_Corpus, removeWords,stopwords("english"))

library("SnowballC")

QA_Corpus <- tm_map(QA_Corpus, stemDocument)
detach("package:SnowballC", unload=TRUE)

QA_Corpus <- tm_map(QA_Corpus,PlainTextDocument)
QA_Corpus_1 <- DocumentTermMatrix(QA_Corpus)

QA_TDM <- as.matrix(QA_Corpus_1)
dim(QA_TDM)

write.csv(tdm,"tdm.csv") 

findFreqTerms(x=QA_Corpus_1, lowfreq = 100)
findFreqTerms(x=QA_Corpus_1, lowfreq = 200)
findFreqTerms(x=QA_Corpus_1, lowfreq = 300)
findFreqTerms(x=QA_Corpus_1, lowfreq = 400)
findFreqTerms(x=QA_Corpus_1, lowfreq = 495)
findFreqTerms(x=QA_Corpus_1, lowfreq = 500)

findAssocs(x=QA_Corpus_1,term="vote",corlimit=0.6)
findAssocs(x=QA_Corpus_1,term="vote",corlimit=0.5)

findFreqTerms(x=QA_Corpus_1, lowfreq = 100)

findAssocs(x=QA_Corpus_1,term="annual",corlimit=0.6)

findFreqTerms(x=QA_Corpus_1, lowfreq = 50)

findAssocs(x=QA_Corpus_1,term="resignation",corlimit=0.6)

QA_removesparse_1 <- removeSparseTerms(QA_Corpus_1,0.3)

QA_removesparse_1

QA_freq <- sort(colSums(as.matrix(QA_Corpus_1)),decreasing = TRUE)
 head(freq,14)

 QA_wf <- data.frame(word=names(QA_freq),QA_freq=QA_freq)
 head(QA_wf)

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("dplyr")

subset(QA_wf,QA_freq>200) %>% ggplot(aes(word,QA_freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))

library("wordcloud")

set.seed(123)

wordcloud(names(QA_freq),QA_freq,min.freq=100,random.color = TRUE, colors=rainbow(7),random.order = FALSE)
