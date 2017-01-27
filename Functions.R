analyzeText <- function(){
  if(is.null(Final_QA_Consolidation)){
    print("Kindly run the Data Extraction Function");
  } else {
    library("tm");
    summary(Final_QA_Consolidation);
    QA_Vector <- VectorSource(Final_QA_Consolidation);
    QA_Corpus <- Corpus(QA_Vector);
    inspect(QA_Corpus);
    QA_Corpus <- tm_map(QA_Corpus, tolower);
    QA_Corpus <- tm_map(QA_Corpus, removePunctuation);
    QA_Corpus <- tm_map(QA_Corpus, removeNumbers);
    QA_Corpus <- tm_map(QA_Corpus, removeWords,stopwords("english"));
    library("SnowballC");
    QA_Corpus <- tm_map(QA_Corpus, stemDocument);
    detach("package:SnowballC", unload=TRUE);
    QA_Corpus <- tm_map(QA_Corpus,PlainTextDocument);
    QA_Corpus_1 <- DocumentTermMatrix(QA_Corpus);
    QA_TDM <- as.matrix(QA_Corpus_1);
    dim(QA_TDM);
    write.csv(QA_TDM,"tdm.csv");
    msg <- "Kindly Check the TextDocumentMatrix file generated in ";
    paste(msg, getwd());
  }
}

getText <- function(URL, Start, End){
  Final_QA_Consolidation <- NULL
  for(i in 1:length(URL))
  {
    start_Line = grep(Start,raw_Data,ignore.case = FALSE);
    end_Line = grep(End,raw_Data,ignore.case = FALSE);
    QA_Section <- raw_Data[start_Line:end_Line];
    fileName <- paste(i,'.txt');
    print(start_Line);
    print(end_Line);
    cat(QA_Section, file=fileName, append = TRUE);
    QA_Section_Clean <- htmlToText(fileName);
    cat(QA_Section_Clean, file=fileName, append = FALSE);
    QA_Section_Latin <- readChar(fileName, file.info(fileName)$size);
    QA_ASCII <- iconv(x = QA_Section_Latin, from = "latin1", to = "ASCII", "");
    Final_QA_Consolidation[i] <- QA_ASCII ;
  }
}