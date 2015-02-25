#!/usr/bin/env Rscript
library("dplyr")
library("tm")


## Read Functions -----------------------------------------

readSMS <- function(filepath) {
    df <- read.csv(filepath, stringsAsFactors=FALSE)
    names(df) <- tolower(names(df))
    df$date <- as.POSIXct(df$date)
    return(df)
}

readSentimentDict <- function(filepath, dict=c("affin", "labmt")) {
    
    if (dict == "affin") {
        cat("Reading Affin word list...")
        con <- file(filepath)
        fileLines <- readLines(con)
        close(con)
        fileList <- strsplit(fileLines, split="\t")
        sentimentDict <- as.data.frame(do.call(rbind, fileList),
                                       stringsAsFactors=FALSE)
        sentimentDict <- preprocessAffinDict(sentimentDict)
        
    } else if (dict == "labmt") {
        cat("Reading LabMT word list...")
        sentimentDict <- read.csv(filepath, skip=3, sep="\t", header=TRUE,
                                  stringsAsFactors=FALSE)
        sentimentDict <- preprocessLabmtDict(sentimentDict)
    }
    cat("Done!\n")
    return(sentimentDict)
}

preprocessAffinDict <- function(dict) {
    
    names(dict) <- c("word", "score")
    dict$score <- as.numeric(dict$score)
    return(dict)
}

preprocessLabmtDict <- function(dict) {
    
    dict <- dict[, c("word", "happiness_average")]
    names(dict) <- c("word", "score")
    dict$score <- as.numeric(dict$score)
    dict$score <- dict$score - mean(dict$score)
    return(dict)
}

## Sentiment Analysis  -------------------------------------

getFullReadout <- function(data, sentimentDict, person=NULL) {
    
    cat("Calculating sentiment score...")
    sentimentScore <- getSentimentScore(data, sentimentDict, person)
    cat("Done!\n")
    cat("Calculating response statistics...")
    smsResponse <- getSMSResponse(data, person)
    cat("Done!\n")
    smsFullReadout <- merge(smsResponse, sentimentScore, by="key")
    return(smsFullReadout)
}

getSentimentScore <- function(data, sentimentDict, person=NULL) {
    
    if (!is.null(person)) {
        data <- data %>%
            filter(to %in% person | from %in% person)
    }
    smsScoreByReceiver <- getScoreByReceiver(data, sentimentDict)
    smsScoreBySender <- getScoreBySender(data, sentimentDict)
    sentimentScore <- merge(smsScoreByReceiver, smsScoreBySender, by="key") %>%
        select(key, sentScore, receivedScore) %>%
        mutate(scoreRatio = sentScore / receivedScore,
               scoreDifference = sentScore - receivedScore)
    return(sentimentScore)
}

getScoreByReceiver <- function(data, sentimentDict) {
    
    smsScoreByReceiver <- getScore(data, sentimentDict) %>%
        filter(from == "Me") %>%
        rename(sentScore = score, key = to)
    return(smsScoreByReceiver)
}

getScoreBySender <- function(data, sentimentDict) {
    
    smsScoreBySender <- getScore(data, sentimentDict) %>%
        filter(to == "Me") %>%
        rename(receivedScore = score, key = from)
    return(smsScoreBySender)
}

getScore <- function(data, sentimentDict) {
    
    smsScore <- data %>%
        group_by(from, to) %>%
        summarize(score = scoreTextSentiment(text, sentimentDict))
    return(smsScore)
}

scoreTextSentiment <- function(textVector, sentimentDict) {
    
    corpus <- preprocessCorpus(textVector)
    corpusDict <- makeCorpusDict(corpus)
    score <- scoreSentiment(corpusDict, sentimentDict)
    return(score)
}

scoreSentiment <- function(corpusDict, sentimentDict) {
    
    sentimentWordsIndex <- na.omit(match(corpusDict[, 1], sentimentDict[, 1]))
    sentimentWords <- sentimentDict[sentimentWordsIndex, 1]
    
    sentimentWordsScore <- sentimentDict[sentimentWordsIndex, 2]
    sentimentWordsFreq <- corpusDict[match(sentimentWords, corpusDict[, 1]), 2]
    score <- sum(sentimentWordsScore * sentimentWordsFreq) / sum(corpusDict[, 2])
    return(score)
}

preprocessCorpus = function(vec) {
    
    ## collapse vector of texts into one element
    textVector <- paste(vec, collapse=" ")
    # the directory must have all the relevant text files
    vs <- VectorSource(textVector)
    # Corpus will make a tm document corpus from this directory
    fp <- Corpus(vs)
    # make all words lower case
    fp <- tm_map(fp, content_transformer(tolower))
    # remove all punctuation
    fp <- tm_map(fp, removePunctuation)
    # remove stopwords like the, a, and so on.	
    fp <- tm_map(fp, removeWords, stopwords("english"))
    # remove extra whitespace
    fp <- tm_map(fp, stripWhitespace)	
    corpus <- unlist(fp)[["content.content"]]
    return(corpus)
}

makeCorpusDict <- function(corpus) {
    # This returns a dataframe that is sorted by the number of times 
    # a word appears
    
    # List of vectors to one big vetor
    fullDict <- unlist(strsplit(corpus, split=" ") )
    # Tabulates the full dictionary
    counts <- tabulate(factor(fullDict)) 
    # Find unique values
    dict <- unique(fullDict) 
    # Sort them alphabetically
    dict <- sort(dict)
    dictDF <- data.frame(word = dict, count = counts)
    corpusDict <- dictDF[order(dictDF$count, decreasing=TRUE), ]
    return(corpusDict)
}

## Quick Summary -------------------------------------

getSMSResponse <- function(data, person=NULL) {
    
    if (!is.null(person)) {
        data <- data %>%
            filter(to %in% person | from %in% person)
    }
    smsCountByReceiver <- getSMSCountByReceiver(data)
    smsCountBySender <- getSMSCountBySender(data)
    smsResponse <- merge(smsCountByReceiver, smsCountBySender, by="key") %>%
        select(key, received, sent) %>%
        mutate(totalMessages = sent + received,
               responseRatio = sent / received)
    return(smsResponse)
}

getSMSCountBySender <- function(data) {
    
    smsCountBySender <- getSMSCount(data) %>%
        filter(from == "Me") %>%
        rename(sent = count, key = to)
    return(smsCountBySender)
}

getSMSCountByReceiver <- function(data) {
    
    smsCountByReceiver <- getSMSCount(data) %>%
            filter(to == "Me") %>%
            rename(received = count, key = from)
    return(smsCountByReceiver)
}

getSMSCount <- function(data) {
    
    smsCount <- data %>%
        group_by(from, to) %>%
        summarize(count=n())
    return(smsCount)
}
