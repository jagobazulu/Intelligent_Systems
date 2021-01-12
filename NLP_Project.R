---
  title: "Project_3"
---


getwd()
setwd("C:/Users/34652/Google Drive/2019-2020/UNI/UPM/INTELIGENT SYSTEMS/Natural Language Processing")

# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging

# The openNLPmodels.en library is not in CRAN; it has to be installed from another repository
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")

# Import libraries and functions

library(NLP)
library(openNLP) 
library(openNLPmodels.en)
library(tm)
library(stringr)



getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  parse_annotator <- Parse_Annotator()
  y3 <- annotate(x, parse_annotator, y2)
  return(y3)  
}

getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
} 

detectPatternOnDocument <- function(doc, pattern) {
  x=as.String(doc)
  res=str_match_all(x,pattern)
  
  dimrow=dim(res[[1]])[1]
  dimcol=dim(res[[1]])[2]
  
  # If there are no rows, no matches have been found
  if (dimrow == 0) {
    return(NA)
  }else{
    if (dimcol > 2){
      # If there are three or more columns, we have to paste all the groups together
      for (i in 1:dimrow) {
        res[[1]][i,2] = paste(res[[1]][i,2:dimcol], collapse = ' ')
      }
    }
    
    # We return all the results found separated by ','
    if (dimcol != 1) {
      result = paste(res[[1]][,2], collapse = ', ')
    }else{
      result = paste(res[[1]][,1], collapse = ', ')
    }
    return(result)
  }
}

detectPatternOnDocumentWithContext <- function(doc, pattern) {
  txt=as.String(doc)
  number=50
  coord=str_locate(txt,pattern)
  res3=substr(txt,coord[1]-number,coord[2]+number)
  return (res3)
}

detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(corpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

detectPatternsInTaggedCorpus = function(corpus, taggedCorpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(taggedCorpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, 
                                             nrow = length(names(df))-1))
  names(entityCountPerPattern) <- c("Entity","Count")
  
  for (i in 2:length(names(df))) {
    entityCountPerPattern$Entity[i-1] = names(df)[i]
    entityCountPerPattern$Count[i-1] = nrow(subset(df, !is.na(df[i])))
  }
  return (entityCountPerPattern)
}

countMatchesPerRow = function (df) {
  entityCountPerFile <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(entityCountPerFile) <- c("File","Count")
  
  for (i in 1:nrow(df)) {
    entityCountPerFile$File[i] = df$File[i]
    entityCountPerFile$Count[i] = length(Filter(Negate(is.na),df[i,2:length(df[i,])]))
  }
  return (entityCountPerFile[entityCountPerFile[2]!=0,])
}

printMatchesPerPattern = function (patterns, matches) {
  for (i in 1:length(patterns)){
    print(paste("PATTERN: ",patterns[i]))
    strings = matches[,i+1][!is.na(unlist(matches[,i+1]))]
    print(strings)
    print(" ") 
  }
}

mergeAllMatchesInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  for (i in 1:nrow(df)) {    
    matches=list()
    for (j in 2:ncol(df)){
      if (grepl(',',df[i,j])){
        b=strsplit(as.character(df[i,j]),split=',')
        for (j in 1:length(b[[1]])){
          matches= c(matches,str_trim(b[[1]][j]))
        }
      }else{
        if (!(is.na(df[i,j]))){
          matches = c(matches,str_trim(df[i,j]))
        }
      }
    }
    matches = unique(matches)
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}

mergeGoldStandardInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unlist(Filter(Negate(is.na),df[i,2:length(df)])))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}

calculateMetrics = function (matches, matches.gs) {
  
  metrics<- data.frame(matrix(NA, ncol = 3, nrow = 1))
  names(metrics) <- c("Precision","Recall","Fmeasure")
  
  numCorrect = 0
  allAnswers = 0
  possibleAnswers = 0
  
  for (i in 1:nrow(matches)) {    
    if (length(matches.gs$Matches[[i]])!=0) {
      l = str_trim(unlist(matches[i,2]))
      l.gs = unname(unlist(matches.gs[i,2]))
      intersection = intersect(l, l.gs)
      numCorrect = numCorrect + length(intersect(l, l.gs))
      allAnswers = allAnswers + length (l)
      possibleAnswers = possibleAnswers + length(l.gs)    
    }
  }
  
  metrics$Precision = numCorrect / allAnswers
  metrics$Recall = numCorrect / possibleAnswers
  
  beta = 1
  if ((metrics$Precision == 0) & (metrics$Recall == 0)) {
    metrics$Fmeasure = 0
  } else {
    metrics$Fmeasure = ((sqrt(beta)+1) * metrics$Precision * metrics$Recall) / 
      ((sqrt(beta)*metrics$Precision) + metrics$Recall)
  }
  
  return(metrics)
}


################################################################################

# Load Corpus
path <- "C:/Users/34652/Google Drive/2019-2020/UNI/UPM/INTELIGENT SYSTEMS/Natural Language Processing/Restaurant_Reviews_Dataset/Modified_Restaurant_Reviews.txt"

x <- scan(path, what="", sep = "\n")
x2 <- paste(x,collapse = " ")


## Use words that are close to adjectives

x3=as.String(x2)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
y1 <- annotate(x3, list(sent_token_annotator, word_token_annotator))
y2 <- annotate(x3, pos_tag_annotator, y1)


a3w <- subset(y2, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

z <- list(a3w$start, a3w$end)

x3 <- strsplit(x2,"")[[1]]

words <- list()

for (i in seq(length(z[[1]])))
{
  words[[length(words)+1]] <- paste(x3[z[[1]][i]:z[[2]][i]],collapse = '')
}

Adj <- words[tags == 'JJ']

data <- data.frame(words,tags)



