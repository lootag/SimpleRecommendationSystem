rm(list = ls())
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
source('/home/alex/repos/docebo/R/functions.R')

data <- read_csv('data.csv')
data$viewed <- gsub(':', '.', data$viewed, fixed = TRUE)
data$viewed <- gsub('-', '.', data$viewed, fixed = TRUE)
data$viewed <- strptime(data$viewed, '%Y.%m.%d %H.%M.%S')
nUsers <- length(unique(data$idUser))
byUser <- as.list(rep(0, nUsers))
users <-  as.list(unique(data$idUser))

byUser <- lapply(users, makeDf, dataset = data)%>%
  lapply(sortDf)

byUserTrain <- lapply(byUser, getTrain)
train <- do.call(rbind, byUserTrain)
byUserTest <- lapply(byUser, getTest)
test <- do.call(rbind, byUserTest)%>%
  na.omit()
userList <- as.list(unique(data$idUser)) 

recommendations <- lapply(userList, recommend, byUser = byUserTrain, formal = train$content[!grepl('CS', train$content, fixed = TRUE)])
dummyRecommendations <- lapply(userList, dummyRecommend, byUser = byUserTrain, formal = train$content[!grepl('CS', train$content, fixed = TRUE)])


accuracyVector <- c()
dummyAccuracyVector <- c()

for(i in 1:length(recommendations))
{
  accuracyVector[i] <- recommendations[[i]] %in% byUserTest[[i]]$content
}

for(i in 1:length(dummyRecommendations))
{
  dummyAccuracyVector[i] <- dummyRecommendations[[i]] %in% byUserTest[[i]]$content
}

accuracy <- sum(accuracyVector)/length(accuracyVector)
dummyAccuracy <- sum(dummyAccuracyVector)/length(dummyAccuracyVector)


