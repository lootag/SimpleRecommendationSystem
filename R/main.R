rm(list = ls())
setwd(getwd())

packages <- c('readr', 'dplyr', 'magrittr')
for( package in 1:length(packages))
{
  if(!(packages[package] %in% installed.packages()))
  {
    install.packages(packages[package])
  }
}
  
library(readr)
library(dplyr)
library(magrittr)
source(paste(getwd(), '/functions.R', sep = ''))

data <- read_csv('../data.csv')
data$viewed <- gsub(':', '.', data$viewed, fixed = TRUE)
data$viewed <- gsub('-', '.', data$viewed, fixed = TRUE)
data$viewed <- strptime(data$viewed, '%Y.%m.%d %H.%M.%S')
nUsers <- length(unique(data$idUser))
byUser <- as.list(rep(0, nUsers))
users <-  as.list(unique(data$idUser))

byUser <- lapply(users, makeDf, dataset = data)%>%
  lapply(sortDf)

byUserFull <- do.call(rbind, byUser) # Point 1
byUserTrain <- lapply(byUser, getTrain)
train <- do.call(rbind, byUserTrain)
byUserTest <- lapply(byUser, getTest)
test <- do.call(rbind, byUserTest)%>%
  na.omit()
userList <- as.list(unique(data$idUser)) 

#recommend is the recommendation function. It takes the  training list, the userid, and the list of all available formal content in the training list as inputs, and ouputs a formal content which the specified user hasn't seen yet. 
recommendations <- lapply(userList, recommend, byUser = byUserTrain, formal = train$content[!grepl('CS', train$content, fixed = TRUE)]) #Point 2
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

accuracy <- paste(sum(accuracyVector)/length(accuracyVector)*100, '%', sep = '')
dummyAccuracy <- paste(sum(dummyAccuracyVector)/length(dummyAccuracyVector)*100, '%', sep = '')
print(paste('The accuracy provided by the developed algorithm is', accuracy))
print(paste('The accuracy provided by the dummy recommender used as a control is', dummyAccuracy))


