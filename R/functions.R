makeDf <- function(user, dataset)
{
  new <-  dataset[dataset$idUser == user,]
  return(new)
}

sortDf <- function(data)
{
  return(data[order(data$viewed),])
}

getTrain <- function(data)
{
  train <- data[1:(round(0.2*nrow(data))),]
  return(train)
}


getTest <- function(data)
{
  test <- data[(round(0.2*nrow(data)) + 1):nrow(data),]
  return(test)  
}


recommend <- function(user, byUser, formal)
{
  require(dplyr)
  require(DescTools)
  myUser <- byUser[[1]]
  sampleSize <- 50
  iterations <- 1:length(byUser)
  for(i in 1:length(byUser))
  {
    if(byUser[[i]]$idUser[1] == user)
    {
      myUser <- byUser[[i]]
      iterations <- iterations[iterations != i]
      break
    }
  }
  userFormal <- myUser$content[!grepl('CS', myUser$content, fixed = TRUE)]
  unseen <- setdiff(formal, userFormal)
  compare <- c()
  if(nrow(myUser) >= sampleSize)
  {
    compare <- sample(myUser$content, sampleSize)
  }
  else
  {
    compare <- myUser$content
  }
  results <- as.list(c())
  for( i in iterations)
  {
    if(length(compare) <= nrow(byUser[[i]]))
    {
      if(any(compare %in% byUser[[i]]$content))
      {
        results[[i]] <- intersect(byUser[[i]]$content, unseen)
      }
    }
  }
  results <- results[!(results == 'NULL')]
  result <- do.call(c, results)
  result <- DescTools::Mode(result)
  return(result[1])
}

dummyRecommend <- function(user, byUser, formal)
{
  require(dplyr)
  require(DescTools)
  for(i in 1:length(byUser))
  {
    if(byUser[[i]]$idUser[1] == user)
    {
      myUser <- byUser[[i]]
      break
    }
  }
  userFormal <- myUser$content[!grepl('CS', myUser$content, fixed = TRUE)]
  unseen <- setdiff(formal, userFormal)
  result <- sample(unseen, 1)
  return(result)
}






