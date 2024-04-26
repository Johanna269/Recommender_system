#Data Preperation
library(reshape2)
library(tidyverse)
library(caret)
library(lsa)
setwd("/Users/johannaschoellhorn/")
data <- read.table("subsetratings.csv",sep=",",header=TRUE)
matrixdata <- dcast(data, userId ~ productId, value.var="rating") 
ratings <- matrixdata[,-1]

#Create Training and Test data for a single user}
trainusers <- ratings[-66,]
testuser <- ratings [66,]

#Highest rated products by test user
observed <- as.vector(testuser[1,])
setwd("/Users/johannaschoellhorn")
products <- read.table("subsetproducts.csv", sep=",", header=TRUE, quote="\"",comment.char="")
user <- data.frame(productId = names(observed), rating = t(testuser))
user <- merge(user,products,by.x="productId", by.y="productId")
userhigh <- user %>% 
  arrange(desc(X66))
head(userhigh)
userlow <- user %>% #
  arrange(X66)
head(userlow)


#Calculating Recommendations
#Finding nearest neighbours}
preproc.vector <- function(l1){
  v1 <- unlist(l1,use.names=FALSE) 
  return(replace(v1,is.na(v1),0)) 
}
distances <- apply(trainusers,1,function(x)cosine(preproc.vector(x), preproc.vector(testuser)))
nearest_neigbhbors <- order(distances)[1:5]
predicted <- colMeans(trainusers[nearest_neigbhbors,], na.rm=TRUE) 
pred_ratings <- data.frame(predicted)
pred_ratings <- cbind(productId = rownames(pred_ratings), pred_ratings) 
rownames(pred_ratings) <- NULL
user <- merge(user, pred_ratings, by.x='productId', by.y = 'productId')


#Making predictions
head(user[order(user$predicted, decreasing = T),c(3,2,4)], n=5)
head(user[order(user$predicted),c(3,2,4)], n=5)

errors <- user$X66 - user$predicted
MAE <- mean(abs(errors), na.rm = T)
MAE
RMSE <- sqrt(mean((errors) ^ 2, na.rm = TRUE))
RMSE


# Fine Tuning the model
We have calculated performance for a single user, but if we want to compare model performance (for example to optimize our model) we do this typically for a larger part of the dataset. Expand the code to do k-fold cross validation (with k = 5), and calculate recommender performance for the following values of K (number of neighbors, not number of folds): 1, 10, 25, 50, 80. We create a data.table to fill, with columns: userid, K (number of neighbors), MAE, RMSE.

performance <- expand.grid(userid = 1:nrow(ratings), K = c(1,10,25,50, 80)) #number of neighbours
performance$MAE <- NA
performance$RMSE <- NA

folds <- createFolds(unique(data$userId), 5, list=T) 
folds

folds <- createFolds(unique(data$userId), 5, list=T)
#Indexes
# i : is list of test users in fold
# j : ID of test user
# k : number of neighbors
for(i in folds){
  trainusers <- ratings[-i,] 
  for(j in i){
    testuser <- ratings[j,]
    distances <- apply(trainusers, 1, function(x) cosine(preproc.vector(x),
                                                         preproc.vector(testuser))) 
    observed <- as.vector(testuser[1,])
    user <- data.frame(productId = names(observed), rating=t(testuser))
    user <- merge(user, products, by.x="productId",by.y="productId")
    for(k in c(1,10,25,50,80)){
      nearest_neighbors <- order(distances)[1:k]
      predicted <- colMeans(trainusers[nearest_neighbors,], na.rm=T)
      pred_ratings <- data.frame(predicted)
      pred_ratings <- cbind(productId = rownames(pred_ratings), pred_ratings) 
      rownames(pred_ratings) <- NULL
      user <- merge(user, pred_ratings, by.x='productId', by.y = 'productId')
      diff <- user[,2] - user$predicted
      RMSE <- sqrt(mean((diff) ^ 2, na.rm = TRUE))
      MAE <- mean(abs(diff), na.rm = T)
      performance$MAE[which(performance$userid == j & performance$K == k)] <- MAE
      performance$RMSE[which(performance$userid == j & performance$K == k)] <- RMSE
      user$predicted <- NULL
    }
  }
}
perf_sum <- performance %>% group_by(K) %>% summarize("MAE" = mean(MAE, na.rm = T), "RMSE" = mean(RMSE, na.rm = T))
perf_sum

#Visualize
ggplot(performance, aes(x = K, y = RMSE)) +
  geom_line() +
  labs(title = "RMSE vs. Number of Neighbors K",
       x = "Number of Neighbors K",
       y = "Root Mean Squared Error (RMSE)") 


#T-Test
performance_k1 <- subset(performance, K==1) # Replace with RMSE values for K=1
performance_k25 <-subset(performance, K==25)  # Replace with RMSE values for K=25
t.test(x=performance_k1$RMSE, y=performance_k25$RMSE, paired=TRUE)
