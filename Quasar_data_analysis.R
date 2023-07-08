###################################################################################################

#Data input

data = read.csv(file.choose())
head(data)
data = data[,c(2 , 4 , 6 , 7 , 9 , 12 , 13 , 14 , 15 , 18 , 20:22 )]
head(data)
dim(data)
attach(data)
set.seed(100)

#####################################################################################################

# Data cleaning for this dataset

library(mice)
library(missForest)

library(cluster)
library(factoextra)
library(fpc)
astr.mis = prodNA(data, noNA = 0.1)
head(astr.mis)
imputed_Data = mice(data, m=2, maxit = 1000, method = 'pmm', seed = 500)
complete_data = complete(imputed_Data , 1)
complete_data2 = complete(imputed_Data, 2)
write.csv(complete_data, "D:/Astro_project/data1.csv")
write.csv(complete_data2, "D:/Astro_project/data2.csv")
head(complete_data)
complete_data = complete_data[,-c(1 , 2 )]
head(complete_data)
sum(is.na(complete_data[,-c(1:2)]))
data1 = complete_data[,-c(1 , 2)]
data2 = complete_data2[, -c(1 , 2)]



##################################################################################################

# Clustering and all for the total dataset

#wss = (nrow(complete_data)-1)*sum(apply(complete_data , 2, var))
wss = c()
for (i in 1:10)
{
  wss[i] =
    sum(kmeans(data2 , centers = i)$withinss)
}
plot(1:10 , wss , type = "l" , 
     xlab = "number of clusters" ,
     ylab = "within group of sum of  square")
#
#
#
#
kmeans = kmeans(data1 , 3)
kmeans
kmeans$cluster
#
#
#
library(clValid) 
dunn(Data = data1 ,clusters =  kmeans$cluster )
library(clusterSim)
c = pam(data1 , 3)
index.DB(x = data1[1:65536,]  , centrotypes = "centroids")



library(clv)
Dunn <- function(data,clust) 
  clv.Dunn( cls.scatt.data(data,clust),
            intracls = c("complete","average","centroid"), 
            intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )
Davies.Bouldin <- function(data,clust) 
  clv.Davies.Bouldin( cls.scatt.data(data,clust),
                      intracls = c("complete","average","centroid"),
                      intercls = c("single", "complete", "average","centroid", "aveToCent", "hausdorff")
  )

kmeans = kmeans(data1 , 3)
kmeans
kmeans$cluster
kmeans1 = kmeans(data1 , 2)
kmeans2 = kmeans(data1 , 4)

Dunn(data1 , kmeans$cluster)
Dunn(data1 , kmeans1$cluster)
Dunn(data1 , kmeans2$cluster)
#install.package("xgboost")
head(data1)


########################################################################################################

head(data21)
head(data1)
final_data = data1[c(1,2,3,6,9)]
head(final_data)

wss = c()
for (i in 1:10)
{
  wss[i] =
    sum(kmeans(final_data , centers = i)$withinss)
}
plot(1:10 , wss , type = "l" , 
     xlab = "number of clusters" ,
     ylab = "within group of sum of  square")

kmean_final1 = kmeans(final_data , 2)
kmean_final2 = kmeans(final_data , 3)
kmean_final3 = kmeans(final_data , 4)
kmean_final4 = kmeans(final_data , 5)

Dunn(final_data , kmean_final1$cluster)
Dunn(final_data , kmean_final2$cluster)
Dunn(final_data , kmean_final3$cluster)
Dunn(final_data , kmean_final4$cluster)
# So now we get here we are taking 3 as the optimal number of clusters w.r.t Distortion and Dunn Index

########################################################################################################

library(xgboost)
library(caret)
parts = createDataPartition(kmean_final1$cluster, p = 0.7, list = F)
train = final_data[parts, ]
test = final_data[-parts, ]
X_train = data.matrix(train[,-1])                  # independent variables for train
y_train = train[,1] 
X_test = data.matrix(test[,-1])                    # independent variables for test
y_test = test[,1]

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

# train a model using our training data
model <- xgboost(data = xgboost_train,                    # the data   
                 max.depth=3, ,                           # max depth 
                 nrounds=50)                              # max number of boosting iterations

summary(model)


#use model to make predictions on test data
pred_test = predict(model, xgboost_test)

pred_test


pred_test[(pred_test>1)] = 1
pred_y = as.factor((levels(y_test))[round(pred_test)])
print(pred_y)

silhouette_score <- function(k){
  km <- kmeans(final_data, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(final_data))
  mean(ss[, 1])
}
k <- 1:5
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
conf_mat = confusionMatrix(y_test, pred_y)
print(conf_mat)
