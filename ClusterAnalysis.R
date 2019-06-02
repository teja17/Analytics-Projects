# get n set working directory
getwd()
setwd("C:\\Users\\Tejaswini\\Desktop\\Advanced Business Analytics\\Assignment")

# Load the data set
library(data.table)
data <- fread("hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
#Extract NC and nearby states data
nc_data <- data[(data$state == "NC") | (data$state == "SC") | (data$state == "VA") | (data$state == "GA") | (data$state == "TN")]
nc_state_data <- data[(data$state == "NC") | (data$state == "SC") | (data$state == "VA") | (data$state == "GA") | (data$state == "TN")]
head(data)
summary(data)

# drop columns which may not be useful
nc_data <- subset(nc_data,select=-c(zip,hid,city,state,th,trauma,rehab))
head(nc_data)
colnames(nc_data)

#perform scaling on data to standardize the data before cluster analysis
if (TRUE){
  df <- scale(nc_data[-1]) # Standardize the data
} else{
  df <- nc_data[-1] 
}
head(df)

k.means.fit <- kmeans(df, 3) # Perform k-means clustering with 3 clusters
attributes(k.means.fit)
k.means.fit$centers # The locations of the centroids
k.means.fit$cluster # The cluster to which each observation belongs
k.means.fit$size # Check the size of each cluster
#check the clustering tendency of data
#library(clustertend)
#hopkins(df, n = nrow(df)-1)

#determine number of clusters 
withinssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(df, nc=10) 

#perform k-means clustersing with k=4
k.means.fit <- kmeans(df, 4)
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$size # Check the size of each cluster

# To create the clusters in 2-dimensional space:
library(cluster)
clusplot(df, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)


#Hierarchial Clustering
# creare proximity matrix
d <- dist(df, method = "euclidean") # using Euclidean distance measure 

# dendogram using Single Link Clustering
H.single <- hclust(d, method="single")
plot(H.single) # display dendogram

H.complete <- hclust(d, method="complete")
plot(H.complete)

H.average <- hclust(d, method="average")
plot(H.average)

H.ward <- hclust(d, method="ward.D2")
plot(H.ward)

groups <- cutree(H.ward, k=3) # cut tree into 3 clusters
#draw dendogram with borders
plot(H.ward)
rect.hclust(H.ward, k=3, border="red")
#plot 2D cluster
clusplot(df, groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#DBSCAN clustering
#install.packages("dbscan")
library(dbscan)

# dimensionality reduction using PCA 
pca <- prcomp(nc_data[-1], center = TRUE, scale. = TRUE) # Variables will be zero-centered and will have unit variance in the PCA
print(pca)

# to determine number of principal components
plot(pca, type = "l")
summary(pca)

# kNN distance plot to look for knee and determine optimal eps value
kNNdistplot(df, k =4)
abline(h=3.4, col="red")

# Run DBSCAN for minPTs=4 and eps=3.4
db <- dbscan(df, eps=3.4, minPts=4)
db

# plot 2D cluster
clusplot(df, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# dimensionality reduction using PCA 
pca <- prcomp(nc_data[-1], center = TRUE, scale. = TRUE) # Variables will be zero-centered and will have unit variance in the PCA
print(pca)

# to determine number of principal components
plot(pca, type = "l")
summary(pca)

## number of PC's : n_pc = 3 
pca_data <- predict(pca, newdata = nc_data)
pc_df <- as.data.frame(scale(pca_data[,c(1:3)]))  

#determine number of clusters 
withinssplot(pc_df, nc=10) 

#perform k-means clustersing with k=4
k.means.fit_pc <- kmeans(pc_df, 4)
attributes(k.means.fit_pc)
k.means.fit_pc$centers
k.means.fit_pc$size # Check the size of each cluster
k.means.fit_pc$cluster
# To create the clusters in 2-dimensional space:
#library(cluster)
clusplot(pc_df, k.means.fit_pc$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)


#Hierarchial Clustering
# creare proximity matrix
d_pc <- dist(pc_df, method = "euclidean") # using Euclidean distance measure 

# dendogram using Single Link Clustering
H.single_pc <- hclust(d_pc, method="single")
plot(H.single_pc) # display dendogram

H.complete_pc <- hclust(d_pc, method="complete")
plot(H.complete_pc)

H.average_pc <- hclust(d_pc, method="average")
plot(H.average_pc)

H.ward_pc <- hclust(d_pc, method="ward.D2")
plot(H.ward_pc)

groups_pc <- cutree(H.ward_pc, k=4) # cut tree into 3 clusters
table(groups_pc)
#draw dendogram with borders
plot(H.ward_pc)
rect.hclust(H.ward_pc, k=4, border="red")
#plot 2D cluster
clusplot(pc_df, groups_pc, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# kNN distance plot to look for knee and determine optimal eps value
kNNdistplot(pc_df, k =4)
abline(h=0.8, col="red")

# Run DBSCAN for minPTs=4 and eps=0.8
db_pc <- dbscan(pc_df, eps=0.8, minPts=4)
db_pc

# plot 2D cluster
clusplot(pc_df, db_pc$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Question 9
pc_df$kmeans <- k.means.fit_pc$cluster
pc_df $hclust <- groups_pc # these groups are created in hierarchical clustering
pc_df $db <- db_pc$cluster
pc_df $hid <- nc_state_data$hid # Add hospital id to pc_df data
final_data <- merge(x=pc_df, y=nc_state_data, key="hid")
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$kmeans), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$hclust), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$db), mean)

k.means.fit_pc$size
table(groups_pc)
db_pc
##silhouette measure
plot(silhouette(k.means.fit$cluster,d))
plot(silhouette(groups,d))
plot(silhouette(db$cluster,d))

