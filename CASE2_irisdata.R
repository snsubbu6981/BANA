data(iris)
dim(iris)
str(iris)
subset<-sample(nrow(iris),nrow(iris)*0.9)
iris.test<-iris[-subset,1:4]
iris<-iris[subset,1:4]

# two clusters#
fit2=kmeans(iris, 2) 
table(fit2$cluster)
fit2
plotcluster(iris, fit2$cluster)
# Three Clusters#
fit3=kmeans(iris,3) 
table(fit3$cluster)
plotcluster(iris, fit3$cluster)
# Four Cluster#
fit4=kmeans(iris,4) 
table(fit4$cluster)
plotcluster(iris, fit4$cluster)
# Five Cluster#
fit5=kmeans(iris,5) 
table(fit5$cluster)
fit5<-as.(as.frame(fit5))
fit5
plotcluster(iris, fit5$cluster)

# Determine number of clusters
wss=(nrow(iris) - 1) * sum(apply(iris, 2, var))
for (i in 2:12) wss[i]=kmeans(iris, centers = i)$tot.withinss
plot(1:12, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
wss
# Determine number of clusters using prediction strength
prediction.strength(iris, Gmin = 2, Gmax = 15, M = 10, cutoff = 0.8)
prediction.strength(iris.test, Gmin = 2, Gmax = 15, M = 2, cutoff = 0.8)
# Determine the number of clusters using Other measures
d = dist(iris, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:12) {
  cluster_result = kmeans(iris, i)
  clusterstat = cluster.stats(d, cluster_result$cluster)
  result[i - 1, 1] = i
  result[i - 1, 2] = clusterstat$avg.silwidth
  result[i - 1, 3] = clusterstat$dunn
}
plot(result[, c(1, 2)], type = "l", ylab = "silhouette width", xlab = "number of clusters")
plot(result[, c(1, 3)], type = "l", ylab = "dunn index", xlab = "number of clusters")



# hierarchical clustering using ward's method
hc_result = hclust(dist(iris[, 1:4]),method = "ward")
plot(hc_result)
# Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k = 2)
rect.hclust(hc_result, k = 3)
rect.hclust(hc_result, k = 4)
rect.hclust(hc_result, k = 5)

# hierarchical clustering using single linkage method
hc_result = hclust(dist(iris[, 1:4]),method = "complete",members=NULL)
plot(hc_result)
# Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k = 2)
rect.hclust(hc_result, k = 3)
rect.hclust(hc_result, k = 4)
rect.hclust(hc_result, k = 5)



#############################################################################

# Zoo Data#
library(arules)

data<- read.csv("C:\\Users\\prakash\\Downloads\\DM2\\zoodataasso.csv")
data <- data[, -1]
data <- as(as.matrix(data), "transactions")
summary(data)
x=data[size(data)>=13]
inspect(x)

itemFrequencyPlot(data, support = 0.1, cex.names = 0.5)
basket_rules=apriori(data, parameter = list(sup = 0.001, conf = 0.8, target = "rules"))
summary(basket_rules)
inspect(basket_rules)
inspect(subset(head(basket_rules)),lift>5)
install.packages("arulesViz")
library("arulesViz")
plot(basket_rules)
plot(basket_rules, interactive = TRUE)

plot(head(sort(basket_rules, by = "lift"), 10), method = "graph")

plot(basket_rules, method = "grouped")

#############################################################

# Zoo data clustering 

z.data<- read.csv("C:\\Users\\prakash\\Downloads\\DM2\\zoodata.csv")

data<-scale(z.data[,2:7])
# two clusters#
fit2=kmeans(data, 2) 
table(fit2$cluster)
plotcluster(data, fit2$cluster)
# Three Clusters#
fit3=kmeans(data,3) 
table(fit3$cluster)
plotcluster(data, fit3$cluster)
# Four Cluster#
fit4=kmeans(data,4) 
table(fit4$cluster)
plotcluster(data, fit4$cluster)
# Five Cluster#
fit5=kmeans(data,5) 
table(fit5$cluster)
plotcluster(data, fit5$cluster)

# Determine number of clusters using within group variance
wss=(nrow(data) - 1) * sum(apply(data, 2, var))
wss
for (i in 2:12) wss[i]=kmeans(data, centers = i)$tot.withinss
plot(1:12, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

## Determine number of clusters using prediction strength
prediction.strength(data, Gmin = 2, Gmax = 15, M = 10, cutoff = 0.8)

# Determine the number of clusters using Other measures
d = dist(data, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:14) {
  cluster_result = kmeans(data, i)
  clusterstat = cluster.stats(d, cluster_result$cluster)
  result[i - 1, 1] = i
  result[i - 1, 2] = clusterstat$avg.silwidth
  result[i - 1, 3] = clusterstat$dunn
}
plot(result[, c(1, 2)], type = "l", ylab = "silhouette width", xlab = "number of clusters")
plot(result[, c(1, 3)], type = "l", ylab = "dunn index", xlab = "number of clusters")



# hierarchical clustering using ward's method
hc_result = hclust(dist(z.data[, 2:7]),method = "ward")
plot(hc_result)
# Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k = 2)
rect.hclust(hc_result, k = 3)
rect.hclust(hc_result, k = 4)
rect.hclust(hc_result, k = 5)

# hierarchical clustering using single linkage method
hc_result = hclust(dist(z.data[,2:7]),method = "complete")
plot(hc_result)
# Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k = 2)
rect.hclust(hc_result, k = 3)
rect.hclust(hc_result, k = 4)
rect.hclust(hc_result, k = 5)
