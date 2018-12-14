library(arules)
install.packages("arulesViz")
library("arulesViz")
TransFood <- read.csv("http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv")
TransFood <- TransFood[, -1]
TransFood <- as(as.matrix(TransFood), "transactions")
str(TransFood)
summary(TransFood)

itemFrequencyPlot(TransFood, support = 0.05, cex.names = 0.8)
# sup = 0.001 conf = 0.4
# sup = 0.001 conf = 0.6
# sup = 0.001 conf = 0.8
# sup = 0.001 conf = 1
basket_rules <- apriori(TransFood, parameter = list(sup = 0.001, conf = 0.9, target = "rules"))
inspect(head(basket_rules))
plot(basket_rules)
plot(basket_rules, method = "grouped")
plot(head(sort(basket_rules, by = "lift"),15), method = "graph")

## clustering

ClusterFood <- read.csv("http://homepages.uc.edu/~maifg/DataMining/data/qry_Food_by_Month.csv")
summary(ClusterFood)

ClusterFood <- scale(x=ClusterFood[,-1])
library(fpc)

fit2 <- kmeans(ClusterFood, 2) 
table(fit2$cluster)
plotcluster(ClusterFood, fit2$cluster)

fit3 <- kmeans(ClusterFood, 3) 
table(fit3$cluster)
plotcluster(ClusterFood, fit3$cluster)

fit4 <- kmeans(ClusterFood, 4) 
table(fit4$cluster)
plotcluster(ClusterFood, fit4$cluster)

fit5 <- kmeans(ClusterFood, 5) 
table(fit5$cluster)
plotcluster(ClusterFood, fit5$cluster)

fit5$centers

#optimal clusters
library(clValid)
intern_iris <- clValid(ClusterFood, 2:5 ,clMethods=c("hierarchical","kmeans"),validation="internal")
summary(intern_iris)

aggregate(ClusterFood, by = list(fit2$cluster), FUN = mean)

# Determine number of clusters
wss <- (nrow(ClusterFood) - 1) * sum(apply(ClusterFood, 2, var))
for (i in 2:12) wss[i] <- sum(kmeans(ClusterFood[,-1], centers = i)$withinss)
plot(1:12, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

prediction.strength(ClusterFood, Gmin = 2, Gmax = 15, M = 10, cutoff = 0.8)

d = dist(ClusterFood, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15) {
  cluster_result = kmeans(ClusterFood, i)
  clusterstat = cluster.stats(d, cluster_result$cluster)
  result[i - 1, 1] = i
  result[i - 1, 2] = clusterstat$avg.silwidth
  result[i - 1, 3] = clusterstat$dunn
}
plot(result[, c(1, 2)], type = "l", ylab = "silhouette width", xlab = "number of clusters")

plot(result[, c(1, 3)], type = "l", ylab = "dunn index", xlab = "number of clusters")

#prediction strength
prediction.strength(ClusterFood, Gmin = 2, Gmax = 15, M = 10, cutoff = 0.8)

# hierarchical
hc_result = hclust(dist(ClusterFood))
plot(hc_result)
# Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k = 2)
rect.hclust(hc_result, k = 3)
rect.hclust(hc_result, k = 4)
rect.hclust(hc_result, k = 5)

#EDA
plot(density(ClusterFood$Oct..10))
plot(density(ClusterFood$Mar..11))
hist(ClusterFood$Oct)
