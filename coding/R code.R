library(factoextra)
library(cluster)
library(mvoutlier)
data = read.csv('C:/Users/user/OneDrive/Even Sems, 2022/Sem 6/Project/Gargi/final dataset after PCA.csv')
data
data = data.frame(data)
data
out = pcout(data, makeplot = TRUE, explvar = 0.99, crit.M1 = 1/3, crit.c1 = 2.5,
            crit.M2 = 1/4, crit.c2 = 0.99, cs = 0.25, outbound = 0.25)
out
which(out$wfinal01==0)
data1 = read.csv('C:/Users/user/OneDrive/Even Sems, 2022/Sem 6/Project/Gargi/final dataset after PCA without outliers.csv')
data1
data1 = data.frame(data1)
data1
data1 = scale(data1)
data1
fviz_nbclust(data1, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat = clusGap(data1,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
gap_stat

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 8 clusters
km = kmeans(data1, centers = 8, nstart = 25)

#view results
km



#plot results of final k-means model
fviz_cluster(km, data = data1)
#find means of each cluster
aggregate(data1, by=list(cluster=km$cluster), mean)
#add cluster assigment to original data
final_data = cbind(data1, cluster = km$cluster)

#view final data
final_data
