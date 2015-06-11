### read the data

### CPC = cost per click

### Competition: The number of advertisers worldwide bidding on 
### each keyword relative to all keywords across Google. 
### In the "Competition" column, you can see whether the 
### competition for a keyword idea is low, medium, or high."
### https://www.en.adwords-community.com/t5/Basics-for-New-Advertisers/
### what-does-competition-mean-in-google-adwords-under-key-words/td-p/151880

setwd('/Users/ethanshao/Desktop/mengchun/DSO530/R')
data = read.csv("googleAdwords.csv")

head(data)

### low competition, no one uses this to search; 
### high competition, too competitive

### k-means clustering

### k-means uses distances to compute the clusters
## eliminate categorical variables
set.seed(1)
km.out = kmeans(data[,-1], 6)

### scale the data to make three variables comparable
### km.out = kmeans(scale(data[,-1], 6))

clusters = km.out$cluster
head(clusters)

cluster1 = data[clusters==5,]
cluster2 = data[clusters==1,]
cluster3 = data[clusters==2,]
cluster4 = data[clusters==3,]
cluster5 = data[clusters==6,]
cluster6 = data[clusters==4,]
summary(cluster1)
summary(cluster2)
summary(cluster3)
#...
summary(cluster6)

library(MASS)
### cluster1/2/3... are data, 
### but "clusters" are clusters themselve, not data
parcoord(cluster1[,-1])

### how do we know which color reperents which cluster
parcoord(data[,-1], col = clusters)

summary(data[,-1])


#### hierarchical

hc.complete = hclust(dist(data[,-1]),
                          method = "complete")

clusters = cutree(hc.complete, 6)
parcoord(data[,-1], col = clusters)

library(rgl)
plot3d(data$competition,
       data$Monthly.Searches,
       data$CPC,
       col = clusters)

