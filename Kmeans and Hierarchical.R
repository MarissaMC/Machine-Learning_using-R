## k-means clustering

set.seed(1)

data=matrix(rnorm(100),ncol=2)

plot(data)

## create some clusters in the dataset

data[1:25,1]=data[1:25,1]+3
data[1:25,2]=data[1:25,2]-4

set.seed(1)
km_output=kmeans(data,2)
names(km_output)

km_output$cluster

## plot the clusters
plot(data,col=km_output$cluster,xlab="X1",ylab="X2",cex=1.5,pch=20)
km_output$centers

points(km_output$centers,col="blue",pch=4,cex=2)
## another way of plotting kmeans

library(cluster)
clusplot(data,km_output$cluster)

# Hierarchical Clustering

hc.complete=hclust(dist(data),method="complete")

hc.single=hclust(dist(data),method="single")

hc.average=hclust(dist(data),method="average")

# plot()

par(mfrow=c(1,3))
plot(hc.complete,main="Complete linkage")
plot(hc.single,main="Single linkage")
plot(hc.average,main="Average linkage")

names(hc.complete)
hc.complete$order

# look at different clusters
par(mfrow=c(1,1))
plot(hc.complete,main="Complete Linkage")

groups=cutree(hc.complete,k=2)
groups

rect.hclust(hc.complete,k=2,border="red")