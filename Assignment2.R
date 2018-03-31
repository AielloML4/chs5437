#Data Load/Pre-treatment
Winesdata <- read.table("C:\\Users\\Linda\\Desktop\\Winesdata.txt", header=TRUE)
WinesScaled <- scale(Winesdata[,-1])
WinesScaled

pc <- prcomp(Winesdata, center=TRUE, scale=TRUE)
summary(pc)
plot(pc)
abline(h=1)
biplot(pc)

#First 3 provide most variance
WinesScaledPCA <- WinesScaled[,c(2:4) ]
WinesScaledPCA

#Cluster Analysis1
d <- dist(WinesScaledPCA)
fith <- hclust(d,"ward.D2")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis2
fith <- hclust(d,"complete")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis3

fith <- hclust(d,"average")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis4

fith <- hclust(d,"single")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis5

fith <- hclust(d,"mcquitty")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis6

fith <- hclust(d,"median")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis6

fith <- hclust(d,"centroid")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)

#Cluster Analysis7

fith <- hclust(d,"ward.D")
plot(fith)

rect.hclust(fith, k=3, border="red")
clusters <- cutree(fith, 3)
clusters
plot(Winesdata, col=clusters)



#
install.packages("fpc")
library("fpc")

