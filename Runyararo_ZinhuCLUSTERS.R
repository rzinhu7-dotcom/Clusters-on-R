library("openxlsx")
myData<-read.xlsx("HClustering_data.xlsx",1)

#standardize
myData_std<-scale(myData[ ,2:4])

class(myData_std)
view(myData_std)
str(myData_std)

help(dist)
d<-dist(myData_std,method="euclidean")
d

help(hclust)
cities<-hclust(d,method="complete")
cities

plot(cities)
clusters_cities<-cutree(cities,k=3)
clusters_cities

clusters_cities<-cutree(cities,h=5)
clusters_cities

clusters_cities<-cutree(cities,h=3.6)
clusters_cities
table(clusters_cities)

myData<-data.frame(myData,clusters_cities)
summary(subset(myData,clusters_cities==1))
summary(subset(myData,clusters_cities==2,
               select=c(Crime,Poverty,Income)))

summary(subset(myData,clusters_cities==2,
               select=c(Crime,Poverty,Income)))

summary(subset(myData,clusters_cities==3,
               select=c(Crime,Poverty,Income)))


write.xlsx(myData,"Cities.xlsx")

library("openxlsx")
myData<-read.xlsx("HClustering_data.xlsx")

#standardize
myData_std<-scale(myData[ ,2:4])
help(dist)
d<-dist(myData_std,method="euclidean")
d

install.packages("cluster")
library(cluster)
help(agnes)
result<-agnes(d,diss=TRUE,method="ward")
result

result$height
result$method
result$diss

#0 indicates weak clustering structure
#data points don't naturally group together
#1 indicates very strong clustering structure
#data points form clear, well-separated structure

plot(result)

help(cutree)
#k is the number of clusters
clusters<-cutree(result,k=3)
clusters
table(clusters)
clusters<-cutree(result,h=6.5)#will not work

hclust_result <- as.hclust(result)
hclust_result
plot(hclust_result)

#h is height 
clusters_hclust <- cutree(hclust_result, h = 6.5)
table(clusters_hclust)

myData<-data.frame(myData,clusters)

View(myData)

summary(as.factor(clusters))
summary(subset(myData,clusters==1))
summary(subset(myData,clusters==2,select=c(Crime,Poverty,Income)))
summary(subset(myData,clusters==3,select=c(Crime,Poverty,Income)))

summary(subset(myData,clusters==1,
               select=c(Crime,Poverty,Income)))

summary(subset(myData,clusters==2,
               select=c(Crime,Poverty,Income)))

summary(subset(myData,clusters==3,
               select=c(Crime,Poverty,Income)))

write.xlsx(myData,"Cities.xlsx")            #create an excel sheet then you can filter the data 

library("openxlsx")
myData<-read.xlsx("HClustering_data.xlsx",2)
View(myData)

#Distance matrix for similarity measure using binary method
help(dist)
d<-dist(myData[, 1:5], method = "binary")
#binary for categorical
d


#hierachical clustering using agnes algorithm
#and average linkage
aResult <- agnes(d, diss = TRUE, method = "average")
aResult
plot(aResult)

#SPECIFYING the number of clusters
aClusters <- cutree(aResult, k = 4)
aClusters
table(aClusters)

hclust_aResult<- as.hclust(aResult)

plot(hclust_aResult)
aClusters1 <- cutree(hclust_aResult, h = 0.8)
table(aClusters1)
aClusters1


myData<- data.frame(myData, aClusters)
write.xlsx(myData,"categorical_data.xlsx")

myData[1:5]<-lapply(myData[1:5], factor)
str(myData)

summary(subset(myData,aClusters==1,
               select=c(x1,x2,x3,x4,x5)))


summary(subset(myData,aClusters==2,
               select=c(x1,x2,x3,x4,x5)))

summary(subset(myData,aClusters==3,
               select=c(x1,x2,x3,x4,x5)))


summary(subset(myData,aClusters==4,
               select=c(x1,x2,x3,x4,x5)))


summary(subset(myData,aClusters==5,
               select=c(x1,x2,x3,x4,x5)))






















