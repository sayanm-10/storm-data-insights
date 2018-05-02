#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : knn
#  First Name : Sagar
#  Last Name  : Jain
#  Id			    : 10429097
#  Date       : 04/20/2018
#################################################

#Clear the environment
rm(list=ls())

#Loading the data
storm <- read.csv("/Users/sagarjain/Desktop/kddm/project/final/storm.csv")

#Remove unwanted columns
storm<-storm[,-c(1,13)]

#To assign integer values to factors
storm$INJURIES<-factor(storm$INJURIES,levels = c("No","Yes"),labels = c(0,1))
storm$DEATHS<-factor(storm$DEATHS,levels = c("No","Yes"),labels = c(0,1))
storm$DAMAGE_PROPERTY<-factor(storm$DAMAGE_PROPERTY,levels = c("Low","Medium","High"),labels = c(0,1,2))
storm$DAMAGE_CROPS<-factor(storm$DAMAGE_CROPS,levels = c("Low","Medium","High"),labels = c(0,1,2));

storm<- storm[sample(1:nrow(storm),nrow(storm)/2),]
#hclust for injuries
storm_dist<-dist( storm[,-c(8)])
hclust_results<-hclust(storm_dist,method="complete")
hclust_2<-cutree(hclust_results,2)
table(hclust_2,storm[,8])

kmeans_2<- kmeans(storm[,-c(8)],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,storm[,8])
?kmeans

kmeans_2<- kmeans(storm[,-c(9)],2,nstart = 10)
kmeans_2$cluster
table

kmeans_2<- kmeans(storm[,-c(10)],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,storm[,10])

kmeans_2<- kmeans(storm[,-c(11)],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,storm[,11])
?kmeans
