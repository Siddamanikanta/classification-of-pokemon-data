############# dataset information ###########
# This dataset contains information on all 800 Pokemon from all six Generations of Pokemon.
  # Is it possible to build a classifier to identify legendary Pokemon?
  
##Dependent variable: LEGENDARY(yes or no)

## we will do following steps
# 1.import the data set
# 2.remove the unnecessary column
# 3.check the data types,dim,summary
# 4.check the missing values
# 5.check the outliers (replace with mean)
# 6.histograms plots for pokemons
# 7.check the legendary pokemon per generation
# 8.categorical in to numerical featurer 
# 9.normalize the data
# 10.apply the kmeans clustering algorithms
# 11.apply the hierarchial clustering algorithms

#read the data
pokemon=read.csv("pokemon.csv")
View(pokemon)
dim(pokemon)
#remove unnecessary columns
pokemon=pokemon[-1:-4]

# summary of data
summary(pokemon)
str(pokemon)


#dimensions of the data
dim(pokemon)

#check the missing values of data
if(length(which(is.na(pokemon)==T))){
  print("missing values are found")
}else{
  print("no missing values are found")
}

#check the outliers of the data

unique(boxplot(pokemon$HP)$out)
unique(boxplot(pokemon$Attack)$out)
boxplot(pokemon$Defense)$out
boxplot(pokemon$Sp..Atk)$out
boxplot(pokemon$Sp..Def)$out
boxplot(pokemon$Speed)$out


#outliers replace with mean
for (colName in c('HP','Attack','Defense','Sp..Atk','Sp..Def','Speed')) {
  high = quantile(pokemon[,colName])[4] + 1.5*IQR(pokemon[,colName])
  low = quantile(pokemon[,colName])[2] - 1.5*IQR(pokemon[,colName])
  for (index in c(1:nrow(pokemon))) {
    pokemon[,colName][index] = ifelse(pokemon[,colName][index] > high, high, pokemon[,colName][index])
    pokemon[,colName][index] = ifelse(pokemon[,colName][index] < low, low, pokemon[,colName][index])
  }
}
#after removing Outliers boxplot
boxplot(pokemon$HP,pokemon$Attack,pokemon$Defense,pokemon$Sp..Atk,pokemon$Sp..Def,pokemon$Speed)

# Load libraries
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(knitr)

#histogram plots of pokemon    
library(tidyverse)
pokemon%>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

#ggplot for total and legendary variables
ggplot(pokemon,aes(x =Legendary, fill =Total)) + 
  geom_bar(position = "stack")+labs(y="total")

#ggplot for input(generation) and output(legendary) variable
ggplot(pokemon, aes(Generation)) + 
  geom_bar(aes(fill =Legendary), position = "fill") +
  coord_flip()
      #NOTE:I am observing 3rd generation pokemons more legendarys compare to other generations. 

#categorical in to numerical 
pokemon$Legendary=as.numeric(factor(pokemon$Legendary))-1

#scale the data
pokemon1=scale(pokemon[1:7])
pokem=pokemon[-1:-7]
pokemonn=cbind(pokemon1,pokem)
# View after normalize the data
View(pokemonn)


############## 1.kmeans clustering ############

km <- kmeans(pokemonn,6) #kmeans clustering
str(km)
# now we have to group by calculating centrioid and finding the distance between 
#centroids and variable and group the variable as per least distanc
library(animation)
km=kmeans.ani(pokemonn, 6)

k2=kmeans(pokemonn,centers = 2,nstart = 25)
k3 <- kmeans(pokemonn, centers = 3, nstart = 25)
k4 <- kmeans(pokemonn, centers = 4, nstart = 25)
k5 <- kmeans(pokemonn, centers = 6, nstart = 25)
library(factoextra)
# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = pokemonn) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = pokemonn) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = pokemonn) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = pokemonn) + ggtitle("k = 6")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# TO decide the k valus use elbow curve/ scree plot or user  k ~ sqrt(n/2) 
# Determine number of clusters by scree-plot
wss = (nrow(pokemonn))*sum(apply(pokemonn,2, var))	  
for (i in 1:9) wss[i] = sum(kmeans(pokemonn, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") # Look for an "elbow" in the scree plot #
title(sub ="K-Means Clustering Scree-Plot")


################## 2.h-clustering ###############

d<-dist(pokemonn,method="euclidean") 
plot(d)
#hclustering model
fit<-hclust(d,method="complete")
#check the clustering using dendogram
plot(fit)
#check all variables in one line
plot(fit,hang=-1)
#looking in to dendogram decide how many clusters we need
#we are dividing dendogramin to 6 clusters by taking k=6
cutree(fit,k=6)
# labelling the groups with red colour
rect.hclust(fit,k=6,border="red")
View(pokemonn)

############# OBSERVATIONS ############
# 1.1st,3rd,5th generations pokemons is more compare to other generations but only 
#  3rd generation pokemons more Legendarys.
# 2.we have to group by calculating centrioid and finding the distance between 
# centroids and variable and group the variable as per least distance we have taken six clusters
