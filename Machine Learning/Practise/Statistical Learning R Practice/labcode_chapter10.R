# This file contains the Lab code for Chapter 10 (Unsupervised
# Learning) of Statistical Learning

########### Principal Components Analysis ##########
data("USArrests")
?USArrests # has 4 variables - Murder, Assault, UrbanPop, Rape
apply(USArrests, 2, mean)
apply(USArrests, 2, var) # variables have substantially different mean dan variance
pr.out <- prcomp(USArrests, scale = TRUE  # specify whether scale the variable
                 )
pr.out
names(pr.out)
pr.out$center   # gives center(mean) of each variable
pr.out$scale    # gives standard deviation 
pr.out$rotation # gives PC loadings (coefficents of linear combination of original variables)
pr.out$x        # gives principle components scores (observations in PCs view)
biplot(pr.out, scale = 0)   # check biplot.prcomp for details
 pr.out$rotation <- -pr.out$rotation
 pr.out$x <- -pr.out$x
 biplot(pr.out, scale = 0) 
pr.out$sdev    # standard deviation of PCs
pr.var <- pr.out$sdev^2   # variance explained by each component
pr.var 
pve <- pr.var/sum(pr.var) # proportion of variance explained
pve
plot(pve, xlab = "Principal Components", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1),
     type = "b")
  plot(cumsum(pve), xlab = "Principal Components", 
       ylab = "Cumulative Proportion of Variance Explained", 
       ylim = c(0,1),
       type = "b")
 
###### K-Means Clustering ######
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)  
x[1:25, 1] <- x[1:25,1] +3
x[1:25, 2] <- x[1:25,2] -4  # first 25 observations are different from last 25
 # create k-means model
km.out <- kmeans(x, 2, # define K
                 nstart = 20 # number of times to run k-means
                 )
names(km.out)
km.out$cluster  # cluster each observation belongs to
plot(x, col = (km.out$cluster + 1), pch = 20, cex = 2)
km.out$tot.withinss   # total within-cluster variance, smaller the better
km.out  

###### Hierachical Clustering ######
hc.complete <- hclust(dist(x),  # object with pairwise dissimilarities
                      method = "complete"  # linkage to use
                      )
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
par(mfrow = c(1,3))
plot(hc.complete, cex = 0.9, main = "Complete Linkage")
plot(hc.average, cex = 0.9, main = "Average Linkage")
plot(hc.single, cex = 0.9, main = "Single Linkage")
 # set cutting line
cutree(hc.complete, 2 # cut into 2 clusters
       )
 # use correlation-linkage
x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1-cor(t(x))) # cor(t(x)) defines correlation among observations
par(old.par)
plot(hclust(dd, method = "complete"),
     main = "Complete Linkage with Correlation-Based Distance")











  
  
  
  
  
  
  
  
  
  
  
  