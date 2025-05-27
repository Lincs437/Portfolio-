
#combine data needed from pre-2020 and post-2020

#>15 and 3 groups for 3 tiers of players
#>25 and 5 groups for 5 types of players

#import data
data_nc19 <- read.csv("data_nc19.csv",header=T)
data_nc <- read.csv("data_nc.csv",header=T)

data_nc_comb <- rbind(data_nc19[,c(3,19:26)],data_nc[,c(3,23:30)])

clustd<-data_nc_comb

fhz<-tapply(clustd$Fairway.Hitsz,clustd$Name,mean,na.rm=T)
c1z<-tapply(clustd$Circle.1z,clustd$Name,mean,na.rm=T)
c2z<-tapply(clustd$Circle.2z,clustd$Name,mean,na.rm=T)
sz<-tapply(clustd$Scramblez,clustd$Name,mean,na.rm=T)
c1pz<-tapply(clustd$Circle.1.Puttingz,clustd$Name,mean,na.rm=T)
c2pz<-tapply(clustd$Circle.2.Puttingz,clustd$Name,mean,na.rm=T)
obz<-tapply(clustd$OBz,clustd$Name,mean,na.rm=T)

names<-names(fhz)

#create dataset for clustering
clustdm<-data.frame(fhz,c1z,c2z,sz,c1pz,c2pz,obz,names)
clustdm<-clustdm[table(data_nc_comb$Name)>5,] #removing players who played fewer than 5 events
clustdm<-clustdm[complete.cases(clustdm),]

#number of clusters (classes or player types)
nc <- c(2:6)

#############################
###hierarchical clustering###
#############################

dist_type <- c("euclidean","maximum","manhattan")
nc_dist_type <- expand.grid(nc,dist_type)
clss_hc <- as.data.frame(matrix(0,nrow=nrow(clustdm),ncol=nrow(nc_dist_type)))
for (i in 1:nrow(nc_dist_type)) {

dist_mat <- dist(clustdm[,1:7], method = nc_dist_type[i,2])
hclust_avg <- hclust(dist_mat, method = 'average')
#plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = nc_dist_type[i,1])
clss_hc[,i] <- as.numeric(cut_avg)
names(clss_hc)[i] <- paste(nc_dist_type[i,1],nc_dist_type[i,2],sep="_")
}

#require(dendextend)
#suppressPackageStartupMessages(library(dendextend))
#avg_dend_obj <- as.dendrogram(hclust_avg)
#avg_col_dend <- color_branches(avg_dend_obj, h = 5)
#plot(avg_col_dend)

####################
###GMM clustering###
####################
require(ClusterR)

#finding number of classes
opt_gmm = Optimal_Clusters_GMM(clustdm[,1:7], max_clusters = 10, criterion = "BIC", 
                               
                               dist_mode = "maha_dist", seed_mode = "random_subset",
                               
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               
                               plot_data = T)

#generate predictions for 2-6 classes
nc <- c(2:6) #setting number of clusters
clss <- as.data.frame(matrix(0,nrow=nrow(clustdm),ncol=length(nc))) #dataframe for output from models
for (i in 1:length(nc)) { #setup loop to run 5 times 

#generating classes
gmm = GMM(clustdm[,1:7], nc[i], dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          
          em_iter = 10, verbose = F)         

#predicting class
pr = predict_GMM(clustdm[,1:7], gmm$centroids, gmm$covariance_matrices, gmm$weights) 

#creating vector of class predictions
clss[,i] <- pr$cluster_labels
names(clss)[i] <- paste("nc",nc[i],sep="")
}

#plotting GMM results
#require(ggplot2)
#ggplot(clustdm, aes(x=c1z, y = c1pz, color = factor(cluster_labels))) + geom_point(size=3)
#pp <- apply(clustdm[,c(2,3,5,6)], 2, function(x) tapply(x, clustdm$cluster_labels, mean,na.rm=T))
#barplot(t(pp),beside=T, ylab = "Standardized Statistic (C1,C2, C1P, C2P)", xlab = "Player Type")

#BDSCAN
#require(dbscan)
#d <- dbscan(clustdm[,1:7], eps = 0.45, minPts =  5)
#d

#spectral clustering
#predictions for 2-6 classes and kernels
require(kernlab)
krn <- c("rbfdot","polydot","laplacedot","anovadot","splinedot")
nc_krn <- expand.grid(nc,krn)
clss_sc <- as.data.frame(matrix(0,nrow=nrow(clustdm),ncol=nrow(nc_krn)))
for (i in 1:nrow(nc_krn)) {
sc <- specc(as.matrix(clustdm[,1:7]), centers = nc_krn[i,1], kernel = nc_krn[i,2], kpar = "local")
clss_sc[,i] <- as.numeric(sc@.Data)
names(clss_sc)[i] <- paste(nc_krn[i,1],nc_krn[i,2],sep="_")
}

#plotting SC results
#ggplot(clustdm, aes(x=c1z, y = c1pz, color = factor(cluster_labels_SC))) + geom_point(size=3)


########################
###k-means clustering###
########################

nc <- c(2:6)
#make dataframe

km_types <- c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen") #different grouping types from k means
nc_kmean <- expand.grid(nc = nc,km_types = km_types) #uses expand grid

clss_kmeans <- as.data.frame(matrix(0, nrow = nrow(clustdm), ncol = nrow(nc_kmean)))

for (i in 1:nrow(nc_kmean)){
  kmout<- kmeans(as.matrix(clustdm[,1:7]),
                           centers = as.numeric(nc_kmean$nc[i]),
                           nstart=20,
                           algorithm = as.character(nc_kmean$km_type[i]))
clss_kmeans[, i] <- kmout$cluster #call correct column of dataframe
names(clss_kmeans)[i] <- paste(nc_kmean[i,1], nc_kmean[i,2], sep = "_")
}

clss_kmeans <- as.data.frame(clss_kmeans)

#combine datasets for cross-validation
clustdm_cv <- cbind(clustdm,clss,clss_sc,clss_hc,clss_kmeans)

