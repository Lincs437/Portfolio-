require(lme4)
require(lmerTest)
require(MuMIn)
require(cAIC4)

# read in age and PDGA data
MPO_ar <- read.csv("MPO_ar.csv",header=T)

#MPO_ar dataset comes from "MPO_Aging.r" script
MPO_ar$years<-scale(MPO_ar$year)
MPO_ar$ages<-scale(MPO_ar$Age)

#combine ratings and classes (data comes from Player_type.r) for cross-validation of ratings
#can I make this betterc(faster) by pasting all at once then just using match to subset rows?
MPO_ar_tune <- data.frame(matrix(0,nrow=nrow(MPO_ar),ncol=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans))))
for (i in 1:nrow(MPO_ar))
{
  if (any(grepl(paste(MPO_ar$Name_f[i],MPO_ar$Name[i],sep=" "),clustdm_cv$names))) {MPO_ar_tune[i,]<-as.numeric(clustdm_cv[grep(paste(MPO_ar$Name_f[i],MPO_ar$Name[i],sep = " "),clustdm_cv$names),c(9:ncol(clustdm_cv))])} 
  else (MPO_ar_tune[i,]<-"NA")
  #print(i)
}

#remove NAs and player type 3 (not enough age data for player type 3)
#MPO_ar_pt <- subset(MPO_ar,MPO_ar$ptype!="NA" & MPO_ar$ptype!="3")


# run cross-validation to compare predictions of age curves including 
# player type versus excluding player type

#create dataframe for tuning and remove NAs
MPO_ar_tune2 <- data.frame(MPO_ar,MPO_ar_tune)
names(MPO_ar_tune2)[(ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)] <- names(clustdm_cv)[(ncol(clustdm_cv)-64):ncol(clustdm_cv)]
MPO_ar_pt <- subset(MPO_ar_tune2,MPO_ar_tune2$nc2!="NA")

#run loops

mean_SS_t_mod <- data.frame(matrix(0,nrow=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans)),ncol=6))
colnames(mean_SS_t_mod) <- c("Mean_SSE_PlayerType", "Mean_SSE_NoPlayerType", "Mean_MSE_NoPlayerType", "Mean_MSE_PlayerType", "Adj_R_Sqr_NoPlayerType", "Adj_R_Sqr_PlayerType")
for (j in (ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)) {
    

sumsq_p<-numeric(1000)
sumsq<-numeric(1000)
adj_r2 <- numeric(1000)
adj_r2_p <- numeric(1000)
mse_p <- numeric(1000)
mse <- numeric(1000)

# check if have >1 player type for analysis
if (length(unique(MPO_ar_pt[,j]))==1) {
  
  mean_SS_t_mod[j+1-13,1] <- "NA"
  mean_SS_t_mod[j+1-13,2] <- "NA"
  mean_SS_t_mod[j+1-13,3] <- "NA"
  mean_SS_t_mod[j+1-13,4] <- "NA"
  mean_SS_t_mod[j+1-13,5] <- "NA"
  mean_SS_t_mod[j+1-13,6] <- "NA"
  
} else {

#training size
tz <- round(length(unique(MPO_ar_pt$PDGA))*.8)
for (i in 1:1000){
train <- sample(unique(MPO_ar_pt$PDGA),tz)
test <- unique(MPO_ar_pt$PDGA)[!unique(MPO_ar_pt$PDGA) %in% train]

MPO_ar_pt[,j]<-as.factor(MPO_ar_pt[,j])

MPO_ar_pt_train <- MPO_ar_pt[MPO_ar_pt$PDGA %in% train,]
MPO_ar_pt_test <- MPO_ar_pt[MPO_ar_pt$PDGA %in% test,]

#ensure all player types from test are in training set
while (!all(unique(MPO_ar_pt_test[,j]) %in% unique(MPO_ar_pt_train[,j]))) {
  train <- sample(unique(MPO_ar_pt$PDGA),tz)
  test <- unique(MPO_ar_pt$PDGA)[!unique(MPO_ar_pt$PDGA) %in% train]
  
  MPO_ar_pt[,j]<-as.factor(MPO_ar_pt[,j])
  
  MPO_ar_pt_train <- MPO_ar_pt[MPO_ar_pt$PDGA %in% train,]
  MPO_ar_pt_test <- MPO_ar_pt[MPO_ar_pt$PDGA %in% test,] 
  
}

pt_type <- MPO_ar_pt_train[,j]
pt_ages <- MPO_ar_pt_train$ages
pt_ratr <- MPO_ar_pt_train$ratr
MPO_ar_pt_train2 <- data.frame(pt_type,pt_ages,pt_ratr)

outp<-lm(pt_ratr~pt_type*pt_ages+pt_type*I(pt_ages^2),data=MPO_ar_pt_train2)

pt_type <- MPO_ar_pt_test[,j]
pt_ages <- MPO_ar_pt_test$ages
pt_ratr <- MPO_ar_pt_test$ratr
MPO_ar_pt_test2 <- data.frame(pt_type,pt_ages,pt_ratr)

#what is our method of measuring model predictive power
sumsq_p[i] <- sum((predict(outp,MPO_ar_pt_test2)-MPO_ar_pt_test2$pt_ratr)^2)

mse_p[i] <- sqrt(mean((predict(outp, MPO_ar_pt_test2) - MPO_ar_pt_test2$pt_ratr)^2))

adj_r2_p[i] <- summary(outp)$adj.r.squared

######### i have no idea if this works ###########

#use r squred to find some model fits

#adj_r2[i] <- summary(outp)$adj.r.squared

# look at MSE

#mse_p[i] <- mean((predict(outp, MPO_ar_pt_test2) - MPO_ar_pt_test2$pt_ratr)^2)

# use AIC and or BIC to see if using more complex 
#models is better (especially with the use of the fourth degree)
  
################################################

# geberi

out<-lm(ratr~ages+I(ages^2),data=MPO_ar_pt_train)

sumsq[i] <- sum((predict(out,MPO_ar_pt_test)-MPO_ar_pt_test$ratr)^2)

mse[i] <- sqrt(mean((predict(out, MPO_ar_pt_test) - MPO_ar_pt_test$ratr)^2))

adj_r2[i] <- summary(out)$adj.r.squared


}
mean_SS_t_mod[j+1-13,1] <- mean(sumsq_p)
mean_SS_t_mod[j+1-13,2] <- mean(sumsq)
mean_SS_t_mod[j+1-13,3] <- mean(mse)
mean_SS_t_mod[j+1-13,4] <- mean(mse_p)
mean_SS_t_mod[j+1-13,5] <- mean(adj_r2)
mean_SS_t_mod[j+1-13,6] <- mean(adj_r2_p)
}

print(j)
}

#best fit player type
names(MPO_ar_pt)[which.min(mean_SS_t_mod$Mean_MSE_PlayerType)+12]
best_6_quad <- names(MPO_ar_pt)[order(mean_SS_t_mod$Mean_MSE_PlayerType)+12]

mean_SS_t_mod[order(mean_SS_t_mod$Mean_MSE_PlayerType),]

#visualizing results
pp <- apply(clustdm[,c(2,3,5,6)], 2, function(x) tapply(x, clustdm_cv$`3_anovadot`, mean,na.rm=T))
barplot(t(pp),beside=T, ylab = "Standardized Statistic (C1,C2, C1P, C2P)", xlab = "Player Type")

clustdm_cv

out<-lm(ratr~-1+`3_anovadot`+`3_anovadot`/ages+`3_anovadot`/I(ages^2),data=MPO_ar_pt)
summary(out)

ages_plot <- seq(-1,1,0.1)

rat_pred1 <- coef(out)[1]+coef(out)[4]*ages_plot+coef(out)[7]*ages_plot^2

rat_pred2 <- coef(out)[2]+coef(out)[5]*ages_plot+coef(out)[8]*ages_plot^2

rat_pred3 <- coef(out)[3]+coef(out)[6]*ages_plot+coef(out)[9]*ages_plot^2


ages_plot2<-ages_plot*sd(MPO_ar$Age)+mean(MPO_ar$Age)

plot(rat_pred2~ages_plot2,type="l",ylim=c(min(rat_pred3),max(rat_pred1)),lwd=2)
points(rat_pred1~ages_plot2,type="l",col="blue",lwd=2)
points(rat_pred3~ages_plot2,type="l",col="red",lwd=2)

## scatter plot with different color points to show 
ggplot(MPO_ar, aes(x = Age, y = ratr)) +
  geom_point(alpha = 0.3) +
  labs(title = "All Players' Ratings by Age",
       x = "Age", y = "Player Rating") +
  theme_minimal()

ggplot(MPO_ar, aes(x = Age, y = ratr, color = as.factor(pt_type))) +
  geom_point(alpha = 0.4) +
  labs(title = "Ratings by Age and Player Type",
       x = "Age", y = "Player Rating", color = "Player Type") +
  theme_minimal()

## table of best clustering types 
tab_best_quad <- data.frame(
  Best_cluster_types = c(best_6_quad[1:6])
)


#########################################
######### Change in rating ##############
#########################################

#####Needs to be restructured similar to raw rating method above########

#Calculate age curve
require(matrixStats)

MPO_ar$Name <- paste(MPO_ar$Name_f,MPO_ar$Name,sep=" ")

#use only players with ages between 16 and 52
MPO_ar_16_52<-subset(MPO_ar,MPO_ar$Age>=16 & MPO_ar$Age<=52)

Nrep<-1000
SS_pt_ch<-data.frame(matrix(0,nrow=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans)),ncol=Nrep))
SS_ch<-data.frame(matrix(0,nrow=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans)),ncol=Nrep))

for (m in 1:Nrep) {
  
#train and test split
tz <- round(length(unique(MPO_ar_16_52$PDGA))*.8)

#Curves by player type
for (i in 9:ncol(clustdm_cv)) {
  
train <- sample(unique(MPO_ar_16_52$PDGA),tz)
test <- unique(MPO_ar_16_52$PDGA)[!unique(MPO_ar_16_52$PDGA) %in% train]
  
MPO_ar_train <- MPO_ar_16_52[MPO_ar_16_52$PDGA %in% train,]
MPO_ar_test <- MPO_ar_16_52[MPO_ar_16_52$PDGA %in% test,]

#generic curve using test
ry<-tapply(MPO_ar_train$ratr,MPO_ar_train[c("Name","Age")],mean,na.rm=T)
ryp<-apply(ry,1,diff)
#generated weighted mean values for each age
w<-t(tapply(MPO_ar_train$ratrd,MPO_ar_train[c("Name","Age")],sum,na.rm=T))
aging1<-numeric(nrow(ryp))
for (k in 1:nrow(ryp)){
aging1[k]<-weighted.mean(ryp[k,],w[k,],na.rm=T)}
aging<-cumsum(c(0,aging1))
year1<-as.numeric(names(cumsum(c(0,rowMeans(ryp,na.rm=T))))[-1])
year<-c((min(year1)-1),year1)
m2<-lm(aging~year+I(year^2)+I(year^3)+I(year^4))

sumsq_ch<-numeric(length(unique(clustdm_cv[,i])))
sumsq_p_ch<-numeric(length(unique(clustdm_cv[,i])))
for (j in unique(clustdm_cv[,i])){

clustdm_cv_sub <- clustdm_cv[clustdm_cv[,i]==unique(clustdm_cv[,i])[j],]

#check to see if any of this iterations p type in training data

if (any(MPO_ar_16_52$Name %in% clustdm_cv_sub$names)) {
 if (any(MPO_ar_train$Name %in% clustdm_cv_sub$names) & any(MPO_ar_test$Name %in% clustdm_cv_sub$names)) {

MPO_ar_subj <- MPO_ar_train[MPO_ar_train$Name %in% clustdm_cv_sub$names,]
ry<-tapply(MPO_ar_subj$ratr,MPO_ar_subj[c("Name","Age")],mean,na.rm=T)
ryp<-apply(ry,1,diff)
  
#generated weighted mean values for each age
w<-t(tapply(MPO_ar_subj$ratrd,MPO_ar_subj[c("Name","Age")],sum,na.rm=T))
aging1<-numeric(nrow(ryp))
for (l in 1:nrow(ryp)){
  aging1[l]<-weighted.mean(ryp[l,],w[l,],na.rm=T)}
aging<-cumsum(c(0,aging1))
year1<-as.numeric(names(cumsum(c(0,rowMeans(ryp,na.rm=T))))[-1])  
year<-c((min(year1)-1),year1)

m3<-lm(aging~year+I(year^2)+I(year^3)+I(year^4))

####NEED TO ACTUALLY USE TEST DATA HERE########
MPO_ar_subj <- MPO_ar_test[MPO_ar_test$Name %in% clustdm_cv_sub$names,]
ry<-tapply(MPO_ar_subj$ratr,MPO_ar_subj[c("Name","Age")],mean,na.rm=T)
ryp<-apply(ry,1,diff)
ryp_test_data<-data.frame(ryp)
ryp_test_data$year<-as.numeric(unlist(dimnames(ryp)[1]))

test_data<-pivot_longer(ryp_test_data,1:(ncol(ryp_test_data)-1),names_to="Name",values_to="aging")
test_data<-test_data[-2]

sumsq_p_ch[j] <- sum((predict(m3,test_data)-test_data$aging)^2,na.rm=T)
sumsq_ch[j] <- sum((predict(m2,test_data)-test_data$aging)^2,na.rm=T)

 } else {
   
sumsq_p_ch[j] <- NA
sumsq_ch[j] <- NA
   
}

} else {
 
  sumsq_p_ch[j] <- 0
  sumsq_ch[j] <- 0
  
}
}

if (any(is.na(sumsq_p_ch) | is.na(sumsq_ch))) {

SS_pt_ch[i+1-9,m] <- NA
SS_ch[i+1-9,m] <- NA

} else {
  
SS_pt_ch[i+1-9,m] <- sum(sumsq_p_ch)
SS_ch[i+1-9,m] <- sum(sumsq_ch)
}

}

print(m)
}

#check if all iterations for a clust method are NA
all_na <- function(x){all(is.na(x))}
which(apply(SS_pt_ch,1,all_na))

rowMeans(SS_pt_ch)
rowMeans(SS_ch)



#plot generic aging curve
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(aging~year,pch=20,col=color,cex=1.5,ylab="Change in Rating",xlab="Age")#not controlling for inflation
lines(year,predict(m2),lwd=2,col="cadetblue4")