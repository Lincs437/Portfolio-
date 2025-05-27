require(lme4)
require(lmerTest)
require(MuMIn)
require(cAIC4)
library(splines)
library(ggplot2)


# Read in age and PDGA data
MPO_ar <- read.csv("MPO_ar.csv", header = TRUE)

# Standardize year and age
MPO_ar$years <- scale(MPO_ar$year)
MPO_ar$ages <- scale(MPO_ar$Age)

# Combine ratings and classes (assumes clss, clss_sc, clss_hc, clss_kmeans, clustdm_cv exist)
MPO_ar_tune <- data.frame(matrix(0, nrow = nrow(MPO_ar), ncol = ncol(cbind(clss, clss_sc, clss_hc, clss_kmeans))))
for (i in 1:nrow(MPO_ar)) {
  if (any(grepl(paste(MPO_ar$Name_f[i], MPO_ar$Name[i], sep = " "), clustdm_cv$names))) {
    MPO_ar_tune[i, ] <- as.numeric(clustdm_cv[grep(paste(MPO_ar$Name_f[i], MPO_ar$Name[i], sep = " "), clustdm_cv$names), c(9:ncol(clustdm_cv))])
  } else {
    MPO_ar_tune[i, ] <- NA
  }
}

# Create tuning dataframe and remove NAs
MPO_ar_tune2 <- data.frame(MPO_ar, MPO_ar_tune)
names(MPO_ar_tune2)[(ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)] <- names(clustdm_cv)[(ncol(clustdm_cv)-64):ncol(clustdm_cv)]
MPO_ar_pt <- subset(MPO_ar_tune2, MPO_ar_tune2$nc2 != "NA")

# Initialize result table
mean_SS_t <- data.frame(matrix(0, nrow = ncol(cbind(clss, clss_sc, clss_hc, clss_kmeans)), ncol = 7))
colnames(mean_SS_t) <- c("Mean_SSE_PlayerType", "Mean_SSE_NoPlayerType", "Mean_MSE_NoPlayerType", "Mean_MSE_PlayerType", "Adj_R_Sqr_NoPlayerType", "Adj_R_Sqr_PlayerType", "DF_Used")

# Cross-validation loop
for (j in (ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)) {
  
  # Check if have >1 player type for analysis
  if (length(unique(MPO_ar_pt[, j])) == 1) {
    mean_SS_t[j + 1 - 13, ] <- NA
  } else {
    
    df_grid <- 3:8  # Try df from 3 to 8
    best_mse <- Inf
    best_df <- NA
    best_adj_r2_p <- NA
    best_sumsq_p <- NA
    best_sumsq <- NA
    
    for (df_try in df_grid) {
      sumsq_p <- numeric(100)
      mse_p <- numeric(100)
      adj_r2_p <- numeric(100)
      sumsq <- numeric(100)
      mse <- numeric(100)
      adj_r2 <- numeric(100)
      
      tz <- round(length(unique(MPO_ar_pt$PDGA)) * 0.8)
      
      for (i in 1:100) {
        train <- sample(unique(MPO_ar_pt$PDGA), tz)
        test <- setdiff(unique(MPO_ar_pt$PDGA), train)
        
        MPO_ar_pt[, j] <- as.factor(MPO_ar_pt[, j])
        
        MPO_ar_pt_train <- MPO_ar_pt[MPO_ar_pt$PDGA %in% train, ]
        MPO_ar_pt_test <- MPO_ar_pt[MPO_ar_pt$PDGA %in% test, ]
        
        while (!all(unique(MPO_ar_pt_test[, j]) %in% unique(MPO_ar_pt_train[, j]))) {
          train <- sample(unique(MPO_ar_pt$PDGA), tz)
          test <- setdiff(unique(MPO_ar_pt$PDGA), train)
          
          MPO_ar_pt_train <- MPO_ar_pt[MPO_ar_pt$PDGA %in% train, ]
          MPO_ar_pt_test <- MPO_ar_pt[MPO_ar_pt$PDGA %in% test, ]
        }
        
        pt_type <- MPO_ar_pt_train[, j]
        pt_ages <- MPO_ar_pt_train$ages
        pt_ratr <- MPO_ar_pt_train$ratr
        MPO_ar_pt_train2 <- data.frame(pt_type, pt_ages, pt_ratr)
        
        # Fit spline models
        outp <- lm(pt_ratr ~ pt_type * bs(pt_ages, degree = 3, df = df_try), data = MPO_ar_pt_train2)
        
        pt_type_test <- MPO_ar_pt_test[, j]
        pt_ages_test <- MPO_ar_pt_test$ages
        pt_ratr_test <- MPO_ar_pt_test$ratr
        MPO_ar_pt_test2 <- data.frame(pt_type = pt_type_test, pt_ages = pt_ages_test, pt_ratr = pt_ratr_test)
        
        preds_p <- predict(outp, newdata = MPO_ar_pt_test2)
        
        sumsq_p[i] <- sum((preds_p - MPO_ar_pt_test2$pt_ratr)^2)
        mse_p[i] <- mean((preds_p - MPO_ar_pt_test2$pt_ratr)^2)
        adj_r2_p[i] <- summary(outp)$adj.r.squared
        
        # No player type model
        out <- lm(ratr ~ bs(ages, degree = 3, df = df_try), data = MPO_ar_pt_train)
        preds <- predict(out, newdata = MPO_ar_pt_test)
        
        sumsq[i] <- sum((preds - MPO_ar_pt_test$ratr)^2)
        mse[i] <- mean((preds - MPO_ar_pt_test$ratr)^2)
        adj_r2[i] <- summary(out)$adj.r.squared
      }
      
      mean_mse_p <- mean(mse_p)
      
      if (mean_mse_p < best_mse) {
        best_mse <- mean_mse_p
        best_df <- df_try
        best_adj_r2_p <- mean(adj_r2_p)
        best_sumsq_p <- sum(sumsq_p)
        best_sumsq <- sum(sumsq)
      }
    }
    
    mean_SS_t[j + 1 - 13, 1] <- best_sumsq_p
    mean_SS_t[j + 1 - 13, 2] <- best_sumsq
    mean_SS_t[j + 1 - 13, 3] <- sqrt(best_sumsq / length(mse))  # RMSE no type
    mean_SS_t[j + 1 - 13, 4] <- sqrt(best_mse)                  # RMSE player type
    mean_SS_t[j + 1 - 13, 5] <- mean(adj_r2)
    mean_SS_t[j + 1 - 13, 6] <- best_adj_r2_p
    mean_SS_t[j + 1 - 13, 7] <- best_df
  }
  
  print(j)
}

# Best player type
names(MPO_ar_pt)[which.min(mean_SS_t$Mean_MSE_PlayerType) + 12]
best_order <- names(MPO_ar_pt)[order(mean_SS_t$Mean_MSE_PlayerType) + 12]

mean_SS_t[order(mean_SS_t$Mean_MSE_PlayerType), ]

# Visualizing results
pp <- apply(clustdm[, c(2, 3, 5, 6)], 2, function(x) tapply(x, clustdm_cv$`3_anovadot`, mean, na.rm = TRUE))
barplot(t(pp), beside = TRUE, ylab = "Standardized Statistic (C1,C2, C1P, C2P)", xlab = "Player Type")

clustdm_cv

# Final polynomial-based visualization for sanity check
out <- lm(ratr ~ -1 + `3_anovadot` + `3_anovadot` / ages + `3_anovadot` / I(ages^2), data = MPO_ar_pt)
summary(out)

ages_plot <- seq(-1, 1, 0.1)

rat_pred1 <- coef(out)[1] + coef(out)[4] * ages_plot + coef(out)[7] * ages_plot^2
rat_pred2 <- coef(out)[2] + coef(out)[5] * ages_plot + coef(out)[8] * ages_plot^2
rat_pred3 <- coef(out)[3] + coef(out)[6] * ages_plot + coef(out)[9] * ages_plot^2

ages_plot2 <- ages_plot * sd(MPO_ar$Age) + mean(MPO_ar$Age)

plot(rat_pred2 ~ ages_plot2, type = "l", ylim = c(min(rat_pred3), max(rat_pred1)), lwd = 2)
points(rat_pred1 ~ ages_plot2, type = "l", col = "blue", lwd = 2)
points(rat_pred3 ~ ages_plot2, type = "l", col = "red", lwd = 2)

### other visualizations to export to rmd ### 

## scatter plot with different color points to show 
ggplot(MPO_ar_pt, aes(x = Age, y = ratr)) +
  geom_point(alpha = 0.3) +
  labs(title = "All Players' Ratings by Age",
       x = "Age", y = "Player Rating") +
  theme_minimal()
# with color
ggplot(MPO_ar_pt, aes(x = Age, y = ratr, color = as.factor(`3_anovadot`))) +
  geom_point(alpha = 0.4) +
  labs(title = "Ratings by Age and Player Type",
       x = "Age", y = "Player Rating", color = "Player Type") +
  theme_minimal()
# using facet wrap 
ggplot(MPO_ar_pt, aes(x = Age, y = ratr, color = as.factor(`3_anovadot`))) +
  geom_point(alpha = 0.4) + 
  facet_wrap(~`6_manhattan`) +
  labs(title = "Ratings by Age and Player Type",
       x = "Age", y = "Player Rating", color = "Player Type") +
  theme_minimal()


#MPO_AR_PT
#change to having factor as what the color is split up as 

## use facet wrap, similar to code for scatter facte_wrap(var(clustering))

## table of best clustering types 
tab_best_order <- data.frame(
  Best_cluster_types = c(best_order[1:6])
)

#performance by cluster on bar plots (MSE measure) 
mean_SS_t$Cluster <- colnames(cbind(clss, clss_sc, clss_hc, clss_kmeans))
ggplot(mean_SS_t, aes(x = reorder(Cluster, Mean_MSE_PlayerType), y = Mean_MSE_PlayerType)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "RMSE of Player-Type Models by Clustering Method",
       x = "Clustering Variable", y = "RMSE (Player-Type)") +
  theme_minimal()










