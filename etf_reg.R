library(readxl)
library(quantmod)
library(tidyverse)
library(lmtest)


etf_path <- 'work/work2024/work43/data'
etf_file <- list.files(etf_path)

ETF <- data.frame()
for (i in etf_file) {
  
  etf_i <- read.csv(paste0(etf_path, '/', i)) %>% mutate(ETF_code = strsplit(i, '_')[[1]][1])
  
  ETF <- rbind(ETF, etf_i)
  
}
rm(etf_i)

View(ETF)


ETF_return <- ETF %>% 
  mutate(Date = as.Date(Date)) %>% arrange(ETF_code, Date) %>% 
  group_by(ETF_code) %>% 
  mutate(Ret = Close / lag(Close) - 1) %>% ungroup() %>% 
  select(ETF_code, Date, Ret)


# Pcs data
pca_coords <- read.csv('work/work2024/work43/pca_coords.csv')
View(pca_coords)

pca_coords$Date <- as.Date(pca_coords$X) + 1


# merge data
merge_data <- merge(ETF_return, pca_coords %>% select(-X), by = 'Date', all.x = T)
View(merge_data)


# define one-by-one regression function
reg <- function(equation, data){
  Coef <- list()
  data <- na.omit(data)
  for (etf in unique(ETF$ETF_code)) {
    model1 <- lm(equation, data[data$ETF_code == etf,])
    Coef[[etf]] <- summary(model1)
    Coef[[paste0(etf, '_AIC')]] <- AIC(model1)
  }
  return(Coef)
}


coef_df4 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4, merge_data)
coef_df5 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5, merge_data)
coef_df6 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, merge_data)
coef_df7 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, merge_data)
coef_df8 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, merge_data)
coef_df9 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9, merge_data)
coef_df10 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, merge_data)
coef_df11 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, merge_data)
coef_df12 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12, merge_data)
coef_df13 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13, merge_data)
coef_df14 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14, merge_data)
coef_df15 <- reg(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15, merge_data)



# "BULD" "CLOU" "MOON" "PSCT" "SMH"  "SPAM" "TIME" "TINY" "VGT"  "XLK" 
# example
coef_df10$CLOU
coef_df10$CLOU_AIC


AIC_list <- list()
for (i in unique(ETF$ETF_code)) {
  
  list1 <- list()
  for (j in paste0('coef_df', c(4:15))) {
    
    list1[[j]] <- get(j)[[paste0(i, '_AIC')]]
    
  }
  AIC_list[[i]] <- list1
}
rm(list1)


for (k in names(AIC_list)) {
  df <- data.frame(t(data.frame(AIC_list[[k]])))
  names(df) <- 'aic'
  df$Model <- row.names(df)
  Model <- df[which.min(df$aic), 2]
  AIC <- min(df$aic)
  cat('ETF_coed:', k, '\t', 'Model:', Model, '\t', 'AIC:', AIC, '\n')
}
rm(df, Model, AIC)

coef_df10$CLOU



# ordinary pooled regression
m1 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4, merge_data %>% na.omit())
summary(m1)
AIC(m1)
m2 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5, merge_data %>% na.omit())
summary(m2)
AIC(m2)
m3 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, merge_data %>% na.omit())
summary(m3)
AIC(m3)
m4 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, merge_data %>% na.omit())
summary(m4)
AIC(m4)
m5 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, merge_data %>% na.omit())
summary(m5)
AIC(m5)
m6 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9, merge_data %>% na.omit())
summary(m6)
AIC(m6)
m7 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, merge_data %>% na.omit())
summary(m7)
AIC(m7)
m8 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, merge_data %>% na.omit())
summary(m8)
AIC(m8)
m9 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12, merge_data %>% na.omit())
summary(m9)
AIC(m9)
m10 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13, merge_data %>% na.omit())
summary(m10)
AIC(m10)
m11 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14, merge_data %>% na.omit())
summary(m11)
AIC(m11)
m12 <- lm(Ret ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15, merge_data %>% na.omit())
summary(m12)
AIC(m12)

which.min(c(AIC(m1),AIC(m2),AIC(m3),AIC(m4),AIC(m5),AIC(m6),AIC(m7),
            AIC(m8),AIC(m9),AIC(m10),AIC(m11),AIC(m12)))
