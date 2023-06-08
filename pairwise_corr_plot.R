setwd("C:/Users/jinzh/OneDrive - Universitetet i Oslo/Documents/WP2-ML/risk_score/Additional Work/new_code/Results_SV")
library(data.table)
library(GGally)
# install.packages("PerformanceAnalytics")
# library(PerformanceAnalytics)
library(flextable)
library(tidyverse)
files <- list.files(pattern = "for_the_tree_")
files <- files[!grepl("SV", files)]
files <- files[!grepl("Death", files)]

for (i in seq_along(files)){
  wave_name <- str_split(files[i], "_")[[1]][4]
  wave_name <- substr(wave_name, 1, 8)
  plot_name1 <- paste0("pairwise_age_", wave_name)
  plot_name2 <- paste0("pairwise_gender_", wave_name)
  plot_title1 <- paste0("Pairwise correlation of measures by age group ", wave_name)
  plot_title2 <- paste0("Pairwise correlation of measures by gender ", wave_name)
  df <- fread(files[i])
  
  df$Ensamble <- as.numeric(gsub(",",".",df$Ensamble))
  df$Probability <- as.numeric(gsub(",",".",df$Probability))
  df$upper <- as.numeric(gsub(",",".",df$upper))
  df$lower <- as.numeric(gsub(",",".",df$lower))
  df$Prevalence <- as.numeric(gsub(",",".",df$prevalence))
  df$Gender <- as.factor(df$Gender) 
  df$Age_group <- as.factor(df$Age_group) 
  
  setnames(df, "Ensamble","DRS")
  
  
  # bmp(filename = plot_name1, width = 1920, height = 1280, units = "px")
  # p1 <- ggpairs(df, columns = c("DRS","Probability","Weights", "Prevalence"),
  #         ggplot2::aes(color = Age_group), 
  #         title = plot_title1)
  # 
  # print(p1)
  # dev.off()
  # 
  # bmp(filename = plot_name2, width = 1920, height = 1280, units = "px")
  # p2 <- ggpairs(df, columns = c("DRS","Probability","Weights", "Prevalence"),
  #         ggplot2::aes(color = Gender), 
  #         title = plot_title2)
  # print(p2)
  # dev.off()
  # 
  # 
  tb1 <- data.table(wave=wave_name, 
                    Age=c("[0, 18)", "[18, 65)", "[65, 75)","[75,108)"),
                    Median = c(median(df[Age_group=="[0, 18)"]$DRS),
                               median(df[Age_group=="[18, 65)"]$DRS),
                               median(df[Age_group=="[65, 75)"]$DRS),
                               median(df[Age_group=="[75,108)"]$DRS)))
  if (i == 1) tb <- tb1
  else tb <- rbind(tb, tb1)
  write.csv2(tb, "median.csv")
}





