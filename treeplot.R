TSD_install_path <- "/ess/p1380/data/durable/vac4eu/R_v4.2_packages_linux/" 

# try plot for the ensemble scores
setwd("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Results/")
# load("../Analysis/Permanent_datasets/temp17.RData")
library(data.table)
library(openxlsx, lib.loc = TSD_install_path)
library(stringr)
library(data.tree, lib.loc = TSD_install_path)
library(networkD3, lib.loc = TSD_install_path)
library(rmarkdown)
library(ggplot2, lib.loc = TSD_install_path)
library(plotly, lib.loc = TSD_install_path)
library(reticulate, lib.loc = TSD_install_path)


par <- c("AllWaves",
         "AllWave1",
         "AllWave2",
         "AllWave3",
         "Death",
         "DeathWave1",
         "DeathWave2",
         "DeathWave3")

for (I in seq_along(par)){
  files <- list.files(pattern = paste0("Output_disease_risk_score_weights_", par[I]))
  center <- par[I]
  for (i in seq_along(files)){
    df1 <- read.xlsx(files[i])
    df1$Age_group <- str_split(files[i], "_")[[1]][7]
    df1$Gender <- substr(str_split(files[i], "_")[[1]][8],1,1)
    if (i==1) df <- df1
    else df <- rbind(df, df1)
  }
  rm(df1); gc()
  df$Ensamble <- as.numeric(df$Ensamble)
  #define the hierarchy (Session/Room/Speaker)
  df <- df[!df$Predictors %like% "var4",]
  df$Pred <- ifelse(df$Predictors %like% "diag", 
                    substr(df$Predictors, 10, 12),
                    substr(df$Predictors, 5, 11))
  
  df$Pred_Ens_Weights <- paste0(df$Pred, ", " ,round(df$Ensamble,2),", ", df$Weights)
  df$pathString <- paste(center, df$Age_group, df$Gender, df$Pred_Ens_Weights, sep="|")
  
  #convert to Node
  patstree <- as.Node(df, pathDelimiter = "|")
  
  #plot with networkD3
  patstreeList <- ToListExplicit(patstree, unname = TRUE)
  
  fig <- radialNetwork(patstreeList)
  fig
  htmlwidgets::saveWidget(
    widget = fig, #the figure
    file = paste0("tree_", center,".html"), #the path & file name
    selfcontained = TRUE #creates a single html file
  )
}