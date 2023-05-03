
country <- "NO"


  # TSD_install_path <- "/ess/p1380/data/durable/vac4eu/R_v4.2_packages_linux/" 
  # 
  # # try plot for the ensemble scores
  # setwd("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Results/")
  # # load("../Analysis/Permanent_datasets/temp17.RData")
  # library(data.table)
  # library(openxlsx, lib.loc = TSD_install_path)
  # library(stringr)
  # library(data.tree, lib.loc = TSD_install_path)
  # library(networkD3, lib.loc = TSD_install_path)
  # library(rmarkdown)
  # library(ggplot2, lib.loc = TSD_install_path)
  # library(plotly, lib.loc = TSD_install_path)
  # library(reticulate, lib.loc = TSD_install_path)

  setwd("C:/Users/jinzh/OneDrive - Universitetet i Oslo/Documents/WP2-ML/risk_score/Additional Work/new_code/Results")
  library(data.table)
  library(openxlsx)
  library(stringr)
  library(data.tree)
  library(networkD3)
  library(rmarkdown)
  library(ggplot2)
  library(plotly)
  library(reticulate)
  library(tidyverse)
  library(gt)
  library(ggpubr)



par <- c("AllWaves",
         "AllWave1",
         "AllWave2",
         "AllWave3",
         "Death",
         "DeathWave1",
         "DeathWave2",
         "DeathWave3")
I=1
# for (I in seq_along(par)){ 
  files <- list.files(pattern = paste0("Output_disease_risk_score_weights_prob_ul", par[I]))
  center <- par[I]
  for (i in seq_along(files)){
    df1 <- read.xlsx(files[i])
    df1$Age_group <- ifelse(is.na(str_split(files[i], "_")[[1]][8]), "All",
                            str_split(files[i], "_")[[1]][8])
    df1$Gender <- ifelse(is.na(substr(str_split(files[i], "_")[[1]][9],1,1)), "Both",
                         substr(str_split(files[i], "_")[[1]][9],1,1))
                         
    if (i==1) df <- df1
    else df <- rbind(df, df1)
  }
  rm(df1); gc()
  df$Ensamble <- as.numeric(df$Ensamble)
  
  #define the hierarchy (Session/Room/Speaker)
  n <- max(nchar(df$Predictors))
  df <- df[!df$Predictors %like% "var4",]
  df$code <- ifelse(df$Predictors %like% "diag",
                    substr(df$Predictors, 10, n),
                    substr(df$Predictors, 5, n))
  
  prev <- fread("prevalence.csv")
  df <- data.table(merge(df, prev, by = "code", all.x = TRUE))
  df <- df[!df$code==""]
  
  write.csv2(df,"for_the_tree.csv", row.names = FALSE)
  
  # df <- fread("for_the_tree.csv")
  
  # aggregate the informations
  df$Pred_Ens_Weights <- paste0(df$Predictors, ", " ,round(df$Ensamble,2),", ", df$Weights, 
                                round(df$Probability,2), ",", round(df$prevalence,2))
  df$pathString <- paste(center, df$Age_group, df$Gender, df$Pred_Ens_Weights, sep="|")
  
  #convert to Node
  patstree <- as.Node(df, pathDelimiter = "|")
  
  #plot with networkD3
  patstreeList <- ToListExplicit(patstree, unname = TRUE)
  
  fig <- radialNetwork(patstreeList,
                       fontSize = 12,
                       fontFamily = "calibri")
  fig
  
  # add legend
  ## Country
  ## Predictors, DRS, Weight, Probability, Prevalence
  
  htmlwidgets::saveWidget(
    widget = fig, #the figure
    file = paste0("tree_", center,"_", "NO",".html"), #the path & file name
    selfcontained = TRUE #creates a single html file
  )
# }
  
  df$Age_group <- as.factor(df$Age_group)
  df$Gender <- as.factor(df$Gender)

# panel plot try 1
  # use facet_grid for aligning the predictors
  # hard to read and so on

  # 
  # p0 <- 
  #   df |>
  #   ggplot(aes(y = fct_rev(Predictors))) + 
  #   theme_classic()
  # p0 <- p0 +
  #   geom_point(aes(x=Probability), shape=15, size=3) +
  #   geom_linerange(aes(xmin=lower, xmax=upper)) +
  #   facet_grid(vars(Age_group), vars(Gender)) +
  #   ylab('Predictors') 
  # p0 
  
# panel plots try 2
 # put subplot together
 # don't know who to align the predictors
  
  p11 <- 
    df[df$Age_group==levels(df$Age_group)[2]] |>
    ggplot(aes(y = fct_rev(Predictors))) + 
    theme_classic()
  p11 <- p11 +
    geom_point(aes(x=Probability), shape=15, size=3) +
    geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Probability, ', levels(df$Age_group)[2]))+
    ylab('Predictors')
  p11
 
   
  p12 <- 
    df[df$Age_group==levels(df$Age_group)[3]] |>
    ggplot(aes(y = fct_rev(Predictors))) + 
    theme_classic()
  p12 <- p12 +
    geom_point(aes(x=Probability), shape=15, size=3) +
    geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Probability, ', levels(df$Age_group)[3]))+
    ylab('Predictors')
  p12
  
  p13 <- 
    df[df$Age_group==levels(df$Age_group)[4]] |>
    ggplot(aes(y = fct_rev(Predictors))) + 
    theme_classic()
  p13 <- p13 +
    geom_point(aes(x=Probability), shape=15, size=3) +
    geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Probability, ', levels(df$Age_group)[4]))+
    ylab('Predictors')
  p13
  
  p14 <- 
    df[df$Age_group==levels(df$Age_group)[5]] |>
    ggplot(aes(y = fct_rev(Predictors))) + 
    theme_classic()
  p14 <- p14 +
    geom_point(aes(x=Probability), shape=15, size=3) +
    geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Probability, ', levels(df$Age_group)[5]))+
    ylab('Predictors')
  p14
  
  

  p1 <- ggarrange(p11, p12, p13, p14, ncol = 4, nrow = 1)
  p1
    

  p21 <- 
    df[df$Age_group==levels(df$Age_group)[2]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Weights)) + 
    theme_classic()
  p21 <- p21 +
    geom_point(aes(x=Weights)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Weights, ', levels(df$Age_group)[2]))+
    ylab('Predictors')
  p21
  
  
  p22 <- 
    df[df$Age_group==levels(df$Age_group)[3]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Weights)) + 
    theme_classic()
  p22 <- p22 +
    geom_point(aes(x=Weights)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Weights, ', levels(df$Age_group)[3]))+
    ylab('Predictors')
  p22
  
  p23 <- 
    df[df$Age_group==levels(df$Age_group)[4]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Weights)) + 
    theme_classic()
  p23 <- p23 +
    geom_point(aes(x=Weights)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Weights, ', levels(df$Age_group)[4]))+
    ylab('Predictors')
  p23
  
  p24 <- 
    df[df$Age_group==levels(df$Age_group)[5]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Weights)) + 
    theme_classic()
  p24 <- p24 +
    geom_point(aes(x=Weights)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Weights, ', levels(df$Age_group)[5]))+
    ylab('Predictors')
  p24
  
  
  
  p2 <- ggarrange(p21, p22, p23, p24, ncol = 4, nrow = 1)
  p2

  p11_24 <- ggarrange(p11, p12, p13, p14, 
                     p21, p22, p23, p24, ncol = 4, nrow = 2) 
  p11_24
  
  
  p31 <- 
    df[df$Age_group==levels(df$Age_group)[2]] |>
    ggplot(aes(y = fct_rev(Predictors), size=prevalence)) + 
    theme_classic()
  p31 <- p31 +
    geom_point(aes(x=prevalence)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Prevalence, ', levels(df$Age_group)[2]))+
    ylab('Predictors')
  p31
  
  
  p32 <- 
    df[df$Age_group==levels(df$Age_group)[3]] |>
    ggplot(aes(y = fct_rev(Predictors), size=prevalence)) + 
    theme_classic()
  p32 <- p32 +
    geom_point(aes(x=prevalence)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Prevalence, ', levels(df$Age_group)[3]))+
    ylab('Predictors')
  p32
  
  p33 <- 
    df[df$Age_group==levels(df$Age_group)[4]] |>
    ggplot(aes(y = fct_rev(Predictors), size=prevalence)) + 
    theme_classic()
  p33 <- p33 +
    geom_point(aes(x=prevalence)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Prevalence, ', levels(df$Age_group)[4]))+
    ylab('Predictors')
  p33
  
  p34 <- 
    df[df$Age_group==levels(df$Age_group)[5]] |>
    ggplot(aes(y = fct_rev(Predictors), size=prevalence)) + 
    theme_classic()
  p34 <- p34 +
    geom_point(aes(x=prevalence)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Prevalence, ', levels(df$Age_group)[5]))+
    ylab('Predictors')
  p34
  
  
  
  p3 <- ggarrange(p31, p32, p33, p34, ncol = 4, nrow = 1)
  p3
  
  # p11_34 <- ggarrange(p11, p12, p13, p14,
  #                     p21, p22, p23, p24,
  #                     p31, p32, p33, p34, ncol = 4, nrow = 3) 
  # 2-row is optimal so...
  
  p41 <- 
    df[df$Age_group==levels(df$Age_group)[2]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Ensamble)) + 
    theme_classic()
  p41 <- p41 +
    geom_point(aes(x=Ensamble)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Disease RIsk Score (DRS), ', levels(df$Age_group)[2]))+
    ylab('Predictors')
  p41
  
  
  p42 <- 
    df[df$Age_group==levels(df$Age_group)[3]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Ensamble)) + 
    theme_classic()
  p42 <- p42 +
    geom_point(aes(x=Ensamble)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Disease RIsk Score (DRS), ', levels(df$Age_group)[3]))+
    ylab('Predictors')
  p42
  
  p43 <- 
    df[df$Age_group==levels(df$Age_group)[4]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Ensamble)) + 
    theme_classic()
  p43 <- p43 +
    geom_point(aes(x=Ensamble)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Disease RIsk Score (DRS), ', levels(df$Age_group)[4]))+
    ylab('Predictors')
  p43
  
  p44 <- 
    df[df$Age_group==levels(df$Age_group)[5]] |>
    ggplot(aes(y = fct_rev(Predictors), size=Ensamble)) + 
    theme_classic()
  p44 <- p44 +
    geom_point(aes(x=Ensamble)) +
    # geom_linerange(aes(xmin=lower, xmax=upper)) +
    facet_wrap(~Gender) +
    xlab(paste0('Disease RIsk Score (DRS), ', levels(df$Age_group)[5]))+
    ylab('Predictors')
  p44
  
  
  
  p4 <- ggarrange(p41, p42, p43, p44, ncol = 4, nrow = 1)
  p4
  
  p31_44 <- ggarrange(p31, p32, p33, p34, 
                      p41, p42, p43, p44, ncol = 4, nrow = 2) 
  p31_44
  
