#############################################################################################
#Module 0: Libraries and function
#Version 1.0 
#Last change: 26/10/2021
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
TSD_install_path <- "N:\\durable\\vac4eu\\R_v4.1_packages\\" 

library(haven)
library(dplyr)
library(Epi, lib.loc = TSD_install_path)
library(stringr)
library(data.table)
library(etm, lib.loc = TSD_install_path)

func1 <- function(temp, var1, var2) {
  temp %>%
    group_by({{ var1 }}) %>%
    arrange ({{ var2 }}) %>%
    filter(row_number()==1)
}

###########################################
# END for DM
###########################################


library(ggplot2)
library(caret)
library(EFS,lib.loc = TSD_install_path)
library(ROCit, lib.loc = TSD_install_path)
library(stringr) # Jing: for str_detect()
library(openxlsx) # Jing: for writing out ensembled score
#library(rmarkdown) # Jing: used sink() instead
library(pracma)
library(lubridate)
library(arules)
# library(table1) # Jing: no need anymore
library(Publish)
library(ROSE)

###########################################
# END for Analysis
###########################################