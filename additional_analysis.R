#############################################################################################
## NO: replace /tsd/p1380/data with /tsd/p1380/data for runing on linux vm anddd add "_L" to module0
## the "_L in line 17 is just for me to install packages for Linux, if you run on windows, remove it."

################################################
## path and packages
################################################
setwd("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Analysis")
result_path <- "/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Results/"

source("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Modules/Module0_L.R") # the "_L just for me to install packages for Linux, if you run on windows, remove it."
source("/tsd/p1380/data/durable/Nordic_CDM/Framework_NCDM/Analysis/Modules/Module0_L.R") # same as above

################################################
## data management
################################################
# get all pop and all predictors & outcomes
load ("Permanent_datasets/tempal.RData")
load ("../Permanent_datasets/case_control_risk_set_matching_formating.RData") # temp17
tempal0 <- tempal

# format dates
ref_date <- as.Date("1901-05-02")
cases_register<- read_sas("/tsd/p1380/data/durable/Nordic_CDM/wp2_cdm/cases.sas7bdat", NULL) 
cases_register$def3_start_date <- ref_date + cases_register$def3_start_date
cases_register$def1_start_date <- ref_date + cases_register$def1_start_date
cases_register$def2_start_date <- ref_date + cases_register$def2_start_date
save(cases_register, file="Permanent_datasets/cases_register.RData")

# age_quantile (removed)& age_group
temp17 <- temp17[!duplicated(temp17$var100),]
temp17$age <- as.numeric(round((temp17$var108-temp17$var102)/365,0))
temp17$age[temp17$age<0] <- 0
temp <- temp17[temp17$age<18,]
max_age <- max(temp17$age)

 # discretize age with cluster 
 # temp17$Age_quantile <- discretize(temp17$age, method = "cluster", breaks=4)
 temp17$Gender <- factor(temp17$var103, labels=c("M", "F"))

 # define age group from comments
 temp17$Age_group <- cut(temp17$age, breaks =  c(-1,17,64,74,max_age),
                      labels = c("[0, 18)", 
                                 "[18, 65)",
                                 "[65, 75)",
                                 paste0("[75,", max_age, ")")))
 
 save(temp17, file="Permanent_datasets/temp17.RData")

levels(temp17$Age_group)

par <- c("AllWaves",
         "AllWave1",
         "AllWave2",
         "AllWave3",
         "Death",
         "DeathWave1",
         "DeathWave2",
         "DeathWave3")



################################################
## waves, death
################################################


for ( i in  seq_along(par)){ #
  wave_name <- par[i]
  load ("Permanent_datasets/temp17.RData")
  pop <- temp17
 for (j in levels(temp17$Age_group)){
    for (k in levels(temp17$Gender)){
      load ("Permanent_datasets/temp17.RData")
      load("Permanent_datasets/cases_register.RData")
      pop <- temp17
    temp17 <- temp17[temp17$Age_group==j & temp17$Gender==k,] #   0 1901   1   397
    file_name <- paste0(wave_name,"_",j,"_",k)

# put in the dates for waves
if (i==1|i==5){ # all waves
  var2<-"2019-02-15"
  var3<-"2999-06-26"
}else if (i==2|i==6){ # wave 1
  var2<-"2020-02-15"
  var3<-"2020-06-04"
}else if (i==3|i==7){ # wave 2
  var2<-"2020-10-14"
  var3<-"2021-06-05"
}else if (i==4|i==8){ # wave 3
  var2<-"2021-06-06"
  var3<-"2021-12-31"
}

################################################
## Table 1
################################################

if (wave_name %like% "Death"){
  if (k=="M"){
    # subset death
    cases <- cases_register[which(cases_register$def3==3),]
    cases<-cases[cases$def3_start_date>=var2 & 
                   cases$def3_start_date<=var3,]
    setnames(cases, "person_id", "var100", skip_absent=TRUE)
    temp000 <- cases[,c("var100","case")]
    temp000 <- unique(temp000)
    cases1<- merge(pop,temp000,by="var100",all.x=T)
    # Jing: assign 1 to NA for case
    cases1$case <- ifelse(is.na(cases1$case), 0, cases1$case) 
    table(cases1$var4, cases1$case)
    cases1$var4 <- ifelse(cases1$var4==1, 
                          ifelse(cases1$case==1,1,0),0)
    cases1$var4 <- ifelse(is.na(cases1$var4),0,cases1$var4)
    df <- as.data.frame(aggregate(cases1$var4, by=list(cases1$var0),FUN=sum))
    colnames(df) <- c("var0","proxy")
    cases1<- merge(cases1,df,by="var0",all.x=T)
    cases1 <- cases1[which(cases1$proxy>0),]
    pop <- cases1
    pop$Death <- factor(pop$var4, labels=c("No", "Yes")) # Hospitalised
    
    tb <- data.table(summary(univariateTable(Death~Age_group+Gender, data=pop))) # Age_quantile+
    # remove "p-value"
    tb = tb[,"p-value":=NULL]
    
    # tb <- table1(~Age_group+Gender|Death, data=pop) # Age_quantile+
    write.xlsx(tb,paste0(result_path, "tb_",wave_name, ".xlsx"))
  }
  }else{
    
    pop <- pop[pop$var108>=var2 & pop$var108<=var3,]
  pop$Hospitalised <- factor(pop$var4, labels=c("No", "Yes")) # Hospitalised
  
  tb <- data.table(summary(univariateTable(Hospitalised~Age_group+Gender, data=pop))) # Age_quantile+
  # remove "p-value"
  tb = tb[,"p-value":=NULL]
  # tb <- table1(~Age_group+Gender|Hospitalised, data=pop) # Age_quantile+
  write.xlsx(tb,paste0(result_path, "tb_",wave_name ,".xlsx"))
}

temp17<-temp17[temp17$var108>=var2 & temp17$var108<=var3,]
if ( i >= 5 ) temp17 <- temp17[temp17$var100 %in% unique(pop$var100),]
#check if it's ok to continue
if (nrow(temp17)>=500){

  ################################################
  ## Module 2
  ################################################
  tempal0<-tempal%>%
    right_join(temp17[c("var100","var4")], by=c("var100")) # same var4 distr

  tempall <- tempal0[,-ncol(tempal0)] # remove var4 outcome
  var4 <- tempal0[,ncol(tempal0)] # same distr
  tempall <- tempall[,-1] # remove id var100

  tempall <- mutate_all(tempall, ~replace(.,is.na(.),0))

  tempall <-tempall[,colSums(tempall !=0)> 0]
  tempall <- tempall[sapply(tempall, is.numeric)]
  var20 <- apply(tempall, 2,var) # Jing: var2 changed to var20
  temp34<-as.data.frame(var20[which(var20>0.001)])
  temp34$var30 <- rownames(temp34) # Jing: var3 changed to var30
  tempall<- tempall[,temp34$var30]
  tempall1<-cbind(tempall,var4)
  tempall1 <- mutate_all(tempall1, ~replace(.,is.na(.),0))

  save(tempall1,file=paste0("Permanent_datasets/tempall1_",
                            file_name,
                            ".RData"))

  ################################################
  ## Module 3
  ################################################

  # rm(list=ls())
  gc()
  # load (paste0("Permanent_datasets/tempall1_",
  #              file_name,
  #              ".RData"))

  temp40 <- ensemble_fs(data =  tempall1,
                        classnumber = length(tempall1),
                        runs = 1,
                        selection = c(TRUE, TRUE, TRUE, TRUE,
                                      FALSE, FALSE, FALSE,FALSE))
  save(temp40,file=paste0("Permanent_datasets/temp40_",
                          file_name,
                          ".RData"))

  ################################################
  ## Module 4
  ## Optimal feature Selection base on ensemble
  ################################################
  # rm(list=ls())
  gc()

  var301<-"U07" # change from var30 to var301

  # load(paste0("Permanent_datasets/tempall1_",
  #             file_name,
  #             ".RData"))
  # load(paste0("Permanent_datasets/temp40_",
  #             file_name,
  #             ".RData"))

  temp35 <- as.data.frame(t(temp40))
  temp35$var35b <- temp35$Median+temp35$P_cor+temp35$S_cor+temp35$LogReg+temp35$ER_RF+temp35$Gini_RF
  temp35 <- arrange(temp35,-var35b)

  #temp35 <- temp35[3:nrow(temp35),] #to be removed
  temp35$var30 <- rownames(temp35)

  temp35 <- temp35 %>%
    filter(!str_detect(var30, var301))

  temp35b <- as.data.frame(temp35$var35b)
  colnames(temp35b) <- "var35b"
  temp35b$var35a <- lag(temp35b$var35b)
  temp35b$var35c <- temp35b$var35b-temp35b$var35a
  temp35b1 <- median(na.omit(temp35b$var35c))
  temp35b$var35d <- ifelse(temp35b$var35c<temp35b1,1,0)
  temp35b$var35d[1] <- temp35b$var35d[2]
  temp35b$var35e <- rowid(rleid(temp35b$var35d))

  temp35b$var35f <- 1:nrow(temp35b)
  temp35b2 <- temp35b[which.max(temp35b$var35e),]
  var35e<- as.numeric(temp35b2$var35f)
  temp36 <- temp35[1:var35e,]

  # barplot to be done for var35b and var3

  bmp(file=paste0(result_path, "Bar_ensemble_score_",
                  file_name,
                  ".bmp"),
      width=8, height=12,unit="in", res=200)

  plot(barchart(reorder(rownames(temp36), var35b)~temp36$var35b, xlim=c(0,1), data=temp36, horiz=T,
                levels=sort(temp36$var35b, decreasing=T), xlab="Ensemble score"))

  dev.off()

  write.xlsx(temp35,paste0(result_path,"ensemble_score_all_",
                           file_name,
                           ".xlsx" ))


  # excel sheet of ensembled score
  write.xlsx(temp36[,c("var30", "var35b")],paste0(result_path,"Bar_ensemble_score_",
                                                  file_name,
                                                  ".xlsx"))



  tempall2=cbind(tempall1[,c(temp36$var30)],var4=tempall1$var4)
  save(tempall2,file=paste0("Permanent_datasets/tempall2_",
                            file_name,
                            ".RData"))

  ###extra features input

  temp36a <- temp35[1:(var35e),] # Jing: removed +2 after var35e

  tempall2a=cbind(tempall1[,c(temp36a$var30)],var4=tempall1$var4)
  save(tempall2a,file=paste0("Permanent_datasets/tempall2a_",
                             file_name,
                             ".RData"))
  gc()
  ################################################
  ## Module 5
  ## Optimal feature Selection based on ROC
  ################################################
  # rm(list=ls())
  # gc()
  # load(paste0("Permanent_datasets/tempall2_",
  #             file_name,
  #             ".RData"))
  # load(paste0("Permanent_datasets/tempall2a_",
  #             file_name,
  #             ".RData"))

  temp37<-tempall2a
  temp37$var4 <- ifelse(temp37$var4==0,"no","yes") #   no 47464  yes 397, remove "" from "0"
  var5 <- trainControl(method = "cv", number = 5,
                       classProbs =TRUE,
                       summaryFunction = twoClassSummary)
  class1 <- caret::train(var4 ~ .,data=temp37,
                         trControl=var5,
                         method="glm",
                         metric="ROC")


  VarImp <- varImp(class1, scale=F)
  a <- VarImp$importance # extract 10 most important features
  a$Var<- rownames(a)

  # -2 here because the function below gives i=1 already.
  temp39 <- vector("numeric",(ncol(tempall2a)-2))


  func1<-function(var6){
    temp <- temp37[,a$Var[1:var6]]
    temp$var4 <-temp37$var4
    var5 <- trainControl(method = "cv",number = 5,
                         classProbs =TRUE,
                         summaryFunction = twoClassSummary )
    class1 <- caret::train(var4 ~ .,data=temp,
                           trControl=var5,
                           method="glm",
                           metric="ROC")
    temp41<-class1$results[,2]
    temp0<- temp41
    return(temp0)
  }
  am <-(ncol(tempall2a)-1)
  for (I in 2:am){
    temp39[I-1]<-func1(I)
  }


  save(temp39,file=paste0("temp39_", file_name, ".RData"))
  gc()

  x <- temp39
  # y=diff(x)
  # output <- vector("character", length(y))
  # for (i in 1: length(y)){
  #   if (y[i]<0){
  #     output[i] <- i
  #   }
  # }
  # output
  # min(as.numeric(output), na.rm = TRUE)

  calliper <- 0.0035 # justify why
  # if the SD from some point to the end is less than
  # our predefined threshold, we pick up that point.
  output <- vector("character", length(x))
  for ( J in 1:(length(x)-1)){
    if (sd(x[J:length(x)-1])<calliper){
      output[J] <- J+1
    }
  }
  output
  # Jing: add the if else for when there wer
  if (!sum(output=="")==length(output)){
    indices<-min(as.numeric(output), na.rm = TRUE)
  }else{indices <- length(output)}

  # plotting


  x=1:length(temp39)
  y=temp39

  bmp(file=paste0(result_path, "ROC_calliper_",
                  file_name,".bmp"),
      width=6, height=8, unit="in", res=200)
  plot(x, y, type = "l", xlab="Number of features", ylab="AUC" )
  points(x, y, pch=19, col='gray')
  abline(h=y[indices], col="gray")
  abline(v=indices, col="gray")
  points(x[indices], y[indices], pch=19, col='red')
  dev.off()

  write.xlsx(as.data.frame(temp39),
             paste0(result_path, "Roc_calliper_",
                    file_name,
                    ".xlsx"))

  # tempall3=cbind(tempall2[,1:indices],var4=tempall2$var4)
  tempall3=cbind(tempall2[,1:indices])

  save(tempall3,file=paste0("Permanent_datasets/tempall3_",
                            file_name,
                            ".RData"))
  gc()
  ################################################
  ## Module 6
  ## Data set based on Optimal feature Selection
  ## and plotting
  ################################################
  # rm(list=ls())
  gc()
  # load(paste0("temp39_",
  #             file_name,
  #             ".RData"))
  bmp(file=paste0(result_path, "M6_AUC_", file_name, ".bmp"),
      width=8, height=12,unit="in", res=200)
  plot(1:length(temp39) , abs(temp39),
       type="l",xlab="N. features", ylab = "AUC")
  dev.off()

  bmp(file=paste0(result_path, "M6_minpeakdistance_", file_name, ".bmp"),
      width=8, height=12,unit="in", res=200)
  plotPeaks = function(df,K){
    p = findpeaks(temp39,minpeakdistance=K,minpeakheight=0)
    plot(1:length(temp39),main=paste("minpeakdistance=",K))
    abline(v=p[,2])
  }

  par(mfrow=c(2,2))
  for(n in c(3,5,7,9)){
    plotPeaks(df,n)
  }
  dev.off()

  ################################################
  ## Module 7
  ## Classification
  ################################################
  # rm(list=ls())
  gc()

  # load(paste0("Permanent_datasets/tempall3_",
  #             file_name,
  #             ".RData"))
  tempall3 <- temp37

  N0<- table(tempall3$var4)
  temp37<- ovun.sample(var4 ~ . ,data=tempall3, method="both", N=N0[2]*2)$data 

  # temp37$var4 <- ifelse(temp37$var4==0,"no","yes")
  var5 <- trainControl(method = "cv", number = 5,
                       classProbs =TRUE, summaryFunction = twoClassSummary)

  class1 <- caret::train(var4 ~ .,data=temp37,trControl=var5, method="glm", metric="ROC")
  # class2 <- caret::train(var4 ~ ., data=temp37,trControl=var5, method='rf', metric="ROC")
  class3 <- caret::train(var4 ~ ., data=temp37,trControl=var5, method='ranger', metric="ROC")
  class4 <- caret::train(var4 ~ ., data=temp37,trControl=var5, method='rpart', metric="ROC")


  save(class1,file=paste0("class1_", file_name, ".RData"))
  # save(class2,file=paste0("class2_", file_name, ".RData"))
  save(class3,file=paste0("class3_", file_name, ".RData"))
  save(class4,file=paste0("class4_", file_name, ".RData"))

  ################################################
  ## Module 9
  ## DISEASE RISK SCORE
  ################################################
  # rm(list=ls())
  gc()

  var301<-"U07" # Jing: var30 to var301
  # load(paste0("Permanent_datasets/temp40_",
  #             file_name,
  #             ".RData"))
  # load(paste0("Permanent_datasets/tempall3_",
  #             file_name,
  #             ".RData"))
  temp35 <- as.data.frame(t(temp40))# scores
  # ensembled scores
  temp35$var8 <- temp35$Median+temp35$P_cor+temp35$S_cor+temp35$LogReg+temp35$ER_RF+temp35$Gini_RF
  # sort the ensembled score
  temp35 <- arrange(temp35,-var8)
  temp35$var30 <- rownames(temp35) # Jing: var3 to var30

  temp35 <- temp35 %>%
    filter(!str_detect(var30, var301))  # Jing
  # select the first 10 important
  temp36 <- temp35[1:ncol(tempall3)-1,]
  # assign the ensembled score to temp41
  temp41 <- temp36$var8
  temp41 <- scale(temp41)
  temp41 <- temp41+2*abs(min(temp41))
  temp41 <- round(temp41)
  tempnow <- as.data.frame(cbind(temp41,temp36$var30, temp36$var8))
  colnames(tempnow) <- c("Weights", "Predictors", "Ensamble")

  write.xlsx(tempnow, paste0(result_path, "Output_disease_risk_score_weights_",
                             file_name,
                             ".xlsx" ))
  write.xlsx(temp35, paste0(result_path, "Output_all_predictors_",
                            file_name,
                            ".xlsx" ))


  var9 = sum(temp41)
  var10 = min(temp41)

  temp37<-tempall3
  for (K in 1:(ncol(tempall3)-1)){
    tempall3[,K]<-tempall3[,K]*temp41[K]
  }
  tempall3$var4 <- as.factor(tempall3$var4)
  tempall3 <- tempall3 %>% mutate(var11=rowSums(across(where(is.numeric))))
  tempall3$var11 <- ifelse(tempall3$var4=="1",tempall3$var11-1,tempall3$var11)
  tempall3$var11 <- ifelse(tempall3$var11<0,0,tempall3$var11)
  tempall3$var11 <- ifelse(tempall3$var11>=8,8,tempall3$var11)
  summary(tempall3$var11)
  tempall4 <- as.data.frame(unclass(table(tempall3$var11, tempall3$var4)))
  colnames(tempall4) <- c("No","Yes")
  tempall4$var12 <- sum(tempall4$No+tempall4$Yes)
  tempall4$var13 <- (tempall4$Yes/tempall4$No)
  tempall4$var11 <- rownames(tempall4)

  bmp(file=paste0(result_path, "Risk_score_",
                  file_name,
                  ".bmp"),
      width=8, height=12,unit="in", res=200)
  plot(tempall4$var11, tempall4$var13, xlab="Risk score", ylab="Odds")
  dev.off()

  write.xlsx(tempall4[,c("var13", "var11")],paste0(result_path, "Risk_score_",
                                                   file_name,
                                                   ".xlsx") )


  #temp37<- tempall3

  N0<- table(tempall3$var4)
  temp37<- ovun.sample(var4 ~ . ,data=tempall3, method="both", N=N0[2]*2)$data

  # temp37$var4 <- ifelse(temp37$var4=="0","no","yes")
  var5 <- trainControl(method = "cv",number = 5,classProbs =TRUE, summaryFunction = twoClassSummary )
  class0 <- caret::train(var4 ~ var11,data=temp37,trControl=var5, method="glm", metric="ROC")


  save(class0,file=paste0("class0_", file_name, ".RData"))

  #load("class0_DeathWave1.RData")
  class0_sum <- summary(class0)
  sink(file=paste0(result_path, "class0_summary", file_name, ".txt"))
  print(class0_sum)
  sink(file=NULL)
  #write.xlsx(class0_sum, paste0(result_path, "class0_summary", file_name, ".xlsx")) # Newly added

  probabities <- class0 %>% predict(temp37, type="prob")
  temp37$var14 <- probabities$yes
  tempnow1 <- tempnow
  prob <- function(x){1/(1+exp(-x))}
  tempnow1$Probability <- prob(class0_sum$coefficients[1,1] + # b0
                                 class0_sum$coefficients[2,1]*as.numeric(tempnow1$Ensamble)) # b1*drs
  tempnow1$upper <- prob((class0_sum$coefficients[1,1]+1.96*class0_sum$coefficients[1,2]) + # b0
                           (class0_sum$coefficients[2,1]+1.96*class0_sum$coefficients[2,2])*as.numeric(tempnow1$Ensamble))
  tempnow1$lower <- prob((class0_sum$coefficients[1,1]-1.96*class0_sum$coefficients[1,2]) + # b0
                           (class0_sum$coefficients[2,1]-1.96*class0_sum$coefficients[2,2])*as.numeric(tempnow1$Ensamble))
  write.xlsx(tempnow1, paste0(result_path, "Output_disease_risk_score_weights_prob_ul", "_",
                              file_name,
                              ".xlsx" ))
  bmp(file=paste0(result_path, "calibration_", file_name, ".bmp"), width=10, height=12,unit="in", res=200)
  tempall5<- temp37 %>%
    ggplot(aes(var11,var14)) + geom_point(alpha=0.2) +
    geom_smooth(formula=y~x, method="glm", method.args=list(family="binomial")) + theme_bw() +
    labs(title="COVID-19 All",
         x="Risk score", y="Probability")

  tempall5
  dev.off()
  round(tapply(temp37$var14, temp37$var11, FUN=median)*100,0)

  write.xlsx(temp37[,c("var4","var11","var14")], paste0(result_path, "Calibration_", file_name,".xlsx"))


  temp36 <-temp35
  # assign the ensembled score to temp41
  temp41 <- temp36$var8
  temp41 <- scale(temp41)
  temp41 <- temp41+2*abs(min(temp41))
  temp41 <- round(temp41)
  tempnow <- as.data.frame(cbind(temp41,temp36$var30, temp36$var8))
  colnames(tempnow) <- c("Weights", "Predictors", "Ensamble")

  write.xlsx(tempnow,paste0(result_path, "Output_disease_risk_score_weights_all_predictors_", file_name, ".xlsx"))
  write.xlsx(temp35,paste0(result_path, "Output_all_predictors_", file_name, ".xlsx" ))

  ################################################
  ## Module8
  ## model_compare
  ################################################
  # rm(list=ls())
  gc()

  # load(paste0("class0_", file_name, ".RData"))
  # load(paste0("class1_", file_name, ".RData"))
  # load(paste0("class2_", file_name, ".RData"))
  # load(paste0("class3_", file_name, ".RData"))
  # load(paste0("class4_", file_name, ".RData"))

  temp38 <- resamples(list(LR=class1,RFranger=class3,RP= class4,DRS=class0))  #RF=class2,



  stemp38 <- summary(temp38)
  sink(file= paste0(result_path, "temp38_summary_",
                    file_name, ".txt"))
  print(stemp38)
  sink(file=NULL)



  temp38a0<- confusionMatrix(class0)

  temp38a1<- confusionMatrix(class1)
  # temp38a2 <- confusionMatrix(class2)
  temp38a3 <- confusionMatrix(class3)
  temp38a4 <- confusionMatrix(class4)


  var38a0 <- sum(diag(temp38a0$table))/100
  var38a1 <- sum(diag(temp38a1$table))/100
  #var38a2 <- sum(diag(temp38a2$table))/100
  var38a3 <- sum(diag(temp38a3$table))/100
  var38a4 <- sum(diag(temp38a4$table))/100


  temp38b <- as.data.frame(c(var38a1,var38a3,var38a4,var38a0))  #var38a2,
  temp38b$var38b <- c("LR","RFranger","RP","DRS")  #"RF",


  colnames(temp38b)<-c("var38c","var38b")
  var38d <- as.character(temp38b[which.max(temp38b$var38c),]$var38b)

  write.xlsx(temp38b, paste0(result_path, "confusionMatrix_results_", file_name, ".xlsx" ))

      } #
    } # 
  } # 
} # 


# temp37$var4 <- ifelse(temp37$var4=="0","no","yes")
var5 <- trainControl(method = "cv",number = 5,classProbs =TRUE, summaryFunction = twoClassSummary )
class0 <- caret::train(var4 ~ var11,data=temp37,trControl=var5, method="glm", metric="ROC")


save(class0,file=paste0("class0_", file_name, ".RData"))

#load("class0_DeathWave1.RData")
class0_sum <- summary(class0)
sink(file=paste0(result_path, "class0_summary", file_name, ".txt"))
print(class0_sum)
sink(file=NULL)
#write.xlsx(class0_sum, paste0(result_path, "class0_summary", file_name, ".xlsx")) # Newly added

probabities <- class0 %>% predict(temp37, type="prob")
temp37$var14 <- probabities$yes
tempnow1 <- tempnow
prob <- function(x){1/(1+exp(-x))}
tempnow1$Probability <- prob(class0_sum$coefficients[1,1] + # b0
  class0_sum$coefficients[2,1]*as.numeric(tempnow1$Ensamble)) # b1*drs
tempnow1$upper <- prob((class0_sum$coefficients[1,1]+1.96*class0_sum$coefficients[1,2]) + # b0
                        (class0_sum$coefficients[2,1]+1.96*class0_sum$coefficients[2,2])*as.numeric(tempnow1$Ensamble))
tempnow1$lower <- prob((class0_sum$coefficients[1,1]-1.96*class0_sum$coefficients[1,2]) + # b0
                         (class0_sum$coefficients[2,1]-1.96*class0_sum$coefficients[2,2])*as.numeric(tempnow1$Ensamble))
write.xlsx(tempnow1, paste0(result_path, "Output_disease_risk_score_weights_prob_ul",
                           file_name,
                           ".xlsx" ))
bmp(file=paste0(result_path, "calibration_", file_name, ".bmp"), width=10, height=12,unit="in", res=200)
tempall5<- temp37 %>%
  ggplot(aes(var11,var14)) + geom_point(alpha=0.2) +
  geom_smooth(formula=y~x, method="glm", method.args=list(family="binomial")) + theme_bw() +
  labs(title="COVID-19 All",
       x="Risk score", y="Probability")

tempall5
dev.off()
round(tapply(temp37$var14, temp37$var11, FUN=median)*100,0)

write.xlsx(temp37[,c("var4","var11","var14")], paste0(result_path, "Calibration_", file_name,".xlsx"))


temp36 <-temp35
# assign the ensembled score to temp41
temp41 <- temp36$var8
temp41 <- scale(temp41)
temp41 <- temp41+2*abs(min(temp41))
temp41 <- round(temp41)
tempnow <- as.data.frame(cbind(temp41,temp36$var30, temp36$var8))
colnames(tempnow) <- c("Weights", "Predictors", "Ensamble")

write.xlsx(tempnow,paste0(result_path, "Output_disease_risk_score_weights_all_predictors_", file_name, ".xlsx"))
write.xlsx(temp35,paste0(result_path, "Output_all_predictors_", file_name, ".xlsx" ))

################################################
## Module8
## model_compare
################################################
# rm(list=ls())
gc()

# load(paste0("class0_", file_name, ".RData"))
# load(paste0("class1_", file_name, ".RData"))
# load(paste0("class2_", file_name, ".RData"))
# load(paste0("class3_", file_name, ".RData"))
# load(paste0("class4_", file_name, ".RData"))

temp38 <- resamples(list(LR=class1,RFranger=class3,RP= class4,DRS=class0))  #RF=class2,



stemp38 <- summary(temp38)
sink(file= paste0(result_path, "temp38_summary_",
                  file_name, ".txt"))
print(stemp38)
sink(file=NULL)



temp38a0<- confusionMatrix(class0)

temp38a1<- confusionMatrix(class1)
# temp38a2 <- confusionMatrix(class2)
temp38a3 <- confusionMatrix(class3)
temp38a4 <- confusionMatrix(class4)


var38a0 <- sum(diag(temp38a0$table))/100
var38a1 <- sum(diag(temp38a1$table))/100
#var38a2 <- sum(diag(temp38a2$table))/100
var38a3 <- sum(diag(temp38a3$table))/100
var38a4 <- sum(diag(temp38a4$table))/100


temp38b <- as.data.frame(c(var38a1,var38a3,var38a4,var38a0))  #var38a2,
temp38b$var38b <- c("LR","RFranger","RP","DRS")  #"RF",


colnames(temp38b)<-c("var38c","var38b")
var38d <- as.character(temp38b[which.max(temp38b$var38c),]$var38b)

write.xlsx(temp38b, paste0(result_path, "confusionMatrix_results_", file_name, ".xlsx" ))

  }                                                    
} 
}                                                                #### comment away for "all"
}                                                                #### comment away for "all"
