setwd("C:/Users/jinzh/OneDrive - Universitetet i Oslo/Documents/WP2-ML/risk_score/Additional Work/new_code/Results")
library(data.table)
library(ggplot2)
library(ggpubr)
df <- fread("for_the_tree.csv")
str(df)

df$Ensamble <- as.numeric(gsub(",",".",df$Ensamble))
df$Probability <- as.numeric(gsub(",",".",df$Probability))
df$upper <- as.numeric(gsub(",",".",df$upper))
df$lower <- as.numeric(gsub(",",".",df$lower))
df$prevalence <- as.numeric(gsub(",",".",df$prevalence))
df$Gender <- as.factor(df$Gender) 
df$Age_group <- as.factor(df$Age_group) 
str(df)


# probability row
plot1 <- ggplot(df, aes(y = Predictors, x = Probability, col= Gender)) +
  geom_point(shape = 18, size = 2) + 
  xlim(0.1,max(df$Probability)+sd(df$Probability))+ 
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25) +  
  scale_color_manual(values = c("#293352", "#FC4E07", "#4E84C4" )) +
  facet_wrap(~Age_group, ncol = 5) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.position="right")
  
# prevalence row
plot2 <- ggplot(df, aes(y = Predictors, x = prevalence, col=df$Gender)) +
  geom_point(shape = 18, size = 3) + 
  geom_segment(x = 0, y= df$Predictors, xend = df$prevalence, yend = df$Predictors) +
  xlim(0,max(df$prevalence)+sd(df$prevalence))+ 
  scale_color_manual(values = c("#293352", "#FC4E07", "#4E84C4" )) +
  facet_wrap(~Age_group, ncol = 5) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.position = "none")

# weights row
plot3 <- ggplot(df, aes(y = Predictors, x = Weights, col=df$Gender)) +
  geom_point(shape = 18, size = df$Weights) + 
  xlim(0,max(df$Weights)+sd(df$Weights))+ 
  scale_color_manual(values = c("#293352", "#FC4E07", "#4E84C4" )) +
  facet_wrap(~Age_group, ncol = 5) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(legend.position = "none") 
  
pp <- ggarrange(plot1,plot2, plot3, ncol=1, nrow=3, common.legend = TRUE)

bmp("panel_plot.bmp", width = 640, height = 960, units = "px")
print(pp)
dev.off()
