library(psych)
library(tidyverse)

#Code for this combined plot is adapted from the following paper:
#Sakaluk, J. K., & Short, S. D. (2016). A Methodological Review of Exploratory Factor Analysis in Sexuality Research: Used Practices, Best Practices, and Data Analysis Resources. Journal of Sex Research.

#Read in data
basic <- read.csv("Basic Data.csv") 
#create a subset of only OKS & APQ items
oksdata <- subset(basic, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))
apqdata <- subset(basic, select=c("APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))

##### OKS ####

#Parallel analysis
parallel <- fa.parallel(oksdata, fm='minres', fa ='fa', n.iter=5000,
                        main="Parallel Analysis Scree Plot of Oxford Knee Score")

#Create data frame from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

#Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs,sim)


apatheme <- theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated 
  
#Plot with dashed line at 1, to denote kaiser
ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  apatheme +
  geom_hline(yintercept=1, linetype = 'dashed') +
  labs(title="Scree Plot & Parallel Analysis of Oxford Knee Score") +
  scale_fill_discrete(labels = c("Observed Data", "Simulated Data"))

##### APQ ####

parallel <- fa.parallel(apqdata, fm='minres', fa ='fa', n.iter=5000,
                        main="Parallel Analysis Scree Plot of Activity & Participation Questionaire")

#Create data frame from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

#Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs,sim)


apatheme <- theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated 

#Plot with dashed line at 1, to denote kaiser
ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  apatheme +
  geom_hline(yintercept=1, linetype = 'dashed') +
  labs(title="Scree Plot & Parallel Analysis of Activity & Participation Questionnaire") +
  scale_fill_discrete(labels = c("Observed Data", "Simulated Data"))

##### Combined ####

#Read in data
#This should be paired responses
oks_apq_combined <- subset(basic, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12", 
                                           "APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))

#Parallel analysis
parallel <- fa.parallel(oks_apq_combined, fm='minres', fa ='fa', n.iter=5000,
                        main="Parallel Analysis Scree Plot of Activity & Participation Questionaire")

#Create data frame from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

#Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs,sim)


apatheme <- theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated 

#Plot with dashed line at 1, to denote kaiser
ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  apatheme +
  geom_hline(yintercept=1, linetype = 'dashed') +
  labs(title="Scree Plot & Parallel Analysis of OKS & APQ") +
  scale_fill_discrete(labels = c("Observed Data", "Simulated Data"))