library(tidyverse) 
library(readxl)
library(janitor)
library(finalfit)
library(DACF)
library(cowplot)
library(grid)
library(psych)
library(REdaS)
library(paran)
library(EFA.dimensions)
library(lavaan)
library(mokken)
library(mirt)
library(lordif)

#### Read in Data ####
basic <- read.csv("Basic Data.csv")
oksdata <- subset(basic, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))
apqdata <- subset(basic, select=c("APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))
oks_apq_combined <- subset(basic, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12", 
                                           "APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))

#Score OKS
basic <- basic %>% 
  rowwise() %>% 
  mutate(
    OKS_complete = sum(!is.na(c(OKS1,OKS2,OKS3,OKS4,OKS5,OKS6,OKS7,OKS8,OKS9,OKS10,OKS11,OKS12))), #how items are complete
    OKS_temp = sum(c(OKS1,OKS2,OKS3,OKS4,OKS5,OKS6,OKS7,OKS8,OKS9,OKS10,OKS11,OKS12)), #makes a raw score of just adding it all up
    OKS_item_avg = OKS_temp / OKS_complete, #get an average score per item
    OKS = OKS_item_avg * 12, #time avg score by 12 to give total score
  )

basic[basic$OKS_complete<10,"OKS"]<-NA #remove all with more than 2 missing responses

#Score APQ
basic <- basic %>% 
  rowwise() %>% 
  mutate(
    APQ_complete = sum(!is.na(c(APQ1, APQ2, APQ3, APQ4, APQ5, APQ6, APQ7, APQ8))), 
    APQ_temp = sum(c(APQ1, APQ2, APQ3, APQ4, APQ5, APQ6, APQ7, APQ8)), 
    APQ_item_avg = APQ_temp / APQ_complete, 
    APQ_temp = APQ_item_avg * 8,
    APQ_score = APQ_temp/32 *100
  )

basic[basic$APQ_complete<6,"APQ_score"]<-NA #removals all with more than 2 missing responses

#### Missing Data ####
#Examine for missing data

library(finalfit)

oksdata %>% 
  ff_glimpse()

oksdata %>% 
  missing_pattern()

apqdata %>% 
  ff_glimpse()

apqdata %>% 
  missing_pattern()

?missing_pattern
library(VIM)

aggr(oksdata, col=c('blue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(oksdata), ylab=c("Histogram of OKS missing data","Pattern"))

aggr(apqdata, col=c('blue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(oksdata), ylab=c("Histogram of APQ missing data","Pattern"))

#### Plot Sumscore OKS ####

basic %>% 
  ggplot(aes(x=OKS, fill=factor(ASSESS))) + #note in this dataframe, $ASSESS denotes whether a participant is pre- or post-operative
  geom_density(alpha=0.5)  +
  scale_fill_brewer(palette= "Set1") +
  theme_bw() +
  labs(title='Distribution of Sumscore Oxford Knee Score', y='Frequency', x='Oxford Knee Score')

#### Factor analysis #### 

#remove NAs from dataframe to faciliate factor analysis
{oksdata <- na.omit(oksdata)
apqdata <- na.omit(apqdata)
oks_apq_combined <- na.omit(oks_apq_combined)}

##### KMO & Bartletts #####
bart_spher(oksdata) #Bartletts test of spehricity. This should be significant
KMO(oksdata) #Kaiser-Meyer-Olkin measure, want this to be >0.7

bart_spher(apqdata) #Bartletts test of spehricity. This should be significant
KMO(apqdata) #Kaiser-Meyer-Olkin measure, want this to be >0.7

bart_spher(oks_apq_combined) #Bartletts test of spehricity. This should be significant
KMO(oks_apq_combined) #Kaiser-Meyer-Olkin measure, want this to be >0.7

# number of factors OKS
{EV_OKS <- eigen(cor(oksdata))
  #Plot it
  plot(EV_OKS$values, ylab="Eigenvalue", xlab="Factor")
  lines(EV_OKS$values, col="black")
  title(main="Scree plot of Eigenvalues for Oxford Knee Score")
  axis(side=1, at=c(0:12))}

# number of factors APQ
{EV_APQ <- eigen(cor(apqdata))
  #Plot it
  plot(EV_APQ$values, ylab="Eigenvalue", xlab="Factor")
  lines(EV_APQ$values, col="black")
  title(main="Scree plot of Eigenvalues for Activity & Participation Questionaire ")
  axis(side=1, at=c(0:12))}

#number of factors in OKS and APQ combined
EV_OKS_APQ <- eigen(cor(oks_apq_combined))
#Plot it
plot(EV_OKS_APQ$values, ylab="Eigenvalue", xlab="Factor")
lines(EV_OKS_APQ$values, col="black")
title(main="Scree plot of Eigenvalues for OKS and APQ")
axis(side=1, at=c(0:20))

#Velicers minimum average partial (MAP) Test, comes from library EFA.dimensions

MAP(oksdata, corkind= 'polychoric')
MAP(apqdata, corkind= 'polychoric')
MAP(oks_apq_combined, corkind= 'polychoric')

##### Parallel analysis #####

paran(oksdata, cfa=TRUE, graph=TRUE, color=TRUE, col=c("black","red","blue"))

# alternate code
paran(oksdata, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#can be done using fa.parallel
fa.parallel(oksdata, fm='minres', fa ='fa', n.iter = 5000,
            main= "Parallel Analysis Scree Plot of Forgotten Joint Score")

fa.parallel.poly(oksdata, fm='minres', fa ='fa', n.iter = 5000,
                 main= "Parallel Analysis Scree Plot of Forgotten Joint Score")

#### Factor loading plot ####

#####Factor analysis OKS #####
names_oks<- names(oksdata)
f1 <- fa(oksdata,1,rotate="oblimin")
#number 1 in the line above means 1 factor
#rotate is for >2 factors

#Factor loading
Load <- loadings(f1)
print(Load,sort=FALSE,digits=2,cutoff=0.01)
plot(Load, type="n", ylim=c(0,1))
title(main="Factor loadings for Oxford Knee Score")
text(Load,labels=names(oksdata))

#####Factor analysis APQ#####
{names_apq<- names(apqdata)
f2 <- fa(apqdata,1,rotate="oblimin")
#Factor loading
Load <- loadings(f2)
print(Load,sort=FALSE,digits=2,cutoff=0.01)
plot(Load, type="n", ylim=c(0,1))
title(main="Factor loadings for Activity and Participation Questionnaire")
text(Load,labels=names(apqdata))}

#####Factor analysis combined #####
names_combined<- names(oks_apq_combined)
f3 <- fa(oks_apq_combined,1,rotate="oblimin") #1 here refers to 1 factor

#Factor loading
Load <- loadings(f3)
print(Load,sort=FALSE,digits=2,cutoff=0.01)
plot(Load, type="n", ylim=c(0,1))
title(main="Factor loadings for OKS & OKS-APQ with 1 Factor")
text(Load,labels=names(oks_apq_combined))

####  CFA #### 
library(lavaan)
#OKS
oksdata.model <- 'oksdata=~ OKS1+OKS2+OKS3+OKS4+OKS5+OKS6+OKS7+OKS8+OKS9+OKS10+OKS11+OKS12'
fit.oksdata <- cfa(oksdata.model, data=oksdata, ordered = TRUE)
summary(fit.oksdata, fit.measures=T, standardized=TRUE)

#APQ
apqdata.model <- 'apqdata=~ APQ1+APQ2+APQ3+APQ4+APQ5+APQ6+APQ7+APQ8'
fit.apqdata <- cfa(apqdata.model, data=apqdata, ordered = TRUE)
summary(fit.apqdata, fit.measures=T, standardized=TRUE)

#Combined OKS and APQ
combined.model <- 'oks_apq_combined=~ OKS1+OKS2+OKS3+OKS4+OKS5+OKS6+OKS7+OKS8+OKS9+OKS10+OKS11+OKS12+APQ1+APQ2+APQ3+APQ4+APQ5+APQ6+APQ7+APQ8'
fit.combined.data <- cfa(combined.model, data=oks_apq_combined, ordered = TRUE)
summary(fit.combined.data, fit.measures=T, standardized=TRUE)

#### Mokken #### 
library(mokken)
##OKS
coefH(oksdata) 
aisp(oksdata) 
summary(check.monotonicity(oksdata))

##APQ
coefH(apqdata) 
aisp(apqdata) 
summary(check.monotonicity(apqdata))

##Combined
coefH(oks_apq_combined) 
aisp(oks_apq_combined) 
summary(check.monotonicity(oks_apq_combined))

#### Graded Response Model ####

#### OKS ####
#create pre and post-op data
preop.oks <- filter(oksdata, ASSESS == 1) #1 represents pre-op in this data frame
postop.oks <- filter(oksdata, ASSESS == 2 | ASSESS == 3) #2 & 3 represent 6 month and 12 month respectively
postop.oks.6month <- filter(basic, ASSESS == 2)
postop.oks.12month <- filter(basic, ASSESS == 3)

#get OKS data only
pre.op.oksdata <- subset(preop.oks, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))
post.op.oksdata <- subset(postop.oks, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))
post.op.6.oksdata <- subset(postop.oks.6month, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))
post.op.12.oksdata <- subset(postop.oks.12month, select=c("OKS1", "OKS2", "OKS3", "OKS4", "OKS5", "OKS6", "OKS7", "OKS8", "OKS9", "OKS10", "OKS11", "OKS12"))


#remove rows with full 12 NA
pre.op.oksdata <- pre.op.oksdata[rowSums(is.na(pre.op.oksdata)) != ncol(pre.op.oksdata), ]
post.op.oksdata <- post.op.oksdata[rowSums(is.na(post.op.oksdata)) != ncol(post.op.oksdata), ]
post.op.6.oksdata <- post.op.6.oksdata[rowSums(is.na(post.op.6.oksdata)) != ncol(post.op.6.oksdata), ]
post.op.12.oksdata <- post.op.12.oksdata[rowSums(is.na(post.op.12.oksdata)) != ncol(post.op.12.oksdata), ]

#create GRM models
grm.oks <- mirt(pre.op.oksdata, model=1, itemtype='graded', SE=TRUE)

##### OKS EAP score #####
f.pre <- fscores(grm.oks)[,1]
f.post <- fscores(grm.oks, response.pattern = post.op.oksdata)[,1]
f.post.6 <- fscores(grm.oks, response.pattern = post.op.6.oksdata)[,1] #only 6month post op
f.post.12 <- fscores(grm.oks, response.pattern = post.op.12.oksdata)[,1] #only 12month post op

score <- c(f.pre, f.post.6, f.post.12)
time <- c(rep("Pre-operative", 1337), rep("6-month", 641), rep("12-month", 970))

df <- cbind(score, time) %>% as.data.frame()
df$score <- as.numeric(df$score)

#plot new fscores
ggplot(df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5)  +
  scale_fill_brewer(palette= "Set1") +
  theme_minimal() + 
  labs(title='Distribution of GRM OKS Score')

#change the score to go from 0-48
minimum_theta <- fscores(grm.oks, response.pattern = rep(4,12))[,1]
maximum_theta <- fscores(grm.oks, response.pattern = rep(0,12))[,1]

pre.op <- 48 - (round((48*(f.pre - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
post.op <- 48 - (round((48*(f.post - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
post.op.6 <- 48 - (round((48*(f.post.6 - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
post.op.12 <- 48 - (round((48*(f.post.12 - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))

score <- c(pre.op, post.op.6, post.op.12)
time <- c(rep("Pre-operative", 1337), rep("6-month", 641), rep("12-month", 970))

OKS.EAP.df <- cbind(score, time) %>% as.data.frame()
OKS.EAP.df$score <- as.numeric(OKS.EAP.df$score)

#Plot 
ggplot(OKS.EAP.df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5) +
  scale_fill_brewer(palette= "Set1") +
  theme_bw() + 
  labs(title='Distribution of GRM Oxford Knee Score', y='Frequency', x='GRM OKS')

##### Means & S.d #####

postop.12.oks.EAP <- filter(OKS.EAP.df, time == "12-month")
postop.6.oks.EAP <- filter(OKS.EAP.df, time == "6-month")
preop.oks.EAP <- filter(OKS.EAP.df, time == "Pre-operative")

OKS.EAP.df %>% 
  ff_glimpse()

preop.oks.EAP %>% 
  ff_glimpse()

postop.6.oks.EAP %>% 
  ff_glimpse()

postop.12.oks.EAP %>% 
  ff_glimpse()

##### OKS GRM Model eval #####
summary(grm.oks)

coef(grm.oks, IRTpars=TRUE, simplify=TRUE) #Gain parameters

#### APQ ####

#create pre and post-op data
preop.apq <- filter(apqdata, ASSESS == 1)
postop.apq.12month <- filter(apqdata, ASSESS == 3)

#get APQ data only
pre.op.apqdata <- subset(preop.apq, select=c("APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))
post.op.12.apqdata <- subset(postop.apq.12month, select=c("APQ1", "APQ2", "APQ3", "APQ4", "APQ5", "APQ6", "APQ7", "APQ8"))

#remove rows with full 12 NA
pre.op.apqdata <- pre.op.apqdata[rowSums(is.na(pre.op.apqdata)) != ncol(pre.op.apqdata), ]
post.op.12.apqdata <- post.op.12.apqdata[rowSums(is.na(post.op.12.apqdata)) != ncol(post.op.12.apqdata), ]

#create GRM models
grm.apq <- mirt(post.op.12.apqdata, model=1, itemtype='graded', SE=TRUE)

##### APQ EAP score #####
f.pre <- fscores(grm.apq, response.pattern = pre.op.apqdata)[,1]
f.post.12 <- fscores(grm.apq, response.pattern = post.op.12.apqdata)[,1] #only 12month post op

score <- c(f.pre, f.post.12)
time <- c(rep("Pre-operative", 684), rep("12-month", 444))

df <- cbind(score, time) %>% as.data.frame()
df$score <- as.numeric(df$score)

#plot new fscores
ggplot(df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5)  +
  scale_fill_brewer(palette= "Set1") +
  theme_minimal() + 
  labs(title='Distribution of GRM Activity & Participation Questionnaire Score')

#change the score to go from 0-100
minimum_theta <- fscores(grm.apq, response.pattern = rep(4,8))[,1]
maximum_theta <- fscores(grm.apq, response.pattern = rep(0,8))[,1]

pre.op <- 100 - (round((100*(f.pre - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
post.op.12 <- 100 - (round((100*(f.post.12 - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))

score <- c(pre.op, post.op.12)
time <- c(rep("Pre-operative", 684), rep("12-month", 444))

apq.EAP.df <- cbind(score, time) %>% as.data.frame()
apq.EAP.df$score <- as.numeric(apq.EAP.df$score)

#Plot 
ggplot(apq.EAP.df, aes(x = score, fill = time)) +
  geom_density(alpha=0.5) +
  scale_fill_brewer(palette= "Set1") +
  theme_bw() + 
  labs(title='Distribution of GRM Activity & Participation Questionnaire Score', y='Frequency', x='GRM APQ')

##### Means & S.d #####

postop.apq.EAP <- filter(apq.EAP.df, time == "12-month")
preop.apq.EAP <- filter(apq.EAP.df, time == "Pre-operative")

apq.EAP.df %>% 
  ff_glimpse()

postop.apq.EAP %>% 
  ff_glimpse()

preop.apq.EAP %>% 
  ff_glimpse()

##### APQ GRM Model eval #####
summary(grm.apq)

coef(grm.apq, IRTpars=TRUE, simplify=TRUE) #Gain parameters
