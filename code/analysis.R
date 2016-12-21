library(plyr); library(dplyr);library(rpart);library(tree);library(extrafont); library(Hmisc);
library(stringi);library(lubridate);
library(mgcv); library(reshape2)
library(ggplot2)


detach(package:lubridate)

#read in data
pollock <- read.csv('data/pollock_mature_2015_3_17.csv')# most recent data set - data from AFSC

#create factors etc.

pollock %>% 
  mutate(date = as.Date(date.time, format='%m/%d/%Y'),
         year = as.numeric(format(date, "%Y")),
         Year = factor(year),
         Mature = factor(mature),
         Age = factor(age),
         hauls = factor(haul/100+year),
         length = round(length)/10) %>% 
   filter(longitude <= -153 & longitude >= -157 & latitude > 55.4,
          longitude <= -155.4 | longitude <= -154 & latitude > 56.5 | longitude <= -153 & latitude >= 57,
          Mature !='NA') -> poll

# write.csv(poll, "output/poll.csv")

#CART model
#Used this to examine the crossvalidataion
tree.fit<-rpart(mature~longitude+latitude,data=poll, method="anova")
plot(tree.fit)
text(tree.fit,cex=.8,digits=3)
p.fit <- prune(tree.fit, cp= tree.fit$cptable[which.min(tree.fit$cptable[,"xerror"]),"CP"])
plot(p.fit)
text(p.fit,cex=.8,digits=3)
plotcp(p.fit)
print(p.fit)
#used this to plot
tree.fit<-tree(mature~longitude+latitude,data=poll)
plot(tree.fit)
text(tree.fit,cex=.8,digits=3)

#rotate data and fit CART
rotate.axis<-function(xy,theta){
   pimult<-(theta * 2 * pi) / 360
   newx<-c(cos(pimult),sin(pimult))
   newy<-c(-sin(pimult),cos(pimult))
   XY<-as.matrix(xy) %*% cbind(newx, newy)
   as.data.frame(XY)
}

xy1<-data.frame(longitude=poll$longitude, latitude=poll$latitude)
rotate1<-rotate.axis(xy1,30)
polly<-cbind(poll,rotate1)

#Used this to examine the crossvalidataion
tree.fit1<-rpart(mature~newx+newy,data=polly, method="anova")
plot(tree.fit1)
text(tree.fit1,cex=.8,digits=3)
p.fit1 <- prune(tree.fit1, cp= tree.fit1$cptable[which.min(tree.fit1$cptable[,"xerror"]),"CP"])
plot(p.fit1)
text(p.fit1,cex=.8,digits=3)
plotcp(p.fit1)
print(p.fit1)
rsq.rpart(p.fit1)
#used this to plot
tree.fit1<-tree(mature~newx+newy,data=polly)
plot(tree.fit1)
text(tree.fit1,cex=.8,digits=3)

#add regions - based upon breaks in tree.fit1
polly$region = NA
polly<-within(polly,region[newx > -105.229 & newy > 127.354] <-'A')
polly<-within(polly,region[newx < -105.229 & newy > 127.354] <-'B')
polly<-within(polly,region[newy  < 127.354] <-'C')
polly$region<-as.factor(polly$region)
polly$Region<-polly$region


##### Section II Data weights
################################################

#Run the fileNames.R function


#______________________________________________________________________
#make adjustments to formats in the file
allSamples <- read.csv('data/allSamples.csv', header=FALSE)
str(allSamples)
names(allSamples) <- c('Region', 'count','length', 'biomass', 'file')
#remove .csv from file name to create a year column
allSamples %>% 
   mutate(year = as.numeric(stri_sub(gsub('.csv','' ,allSamples$file),8))+2000,
          Year = factor(year)) -> allSamples
allSamples %>% 
   group_by(Region) %>% 
   summarize(totalcount = max(count)) -> out
out %>% 
   left_join(allSamples) %>% 
   mutate(wtbio = count/totalcount*biomass) ->allSamples

#add on rows to balance weights - otherwise will not have complete blocks of data
#this is necessary when the data get matched up to the maturity data
addon <- data.frame (Region=c('B', 'A', 'C', 'A'), 
                     count=c(0,0,0,0),
                     length = c(66,66,70,70), 
                     biomass=.001,
                     Year = c('2003', '2005', '2006', '2012'), 
                     year=c(2003,2005,2006,2012), 
                     file='', 
                     totalcount=c(238, 95,634,119), 
                     wtbio=.001)
allSamples <- rbind(allSamples, addon)

#clean up files
rm(addon)

#########################################################################################################################
#
#
###########################################################################################################################
#Add weight data to pollock maturity data
polly$length.class <- cut(polly$length, c(5,10,15, 20,25, 30,35, 40,45, 50,55, 60,65, 70,75, 80), include.lowest = TRUE)
allSamples$length.class <- cut(allSamples$length, c(5,10,15, 20,25, 30,35, 40,45, 50,55, 60,65, 70,75, 80), include.lowest = TRUE)
wt.data<-aggregate(wtbio~Region+Year+length.class, allSamples, FUN=sum)
#Add a dummy variable for the random effects component
polly$dum <- 1

polly %>% 
   left_join(wt.data) %>% 
   mutate(wt = 1/(wtbio/100000000), Year = factor(Year),
          age =ifelse(age>10,10,age), Age = factor(age)) %>%
   filter(age > 0) -> pollya  #Age ONLY DATA for comparing age and length models
pollya %>% 
   filter(wt > 0) %>% droplevels()-> pollywt #inverse of the weights shrunken a bit

# Section III Models #########################################################################################################

###############################################################################
#FOLLOWING MODELS ARE BASED UPON THE AGE LIMITED DATASET
#Age models 
base <- glm(Mature~age*Year-1,family=binomial, data=pollya)
sp <- bam(Mature~s(age, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4, data=pollya, method='REML')

#Cross validataion of glm
cv.err <- cv.glm(pollya,base1)
cv.err.10 <- cv.glm(pollya,base1, K=10)


##################################################
#################################################
# Age equivalent length models
basel <- glm(Mature~length*Year-1,family=binomial, data=pollya)
spl <- bam(Mature~s(length, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4, data=pollya, method='REML')

#################################################
# Length models on all available length data
#################################################
# all.basel <- glm(Mature~length*Year-1,family=binomial, data=polly)
# all.spl <- bam(Mature~s(length, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4, data=polly, method='REML')
# 
# 
# AIC(base,basel,sp,spl)

####################################################################################################
# ############################################################################
# Models with weighted data reduced years dataset (2003-2013)
############################################################################

# Age models - age limited data set
basew <- glm(Mature~age*Year-1,family=binomial, data=pollywt)
spw <- bam(Mature~s(age, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4, 
           data=pollywt, method='REML') # takes a while to run
wt <- bam(Mature~s(age, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4, 
          data=pollywt, weight=wt, method='REML')

# Length based models - age limited data set
baselw <- glm(Mature~length*Year-1,family=binomial, data=pollywt)
splw <- bam(Mature~s(length, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,
            gamma=1.4, data=pollywt, method='REML') # takes a while to run
wtl <- bam(Mature~s(length, k=4)+te(longitude,latitude)+s(hauls, bs='re', by = dum)+Year-1, family=binomial,gamma=1.4,
           data=pollywt, weight=wt, method='REML')

# Model comparisons - only good if using the pollywt dataset
AIC(basew, spw, wt, baselw, splw, wtl)


#Export models
#save models to fecundity folder
# save(base, file = "./chapter_1/models/base.rda")
# save(sp, file = "./chapter_1/models/sp.rda")
# save(basel, file = "./chapter_1/models/basel.rda")
# save(spl, file = "./chapter_1/models/spl.rda")
# save(basew, file = "./chapter_1/models/basew.rda")
# save(spw, file = "./chapter_1/models/spw.rda")
# save(wt, file = "./chapter_1/models/wt.rda")
# 
# save(baselw, file = "./chapter_1/models/baselw.rda")
# save(splw, file = "./chapter_1/models/splw.rda")
# save(wtl, file = "./chapter_1/models/wtl.rda")
# 
# 
# 
# write.csv(polly, "./chapter_1/output/polly.csv")
# write.csv(pollya, "./chapter_1/output/pollya.csv")
# write.csv(pollywt, "./chapter_1/output/pollywt.csv")
