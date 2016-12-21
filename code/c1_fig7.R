###### Look at the A50


library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(reshape2)
library(dplyr)
library(RColorBrewer)

pollya <- read.csv("./chapter_1/output/pollya.csv")
pollya %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls), Mature = factor(Mature)) -> pollya

newd2 <- expand.grid(age=seq(1,10,.01), year=as.numeric(levels(pollya$Year)), 
                     latitude=median(pollya$latitude  ),longitude=median(pollya$longitude),    
                     dum=0, hauls='2013.4')
newd2$Year <- factor(newd2$year)

load('./chapter_1/models/base.rda')
load('./chapter_1/models/sp.rda')
load('./chapter_1/models/wt.rda')


pred <- predict(base, newdata=newd2, type='response', se=TRUE)
newd2$x <- pred$fit
part1 <- aggregate(age~year, data=subset(newd2,x>.499 & x<.509), FUN=mean)
names(part1) <- c('year',"base")
part1$age <- round(part1$base, 1)

newd2$se <- pred$se.fit
aa50 <- merge(part1, newd2,by=c('year','age'))
aa50$lci <- aa50$age-2*aa50$se
aa50$uci <- aa50$age+2*aa50$se


pred <- predict(sp, newdata=newd2, type='response', se=TRUE)
newd2$x <- pred$fit
part2 <- aggregate(age~year, data=subset(newd2,x>.49 & x<.51), FUN=mean)
names(part2) <- c('year',"sp")
part2$age <- round(part2$sp, 1)

newd2$se <- pred$se.fit
aa50s <- merge(part2, newd2,by=c('year','age'))
aa50s$lci <- aa50s$age-2*aa50s$se
aa50s$uci <- aa50s$age+2*aa50s$se

pollywt <- read.csv("./chapter_1/output/pollywt.csv")

pollywt %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls), Mature = factor(Mature)) -> pollywt

newd3 <- expand.grid(age=seq(1,10,.01), year=as.numeric(levels(pollywt$Year)), 
                     latitude=median(pollywt$latitude  ),longitude=median(pollywt$longitude),    
                     dum=0, hauls='2013.4')
newd3$Year <- factor(newd3$year)

pred  <- predict(wt, newdata=newd3, type='response', se=TRUE)
newd3$x <- pred$fit
part3 <- aggregate(age~year, data=subset(newd3,x>.49 & x<.51), FUN=mean)
names(part3) <- c('year',"wt")
part3$age <- round(part3$wt, 1)

newd3$se <- pred$se.fit
aa50w <- merge(part3, newd3,by=c('year','age'))
aa50w$lci <- aa50w$age-2*aa50w$se
aa50w$uci <- aa50w$age+2*aa50w$se

head(aa50)
aa <- melt(aa50,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'age'))
head(aa50s)
bb <- melt(aa50s,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'age'))
head(aa50w)
cc <- melt(aa50w,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'age'))

aa <- rbind(aa, bb)
aa <- rbind(aa, cc)
tiff(filename = "./chapter_1/figs/Fig7.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
ggplot(aa, aes(year, value, group=variable, color=variable, fill=variable))+geom_line()+
   geom_ribbon(aes(ymin=lci, ymax=uci), alpha=.2, color=NA)+
   scale_fill_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                     name="Model",
                     breaks=c("base", "sp", "wt"),
                     labels=c("base", "spatial", "weighted spatial"))+
   scale_color_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                      name="Model",
                      breaks=c("base", "sp", "wt"),
                      labels=c("base", "spatial", "weighted spatial"))+ 
   theme(legend.justification=c(1,0), legend.position=c(1,.7),legend.key = element_blank()) +
   guides(color=guide_legend(override.aes=list(fill=NA)))+ylab('Age')+
   scale_x_continuous(breaks=1983:2013, labels=c("","",1985, rep('',4),1990, rep('',4),1995, rep('',4),2000, rep('',4),2005, rep('',4),2010, rep('',3)))+xlab('Year')
dev.off()
ggsave("./chapter_1/figs/Fig7.tiff", dpi=300, height=6, width=6, units="in")
