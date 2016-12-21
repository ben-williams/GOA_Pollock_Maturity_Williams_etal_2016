###### Look at the L50

library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(reshape2)
library(plyr)
library(dplyr)
library(RColorBrewer)

pollya <- read.csv("./chapter_1/output/pollya.csv")
pollya %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls), Mature = factor(Mature)) -> pollya

newd2l <- expand.grid(length=seq(10,70,.01), year=as.numeric(levels(pollya$Year)), 
                     latitude=median(pollya$latitude  ),longitude=median(pollya$longitude),    
                     dum=0, hauls='2013.4')
newd2l$Year <- factor(newd2l$year)

load('./chapter_1/models/basel.rda')
load('./chapter_1/models/spl.rda')
load('./chapter_1/models/wtl.rda')


pred <- predict(basel, newdata=newd2l, type='response', se=TRUE)
newd2l$x <- pred$fit
part1 <- aggregate(length~year, data=subset(newd2l,x>.499 & x<.509), FUN=mean)
names(part1) <- c('year',"base")
part1$length <- round(part1$base, 1)

newd2l$se <- pred$se.fit
aa50 <- merge(part1, newd2l,by=c('year','length'))
aa50$lci <- aa50$length-2*aa50$se
aa50$uci <- aa50$length+2*aa50$se


pred <- predict(spl, newdata=newd2l, type='response', se=TRUE)
newd2l$x <- pred$fit
part2 <- aggregate(length~year, data=subset(newd2l,x>.49 & x<.51), FUN=mean)
names(part2) <- c('year',"sp")
part2$length <- round(part2$sp, 1)

newd2l$se <- pred$se.fit
aa50s <- merge(part2, newd2l,by=c('year','length'))
aa50s$lci <- aa50s$length-2*aa50s$se
aa50s$uci <- aa50s$length+2*aa50s$se

pollywt <- read.csv("./chapter_1/output/pollywt.csv")

pollywt %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls), Mature = factor(Mature)) -> pollywt

newd3 <- expand.grid(length=seq(10,70,.01), year=as.numeric(levels(pollywt$Year)), 
                     latitude=median(pollywt$latitude  ),longitude=median(pollywt$longitude),    
                     dum=0, hauls='2013.4')
newd3$Year <- factor(newd3$year)

pred  <- predict(wtl, newdata=newd3, type='response', se=TRUE)
newd3$x <- pred$fit
part3 <- aggregate(length~year, data=subset(newd3,x>.49 & x<.51), FUN=mean)
names(part3) <- c('year',"wt")
part3$length <- round(part3$wt, 1)

newd3$se <- pred$se.fit
aa50w <- merge(part3, newd3,by=c('year','length'))
aa50w$lci <- aa50w$length-2*aa50w$se
aa50w$uci <- aa50w$length+2*aa50w$se

aa <- melt(aa50,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'length'))

bb <- melt(aa50s,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'length'))

cc <- melt(aa50w,id.vars=c('year','latitude','longitude', 'dum', 'hauls', 'Year', 'x', 'se', 'lci', 'uci', 'length'))

aa <- rbind(aa, bb)
aa <- rbind(aa, cc)
tiff(filename = "./chapter_1/figs/Fig8.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
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
   theme(legend.justification=c(1,0), legend.position=c(1,0),legend.key = element_blank()) +
   guides(color=guide_legend(override.aes=list(fill=NA)))+ylab('Length (cm)')+
   scale_x_continuous(breaks=1983:2013, labels=c("","",1985, rep('',4),1990, rep('',4),1995, rep('',4),2000, rep('',4),2005, rep('',4),2010, rep('',3)))+xlab('Year')
dev.off()
#ggsave("./chapter_1/figs/Fig8.tiff", dpi=300, height=6, width=6, units="in")



#Why the strong decline in 2004
#look at just age 4 and 5
# ggplot(subset(pollya,  age>2 & age<6), aes(Year,length, color=Mature))+geom_jitter(alpha=.3)+ylim(0,75)+
#    scale_fill_manual(values = c("#0072B2","#D55E00"),labels = c("Immature","Mature"),
#                      name = "")+
#    theme(legend.key = element_blank(),legend.justification=c(1,0), legend.position=c(1,0)) +
#    guides(fill = guide_legend(reverse=TRUE))+scale_x_discrete(breaks=seq(1980,2013,5))+ylab('Length (cm)')+xlab('Year')
# 
# # #by age
# ggplot(subset(pollya,  age>2 & age<6), aes(Mature,length, fill=Year))+geom_boxplot()
# 
# geom_jitter(alpha=.35)+
#    scale_fill_manual(values = c("white","#D55E00"),labels = c("Immature","Mature"),name = "")+
#    theme(legend.key = element_blank(),legend.justification=c(1,0), legend.position=c(.89,.7)) +
#    guides(fill = guide_legend(reverse=TRUE))+scale_x_discrete(breaks=seq(1980,2013,5))+ylab('Age')+xlab('Year')

