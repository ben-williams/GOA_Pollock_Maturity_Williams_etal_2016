
#Section V SSB estimates
##################################################
################################################
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

pollywt <- read.csv("./chapter_1/output/pollywt.csv")
pollya <- read.csv("./chapter_1/output/pollya.csv")
pollywt %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls)) -> pollywt


#Create a new dataset for predictions
rdatar <- data.frame(expand.grid(age=seq(1,10,1), year=as.numeric(levels(pollywt$Year)), dum=0), latitude=median(pollya$latitude), longitude=median(pollya$longitude), hauls='2013.4')
rdatar$Year <- factor(rdatar$year)

load('./chapter_1/models/base.rda')
load('./chapter_1/models/sp.rda')
load('./chapter_1/models/wt.rda')

r <- predict(base,rdatar, se=T, type='response')
rdatar$base <- predict(base, rdatar,type='response')
rdatar$base.se2 <- r$se.fit^2

r <- predict(sp,rdatar, se=T, type='response')
rdatar$sp <- as.numeric(predict(sp, rdatar,type='response'))
rdatar$sp.se2 <- as.numeric(r$se.fit^2)

r <- predict(wt,rdatar, se=T, type='response')
rdatar$wt <- as.numeric(predict(wt, rdatar,type='response'))
rdatar$wt.se2 <- as.numeric(r$se.fit^2)

rdatar %>% 
   group_by(age) %>% 
   summarise(base = mean(base), base.se = sqrt(sum(base.se2)/10),sp = mean(sp), sp.se = sqrt(sum(sp.se2)/10),wt = mean(wt), wt.se = sqrt(sum(wt.se2)/10)) %>% 
   mutate(basell = base - 2*base.se,baseul = base + 2*base.se,
          spll = sp - 2*sp.se,spul = sp + 2*sp.se,
          wtll = wt - 2*wt.se,wtul = wt + 2*wt.se) ->mats


biomass <- read.csv('./chapter_1/data/goa_safe_15_abundance.csv') #
old.biomass <- read.csv('./chapter_1/data/pollock_abundance.csv') #

old.biomass %>% select(year, age, weight)  -> old.b 

biomass %>% 
   select(year, age, abundance) %>% 
   mutate(abundance = abundance/2) %>% #remove half (males)
   left_join(old.b) %>% 
   left_join(mats) %>% 
   mutate(base.ssb = base *abundance * weight,
          sp.ssb = sp *abundance * weight,
          wt.ssb = wt *abundance * weight,
          
          base.ssb.ll = basell *abundance * weight,
          sp.ssb.ll = spll *abundance * weight,
          wt.ssb.ll = wtll *abundance * weight,
          
          base.ssb.ul = baseul *abundance * weight,
          sp.ssb.ul = spul *abundance * weight,
          wt.ssb.ul = wtul *abundance * weight) %>% 
   group_by(year) %>% 
   summarise(base = sum(base.ssb), sp = sum(sp.ssb), wt = sum(wt.ssb),
             base.ll = sum(base.ssb.ll), sp.ll = sum(sp.ssb.ll), wt.ll = sum(wt.ssb.ll),
             base.ul = sum(base.ssb.ul), sp.ul = sum(sp.ssb.ul), wt.ul = sum(wt.ssb.ul)) %>% 
   filter(complete.cases(.))-> abundance

ab <- melt(abundance, id.vars = c('year', 'base.ll', 'base.ul', 'sp.ll', 'sp.ul', 'wt.ll', 'wt.ul'))

ab %>% 
   mutate(ll = ifelse(variable=='base', base.ll, 
                      ifelse(variable=='sp', sp.ll, 
                             ifelse(variable=='wt', wt.ll, NA))),
          ul = ifelse(variable=='base', base.ul, 
                      ifelse(variable=='sp', sp.ul, 
                             ifelse(variable=='wt', wt.ul, NA)))) -> ac
tiff(filename = "./chapter_1/figs/Fig10.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
ggplot(ac, aes(year, value, color=variable))+geom_line()+geom_ribbon(aes(ymin= ll, ymax = ul, fill = variable), alpha= .2, color = NA)+
   scale_fill_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                     name="Model",
                     breaks=c("base", "sp", "wt"),
                     labels=c("base", "spatial", "weighted spatial"))+
   scale_color_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                      name="Model",
                      breaks=c("base", "sp", "wt"),
                      labels=c("base", "spatial", "weighted spatial"))+ 
   theme(legend.justification=c(1,0), legend.position=c(1,.7),legend.key = element_blank()) +
   guides(color=guide_legend(override.aes=list(fill=NA)))+
   xlab('Year')+ylab('Spawning stock biomass (1,000 t)')+
   scale_x_continuous(breaks=1977:2013, labels=c("","","",1980, rep('',4),1985, rep('',4),1990, rep('',4),1995, rep('',4),2000, rep('',4),2005, rep('',4),2010, rep('',3)))
dev.off()
#ggsave("./chapter_1/figs/Fig10.tiff", dpi=300, height=6, width=6, units="in")

abundance %>% 
   transmute(year = year,wt.diff = wt-base, sp.diff = sp - base) %>% data.frame-> diff

diff %>% 
   filter(year>2002) %>% 
   summarise(minwt = min(wt.diff, na.rm = T), maxwt = max(wt.diff, na.rm = T), meanwt = mean(wt.diff, na.rm = T), sdwt = sd(wt.diff,na.rm=T),
             minsp = min(sp.diff, na.rm = T), maxsp = max(sp.diff, na.rm = T), meansp = mean(sp.diff, na.rm = T), sdsp = sd(sp.diff, na.rm = T))

abundance %>% 
   filter(year>2002) %>% select(year, base, sp, wt) %>% 
   mutate(sp.diff = (sp-base)/abs(base)*100, wt.diff = (wt-base)/abs(base)*100) %>% 
   summarise(sp.d = sd(sp.diff),wt.d = sd(wt.diff))
