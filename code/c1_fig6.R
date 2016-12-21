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

pollywt %>% 
   mutate(Year=factor(Year), Age = factor(Age), hauls = factor(hauls)) -> pollywt
#Age Model prediction data
newda <- data.frame(expand.grid(age=seq(1,10,1), 
                                year=as.numeric(levels(pollywt$Year)), dum=0,
                                latitude=median(pollywt$latitude), longitude
                                =median(pollywt$longitude), hauls='2013.4'))
newda$Year <- factor(newda$year)

#load models
load('./chapter_1/models/basew.rda')
load('./chapter_1/models/spw.rda')
load('./chapter_1/models/wt.rda')

#model predictions
newda$base <- predict(basew, newda,type='response')
newda$sp <- predict(spw, newda,type='response')
newda$wt <- predict(wt, newda,type='response')

#####################################################
pred.age <- melt(newda, id.vars=c('age','year','Year','dum','latitude','longitude','hauls'))

tiff(filename = "./chapter_1/figs/Fig6.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
ggplot(pred.age, aes(age, value,color=variable, fill=variable))+
   stat_summary(fun.data='mean_cl_boot', geom='smooth', alpha=.3)+
   scale_x_continuous(breaks=seq(1,100,1))+xlab('Age')+ylab('Proportion Mature')+
   scale_fill_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                     name="Model",
                     breaks=c("base", "sp", "wt"),
                     labels=c("base", "spatial", "weighted spatial"))+
   scale_color_manual(values=c('#cccccc','#a6bddb','#d95f0e'),
                      name="Model",
                      breaks=c("base", "sp", "wt"),
                      labels=c("base", "spatial", "weighted spatial"))+ 
   theme(legend.justification=c(1,0), legend.position=c(1,0),legend.key = element_blank()) +
   guides(color=guide_legend(override.aes=list(fill=NA)))
dev.off()
ggsave("./chapter_1/figs/Fig6.tiff", dpi=300, height=6, width=6, units="in")
