library(reshape2)
library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(dplyr)

# Data
biomass <- read.csv('./chapter_1/data/goa_safe_15_abundance.csv') #year biomass and ssb from SAFE document: 

biomass %>% 
   select(year, bio, ssb) %>% 
   filter(bio>0) -> biomass
names(biomass) <- c('year','Total biomass', 'Spawning biomass')
mbio <- melt(biomass, id.vars='year')

#plot biomass
tiff(filename = "./chapter_1/figs/Fig1.tiff", width = 5.2, height = 3.2, units = "in", res=600, compression="lzw")
ggplot(mbio, aes(year,value/1000,  group = variable, lty = variable))+geom_line()+ guides(lty=guide_legend(title=NULL))+ 
   theme(legend.justification=c(1,0), legend.position=c(1,.65), legend.key = element_blank())+
   xlab('Year')+ ylab('Biomass (M ton)')+  scale_x_continuous(breaks=1975:2013, 
                      labels=c( 1975,rep('',4),1980,rep('',4), 1985, rep('',4), 1990, 
                                rep('',4), 1995,rep('',4), 2000, rep('',4), 2005, rep('',4),
                                2010, rep('',3)))+scale_y_continuous(breaks= seq(0,3.0,.5))
dev.off()
ggsave("./chapter_1/figs/Fig1.tiff", dpi=300, height=6, width=6, units="in")
