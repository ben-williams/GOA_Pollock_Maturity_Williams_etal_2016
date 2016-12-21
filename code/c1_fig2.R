
library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(dplyr)

polly <- read.csv('./chapter_1/output/polly.csv')
polly %>% 
   filter(age<11) %>% 
   mutate(Age=factor(Age), weight = weight/1000) ->dat


tiff(filename = "./chapter_1/figs/Fig2.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")

ggplot(dat, aes(year,weight, color=Age, fill=Age))+
   stat_smooth(method='gam',formula=y~s(x,k=8),alpha=.3)+
   scale_color_manual(guide=guide_legend(reverse=TRUE), 
                      values=c('#c2e699','#a1dab4','#41b6c4','#2c7fb8','#253494','#c2e699','#a1dab4','#41b6c4','#2c7fb8','#253494'))+
   scale_fill_manual(guide=guide_legend(reverse=TRUE), 
                     values=c('#c2e699','#a1dab4','#41b6c4','#2c7fb8','#253494','#c2e699','#a1dab4','#41b6c4','#2c7fb8','#253494'))+
   xlab('Year')+ylab('Weight (kg)')+
   scale_x_continuous(breaks=1985:2013, 
                      labels=c(1985, rep('',4),1990, rep('',4),1995, rep('',4),2000, rep('',4),2005, rep('',4),2010, rep('',3)))+
   scale_y_continuous(breaks=seq(0,3.5,.5))+ 
   guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
   theme(legend.justification=c(1,0), legend.position=c(.3,.65))
dev.off()

#ggsave("./chapter_1/figs/Fig2.tiff", dpi=300, height=6, width=6, units="in")
