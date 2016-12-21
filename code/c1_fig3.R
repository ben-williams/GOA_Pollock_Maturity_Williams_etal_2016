
library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(dplyr)
library(maps)
library(mapdata)
library(mapproj)
library(grid)

polly <- read.csv('./chapter_1/output/polly.csv')
#Map of sample locations
#plot Map
ak<-map_data('worldHires','USA:Alaska')
akmap<-ggplot()+geom_polygon(data=ak,aes(long,lat,group=group),fill=8,color="black")+
   theme(panel.background=element_rect(fill='white'))+
   xlab(expression(paste(Longitude^o,~'W')))+
   ylab(expression(paste(Latitude^o,~'N')))+
   coord_map(xlim = c(-165, -150),ylim = c(54, 60))

tiff(filename = "./chapter_1/figs/Fig3.tiff", width = 5.2, height = 4.2, units = "in", res=600, compression="lzw")
akmap+geom_jitter(data=polly, aes(longitude,latitude),alpha=.2, shape=1, color='blue')+
   annotate("text", x = -156, y = 58.3, label = "Alaska Peninsula",
            angle = 46, family="Times", size=4)+
   annotate("text", x = -162, y = 57.5, label = "Bering Sea", family="Times", size=4)+
   annotate("text", x = -155, y = 55, label = "Gulf of Alaska", family="Times", size=4)+
   scale_x_continuous(breaks=c(-165:-150),labels=c(c("-165","","","","","-160",
                                                     "","","","","-155","","","","","-150")))+theme(plot.margin=unit(c(-9,.5,-9,.2),"cm"))
dev.off()
#ggsave("./chapter_1/figs/Fig3.tiff", dpi=300, height=6, width=6, units="in")
