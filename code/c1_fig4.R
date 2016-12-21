library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(tree)

poll <- read.csv("./chapter_1/output/poll.csv")

#used this to plot for the manuscript
tree.fit<-tree(mature~longitude+latitude,data=poll)

#plot CART
tiff(filename = "./chapter_1/figs/Fig4.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
plot(poll$longitude,poll$latitude,pch=19,col='gray',xlab=(expression(paste(Longitude^o,~'W'))),ylab=(expression(paste(Latitude^o,~'N'))),family='Times')
partition.tree(tree.fit,ordvars=c('longitude','latitude'),add=T,family='Times')
dev.off()
