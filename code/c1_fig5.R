library(extrafont)
loadfonts(device="win")
library(ggplot2)
#set plotting parameters
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times')+ 
             theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank()))
library(tree)

polly <- read.csv("./chapter_1/output/polly.csv")

#used this to plot for the manuscript
tree.fit1<-tree(mature~newx+newy,data=polly)

#plot CART
tiff(filename = "./chapter_1/figs/Fig5.tiff", width = 5.2, height = 5.2, units = "in", res=600, compression="lzw")
plot(polly$newx,polly$newy,pch=19,col='gray',xlab=(expression(paste(Rotated~Longitude^o,~'W'))),ylab=(expression(paste(Rotated~Latitude^o,~'N'))),family='Times')
partition.tree(tree.fit1,ordvars=c('newx','newy'),add=T,family='Times')
dev.off()
