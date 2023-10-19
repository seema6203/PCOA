
working_dir<-"/Users/Seema/Documents/pcoa/"
file<-"pcoa.csv"
setwd(working_dir)
library(ggplot2)
library(poppr)
nou<-read.genalex(file,genclone = FALSE)
D<-nei.dist(nou)
# pop(nou)
colr<-c(rep("firebrick",48),rep("#008b8b",48),rep("#7b68ee",32))
pco<-dudi.pco(D,scannf = FALSE,nf=3)
population<-c(rep("Cluster1",48),rep("Cluster2",48),rep("Cluster3",32))
#population[6]<-"Cluster2"
#population[21]<-"Cluster2"
p<-ggFunctions::s.class(pco$li,fac=population,xax=1,yax=2,drawEllipse=FALSE,drawSegment = FALSE,cellipse = 1)

p<-p+theme(
   legend.background = element_rect(fill="transparent"), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent",color=NA),
           panel.grid.major = element_line(color="grey",size = 0.1),
          panel.grid.minor = element_blank(),
           plot.background   = element_rect(fill = "white",color=NA),
           panel.background   = element_rect(fill="transparent",color="black",size=0.6)
         ,legend.position="none" )
       
p<-p + labs(x="PCo1 (18.74%)", y="PCo2 (7.98%)")

q<-ggplot_build(p)
q$data[[3]]$colour<-c(rep("red",2),rep("purple",39),rep("red",22),rep("orange",29))
q$data[[3]]$size<-c(rep(2,92))
colours <- unique(q$data[[3]][, "colour"])
colours
p<-p+scale_color_manual(values = colours)
q<-ggplot_build(p)
q$data[[2]]$size<-c(rep(0.5,92))
q$data[[1]]$size<-c(rep(0.5,92))



q$data[[4]]$vjust<-c(rep(1.2,92))
q$data[[4]]$hjust<-c(rep(1.2,92))
q$data[[4]]$label<-indNames(nou)
q$data[[4]]$size<-c(rep(1.9,92))

s<-ggplot_gtable(q)
# s+scale_color_manual(values = my_colors)
pdf("pcoa.pdf")
plot(s)
dev.off()

