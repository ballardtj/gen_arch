#clear workspace
rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(grid)
library(gridExtra)

w1 = c(1,1,0,0,0,0)
w2 = c(0,0,1,0,0,0)
w3 = c(0,0,0,1,1,1)
delta = c(-0.5,0.5,0,0,0,0)
tau = c(0,0,-0.5,0,0,0)
alpha = c(0.5,0.5,0.5,0.001,0.999,0.5)

#create data frame - outer loop is frame, middle loop is distance, inner loop is time
levs= seq(0.01,0.99,by=0.01)
pd<-expand.grid(perspective = 1:6,distance =levs,time=levs) %>%
  mutate(stg_max =1,# 2*(1-alpha[perspective])*sqrt(alpha[perspective]/(1-alpha[perspective])),
          motivation = w1[perspective]*((delta[perspective]>=0)*distance^delta[perspective] + (delta[perspective]<0)*(1-distance^-delta[perspective])) +
                      w2[perspective]*((tau[perspective]>=0)*time^tau[perspective] + (tau[perspective]<0)*(1-time^-tau[perspective])) +
                      w3[perspective]*stg_max/( alpha[perspective]*(time/distance)  + (1-alpha[perspective])*(distance/time) )) %>%
  group_by(perspective) %>%
  mutate(motivation_scaled=(motivation-min(motivation))/(max(motivation)-min(motivation)))


pd$perspective_factor = factor(pd$perspective,1:6,c('Proximity Perspctive','Discrepancy Perspective',' ',
                                                    'Expectancy Perspective','Difficulty Perspective','Non-Monotonic Perspective'))
pd$distance_factor = factor(pd$distance,levels=0:(length(levs)))
pd$time_factor = factor(pd$time,levels=0:(length(levs)))

plotList=list()

for(p in 1:6){

pdp <- pd %>% filter(perspective==p)

plotList[[p]] <- ggplot(data=pdp,aes(x=distance,y=time)) +
  ggtitle( levels(pd$perspective_factor)[p] ) +
  geom_raster(aes(fill=motivation_scaled)) +
  scale_x_continuous(expand=c(0,0),breaks=c(0.22,0.78),labels=c("Shorter","Longer"))+
  scale_y_continuous(expand=c(0,0),breaks=c(0.22,0.78),labels=c("Shorter","Longer"))+
  #scale_x_discrete(labels=c(0,rep(' ',20),'Shorter',rep(' ',60),'Longer',rep(' ',28))) +
  #scale_y_discrete(labels=c(0,rep(' ',20),'Shorter',rep(' ',60),'Longer',rep(' ',28))) +
  geom_contour(aes(x=distance,y=time,z=motivation_scaled),colour="black",binwidth=0.1,size=.25) +
  scale_fill_distiller(palette="Spectral",direction=-1, na.value="white",limits=c(0, 1),breaks=seq(.1,.9,by=0.8),labels=c('Lower','Higher'),name="Motivational Value") +
  coord_cartesian(ylim= c(0.01,0.99),xlim=c(0.01,0.99)) +
  xlab('Distance to Goal') +
  ylab("Time to Deadline") +
  theme(plot.title = element_text(size=10,face="italic",family="Times"),
        strip.text.x = element_text(size=12,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=12, face="bold","black",family="Times"),
        strip.background = element_rect(colour="black", fill="gray"),
        plot.background = element_rect(fill = "transparent"),
        #plot.border= element_rect(fill = "transparent"),
        panel.border = element_rect(color="black", linetype="solid",fill=NA),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8,colour="black",family="Times"),
        axis.text.y = element_text(size=8,colour="black",family="Times",angle=90,hjust=0.5),
        axis.title.x = element_text(size=10,colour="black",face="bold",family="Times",vjust=-0.5),
        axis.title.y = element_text(size=10,colour="black",face="bold",family="Times",vjust=1),
        #axis.ticks.length = unit(-0.2,"cm"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_text(size=12,face="bold",family="Times"),
        legend.title.align = 0.5,
        legend.text=element_text(size=10,family="Times"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.position="bottom",
        legend.box="horizontal") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth =30,raster=T))

#ggsave(paste0(Sys.getenv('HOME'),"/Dropbox/Fun/Grants/2017 - DECRA/Gradient.pdf"),width=6,height=3)


}

plot.layout=matrix(c(1,1,2,3,4,5,6,6,6,7,8,9,10,10,10),5,3,byrow = TRUE)
#nf <- layout(plot.layout,c(1,1,1),c(0.2,1,0.2,1,0.2),respect=TRUE)
#layout.show(nf)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plotList[[1]] )

t1 <- grid.text("Spatial Gradient",gp = gpar(fontsize = 14, fontface = "bold.italic",fontfamily="Times"))
t2 <- grid.text("Temporal Gradient",gp = gpar(fontsize = 14, fontface = "bold.italic",fontfamily="Times"))
t3 <- grid.text("Spatiotemporal Gradient",gp = gpar(fontsize = 14, fontface = "bold.italic",fontfamily="Times"))

p = arrangeGrob(t1,t2,
                         plotList[[1]] + theme(legend.position="none"),plotList[[2]] + theme(legend.position="none"),plotList[[3]] + theme(legend.position="none"),
                         t3,
                         plotList[[4]] + theme(legend.position="none"),plotList[[5]] + theme(legend.position="none"),plotList[[6]] + theme(legend.position="none"),
                         mylegend,nrow=5,layout_matrix=plot.layout,heights=c(.1,1,.15,1,.3),widths=c(1,1,1),respect=T)

ggsave(file="figures/hypothetical_gradients.pdf",plot=p,width=8,height=6)


#Create legend.
#Make sure figures are as close together as possible


