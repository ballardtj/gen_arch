rm(list=ls())
library(tidyverse)
library(gridExtra)
library(naniar)

# SET THEME FOR FIGURE ----------------

theme.goal <- theme(strip.text.x = element_text(size=12,face="bold",colour="black",family="Times"),
          strip.text.y = element_text(size=12, face="bold","black",family="Times"),
          strip.background = element_rect(colour="black", fill="gray"),
          plot.background = element_rect(fill = "transparent"),
          #plot.border= element_rect(fill = "transparent"),
          panel.border = element_rect(color="black", linetype="solid",fill=NA),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
       axis.text.x = element_text(size=12,colour="black",family="Times"),
       axis.text.y = element_text(size=12,colour="black",family="Times"),
       axis.title.x = element_text(size=18,colour="black",face="bold",family="Times",vjust=-0.5),
       axis.title.y = element_text(size=18,colour="black",face="bold",family="Times",vjust=1),
       #axis.ticks.length = unit(-0.2,"cm"),
       legend.title=element_text(size=16,face="bold",family="Times"),
       legend.text=element_text(size=14,family="Times"),
       legend.key=element_rect(color='transparent',fill='white' ),
       #legend.title=element_text(size=10,face="bold",family="Times"),
        legend.title.align = 0.5,
        #legend.text=element_text(size=6,family="Times"),
        legend.text.align = 0.5,
        #legend.key=element_rect(color='white',fill='white'),
       plot.title = element_text(size=24,face="bold",family="Times",hjust=0.5))#,
      # legend.key = element_rect(fill = "transparent", colour = "transparent"))



# IMPORT DATA -------

load("data/clean/dp_data_expt12.RData")
data_bound_expt12 = data_bound

load("data/clean/dp_data_expt3.RData")
data_bound_expt3 = data_bound

data_bound = rbind(data_bound_expt12,data_bound_expt3) %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2)

# create function to generate vector plot------
raw_data_vector_plot = function(data,min_n,goal_type_to_plot,source_to_plot){

  # if(goal_type_to_plot=="Approach"){ vector_color = "Blues" }
  # if(goal_type_to_plot=="Avoidance"){ vector_color = "Reds" }
  plot = data %>%
    filter(n >= min_n,
           source == source_to_plot,
           goal_type == goal_type_to_plot) %>%

    ggplot(aes(xend=xend,yend=yend,y = left_current_distance_bin, x = right_current_distance_bin)) +
    # geom_raster() +
    #geom_segment(arrow = arrow(length = unit(0.02, "npc")),alpha=1 ) +
    facet_grid(left_start_deadlineF ~ right_start_deadlineF ) +
    ylab('Left Distance to Goal (D)') +
    xlab("Right Distance to Goal (D)") +
    theme.goal +
    #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
    #scale_colour_distiller(palette = vector_color, limits = c(0, 1)) +
    labs(fill = "Proportion Prioritizing\nRight-hand Crop/Weed") +
    scale_x_reverse() +
    scale_y_reverse() +
    geom_vline(xintercept=0,size=0.5,linetype="longdash") +
    geom_hline(yintercept=0,size=0.5,linetype="longdash") +
    #geom_vline(xintercept=0,size=0.5,color='yellow') +
    #geom_hline(yintercept=0,size=0.5,color='yellow') +
    theme(legend.position = "none") +
    ggtitle(goal_type_to_plot)

  return(plot)

}


#create four different sets of plots
#1) across experiments
#2) experiment 1
#3) experiment 2
#4) experiment 3

for(i in 1:4){

  if(i == 1){
    data_bound_tmp = data_bound
    label=""
  }
  if(i == 2){
    data_bound_tmp = filter(data_bound,expt==1)
    label="_expt1"
  }
  if(i == 3){
    data_bound_tmp = filter(data_bound,expt==2)
    label="_expt2"
  }
  if(i == 4){
    data_bound_tmp = filter(data_bound,expt==3)
    label="_expt3"
  }

# PREPARE DATA FOR VECTOR PLOT -------

vector_plot_data = data_bound_tmp %>%
  filter(phase==1,left_current_distance > 0 , right_current_distance > 0 )   %>%
  mutate(
         #deadline was on a different scale in E3. So for E1 and E2 the normalised deadline variable (T) is calculated
         #by dividing by 91 (1 + the maximum number of days). For E3 the normalised deadline variable is calculated
         #by dividing by 9 (1 + the maximum number of months).
         max_dl = case_when(
         expt == 1 ~ 91,
         expt == 2 ~ 91,
         expt >= 3 ~ 9),
         a_t = ceiling(right_days_remaining) / max_dl,
         b_t = ceiling(left_days_remaining) / max_dl,
         #distance was on the same scale in all three experiments. The max distance observed in any experiment was
         #187. So we normalise the distance variable by dividing by 188.
         a_d = ceiling(right_current_distance) /188,
         b_d = ceiling(left_current_distance) /188,
         #bin deadline variable into three categories to facilitate plotting
         right_deadline_bin = case_when(
           a_t < 0.25 ~ 1,
           a_t>=0.25 & a_t<0.5 ~ 2,
           a_t >= 0.5 ~ 3),
         left_deadline_bin = case_when(
           b_t < 0.25 ~ 1,
           b_t>=0.25 & b_t<0.5 ~ 2,
           b_t >= 0.5 ~ 3),
         #bin distance variable by rounding to nearest 10 cm increment.
         left_current_distance_bin = round(b_d*10)/10,
         right_current_distance_bin = round(a_d*10)/10,
         policy_na = policy) %>%
  replace_with_na(replace = list(policy_na = 0.5)) %>% #replaces values of 0.5 in policy_na with na
  #summarise choice proportion for each combination of distance, deadline, and goal type.
  group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type) %>%
  summarise(  n = n(),
              accuracy = mean(prioritise_right == policy_na,na.rm=T),
              prioritise_right = mean(prioritise_right), #observed choices
              policy_right = mean(policy)) %>%          #choices made by dynamic programming model
  gather(source,choice_right,prioritise_right,policy_right) %>%
  #convert binned deadline variable and source variable into a factor so the labels will show up in the plot.
  mutate(left_start_deadlineF = factor(left_deadline_bin,levels=1:3,labels=c("Left: T < 0.25","Left: 0.25 < T < 0.5","Left: T > 0.5")),
         right_start_deadlineF = factor(right_deadline_bin,levels=1:3,labels=c("Right: T < 0.25","Right: 0.25 < T < 0.5","Right: T > 0.5")),
         source = factor(source,levels = c('prioritise_right', 'policy_right'),labels = c('Observed', 'Achievement Maximizing'))) %>%
  #calculate coordinates for the end of each vector.
  mutate(xend = right_current_distance_bin - (goal_type=="Approach")*choice_right*0.1 - (goal_type=="Avoidance")*(1-choice_right)*0.1,
         yend = left_current_distance_bin - (goal_type=="Approach")*(1-choice_right  )*0.1 - (goal_type=="Avoidance")*choice_right*0.1)

#create plots for approach and avoidance conditions
ap_plot = raw_data_vector_plot(data=vector_plot_data,min_n=10,goal_type_to_plot="Approach",source_to_plot = "Observed") +
  geom_segment(aes(colour=accuracy),arrow = arrow(length = unit(0.02, "npc")),colour="blue")

av_plot = raw_data_vector_plot(data=vector_plot_data,min_n=10,goal_type_to_plot="Avoidance",source_to_plot = "Observed") +
  geom_segment(aes(colour=accuracy),arrow = arrow(length = unit(0.02, "npc")),colour="red")

figure = grid.arrange(ap_plot,
                      av_plot,
                      ncol=2)

ggsave(file=paste0("figures/descriptive_vectors",label,".pdf"),plot=figure,height=6,width=12)

}











### NORMATIVE MODEL ###

#create plots for approach and avoidance conditions
ap_plot = raw_data_vector_plot(data=vector_plot_data,min_n=10,goal_type_to_plot="Approach",source_to_plot = "Observed") +
  geom_segment(aes(colour=accuracy),arrow = arrow(length = unit(0.02, "npc"))) +
  scale_colour_distiller(palette="RdYlBu",limits=c(0,1),na.value="gray20") +
  #scale_colour_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  theme(legend.position = "bottom",panel.background = element_rect(fill="gray20"),
        legend.text = element_text(size=10)) +
  labs(colour="Proportion of Decisions Aligning with Normative Model")

av_plot = raw_data_vector_plot(data=vector_plot_data,min_n=10,goal_type_to_plot="Avoidance",source_to_plot = "Observed") +
  geom_segment(aes(colour=accuracy),arrow = arrow(length = unit(0.02, "npc"))) +
  scale_colour_distiller(palette="RdYlBu",limits=c(0,1),na.value="gray20") +
  #scale_colour_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  theme(panel.background = element_rect(fill="gray20"))

#function to extract legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend = g_legend(ap_plot)

figure = grid.arrange(
            arrangeGrob(ap_plot + theme(legend.position = "none"),
                        av_plot,
                        ncol=2),
            legend,
            nrow=2,
            heights=c(10,1))


ggsave(file="figures/normative_gradient.pdf",plot=figure,height=6,width=12)


#divide into four cells

# Right goal longer deadline, longer distance
# Right goal longer deadline, same distance
# Right goal longer deadline, shorter distance
# Right goal same deadline, longer distance
# Right goal same deadline, same distance
# Right goal same deadline, shorter distance
# Right goal shorter distance, longer deadline
# Right goal shorter distance

data_bound %>%
  filter(phase==1,left_current_distance > 0 , right_current_distance > 0, policy!=0.5) %>%
  mutate(current_distance_bin_tmp = case_when(
          right_current_distance < left_current_distance ~ 1, #right shorter distance
          right_current_distance == left_current_distance ~ 2, #equal
          right_current_distance > left_current_distance ~ 3  #left shorter
          ),
         current_deadline_bin_tmp = case_when(
           right_days_remaining < left_days_remaining ~ 1, #right shorter deadline
           right_days_remaining == left_days_remaining ~ 2, #equal
           right_days_remaining > left_days_remaining ~ 3  #left shorter
         ),
         current_distance_bin = factor(current_distance_bin_tmp,levels=1:3,labels=c('Right shorter distance','Same distance','Left shorter distance')),
         current_deadline_bin = factor(current_deadline_bin_tmp,levels=1:3,labels=c('Right shorter deadline','Same deadline','Left shorter deadline'))) %>%
  group_by(goal_type,current_distance_bin,current_deadline_bin) %>%
  summarise(policy_right = mean(policy),
            prioritise_right = mean(prioritise_right)) %>%
  gather(source,choice_right,prioritise_right,policy_right) %>%
  ggplot(aes(x=current_distance_bin,y=choice_right,group=source,colour=source)) +
  geom_line() +
  facet_grid(current_deadline_bin~goal_type) +
  labs(y = "Proportion Prioritizing Right-hand Crop/Weed") +
  theme.goal





#color of arrows representing how accurate people were

# proportion of decisions that matched the normative model

data_bound %>%
  filter(phase==1,left_current_distance > 0 , right_current_distance > 0, policy!=0.5 ) %>%
  group_by(goal_type) %>%
  summarise(accuracy = mean(prioritise_right==policy))


data_bound %>%
  filter(phase==1,left_current_distance > 0 , right_current_distance > 0, policy!=0.5 ) %>%
  group_by(goal_type,subject) %>%
  summarise(accuracy = mean(prioritise_right==policy)) %>%
  group_by(goal_type) %>%
  summarise(mean_acc = mean(accuracy),
            sd_acc = sd(accuracy),
            min_acc = min(accuracy),
            max_acc = max(accuracy))

  ggplot() +
  geom_histogram(aes(x=accuracy)) +
  facet_grid(.~goal_type)



vector_plot_data




figure = grid.arrange(ap_plot,av_plot,ncol=2)



