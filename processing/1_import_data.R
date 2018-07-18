#Clear all variables from R's memory
rm(list=ls(all=T))

#initialise packages
require(tidyverse)

#load the data file
raw_data.exp1 = read.table("./data/raw/exp1/parsed_data.txt",header=T,sep=",")
raw_data.exp2 = read.table("./data/raw/exp2/parsed_data.txt",header=T,sep=",")

raw_data.exp1$expt = 1
raw_data.exp2$expt = 2

#NOTE - approximately 55 participants for distance condition are missing last 4 trials because of technical issues in the javascript (slicing error - resolved 12/5/16)
#NOTE - approximately 39 participants for deadline condition are missing last 4 trials because of technical issues in the javascript (slicing error - resolved 12/5/16)
imported_data = bind_rows(raw_data.exp1,raw_data.exp2) %>%
  mutate(subject = as.numeric(as.factor(subject_id)),
         day = stage,
         goal_type = factor(goal_type,levels=1:2,labels=c("Approach","Avoidance")),
         left_plant_type = left_crop_type,
         right_plant_type = right_crop_type,
         left_start_height = experimental_start_height*(experimental_crop_side=='Left') + fixed_start_height*(experimental_crop_side=='Right'),
         right_start_height = experimental_start_height*(experimental_crop_side=='Right') + fixed_start_height*(experimental_crop_side=='Left'),
         left_days_total = experimental_weeks_total*(experimental_crop_side=='Left') + fixed_weeks_total*(experimental_crop_side=='Right'),
         right_days_total = experimental_weeks_total*(experimental_crop_side=='Right') + fixed_weeks_total*(experimental_crop_side=='Left'),
         left_days_remaining = left_weeks_remaining,
         right_days_remaining = right_weeks_remaining) %>%
         select(subject,expt,trial_kind,trial_number,goal_type,left_plant_type,right_plant_type,
                left_start_height,right_start_height,left_days_total,right_days_total,
                day,left_current_height,right_current_height,left_days_remaining,right_days_remaining,response,response_time,left_growth,right_growth) %>%
         arrange(subject,trial_kind,trial_number,day) %>%
  #filter subjects who didn't finish the task. Subjects who finish have the final trial as either 52 or 56 (the 52 was due to technical difficulties see above).
  group_by(subject) %>%
  mutate(maxtrial=max(trial_number)) %>%
  filter(maxtrial==52|maxtrial==56) %>%
  unique(by=c("subject","trial_number","day")) #This is to remove duplicates observations for subject 76 that were caused by program
#Note that there is one participant who's final trial is 53. Not sure why they weren't excluded from initial analyses, but we delete them here  (e.g., complete trial 56)

#create new variables from data (CHANGE GOALS)
transformed_data = imported_data %>%
  mutate(left_start_distance = 240 - left_start_height,
         right_start_distance = 240 - right_start_height,
         left_current_distance = 240 - left_current_height,
         right_current_distance = 240 - right_current_height,
         closer_start_distance = pmin(left_start_distance,right_start_distance),
         farther_start_distance = pmax(left_start_distance,right_start_distance),
         closer_start_distance_current = ((closer_start_distance==left_start_distance)&(closer_start_distance!=right_start_distance))*left_current_distance +
                                         ((closer_start_distance!=left_start_distance)&(closer_start_distance==right_start_distance))*right_current_distance +
                                         ((closer_start_distance==left_start_distance)&(closer_start_distance==right_start_distance))*right_current_distance,
         farther_start_distance_current = ((farther_start_distance==left_start_distance)&(farther_start_distance!=right_start_distance))*left_current_distance +
                                          ((farther_start_distance!=left_start_distance)&(farther_start_distance==right_start_distance))*right_current_distance +
                                          ((farther_start_distance==left_start_distance)&(farther_start_distance==right_start_distance))*right_current_distance,
         closer_current_distance = pmin(left_current_distance,right_current_distance),
         farther_current_distance = pmax(left_current_distance,right_current_distance),
         closer_start_deadline = pmin(left_days_total,right_days_total),
         farther_start_deadline = pmax(left_days_total,right_days_total),
         closer_start_deadline_current = ((closer_start_deadline==left_days_total)&(closer_start_deadline!=right_days_total))*left_days_remaining +
                                         ((closer_start_deadline!=left_days_total)&(closer_start_deadline==right_days_total))*right_days_remaining +
                                         ((closer_start_deadline==left_days_total)&(closer_start_deadline==right_days_total))*right_days_remaining,
         farther_start_deadline_current = ((farther_start_deadline==left_days_total)&(farther_start_deadline!=right_days_total))*left_days_remaining +
                                          ((farther_start_deadline!=left_days_total)&(farther_start_deadline==right_days_total))*right_days_remaining +
                                          ((farther_start_deadline==left_days_total)&(farther_start_deadline==right_days_total))*right_days_remaining,
         closer_start_deadline_current_distance = ((closer_start_deadline==left_days_total)&(closer_start_deadline!=right_days_total))*left_current_distance +
                                          ((closer_start_deadline!=left_days_total)&(closer_start_deadline==right_days_total))*right_current_distance,

         farther_start_deadline_current_distance = ((farther_start_deadline==left_days_total)&(farther_start_deadline!=right_days_total))*left_current_distance +
                                          ((farther_start_deadline!=left_days_total)&(farther_start_deadline==right_days_total))*right_current_distance,
         prioritise_right = response,
         prioritise_farther_distance = ((right_current_distance==farther_current_distance)&(prioritise_right==1))*1 +
                                       ((left_current_distance==farther_current_distance)&(prioritise_right==0))*1,
         prioritise_closer_deadline = ((right_days_total==closer_start_deadline)&(prioritise_right==1))*1 +
                                      ((left_days_total==closer_start_deadline)&(prioritise_right==0))*1,
         left_prob_achieved_full_start =  (goal_type=='Approach')*pnorm((3*left_days_total-left_start_distance)/(3*left_days_total)) +
                                          (goal_type=='Avoidance')*(1-pnorm((3*left_days_total-left_start_distance)/(3*left_days_total))),
         right_prob_achieved_full_start = (goal_type=='Approach')*pnorm((3*right_days_total-right_start_distance)/(3*right_days_total)) +
                                          (goal_type=='Avoidance')*(1-pnorm((3*right_days_total-right_start_distance)/(3*right_days_total))),
         left_prob_achieved_half_start =  (goal_type=='Approach')*pnorm((6*left_days_total-left_start_distance)/(3*left_days_total)) +
                                          (goal_type=='Avoidance')*(1-pnorm((0*left_days_total-left_start_distance)/(3*left_days_total))),
         right_prob_achieved_half_start = (goal_type=='Approach')*pnorm((6*right_days_total-right_start_distance)/(3*right_days_total)) +
                                          (goal_type=='Avoidance')*(1-pnorm((0*right_days_total-right_start_distance)/(3*right_days_total))),
         harder_goal = (left_prob_achieved_full_start<right_prob_achieved_full_start)*1 + (left_prob_achieved_full_start>right_prob_achieved_full_start)*2,
         harder_prob_achieved_full_start = (harder_goal<2)*left_prob_achieved_full_start + (harder_goal==2)*right_prob_achieved_full_start,
         easier_prob_achieved_full_start = (harder_goal<2)*right_prob_achieved_full_start + (harder_goal==2)*left_prob_achieved_full_start,
         harder_prob_achieved_half_start = (harder_goal<2)*left_prob_achieved_half_start + (harder_goal==2)*right_prob_achieved_half_start,
         easier_prob_achieved_half_start = (harder_goal<2)*right_prob_achieved_half_start + (harder_goal==2)*left_prob_achieved_half_start,
         prioritise_harder = (harder_goal==1)*(prioritise_right==0) + (harder_goal==2)*(prioritise_right==1)       )

transformed_data$farther_start_distance_current[transformed_data$right_start_distance==transformed_data$left_start_distance] <- NaN
transformed_data$closer_start_distance_current[transformed_data$right_start_distance==transformed_data$left_start_distance] <- NaN

transformed_data$farther_start_deadline_current[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN
transformed_data$closer_start_deadline_current[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN

transformed_data$farther_start_deadline_current_distance[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN
transformed_data$closer_start_deadline_current_distance[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN

transformed_data$prioritise_farther_distance[transformed_data$left_current_distance==transformed_data$right_current_distance] <- NA
transformed_data$prioritise_closer_deadline[transformed_data$left_days_remaining==transformed_data$right_days_remaining] <- NA
transformed_data$prioritise_harder[transformed_data$left_prob_achieved_full_start==transformed_data$right_prob_achieved_full_start] <- NA


save(transformed_data,file="./data/clean/transformed_data.RData")
