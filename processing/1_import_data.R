#Clear all variables from R's memory
rm(list=ls(all=T))

#initialise packages
require(tidyverse)

#load the data files from experiments 1, 2, and 3
raw_data.exp1 = read.table("./data/raw/exp1/parsed_data.txt",header=T,sep=",")
raw_data.exp2 = read.table("./data/raw/exp2/parsed_data.txt",header=T,sep=",")
raw_data.exp3 = read.table("./data/raw/exp3/parsed_data_old.txt",header=T,sep=",") %>% select(-gender_key,-age,-session_key,-birth_month,-child_street,-mothers_name)

vars = names(raw_data.exp1)

raw_data.exp1$expt = 1
raw_data.exp2$expt = 2
raw_data.exp3$expt = 3

#merge subject data files from experiment 4

files = list.files(path = "data/raw/exp4", pattern = '.csv')
nSubj = length(files)

datalist = list()
dataset.ctr=0
for(subject in 1:nSubj){
  raw_dat = read.csv(paste0("data/raw/exp4/",files[subject]))
  behav_dat <- raw_dat %>% filter(trial_type=='plant-game') %>%
    mutate(goal_type = 3,
           subject_id = as.numeric(gsub(".csv","",files[subject])),
           left_goal_type = case_when(
             experimental_crop_side == "Right" ~ "Fixed",
             experimental_crop_side == "Left" ~ "Experimental"
            ),
           right_goal_type = case_when(
             experimental_crop_side == "Right" ~ "Experimental",
             experimental_crop_side == "Left" ~ "Fixed"
           )
    )
  for(trial in 1:dim(behav_dat)[1]){
    dataset.ctr=dataset.ctr+1

    n_obs_in_trial =  length(as.numeric(strsplit(as.character(behav_dat[trial,"left_current_height"]),",")[[1]] ) )

    trial_dat <- data.frame(matrix(ncol = length(vars), nrow = n_obs_in_trial  ))
    colnames(trial_dat) <- vars

    for(var in vars){
      if(var == "mouse_click"){var = "mouse_clicked"}

      value_1 = behav_dat[trial,var]
      value_2 = strsplit( as.character(value_1),"," )[[1]]

      if(any(!is.na(as.numeric(value_2)))){
        value_2 = as.numeric(value_2)
      }
      trial_dat[var] = value_2
    }
    datalist[[dataset.ctr]] <- trial_dat
  }
}

raw_data.exp4 <- bind_rows(datalist)
raw_data.exp4$expt = 4

#NOTE - approximately 55 participants for distance condition are missing last 4 trials because of technical issues in the javascript (slicing error - resolved 12/5/16)
#NOTE - approximately 39 participants for deadline condition are missing last 4 trials because of technical issues in the javascript (slicing error - resolved 12/5/16)
#NOTE - 3 participants did not complete expt3 (one did not even start, only provided demographic data so is not in the data frame at all)
imported_data_tmp = bind_rows(raw_data.exp1,raw_data.exp2,raw_data.exp3,raw_data.exp4) %>%
  mutate(subject = as.numeric(as.factor(subject_id)),
         day = stage,
         goal_type = factor(goal_type,levels=1:3,labels=c("Approach","Avoidance","ApAv")),
         #for expt 4, the plant denoted as "right" plant is always the approach (even though in reality, the approach side was randomised)
         left_plant_type = case_when(
           expt < 4 ~ left_crop_type,
           expt == 4 ~ fixed_crop_type
          ),
         right_plant_type = case_when(
           expt < 4 ~ right_crop_type,
           expt == 4 ~ experimental_crop_type
          ),
         left_start_height = case_when(
           expt < 4 & experimental_crop_side=='Left' ~ experimental_start_height,
           expt < 4 & experimental_crop_side=='Right' ~ fixed_start_height,
           expt == 4 ~ fixed_start_height
          ),
         right_start_height = case_when(
           expt < 4 & experimental_crop_side=='Right' ~ experimental_start_height,
           expt < 4 & experimental_crop_side=='Left' ~ fixed_start_height,
           expt == 4 ~ experimental_start_height
         ),
         left_current_height_tmp = case_when(
           expt < 4  ~ left_current_height,
           expt == 4 & experimental_crop_side=='Right' ~ left_current_height,
           expt == 4 & experimental_crop_side=='Left' ~ right_current_height
         ),
         right_current_height_tmp = case_when(
           expt < 4 ~ right_current_height,
           expt == 4 & experimental_crop_side=='Right' ~ right_current_height,
           expt == 4 & experimental_crop_side=='Left' ~ left_current_height
         ),
         left_current_height = left_current_height_tmp,
         right_current_height = right_current_height_tmp,
         left_days_total = case_when(
           expt < 4 & experimental_crop_side=='Left' ~ experimental_weeks_total,
           expt < 4 & experimental_crop_side=='Right' ~ fixed_weeks_total,
           expt == 4 ~ fixed_weeks_total
         ),
         right_days_total = case_when(
           expt < 4 & experimental_crop_side=='Right' ~ experimental_weeks_total,
           expt < 4 & experimental_crop_side=='Left' ~ fixed_weeks_total,
           expt == 4 ~ experimental_weeks_total
         ),
         left_days_remaining_tmp = case_when(
           expt < 4  ~ left_weeks_remaining,
           expt == 4 & experimental_crop_side=='Right' ~ left_weeks_remaining,
           expt == 4 & experimental_crop_side=='Left' ~ right_weeks_remaining
         ),
         right_days_remaining_tmp = case_when(
           expt < 4 ~ right_weeks_remaining,
           expt == 4 & experimental_crop_side=='Right' ~ right_weeks_remaining,
           expt == 4 & experimental_crop_side=='Left' ~ left_weeks_remaining
         ),
         left_days_remaining = left_days_remaining_tmp,
         right_days_remaining = right_days_remaining_tmp) %>%
         select(subject,expt,trial_kind,trial_number,goal_type,left_plant_type,right_plant_type,
                left_start_height,right_start_height,left_days_total,right_days_total,
                day,left_current_height,right_current_height,left_days_remaining,right_days_remaining,response,response_time,left_growth,right_growth) %>%
         arrange(subject,trial_kind,trial_number,day) %>%
  #filter subjects who didn't finish the task. Subjects who finish have the final trial as either 52 or 56 (the 52 was due to technical difficulties see above).
  group_by(subject) %>%
  mutate(maxtrial=max(trial_number),
         complete = (expt<3)*(maxtrial==52|maxtrial==56) +
                    (expt==3)*(maxtrial==240) +
                    (expt==4)*(maxtrial==256))

imported_data = imported_data_tmp %>%
  filter(complete==1) %>%
  unique(by=c("expt","subject","trial_number","day")) #This is to remove duplicates observations for subject 76 that were caused by program
#Note that there is one participant who's final trial is 53. Not sure why they weren't excluded from initial analyses, but we delete them here  (e.g., complete trial 56)

#create new variables from data (CHANGE GOALS)
transformed_data = imported_data %>%
  mutate(goal = case_when(
                    expt < 3 ~ 240,
                    expt >= 3 ~ 200
                ),
         left_start_distance = goal - left_start_height,
         right_start_distance = goal  - right_start_height,
         left_current_distance = goal - left_current_height,
         right_current_distance = goal  - right_current_height,
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
                                      ((left_days_total==closer_start_deadline)&(prioritise_right==0))*1)

transformed_data$farther_start_distance_current[transformed_data$right_start_distance==transformed_data$left_start_distance] <- NaN
transformed_data$closer_start_distance_current[transformed_data$right_start_distance==transformed_data$left_start_distance] <- NaN

transformed_data$farther_start_deadline_current[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN
transformed_data$closer_start_deadline_current[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN

transformed_data$farther_start_deadline_current_distance[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN
transformed_data$closer_start_deadline_current_distance[transformed_data$right_days_remaining==transformed_data$left_days_remaining] <- NaN


save(transformed_data,file="./data/clean/transformed_data.RData")

### DEMOGRAPHIC DETAILS ###

#AGE AND GENDER

#Expt 1

age = read.table("data/raw/exp1/age.txt",header=T)

range(age$age)
mean(age$age)
sd(age$age)

gender = read.table("data/raw/exp1/gender.txt",header=T)
count(gender,gender) %>% mutate(prop = n/sum(n))

length(gender$gender) #full sample size (number of participants who completed first question)

#Expt 2

age = read.table("data/raw/exp2/age.txt",header=T)

range(age$age)
mean(age$age)
sd(age$age)

gender = read.table("data/raw/exp2/gender.txt",header=T)
count(gender,gender) %>% mutate(prop = n/sum(n))

length(gender$gender) #full sample size (number of participants who completed first question)

#Expt 3

age = read.table("data/raw/exp3/age.txt",header=T)

range(age$age)
mean(age$age)
sd(age$age)

gender = read.table("data/raw/exp3/gender.txt",header=T)
count(gender,gender) %>% mutate(prop = n/sum(n))

length(gender$gender)

#NUMBER WHO COMPLETED EXPERIMENT

#Sample size

imported_data %>%
  filter(expt<4) %>%
  summarise(expt=head(expt,1),complete=head(complete,1)) %>%
  group_by(expt) %>%
  summarise(n_complete = sum(complete))

#Note that some participants only provided age and gender data.


