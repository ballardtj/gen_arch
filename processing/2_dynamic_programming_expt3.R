#This script runs the implemented the dynamic programming required
#to calculate the optimal decisions. It saves a new data frame with
#some extra variables related to the optimality of decisions.

rm(list=ls())
library(tidyverse)
# library(foreach)
# library(doMC)
# registerDoMC()

#Load Stream B Data
load("./data/clean/transformed_data.RData")

transformed_data = filter(transformed_data,trial_kind=="experimental",expt==3)
max(transformed_data$left_current_distance,transformed_data$right_current_distance)
min(transformed_data$left_current_distance,transformed_data$right_current_distance)


#Set Parameters
bin_size = 5 #number of states that are put together in same bin
max_distance = 200 # 200 cm away from goal (i.e., height of 0)
min_distance = -100 # 100 cm above goal (i.e., height of 280)
states = (max_distance-min_distance)/bin_size + 1 #add 1 for the min and max
#bins = seq(max_distance-bin_size/2,min_distance+bin_size/2,-bin_size)
bins = seq(max_distance,min_distance,-bin_size)
min_bins = c(Inf,bins[2:length(bins)])+bin_size/2
max_bins = c(bins[1:(length(bins)-1)],-Inf)-bin_size/2
mean_real_growth = 20#
sd_real_growth = 20


#round to the nearest bin. Value of 'bin' variable represents the centre of the bin
#transformed_data$left_current_distance_bin = max( round(  (transformed_data$left_current_distance+bin_size/2)/bin_size )*bin_size - bin_size/2 , 60-bin_size/2)
#transformed_data$right_current_distance_bin = max(  round(  (transformed_data$right_current_distance+bin_size/2)/bin_size )*bin_size - bin_size/2 , 60-bin_size/2)
transformed_data$left_current_distance_bin = pmax( round(transformed_data$left_current_distance/bin_size )*bin_size , -60)
transformed_data$right_current_distance_bin = pmax(  round(transformed_data$right_current_distance/bin_size )*bin_size  , -60)
transformed_data$stage = transformed_data$day
transformed_data$frame = (transformed_data$goal_type=="Approach") + (transformed_data$goal_type=="Avoidance")*2
transformed_data$left_start_deadline = transformed_data$left_days_total
transformed_data$right_start_deadline = transformed_data$right_days_total
data=transformed_data
rm(transformed_data)

ctr=0
deadlines=c(1,2,4,8)
datalist = list()
for(right_dl in deadlines){
#foreach(right_dl = deadlines) %dopar% {
  for(left_dl in deadlines){
    print(c(right_dl,left_dl))
    ctr=ctr+1#(match(right_dl,deadlines)-1)*length(deadlines) + match(left_dl,deadlines)
    stages = max(right_dl,left_dl) #number of weeks in the longest growing season
    phases = c(rep(1,min(right_dl,left_dl)),rep(2, max(right_dl,left_dl)-min(right_dl,left_dl)))                       #phase number of each stage


    #list of matricies for each phase
    P = array(0,dim=c(states^2,states^2,2,2))
    #P2 = P1


    #P_tmp = list(array(0,dim=c(states^2,states^2,2)))
    #P = rep(list(P_tmp),2)

    #loop through each row of transition matrix



    for(phase in 1:2){ #move back to phase 2 when creating deadline model
      row=0
      for(left_bin in bins){
        for(right_bin in bins){
          row=row+1

          #print(row)
          if(phase==1){
            left_probP = pnorm(min_bins, left_bin-mean_real_growth,  sd_real_growth) -
              pnorm(max_bins, left_bin-mean_real_growth,  sd_real_growth)

            left_probNP = pnorm(min_bins, left_bin,  sd_real_growth) -
              pnorm(max_bins, left_bin,  sd_real_growth)

            right_probP = pnorm(min_bins, right_bin-mean_real_growth,  sd_real_growth) -
              pnorm(max_bins, right_bin-mean_real_growth,  sd_real_growth)

            right_probNP = pnorm(min_bins, right_bin,  sd_real_growth) -
              pnorm(max_bins, right_bin,  sd_real_growth)
          }

          if(phase==2){
            if(right_dl<left_dl){
              left_probP = pnorm(min_bins, left_bin-mean_real_growth,  sd_real_growth) -
                pnorm(max_bins, left_bin-mean_real_growth,  sd_real_growth)

              left_probNP = pnorm(min_bins, left_bin,  sd_real_growth) -
                pnorm(max_bins, left_bin,  sd_real_growth)

              right_probP = pnorm(min_bins, right_bin,  0) -
                pnorm(max_bins, right_bin,  0)

              right_probNP = pnorm(min_bins, right_bin,  0) -
                pnorm(max_bins, right_bin,  0)

            }

            if(right_dl>left_dl){
              left_probP = pnorm(min_bins, left_bin,  0) -
                pnorm(max_bins, left_bin, 0)

              left_probNP = pnorm(min_bins, left_bin,  0) -
                pnorm(max_bins, left_bin,  0)

              right_probP = pnorm(min_bins, right_bin-mean_real_growth,  sd_real_growth) -
                pnorm(max_bins, right_bin-mean_real_growth,  sd_real_growth)

              right_probNP = pnorm(min_bins, right_bin,  sd_real_growth) -
                pnorm(max_bins, right_bin,  sd_real_growth)

            }



          }


          #if left (outer loop) crop is irrigated
          leftProb_leftChosen = kronecker(left_probP,matrix(1,states,1)) #left increments by the amount it does if prioritised
          rightProb_leftChosen = rep(right_probNP,states) #right increments by the non-prioritized amount
          P[row,,1,phase] = leftProb_leftChosen*rightProb_leftChosen

          #if right (outer loop) crop is irrigated
          leftProb_rightChosen = kronecker(left_probNP,matrix(1,states,1)) #left increments by the amount it does if prioritised
          rightProb_rightChosen = rep(right_probP,states) #right increments by the non-prioritized amount
          P[row,,2,phase] = leftProb_rightChosen*rightProb_rightChosen

        }
      }
    }



    #CREATE REWARD VECTORS
    singleRewards = c(rep(0,max_distance/bin_size),0.5,rep(1,-min_distance/bin_size))
    longRewards = kronecker(singleRewards,matrix(1,states,1)  )
    shortRewards = rep(singleRewards,states)
    rewards = longRewards+shortRewards
    rewards = cbind(rewards,2-rewards) #first column approach, second column avoidance

    #create dp dataframe to store values
    dpdata = expand.grid(right_current_distance_bin=bins,left_current_distance_bin=bins,stage=1:stages,frame=1:2,left_start_deadline=left_dl,right_start_deadline=right_dl,value=NaN,ev_right=NaN,ev_left=NaN)

    discount = 1

    for(f in 1:2){
      Vprev = rewards[,f] #get value of previous state
      for(n in seq(stages,1,by=-1)){
        inds = (f-1)*states^2*stages +(((n-1)*states^2)+1):(n*states^2)
        if(f==1){
          #dpdata$ev_left[inds] = discount*P[,,1]%*%Vprev
          #dpdata$ev_right[inds] = discount*P[,,2]%*%Vprev
          dpdata$ev_left[inds] = discount*P[,,1,phases[n]]%*%Vprev
          dpdata$ev_right[inds] = discount*P[,,2,phases[n]]%*%Vprev

        } else {
          dpdata$ev_left[inds] = discount*P[,,2,phases[n]]%*%Vprev
          dpdata$ev_right[inds] = discount*P[,,1,phases[n]]%*%Vprev
          #dpdata$ev_left[inds] = discount*P[,,2]%*%Vprev
          #dpdata$ev_right[inds] = discount*P[,,1]%*%Vprev
        }
        dpdata$value[inds] = pmax(dpdata$ev_left[inds],dpdata$ev_right[inds])
        Vprev = dpdata$value[inds]
      }
    }

    datalist[[ctr]] <- dpdata



  }
}


full_dpdata <- bind_rows(datalist)

data_bound = left_join(data,full_dpdata,by=c("right_current_distance_bin","left_current_distance_bin","stage","frame","left_start_deadline","right_start_deadline"))

# data$value_diff = data$ev_left - data$ev_right

#at what point do we declare the policies equivilant
equivilance = 10 #decimal places

# full_dpdata$policy = (round(full_dpdata$ev_left,equivilance)>round(full_dpdata$ev_right,equivilance))*1 +
#   (round(full_dpdata$ev_left,equivilance)==round(full_dpdata$ev_right,equivilance))*0.5


data_bound$policy = (round(data_bound$ev_left,equivilance)<round(data_bound$ev_right,equivilance))*1 +
  (round(data_bound$ev_left,equivilance)==round(data_bound$ev_right,equivilance))*0.5

# dpdata$policy = (round(dpdata$ev_left,equivilance)>round(dpdata$ev_right,equivilance))*1 +
#   (round(dpdata$ev_left,equivilance)==round(dpdata$ev_right,equivilance))*0.5

#save data file with optimal decisions
save(data_bound,file="data/clean/dp_data_expt3.RData")

#option to save just the full dynamic programing results for every bin (the file is approximately 500MB)
#save(full_dpdata,file="./data/clean/DPOnly.Rda")
