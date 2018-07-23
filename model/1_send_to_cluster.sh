#!/bin/bash

#upload model files to cmdstan folder on cluster
scp -r model uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/

#upload data to new model folder
if [ $1 == "ap" ]; then
	if [ $2 == "obs" ]; then
		echo "launching model of observed decisions in the approach condition!"
		#upload data file to cluster
    	scp data/clean/obs_ap_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
    	fi
  if [ $2 == "opt" ]; then
		echo "launching model of optimal decisions in the approach condition!"
		#upload data file to cluster
    	scp data/clean/opt_ap_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
    	fi
	fi

if [ $1 == "av" ]; then
	if [ $2 == "obs" ]; then
		echo "launching model of observed decisions in the avoidance condition!"
		#upload data file to cluster
    	scp data/clean/obs_av_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
    	fi
  if [ $2 == "opt" ]; then
		echo "launching model of optimal decisions in the avoidance condition!"
		#upload data file to cluster
    	scp data/clean/opt_av_rdump.R uqtballa@tinaroo.rcc.uq.edu.au:~/cmdstan/model/
    	fi
	fi

#run job script on cluster
ssh uqtballa@tinaroo.rcc.uq.edu.au bash cmdstan/model/2_make_goal.sh $1 $2


