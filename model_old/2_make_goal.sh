#!/bin/bash
pwd

cd cmdstan

pwd

source ~/.bash_profile;

#make goal
#make model/goal_${3}_${4}

#submit sampling job
job1=$(qsub -v FRAME=$1,SOURCE=$2,STRUCTURE=$3,MODEL=$4 -o ~/cmdstan/model/ -e ~/cmdstan/model/ model/3_collect_samples.pbspro)

#wait for sampling job to complete and submit compile job
qsub -v FRAME=$1,SOURCE=$2,STRUCTURE=$3,MODEL=$4 -W depend=afterok:$job1 -o ~/cmdstan/model/ -e ~/cmdstan/model/ model/5_compile_samples.pbspro

