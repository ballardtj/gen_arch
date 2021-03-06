#!/bin/bash
pwd

#source ~/.bash_profile;

#make goal
#make model/goal_${3}_${4}

#2 runs: 1 for approach and one for avoidance. Could be expanded to run different models, data sources, expts, etc.

for R in 3 4
do

#submit sampling job
job1=$(qsub -v RUN=$R /QRISdata/Q0993/processing/5.2_collect_samples.pbspro)

#wait for sampling job to complete and submit compile job
qsub -W depend=afterok:$job1 -v RUN=$R /QRISdata/Q0993/processing/5.4_compile_samples.pbspro

done
