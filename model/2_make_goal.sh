#!/bin/bash
pwd

cd cmdstan

#make goal
make model/goal

#submit sampling job
job1=$(qsub -v FRAME=$1,SOURCE=$2 -o ~/cmdstan/model/ -e ~/cmdstan/model/ model/3_collect_samples.pbs)

#wait for sampling job to complete and submit compile job
qsub -v FRAME=$1,SOURCE=$2 -W depend=afterok:$job1 -o ~/cmdstan/model/ -e ~/cmdstan/model/ model/5_compile_samples.pbs

