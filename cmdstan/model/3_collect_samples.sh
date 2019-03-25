#!/bin/bash

# cd cmdstan

echo $1
echo $2

if [ "$2" -eq 1 ]
then
	COND="ap"
fi

if [ "$2" -eq 2 ]
then
	COND="av"
fi

THREADS=-1

export STAN_NUM_THREADS=$THREADS

/QRISdata/Q0993/cmdstan/model/goal_hier_space_expt123_mpi sample num_samples=10000 num_warmup=4000 thin=10 id=$1 data file=/QRISdata/Q0993/cmdstan/model/obs_"${COND}"_rdump_expt123.R random seed=12345 output file=/QRISdata/Q0993/cmdstan/model/samples_expt123_"${COND}"_$1.csv > /QRISdata/Q0993/cmdstan/model/samples_expt123_"${COND}"_$1.txt