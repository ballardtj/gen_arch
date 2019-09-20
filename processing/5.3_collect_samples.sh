#!/bin/bash

# cd cmdstan

echo $1
echo $2

if [ "$2" -eq 1 ]
then
	COND="ap"
	SOURCE="obs"
	MODEL="hier"
fi

if [ "$2" -eq 2 ]
then
	COND="av"
	SOURCE="obs"
	MODEL="hier"
fi

if [ "$2" -eq 3 ]
then
	COND="ap"
	SOURCE="opt"
	MODEL="fixed"
fi

if [ "$2" -eq 4 ]
then
	COND="av"
	SOURCE="opt"
	MODEL="fixed"
fi

THREADS=-1

export STAN_NUM_THREADS=$THREADS

/QRISdata/Q0993/models/goal_"${MODEL}"_space_expt123_bndshape_mpi sample num_samples=10000 num_warmup=4000 thin=1 id=$1 data file=/QRISdata/Q0993/data/clean/"${SOURCE}"_"${COND}"_rdump_expt123.R random seed=12345 output file=/QRISdata/Q0993/processing/samples_expt123_"${SOURCE}"_"${COND}"_$1.csv > /QRISdata/Q0993/processing/samples_expt123_"${SOURCE}"_"${COND}"_$1.txt

# /QRISdata/Q0993/models/goal_"${MODEL}"_space_expt123_mpi sample num_samples=10000 num_warmup=4000 thin=1 id=$1 data file=/QRISdata/Q0993/data/clean/"${SOURCE}"_"${COND}"_rdump_expt123.R random seed=12345 output file=/QRISdata/Q0993/processing/samples_expt123_"${SOURCE}"_"${COND}"_$1.csv > /QRISdata/Q0993/processing/samples_expt123_"${SOURCE}"_"${COND}"_$1.txt
