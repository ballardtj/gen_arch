#!/bin/bash

cd cmdstan

#1
#2

model/goal sample num_samples=10000 num_warmup=4000 thin=10 id=$2 data file=model/obs_$1_rdump.R random seed=12345 output file=model/samples_$1_$2.csv > model/output_$1_$2.txt

