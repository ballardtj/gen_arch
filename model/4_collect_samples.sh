#!/bin/bash

cd cmdstan

#1
#2

#model/goal sample num_samples=10000 num_warmup=4000 thin=10 id=$3 data file=model/$2_$1_rdump.R random seed=12345 output file=model/samples_$1_$2_$3.csv > model/output_$1_$2_$3.txt

model/goal sample num_samples=10 num_warmup=0 thin=1 id=$3 data file=model/$2_$1_rdump.R random seed=12345 output file=model/samples_$1_$2_$3.csv > model/output_$1_$2_$3.txt
