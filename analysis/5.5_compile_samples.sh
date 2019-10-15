#!/bin/bash

echo 1.5
echo ${1}

#Runs the R script that compiles the samples
Rscript /QRISdata/Q0993/processing/5.6_compile_samples.R --${1}
