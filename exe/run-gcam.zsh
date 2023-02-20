#!/bin/zsh
#SBATCH -A CITIES
#SBATCH -t 1440
#SBATCH -N 1
 
job=$SLURM_JOB_NAME
 
 
echo 'Library config:'
ldd ./gcam.exe
 
date
time ./gcam.exe -C$job -Llog_conf.xml
date