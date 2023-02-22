#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=1000
#SBATCH --job-name=lhs
#SBATCH -A cities

# README -----------------------------------------------------------------------
#
# This script will launch SLURM tasks that will execute GCAM
# sbatch gcam_run_single.sh
#
# ------------------------------------------------------------------------------

# Load Modules
modules purge
ulimit -c unlimited
module load git/2.31.1
module load java/1.8.0_31
module load R/4.0.2
module load python/3.7.0
module load gcc/10.2.0
export RENV_PATHS_CACHE=/rcfs/projects/GCAM/renv
alias pyenv_activate='source /rcfs/projects/GCAM/pyenv3.7.0/bin/activate'

CONFIG='configuration_thailand_high.xml'

date
time ./gcam.exe -C$CONFIG -Llog_conf.xml
date

echo 'updating persmissions'
echo '.......................'
chmod -R 777 /rcfs/projects/gcims/projects/seasia/gcam6p0_seasia/output

echo 'completed'

