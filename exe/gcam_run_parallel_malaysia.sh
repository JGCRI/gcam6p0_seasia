#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=3000
#SBATCH --job-name=lhs
#SBATCH -A cities
#SBATCH --array=0-7

# README -----------------------------------------------------------------------
#
# This script will launch SLURM tasks that will execute GCAM
# sbatch gcam_run_parallel.sh
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

# Files to run in parallel
FILES=('configuration_malaysia_carbon_neutral_ambitious_city.xml' 'configuration_malaysia_ambitious_city.xml' 'configuration_malaysia_carbon_neutral_high.xml' 'configuration_malaysia_high.xml' 'configuration_malaysia_carbon_neutral_low.xml' 'configuration_malaysia_low.xml' 'configuration_malaysia_carbon_neutral_ref.xml' 'configuration_malaysia_ref.xml')

CONFIG=${FILES[$SLURM_ARRAY_TASK_ID]}

echo "Run ID: ${SLURM_ARRAY_TASK_ID}"
echo "FILES:  ${FILES[@]}"
echo "CONFIG:  ${CONFIG}"

date
time ./gcam.exe -C$CONFIG -Llog_conf.xml
date

echo 'completed'

