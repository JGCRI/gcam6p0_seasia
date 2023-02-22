#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=3000
#SBATCH --job-name=lhs
#SBATCH -A cities
#SBATCH --array=0-11

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
FILES=('configuration_thailand_carbon_neutral_LUC_high.xml' 'configuration_thailand_carbon_neutral_LUC_low.xml' 'configuration_thailand_carbon_neutral_LUC_ref.xml' 'configuration_thailand_carbon_neutral_LUC_ambitious_city.xml' 'configuration_thailand_carbon_neutral_high.xml' 'configuration_thailand_carbon_neutral_ambitious_city.xml' 'configuration_thailand_carbon_neutral_low.xml' 'configuration_thailand_carbon_neutral_ref.xml' 'configuration_thailand_high.xml' 'configuration_thailand_ambitious_city.xml' 'configuration_thailand_low.xml' 'configuration_thailand_ref.xml')
DATABASES=('thailand_carbon_neutral_LUC_high' 'thailand_carbon_neutral_LUC_low' 'thailand_carbon_neutral_LUC_ref' 'thailand_carbon_neutral_LUC_ambitious_city' 'thailand_carbon_neutral_high' 'thailand_carbon_neutral_ambitious_city' 'thailand_carbon_neutral_low' 'thailand_carbon_neutral_ref' 'thailand_high' 'thailand_ambitious_city' 'thailand_low' 'thailand_ref')

CONFIG=${FILES[$SLURM_ARRAY_TASK_ID]}
DATABASE=${DATABASES[$SLURM_ARRAY_TASK_ID]}

echo "Run ID: ${SLURM_ARRAY_TASK_ID}"
echo "FILES:  ${FILES[@]}"
echo "CONFIG:  ${CONFIG}"

date
time ./gcam.exe -C$CONFIG -Llog_conf.xml
date

echo 'updating persmissions'
echo '.......................'
chmod -R 777 /rcfs/projects/gcims/projects/seasia/gcam6p0_seasia/output/${DATABASE}

echo 'completed'

