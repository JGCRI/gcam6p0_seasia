#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=300
#SBATCH --job-name=lhs
#SBATCH -A cities
#SBATCH --array=0-11

# README -----------------------------------------------------------------------
#
# This script will zip all databases in the output folder
# sbatch zip_db.sh
#
# ------------------------------------------------------------------------------

DATABASES=('thailand_carbon_neutral_LUC_high' 'thailand_carbon_neutral_LUC_low' 'thailand_carbon_neutral_LUC_ref' 'thailand_carbon_neutral_LUC_ambitious_city' 'thailand_carbon_neutral_high' 'thailand_carbon_neutral_ambitious_city' 'thailand_carbon_neutral_low' 'thailand_carbon_neutral_ref' 'thailand_high' 'thailand_ambitious_city' 'thailand_low' 'thailand_ref')

echo "zipping database ${DATABASES[$SLURM_ARRAY_TASK_ID]}"

date
cd /rcfs/projects/gcims/projects/seasia/gcam6p0_seasia/output
time zip -r ${DATABASES[$SLURM_ARRAY_TASK_ID]} ${DATABASES[$SLURM_ARRAY_TASK_ID]}
date

echo 'completed'

