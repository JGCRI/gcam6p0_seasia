#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=300
#SBATCH --job-name=lhs
#SBATCH -A cities
#SBATCH --array=0-23

# README -----------------------------------------------------------------------
#
# This script will zip all databases in the output folder
# sbatch zip_db.sh
#
# ------------------------------------------------------------------------------

DATABASES=('malaysia_carbon_neutral_LUC_high' 'malaysia_carbon_neutral_LUC_low' 'malaysia_carbon_neutral_LUC_ref' 'malaysia_carbon_neutral_LUC_ambitious_city' 'malaysia_carbon_neutral_high' 'malaysia_carbon_neutral_ambitious_city' 'malaysia_carbon_neutral_low' 'malaysia_carbon_neutral_ref' 'malaysia_high' 'malaysia_ambitious_city' 'malaysia_low' 'malaysia_ref')

echo "zipping database ${DATABASES[$SLURM_ARRAY_TASK_ID]}"

date
time zip -r /rcfs/projects/gcims/projects/seasia/gcam6p0_seasia/output/${DATABASES[$SLURM_ARRAY_TASK_ID]} /rcfs/projects/gcims/projects/seasia/gcam6p0_seasia/output/${DATABASES[$SLURM_ARRAY_TASK_ID]}
date

echo 'completed'

