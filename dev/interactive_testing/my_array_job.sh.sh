#!/bin/bash
#SBATCH -p shared
#SBATCH --mem-per-cpu=10G
#SBATCH --job-name=my_array_job.sh
#SBATCH -c 1
#SBATCH -o logs/my_array_job.sh.%a.txt
#SBATCH -e logs/my_array_job.sh.%a.txt
#SBATCH --mail-type=ALL
#SBATCH --array=1-10%5

set -e

echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${USER}"
echo "Job id: ${SLURM_JOB_ID}"
echo "Job name: ${SLURM_JOB_NAME}"
echo "Node name: ${SLURMD_NODENAME}"
echo "Task id: ${SLURM_ARRAY_TASK_ID}"

## Load the R module
module load conda_R

## List current modules for reproducibility
module list

## Edit with your job command
Rscript -e "options(width = 120); sessioninfo::session_info()"

echo "**** Job ends ****"
date

## This script was made using slurmjobs version 0.99.0
## available from http://research.libd.org/slurmjobs/
