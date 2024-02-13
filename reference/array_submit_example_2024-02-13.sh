#!/bin/bash
#SBATCH -p shared
#SBATCH --mem=10G
#SBATCH --job-name=array_submit_example_2024-02-13
#SBATCH -c 1
#SBATCH -t 1-00:00:00
#SBATCH -o logs/array_submit_example_2024-02-13.%a.txt
#SBATCH -e logs/array_submit_example_2024-02-13.%a.txt
#SBATCH --mail-type=ALL
#SBATCH --array=1-100%20

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
module load conda_R/4.3

## List current modules for reproducibility
module list

## Edit with your job command
Rscript -e "options(width = 120); sessioninfo::session_info()"

echo "**** Job ends ****"
date

## This script was made using slurmjobs version 1.2.0
## available from http://research.libd.org/slurmjobs/
