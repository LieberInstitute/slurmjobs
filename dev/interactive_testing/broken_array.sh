#!/bin/bash
#SBATCH -p shared
#SBATCH --mem=3G
#SBATCH --job-name=broken_array
#SBATCH -c 1
#SBATCH -o logs/broken_array.$SLURM_ARRAY_TASK_ID.txt
#SBATCH -e logs/broken_array.$SLURM_ARRAY_TASK_ID.txt
#SBATCH --array=1-10%10

echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${USER}"
echo "Job id: ${SLURM_JOB_ID}"
echo "Job name: ${SLURM_JOB_NAME}"
echo "Node name: ${SLURMD_NODENAME}"
echo "Task id: ${SLURM_ARRAY_TASK_ID}"

module load conda_R/4.3
Rscript broken_array.R

echo "**** Job ends ****"
date

## This script was made using slurmjobs version testing
## available from http://research.libd.org/slurmjobs/
