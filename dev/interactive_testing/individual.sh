#!/bin/bash
#SBATCH -p shared
#SBATCH --mem=3G
#SBATCH --job-name=individual
#SBATCH -c 1
#SBATCH -o logs/individual.txt
#SBATCH -e logs/individual.txt

#   A minimal example of a non-array batch job

echo "**** Job starts ****"
date

echo "**** JHPCE info ****"
echo "User: ${USER}"
echo "Job id: ${SLURM_JOB_ID}"
echo "Job name: ${SLURM_JOB_NAME}"
echo "Node name: ${SLURMD_NODENAME}"
echo "Task id: ${SLURM_ARRAY_TASK_ID}"
echo "**** Job ends ****"
date

## This script was made using slurmjobs version testing
## available from http://research.libd.org/slurmjobs/
