#!/bin/bash
#SBATCH -p shared
#SBATCH --mem-per-cpu=2G
#SBATCH --job-name=broken_array_loop
#SBATCH -c 1
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --mail-type=ALL
#SBATCH --array=1-24%20

## Define loops and appropriately subset each variable for the array task ID
all_letter=(a b c)
letter=${all_letter[$(( $SLURM_ARRAY_TASK_ID / 8 % 3 ))]}

all_number=(1 2)
number=${all_number[$(( $SLURM_ARRAY_TASK_ID / 4 % 2 ))]}

all_combo=(one two three four)
combo=${all_combo[$(( $SLURM_ARRAY_TASK_ID / 1 % 4 ))]}

## Explicitly pipe script output to a log
log_path=logs/broken_array_loop_${letter}_${number}_${combo}_${SLURM_ARRAY_TASK_ID}.txt

{
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
Rscript -e "options(width = 120); print('${letter}'); print('${number}'); print('${combo}'); sessioninfo::session_info()"

echo "**** Job ends ****"
date

#   Make some tasks fail
if [[ $number -eq 1 ]]; then
    echo "Failed task"
    exit 1
fi

} > $log_path 2>&1

## This script was made using slurmjobs version 0.99.0
## available from http://research.libd.org/slurmjobs/

