#   This script runs as part of an array job, and only specific task IDs are
#   intended to fail (nonzero exit status)

#   SLURM doesn't seem to accurately report memory for sufficiently short jobs
wait_time = 60

task_id = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

bad_ids = c(1, 4, 7:9)

if (task_id %in% bad_ids) {
    Sys.sleep(wait_time)
    stop("This task failed")
}

#   Do a dummy test just to use RAM
a = 1:(10000000 * task_id)
b = a * 2
gc()
Sys.sleep(wait_time)

print("Succeeded")
