task_id = Sys.getenv('SLURM_ARRAY_TASK_ID')

bad_ids = c(1, 4, 7:9)

if (task_id %in% bad_ids) {
    stop("This task failed")
}

print("Succeeded")
