library(here)
library(dplyr)
library(stringr)
source(here('R', 'job_info.R'))

#   Randomly grab 100 jobs running now on the 'shared' partition
job_df = job_info(user = NULL) |>
    sample_n(size = 100) |>
    arrange(job_id)

#   A vector whose values are anonymous usernames and whose names are the
#   original usernames
user_map = paste0('user', 1:length(unique(job_df$user)))
names(user_map) = unique(job_df$user)

#   Similarly for job names, though we'll keep the generic name for interactive
#   jobs ('bash')
name_map = paste0('my_job_', 1:length(unique(job_df$name)))
names(name_map) = unique(job_df$name)
name_map['bash'] = 'bash'

#   Anonymize username and job name
job_df = job_df  |>
    mutate(
        user = user_map[user],
        name = name_map[name]
    )

saveRDS(job_df, here('inst', 'extdata', 'job_info_df.rds'))
