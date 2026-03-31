# slurmjobs 1.3.0

NEW FEATURES

* New function `renumber()` renames a set of scripts along with their associated logs and content

BUG FIXES

* `job_loop()` now respects absolute paths to the scripts to create

# slurmjobs 1.2.5

BUG FIXES

* Fix currently running jobs crashing `job_report()`

# slurmjobs 1.2.4

BUG FIXES

* Recognize/ handle `OUT_OF_MEMORY` job status, fixing a fatal error in `job_report()`

# slurmjobs 1.2.3

BUG FIXES

* Fix submission of jobs, modification of the appropriate shell script, and finding of log files when using `array_submit()` with an absolute path to the `name` parameter

# slurmjobs 1.2.2

* Use the `HOSTNAME` instead of `SLURMD_NODENAME` environment variable to determine host running a SLURM job

# slurmjobs 1.2.1

BUG FIXES

* Recognize cancelled jobs as completed, allowing `job_report()` to continue without errors when attempting to retrieve memory info for finished jobs

# slurmjobs 1.2.0

* `job_single()` and `job_loop()` accept a new parameter, `time_limit`, which default to 1 day

# slurmjobs 1.1.0

NEW FEATURES

* `job_single()`, `job_loop()`, and `array_submit()` now may accept a name, relative path, or absolute path as the `name` argument. This means the working directory is no longer relevant in successfully calling these functions
* `job_info()` and `job_report()` now also return a `wallclock_time` column, giving the elapsed wallclock time of jobs as `difftime`s

# slurmjobs 1.0.0

NEW FEATURES

* Two additional monitoring functions: `job_report()` and `partition_info()`
* Two new job-submission functions: `array_submit()` and `job_loop()`
* New data included as part of the package: `job_info_df` and `partition_df`. Example outputs of `job_info()` and `partition_info()`

SIGNIFICANT USER-VISIBLE CHANGES

* Memory for `job_single()` and `job_loop()` is now specified in total (i.e. through `--mem`) instead of per core (i.e. through `--mem-per-cpu`)
* Complete vignette and documentation

# slurmjobs 0.99.0

NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.
* Added core files (such as `DESCRIPTION`, `NAMESPACE`, etc) to make `slurmjobs` a Bioconductor-style package

SIGNIFICANT USER-VISIBLE CHANGES

* Experimental `job_info()` function
* Experimental `job_single()` function
