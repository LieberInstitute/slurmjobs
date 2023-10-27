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
