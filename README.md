
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmjobs

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/LieberInstitute/slurmjobs)](https://github.com/LieberInstitute/slurmjobs/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/LieberInstitute/slurmjobs)](https://github.com/LieberInstitute/slurmjobs/pulls)
[![R-CMD-check-bioc](https://github.com/LieberInstitute/slurmjobs/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/LieberInstitute/slurmjobs/actions/workflows/check-bioc.yml)
[![Codecov test
coverage](https://codecov.io/gh/LieberInstitute/slurmjobs/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/LieberInstitute/slurmjobs?branch=devel)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`slurmjobs` provides helper functions for interacting with
[SLURM](https://slurm.schedmd.com/)-managed high-performance-computing
environments from R. It includes functions for creating submittable jobs
(including array jobs), monitoring partitions, and extracting info about
running or complete jobs. For details, check out the [documentation
site](http://research.libd.org/slurmjobs/).

It was developed at [JHPCE](https://jhpce.jhu.edu/) with SLURM 22.05.9
in mind, but is intended to generalize to other clusters and newer SLURM
versions.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `slurmjobs` from
[GitHub](https://github.com/LieberInstitute/slurmjobs) with:

``` r
remotes::install_github("LieberInstitute/slurmjobs")
```

## Citation

Below is the citation output from using `citation('slurmjobs')` in R.
Please run this yourself to check for any updates on how to cite
**slurmjobs**.

``` r
print(citation("slurmjobs"), bibtex = TRUE)
#> To cite package 'slurmjobs' in publications use:
#> 
#>   LieberInstitute (2024). _slurmjobs: Helper Functions for SLURM Jobs_.
#>   doi:10.18129/B9.bioc.slurmjobs
#>   <https://doi.org/10.18129/B9.bioc.slurmjobs>,
#>   https://github.com/LieberInstitute/slurmjobs/slurmjobs - R package
#>   version 1.1.0, <http://www.bioconductor.org/packages/slurmjobs>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {slurmjobs: Helper Functions for SLURM Jobs},
#>     author = {{LieberInstitute}},
#>     year = {2024},
#>     url = {http://www.bioconductor.org/packages/slurmjobs},
#>     note = {https://github.com/LieberInstitute/slurmjobs/slurmjobs - R package version 1.1.0},
#>     doi = {10.18129/B9.bioc.slurmjobs},
#>   }
#> 
#>   LieberInstitute (2024). "slurmjobs: Helper Functions for SLURM Jobs."
#>   _bioRxiv_. doi:10.1101/TODO <https://doi.org/10.1101/TODO>,
#>   <https://www.biorxiv.org/content/10.1101/TODO>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {slurmjobs: Helper Functions for SLURM Jobs},
#>     author = {{LieberInstitute}},
#>     year = {2024},
#>     journal = {bioRxiv},
#>     doi = {10.1101/TODO},
#>     url = {https://www.biorxiv.org/content/10.1101/TODO},
#>   }
```

Please note that the `slurmjobs` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `slurmjobs` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.17/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation
  website](http://LieberInstitute.github.io/slurmjobs) is automatically
  updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.17/biocthis)*.
