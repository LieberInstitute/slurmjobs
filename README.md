
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmjobs

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/LieberInstitute/slurmjobs)](https://github.com/LieberInstitute/slurmjobs/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/LieberInstitute/slurmjobs)](https://github.com/LieberInstitute/slurmjobs/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Bioc release
status](http://www.bioconductor.org/shields/build/release/bioc/slurmjobs.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/slurmjobs)
[![Bioc devel
status](http://www.bioconductor.org/shields/build/devel/bioc/slurmjobs.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/slurmjobs)
[![Bioc downloads
rank](https://bioconductor.org/shields/downloads/release/slurmjobs.svg)](http://bioconductor.org/packages/stats/bioc/slurmjobs/)
[![Bioc
support](https://bioconductor.org/shields/posts/slurmjobs.svg)](https://support.bioconductor.org/tag/slurmjobs)
[![Bioc
history](https://bioconductor.org/shields/years-in-bioc/slurmjobs.svg)](https://bioconductor.org/packages/release/bioc/html/slurmjobs.html#since)
[![Bioc last
commit](https://bioconductor.org/shields/lastcommit/devel/bioc/slurmjobs.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/slurmjobs/)
[![Bioc
dependencies](https://bioconductor.org/shields/dependencies/release/slurmjobs.svg)](https://bioconductor.org/packages/release/bioc/html/slurmjobs.html#since)
[![R-CMD-check-bioc](https://github.com/LieberInstitute/slurmjobs/actions/workflows/R-CMD-check-bioc.yaml/badge.svg)](https://github.com/LieberInstitute/slurmjobs/actions/workflows/R-CMD-check-bioc.yaml)
[![Codecov test
coverage](https://codecov.io/gh/LieberInstitute/slurmjobs/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/LieberInstitute/slurmjobs?branch=devel)
<!-- badges: end -->

The goal of `slurmjobs` is to …

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `slurmjobs` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("slurmjobs")
```

And the development version from
[GitHub](https://github.com/LieberInstitute/slurmjobs) with:

``` r
BiocManager::install("LieberInstitute/slurmjobs")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("slurmjobs")
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!

## Citation

Below is the citation output from using `citation('slurmjobs')` in R.
Please run this yourself to check for any updates on how to cite
**slurmjobs**.

``` r
print(citation("slurmjobs"), bibtex = TRUE)
#> To cite package 'slurmjobs' in publications use:
#> 
#>   LieberInstitute (2023). _slurmjobs: Helper Functions for SLURM Jobs_.
#>   doi:10.18129/B9.bioc.slurmjobs
#>   <https://doi.org/10.18129/B9.bioc.slurmjobs>,
#>   https://github.com/LieberInstitute/slurmjobs/slurmjobs - R package
#>   version 0.99.0, <http://www.bioconductor.org/packages/slurmjobs>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {slurmjobs: Helper Functions for SLURM Jobs},
#>     author = {{LieberInstitute}},
#>     year = {2023},
#>     url = {http://www.bioconductor.org/packages/slurmjobs},
#>     note = {https://github.com/LieberInstitute/slurmjobs/slurmjobs - R package version 0.99.0},
#>     doi = {10.18129/B9.bioc.slurmjobs},
#>   }
#> 
#>   LieberInstitute (2023). "slurmjobs: Helper Functions for SLURM Jobs."
#>   _bioRxiv_. doi:10.1101/TODO <https://doi.org/10.1101/TODO>,
#>   <https://www.biorxiv.org/content/10.1101/TODO>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {slurmjobs: Helper Functions for SLURM Jobs},
#>     author = {{LieberInstitute}},
#>     year = {2023},
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
