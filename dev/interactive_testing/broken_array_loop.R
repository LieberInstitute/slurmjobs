library(getopt)
library(sessioninfo)

# Import command-line parameters
spec <- matrix(
    c(
        c("letter", "number", "combo"),
        c("l", "n", "c"),
        rep("1", 3),
        rep("character", 3),
        rep("Add variable description here", 3)
    ),
    ncol = 5
)
opt <- getopt(spec)

print("Using the following parameters:")
print(opt)

session_info()

## This script was made using slurmjobs version 0.99.0
## available from http://research.libd.org/slurmjobs/
