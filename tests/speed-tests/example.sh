#!/bin/bash

# Commands to produce data comparing the speed of the installed version of 
# R without compilation and with compilation on the simple test programs, 
# and then produce a plot of the results, in example.pdf.

set -e

export version=`R RHOME`

rm -f test-times

rep=2       do-prog
rep=2 cmp=T do-prog

R CMD BATCH example.r
