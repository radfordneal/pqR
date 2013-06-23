# Example of data plotting.  Uses the data produced by example.sh.

source("analyse.r")
read.times()

pdf("example.pdf",height=11,width=8.5)

plot.ratios (sel (cmp=FALSE), sel(cmp=TRUE))
title ("R interpreted / R compiled")

dev.off()
