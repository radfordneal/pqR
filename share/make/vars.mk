## Static (i.e., not determined by configure) Make variables to be
## shared *and* grepped from m4/R.m4 and tools/*-recommended.

R_PKGS_BASE = base tools utils grDevices graphics stats datasets methods grid splines stats4 compiler parallel
## Those which can be installed initially compiled (not base tools)
R_PKGS_BASE1 = utils grDevices graphics stats datasets methods grid splines stats4  parallel
## Those with standard R directories (not datasets, methods)
R_PKGS_BASE2 = base tools utils grDevices graphics stats grid splines stats4  compiler parallel

R_PKGS_RECOMMENDED =  MASS lattice Matrix nlme survival boot cluster codetools foreign KernSmooth rpart class nnet spatial mgcv
# there are dependencies in src/library/Recommended/Makefile*
# which this order respects
