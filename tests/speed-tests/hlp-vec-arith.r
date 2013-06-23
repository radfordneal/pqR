source("time.r")
 
# Assess degree of parallelism using helper threads for arithmetic on vectors.

setup <- function (n)
{
  va <<- seq(0,1,length=n)
  vb <<- seq(1,2,length=n)
  vc <<- seq(2,3,length=n)
}

# Comparisons below should force computations to finish.

tstfunc1 <- test.cmp (function (n)
{
  for (i in 1:n) 
  { x <- va^2 + vb/vc + (va+vc)/vb + vb/va - 1 + (vc*va+vb*vc)/(-va+vb)
    if (x[length(x)]<0) s <<- 1
  }
  x
})

tstfunc2 <- test.cmp (function (n)
{
  for (i in 1:n) 
  { x <- tanh(vb) + exp(va+vb) + abs(1-vc^3)
    if (x[length(x)]<0) s <<- 1
  }
  x
})


test.name <- "hlp-vec-arith.small1"

setup(1000)
n <- 30000

options(helpers_disable=TRUE)
sys.time(sd <- tstfunc1(n))

options(helpers_disable=FALSE)
sys.time(se <- tstfunc1(n))

print(identical(sd,se))

test.name <- "hlp-vec-arith.small2"

options(helpers_disable=TRUE)
sys.time(sd <- tstfunc2(n/3))

options(helpers_disable=FALSE)
sys.time(se <- tstfunc2(n/3))

print(identical(sd,se))


test.name <- "hlp-vec-arith.large1"

setup(10000)
n <- 3000

options(helpers_disable=TRUE)
sys.time(ld <- tstfunc1(n))

options(helpers_disable=FALSE)
sys.time(le <- tstfunc1(n))

print(identical(ld,le))

test.name <- "hlp-vec-arith.large2"

options(helpers_disable=TRUE)
sys.time(ld <- tstfunc2(n/3))

options(helpers_disable=FALSE)
sys.time(le <- tstfunc2(n/3))

print(identical(ld,le))


test.name <- "hlp-vec-arith.huge1"

setup(100000)
n <- 300

options(helpers_disable=TRUE)
sys.time(hd <- tstfunc1(n))

options(helpers_disable=FALSE)
sys.time(he <- tstfunc1(n))

print(identical(hd,he))

test.name <- "hlp-vec-arith.huge2"

options(helpers_disable=TRUE)
sys.time(hd <- tstfunc2(n/3))

options(helpers_disable=FALSE)
sys.time(he <- tstfunc2(n/3))

print(identical(hd,he))
