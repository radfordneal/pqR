# SUPPORT FOR RUNNING TIMING TESTS.

library(methods)  # since not loaded by default with Rscript


# LOAD COMPILER.  But it's not necessarily used.

have.compiler <- getRversion() >= "2.13.0"
if (have.compiler) library(compiler)


# GET TEST REPETITION FACTOR.  Used to produce reasonable times on the
# platform being tested.  Default is 1, or the value of the "rep"
# environment variable, if it is set.  May be changed after this file
# is sourced.

test.rep <- as.numeric(Sys.getenv("rep",unset="1"))


# FILE TO APPEND TIMES TO.  May be changed after sourcing this file.

test.times.file <- "test-times"


# NAME AND NUMBER OF TEST BEING PERFORMED.  The name of the test should
# be changed to something suitable after sourcing this file, before each
# test is run.  The number is automatically incremented for each call of 
# the sys.time function below, and automatically reset to 1 when the 
# test name changes.

test.name <- test.name.prev <- "test"
test.number <- 0


# FUNCTION TO MAYBE COMPILE A FUNCTION.  Set to not compile (ie, to the
# identity function) by default, unless the "cmp" environment variable is
# "T", in which case it is set to cmpfun.  May be changed after this
# file is sourced.

do.not.cmpfun <- function (x) x

test.cmp <- if (have.compiler && Sys.getenv("cmp",unset="F")=="T")
              cmpfun else do.not.cmpfun


# FIND CONFIGURATION.  Taken from the conf environment variable or the 
# last component of the R.home path.

this.R.config <- Sys.getenv("conf")
if (this.R.config=="") this.R.config <- sub(".*/","",R.home())


# FIND NUMBER OF HELPERS.  Taken from the R_HELPERS environment variable.

n.helpers <- Sys.getenv("R_HELPERS",unset="0")


# FUNCTION TO OBTAIN, STORE, AND PRINT COMPUTE TIMES FOR AN EXPRESSION.
#
# NOTE!  Times printed are in the order elapsed, user, system.
# Does a garbage colllection beforehand for improved timing
# consistency.  The expression is repeated test.rep times, and the
# time then divided by test.rep.

sys.time <- function (ex)
{ 
  atm <- system.time(
           for (i in 1:test.rep) eval.parent(substitute(ex)),
         gcFirst=TRUE)
  se <- atm["elapsed"] / test.rep
  su <- atm["user.self"] / test.rep
  ss <- atm["sys.self"] / test.rep

  if (test.name!=test.name.prev)
  { test.number <<- 0
    test.name.prev <<- test.name
    cat("\n----",test.name,"\n\n")
  }
  test.number <<- test.number + 1

  res1 <- paste (format(round(se,3),nsmall=3,width=8),
                 format(round(su,3),nsmall=3,width=8),
                 format(round(ss,3),nsmall=3,width=8))
  res2 <- paste (test.name, test.number, this.R.config, n.helpers,
                 if (identical(test.cmp,do.not.cmpfun)) "F" else "T", 
                 test.rep)

  a <- file("test-times","a")
  writeLines (paste (substr(Sys.time(),1,19), res1, res2), a)
  close(a)

  cat ("Time:", res1, " | ", res2, "\n")
}
