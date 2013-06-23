source("time.r")
 
# Timing tests:

vl <- list (dd=1.4, cc=1.3, bb=1.2, aa=1.1)

pl <- pairlist (dd=1.4, cc=1.3, bb=1.2, aa=1.1)

ev <- new.env(parent=emptyenv())
ev$aa <- 1.1
ev$bb <- 1.2
ev$cc <- 1.3
ev$dd <- 1.4

n <- 1000000

test.name <- "dollar.exact-name"

f <- 
  test.cmp (function () {for (i in 1:n) s <- vl$aa+vl$bb+vl$cc+vl$dd; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- pl$aa+pl$bb+pl$cc+pl$dd; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- ev$aa+ev$bb+ev$cc+ev$dd; s})
sys.time(s<-f())
print(s)

test.name <- "dollar.exact-string"

f <- test.cmp (
  function () {for (i in 1:n) s <- vl$"aa"+vl$"bb"+vl$"cc"+vl$"dd"; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- pl$"aa"+pl$"bb"+pl$"cc"+pl$"dd"; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- ev$"aa"+ev$"bb"+ev$"cc"+ev$"dd"; s})
sys.time(s<-f())
print(s)

test.name <- "dollar.partial-name"

f <- test.cmp (
  function () {for (i in 1:n) s <- vl$a+vl$b+vl$c+vl$d; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- pl$a+pl$b+pl$c+pl$d; s})
sys.time(s<-f())
print(s)

test.name <- "dollar.partial-string"

f <- test.cmp (
  function () {for (i in 1:n) s <- vl$"a"+vl$"b"+vl$"c"+vl$"d"; s})
sys.time(s<-f())
print(s)

f <- test.cmp (
  function () {for (i in 1:n) s <- pl$"a"+pl$"b"+pl$"c"+pl$"d"; s})
sys.time(s<-f())
print(s)

# Correctness checks:

ab <- list(a=77,b=66)
class(ab) <- "george"
print(ab$b)

fr <- list(a=9,b=89)
class(fr) <- "fred"
"$.fred" <- function (x,e) paste(x[[1]],e)

print(fr$a)
print(fr$"b")

a <- 1
class(a) <- "mary"
"$.mary" <- function (x,e) deparse(substitute(x))
print(a$glub)

f <- function(...) c(...$"a", ...$b)
print(f(fr))
print(f(list(x=0,a=99,b=77)))

