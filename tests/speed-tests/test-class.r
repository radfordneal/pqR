source("time.r")
 
test.name <- "class.S3"

a <- 5; class(a) <- "fred"
b <- 7; class(b) <- "mary"

addzero <- test.cmp (function (x) UseMethod("addzero"))

addzero.fred <- test.cmp (function (x) c(x,0))
addzero.mary <- test.cmp (function (x) c(0,x))

f <- test.cmp (function (n,a,b) 
{ for (i in 1:n) r <- list(addzero(a),addzero(b))
  r
})

sys.time(r <- f(300000,a,b))
print(r)

seq.fred <- function (x,y) c(x,0,y)

f <- test.cmp (function (n,x,y) 
{ for (i in 1:n) r <- seq(x,y)
  r
})

sys.time(r <- f(500000,a,9))
print(r)


test.name <- "class.S4"

imsqrt <- test.cmp (function (x) standardGeneric("imsqrt"))
fiddle <- test.cmp (function (x) standardGeneric("fiddle"))

setClass ("bert",representation(a="complex"),prototype(a=0+0i))
setClass ("gwen",representation(a="complex"),prototype(a=0+0i))

setMethod ("sqrt", "bert", test.cmp (function(x) sqrt(Re(x@a))))
setMethod ("imsqrt", "bert", test.cmp (function(x) sqrt(Im(x@a))))
setMethod ("imsqrt", "gwen", test.cmp (function(x) sqrt(Re(x@a)+Im(x@a))))
setMethod ("fiddle", "bert", test.cmp (function(x) new("gwen",a=x@a+1)))
setMethod ("fiddle", "gwen", test.cmp (function(x) new("bert",a=x@a-1i)))

z <- new("bert")
z@a <- 2+5i

w <- new("gwen")
w@a <- 2+5i

f <- test.cmp (function (n,z) 
{ for (i in 1:n) r <- sqrt(z); 
  r 
})

sys.time(r <- f(1000000,z))
print(r)

f <- test.cmp (function (n,w,z) 
{ for (i in 1:n) r <- 100*imsqrt(w) + imsqrt(z); 
  r 
})

sys.time(r <- f(30000,w,z))
print(r)

f <- test.cmp (function (n,w) { for (i in 1:n) w <- fiddle(w); w })

sys.time(r <- f(5000,w))
print(r)
