# Test gradient computations.
#
# Added for pqR, 2018 Radford M. Neal.


# Check consistency of simple math derivatives using track_gradient with 'D'

x <- 0.32739

print (identical (track_gradient (x) gradient_of (sqrt(x)),
                  eval (D (quote (sqrt(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (exp(x)),
                  eval (D (quote (exp(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (expm1(x)),
                  eval (D (quote (expm1(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (log1p(x)),
                  eval (D (quote (log1p(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (log(x)),
                  eval (D (quote (log(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (cos(x)),
                  eval (D (quote (cos(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (sin(x)),
                  eval (D (quote (sin(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (tan(x)),
                  eval (D (quote (tan(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (acos(x)),
                  eval (D (quote (acos(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (asin(x)),
                  eval (D (quote (asin(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (cosh(x)),
                  eval (D (quote (cosh(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (sinh(x)),
                  eval (D (quote (sinh(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (tanh(x)),
                  eval (D (quote (tanh(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (lgamma(x)),
                  eval (D (quote (lgamma(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (gamma(x)),
                  eval (D (quote (gamma(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (digamma(x)),
                  eval (D (quote (digamma(x)), "x")) ))

print (identical (track_gradient (x) gradient_of (trigamma(x)),
                  eval (D (quote (trigamma (x)), "x")) ))

print (identical (track_gradient (x) gradient_of (+x),
                  eval (D (quote (+x), "x")) ))

print (identical (track_gradient (x) gradient_of (-x),
                  eval (D (quote (-x), "x")) ))

print (identical (track_gradient (x) gradient_of (3.1+x),
                  eval (D (quote (3.1+x), "x")) ))

print (identical (track_gradient (x) gradient_of (3.1-x),
                  eval (D (quote (3.1-x), "x")) ))

print (identical (track_gradient (x) gradient_of (3.1*x),
                  eval (D (quote (3.1*x), "x")) ))

print (identical (track_gradient (x) gradient_of (3.1/x),
                  eval (D (quote (3.1/x), "x")) ))

print (identical (track_gradient (x) gradient_of (3.1^x),
                  eval (D (quote (3.1^x), "x")) ))

print (identical (track_gradient (x) gradient_of (x+3.1),
                  eval (D (quote (x+3.1), "x")) ))

print (identical (track_gradient (x) gradient_of (x-3.1),
                  eval (D (quote (x-3.1), "x")) ))

print (identical (track_gradient (x) gradient_of (x*3.1),
                  eval (D (quote (x*3.1), "x")) ))

print (identical (track_gradient (x) gradient_of (x/3.1),
                  eval (D (quote (x/3.1), "x")) ))

print (identical (track_gradient (x) gradient_of (x^3.1),
                  eval (D (quote (x^3.1), "x")) ))

print (identical (track_gradient (x) gradient_of (x+x),
                  eval (D (quote (x+x), "x")) ))

print (identical (track_gradient (x) gradient_of (x-x),
                  eval (D (quote (x-x), "x")) ))

print (identical (track_gradient (x) gradient_of (x*x),
                  eval (D (quote (x*x), "x")) ))

print (identical (track_gradient (x) gradient_of (x/x),
                  eval (D (quote (x/x), "x")) ))

print (identical (round (track_gradient (x) gradient_of (x^x), 14),
                  round (eval (D (quote (x^x), "x")), 14) ))


# Check gradient computations passing through mathematical function dispatch.

a <- 256; class(a) <- "fred"
cos.fred <- function (x) tan(x)
log2.fred <- function (x) tan(tan(x))
Math.fred <- function (x,...) {
    if (.Generic=="log") sqrt(x)
    else if (.Generic=="log10") sqrt(sqrt(x))
    else if (.Generic=="exp") sqrt(sqrt(sqrt(x)))
    else NextMethod()
}

print(cos(a))
print(log2(a))
print(log(a))
print(log10(a))
print(exp(a))
print(sin(a))

print (with_gradient (a) cos(a))
print (with_gradient (a) log2(a))
print (with_gradient (a) log(a))
print (with_gradient (a) log10(a))
print (with_gradient (a) exp(a))
print (with_gradient (a) sin(a))


# Check tracking of gradients through S3 methods.

fuddle <- function (x,y) UseMethod("fuddle")
fuddle.default <- function (x,y) x^2+y^3
fuddle.mary <- function (x,y) sin(x) + log(y)
fuddle.bert <- function (x,y) NextMethod("fuddle")

a <- 256; class(a) <- "mary"
b <- 200; class(b) <- "bert"

with_gradient (a=256,b=200) fuddle(a,b)
with_gradient (a,b) fuddle(a,b)
with_gradient (a,b) fuddle(b,a)

biffle <- function (x) UseMethod("biffle")
biffle.mary <- function (x) NextMethod("biffle",x,x^2,x^3)
biffle.bert <- function (x,y,z) sin(x)+cos(y)+exp(-sqrt(z)/2000)

b <- 200; class(b) <- c("mary","bert")
biffle(b)
with_gradient (b) biffle(b)


# Check tracking of gradients through S4 methods.  Not currently implemented,
# so this is disabled.

if (FALSE) {

setGeneric ("fiddler", function (x,y) x^2+y^3)
fiddler(2,3)
with_gradient (x=2,y=3) fiddler(x,y)

}


# Check consistency of results between with_gradient and numericDeriv.

x <- 0.32739
x1 <- 0.47718; x2 <- 0.89472; x3 <- 0.67325
y1 <- -0.3721; y2 <- -0.8131; y3 <- 1.22213
z1 <- 11.4319; z2 <- 13.1133; z3 <- 6.68901
i1 <- 3

bindgrads <- function (r1,r2) 
    cbind (rbind(r1,r2), rbind(attr(r1,"gradient"),unlist(attr(r2,"gradient"))))

test1 <- function (fun,...)
    print (bindgrads (numericDeriv(quote(fun(x,...)),"x"),
                      with_gradient (x) fun(x,...)))
test1p1 <- function (fun,...)
    print (bindgrads (numericDeriv(quote(fun(x+1,...)),"x"),
                      with_gradient (x) fun(x+1,...)))

test1r <- function (fun,...) {
    f <- function (x) { set.seed(179); fun(1,x,...) }
    print (bindgrads (numericDeriv(quote(f(x)),"x"),
                      with_gradient (x) f(x)))
}

test2 <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x1"),
                      with_gradient (x1) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x2"),
                      with_gradient (x2) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),c("x1","x2")),
                      with_gradient (x1,x2) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),c("x1","x2")),
      { r <- with_gradient (x1) { s <- with_gradient (x2) fun(x1,x2,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test2z <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),"z1"),
                      with_gradient (z1) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),"z2"),
                      with_gradient (z2) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),c("z1","z2")),
                      with_gradient (z1,z2) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),c("z1","z2")),
      { r <- with_gradient (z1) { s <- with_gradient (z2) fun(z1,z2,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test2i <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(i1,x2,...)),"x2"),
                      with_gradient (x2) fun(i1,x2,...)))
}

test2r <- function (fun,...) {
    f <- function (x1,x2) { set.seed(179); fun(1,x1,x2,...) }
    print (bindgrads (numericDeriv(quote(f(x1,x2)),c("x1","x2")),
                      with_gradient (x1,x2) f(x1,x2)))
}

test3 <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x1"),
                      with_gradient (x1) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x2"),
                      with_gradient (x2) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x3"),
                      with_gradient (x3) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),c("x1","x2","x3")),
                      with_gradient (x1,x2,x3) fun(x1,x2,x3,...)))
}

test3y <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y1"),
                      with_gradient (y1) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y2"),
                      with_gradient (y2) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y3"),
                      with_gradient (y3) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),c("y1","y2","y3")),
                      with_gradient (y1,y2,y3) fun(y1,y2,y3,...)))
}

test3z <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z1"),
                      with_gradient (z1) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z2"),
                      with_gradient (z2) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z3"),
                      with_gradient (z3) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),c("z1","z2","z3")),
                      with_gradient (z1,z2,z3) fun(z1,z2,z3,...)))
}

test1(abs)

test1(sqrt)

test1(exp)
test1(expm1)

test1(log1p)
test1(log)
test1(log2)
test1(log10)

test1(cos)
test1(sin)
test1(tan)

test1(acos)
test1(asin)
test1(atan)

test1(cosh)
test1(sinh)
test1(tanh)

test1p1(acosh)
test1(asinh)
test1(atanh)

test1(gamma)
test1(lgamma)
test1(digamma)
test1(trigamma)

test2(atan2)
test2z(atan2)

test2(beta)
test2z(beta)

test2(lbeta)
test2z(lbeta)

test2(dexp)
test2(dexp,log=TRUE)
test1r(rexp)

test2i(dgeom)
test2i(dgeom,log=TRUE)

test2i(dpois)
test2i(dpois,log=TRUE)

test3(pcauchy)
test3(pcauchy,log=TRUE)
test3(pcauchy,lower=FALSE)
test3(pcauchy,log=TRUE,lower=FALSE)
test3z(pcauchy)
test3z(pcauchy,log=TRUE)
test3z(pcauchy,lower=FALSE)
test3z(pcauchy,log=TRUE,lower=FALSE)

test3(qcauchy)
test3y(qcauchy,log=TRUE)
test3(qcauchy,lower=FALSE)
test3y(qcauchy,log=TRUE,lower=FALSE)

test2r(rcauchy)

test2r(rlnorm)

test3(dlogis)
test3(dlogis,log=TRUE)
test3z(dlogis)
test3z(dlogis,log=TRUE)

test3(plogis)
test3(plogis,log=TRUE)
test3(plogis,lower=FALSE)
test3(plogis,log=TRUE,lower=FALSE)
test3z(plogis)
test3z(plogis,log=TRUE)
test3z(plogis,lower=FALSE)
test3z(plogis,log=TRUE,lower=FALSE)

test3(qlogis)
test3y(qlogis,log=TRUE)
test3(qlogis,lower=FALSE)
test3y(qlogis,log=TRUE,lower=FALSE)

test2r(rlogis)

test3(dnorm)
test3(dnorm,log=TRUE)
test3z(dnorm)
test3z(dnorm,log=TRUE)

test3(pnorm)
test3(pnorm,log=TRUE)
test3(pnorm,lower=FALSE)
test3(pnorm,log=TRUE,lower=FALSE)
test3z(pnorm)
test3z(pnorm,log=TRUE)
test3z(pnorm,lower=FALSE)
test3z(pnorm,log=TRUE,lower=FALSE)

test3(qnorm)
test3y(qnorm,log=TRUE)
test3(qnorm,lower=FALSE)
test3y(qnorm,log=TRUE,lower=FALSE)

test2r(rnorm)

test2r(runif)

test2r(rweibull)
