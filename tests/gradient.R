# Test gradient computations.
#
# Added for pqR, 2019 Radford M. Neal.


# Check consistency of simple math derivatives using track gradient with 'D'

x <- 0.32739

print (identical (track gradient (x) gradient_of (sqrt(x)),
                  eval (D (quote (sqrt(x)), "x")) ))

print (identical (track gradient (x) gradient_of (exp(x)),
                  eval (D (quote (exp(x)), "x")) ))

print (identical (track gradient (x) gradient_of (expm1(x)),
                  eval (D (quote (expm1(x)), "x")) ))

print (identical (track gradient (x) gradient_of (log1p(x)),
                  eval (D (quote (log1p(x)), "x")) ))

print (identical (track gradient (x) gradient_of (log(x)),
                  eval (D (quote (log(x)), "x")) ))

print (identical (track gradient (x) gradient_of (cos(x)),
                  eval (D (quote (cos(x)), "x")) ))

print (identical (track gradient (x) gradient_of (sin(x)),
                  eval (D (quote (sin(x)), "x")) ))

print (identical (track gradient (x) gradient_of (tan(x)),
                  eval (D (quote (tan(x)), "x")) ))

print (identical (track gradient (x) gradient_of (acos(x)),
                  eval (D (quote (acos(x)), "x")) ))

print (identical (track gradient (x) gradient_of (asin(x)),
                  eval (D (quote (asin(x)), "x")) ))

print (identical (track gradient (x) gradient_of (cosh(x)),
                  eval (D (quote (cosh(x)), "x")) ))

print (identical (track gradient (x) gradient_of (sinh(x)),
                  eval (D (quote (sinh(x)), "x")) ))

print (identical (track gradient (x) gradient_of (tanh(x)),
                  eval (D (quote (tanh(x)), "x")) ))

print (identical (track gradient (x) gradient_of (lgamma(x)),
                  eval (D (quote (lgamma(x)), "x")) ))

print (identical (track gradient (x) gradient_of (gamma(x)),
                  eval (D (quote (gamma(x)), "x")) ))

print (identical (track gradient (x) gradient_of (digamma(x)),
                  eval (D (quote (digamma(x)), "x")) ))

print (identical (track gradient (x) gradient_of (trigamma(x)),
                  eval (D (quote (trigamma (x)), "x")) ))

print (identical (track gradient (x) gradient_of (+x),
                  eval (D (quote (+x), "x")) ))

print (identical (track gradient (x) gradient_of (-x),
                  eval (D (quote (-x), "x")) ))

print (identical (track gradient (x) gradient_of (3.1+x),
                  eval (D (quote (3.1+x), "x")) ))

print (identical (track gradient (x) gradient_of (3.1-x),
                  eval (D (quote (3.1-x), "x")) ))

print (identical (track gradient (x) gradient_of (3.1*x),
                  eval (D (quote (3.1*x), "x")) ))

print (identical (track gradient (x) gradient_of (3.1/x),
                  eval (D (quote (3.1/x), "x")) ))

print (identical (track gradient (x) gradient_of (3.1^x),
                  eval (D (quote (3.1^x), "x")) ))

print (identical (track gradient (x) gradient_of (x+3.1),
                  eval (D (quote (x+3.1), "x")) ))

print (identical (track gradient (x) gradient_of (x-3.1),
                  eval (D (quote (x-3.1), "x")) ))

print (identical (track gradient (x) gradient_of (x*3.1),
                  eval (D (quote (x*3.1), "x")) ))

print (identical (track gradient (x) gradient_of (x/3.1),
                  eval (D (quote (x/3.1), "x")) ))

print (identical (track gradient (x) gradient_of (x^3.1),
                  eval (D (quote (x^3.1), "x")) ))

print (identical (track gradient (x) gradient_of (x+x),
                  eval (D (quote (x+x), "x")) ))

print (identical (track gradient (x) gradient_of (x-x),
                  eval (D (quote (x-x), "x")) ))

print (identical (track gradient (x) gradient_of (x*x),
                  eval (D (quote (x*x), "x")) ))

print (identical (track gradient (x) gradient_of (x/x),
                  eval (D (quote (x/x), "x")) ))

print (identical (round (track gradient (x) gradient_of (x^x), 14),
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

print (with gradient (a) cos(a))
print (with gradient (a) log2(a))
print (with gradient (a) log(a))
print (with gradient (a) log10(a))
print (with gradient (a) exp(a))
print (with gradient (a) sin(a))


# Check tracking of gradients through S3 methods.

fuddle <- function (x,y) UseMethod("fuddle")
fuddle.default <- function (x,y) x^2+y^3
fuddle.mary <- function (x,y) sin(x) + log(y)
fuddle.bert <- function (x,y) NextMethod("fuddle")

a <- 256; class(a) <- "mary"
b <- 200; class(b) <- "bert"

with gradient (a=256,b=200) fuddle(a,b)
with gradient (a,b) fuddle(a,b)
with gradient (a,b) fuddle(b,a)

biffle <- function (x) UseMethod("biffle")
biffle.mary <- function (x) NextMethod("biffle",x,x^2,x^3)
biffle.bert <- function (x,y,z) sin(x)+cos(y)+exp(-sqrt(z)/2000)

b <- 200; class(b) <- c("mary","bert")
biffle(b)
with gradient (b) biffle(b)

`[[.xyzzy` <- 
  function (x,i) if (i==0) x$x else x$y
`[[<-.xyzzy` <- 
  function (x,i,value) { if (i==0) x$x<-value else x$y<-value; x }

with gradient (a=7) { L <- list(x=1,y=2); class(L)<-"xyzzy"; L$x<-a^2; L[[0]] }
with gradient (a=7) { L <- list(x=1,y=2); class(L)<-"xyzzy"; L[[0]]<-a^2; L }

rm(`[[.xyzzy`)
rm(`[[<-.xyzzy`)

`$.xyzzy` <- 
  function (x,i) if (i=="x") x[[1]] else x[[2]]
`$<-.xyzzy` <- 
  function (x,i,value) { if (i=="x") x[[1]]<-value else x[[2]]<-value; x }

with gradient (a=7) { L <- list(x=1,y=2); class(L)<-"xyzzy"; L[[1]]<-a^2; L$x }
with gradient (a=7) { L <- list(x=1,y=2); class(L)<-"xyzzy"; L$x<-a^2; L }

rm(`$.xyzzy`)
rm(`$<-.xyzzy`)


# Check tracking of gradients through S4 methods.  Not currently implemented,
# so this is disabled.

if (FALSE) {

setGeneric ("fiddler", function (x,y) x^2+y^3)
fiddler(2,3)
with gradient (x=2,y=3) fiddler(x,y)

}


# Check consistency of results between with gradient and numericDeriv.

x <- 0.32739
x1 <- 0.47718; x2 <- 0.89472; x3 <- 0.67325
y1 <- -0.3721; y2 <- -0.8131; y3 <- 1.22213
z1 <- 11.4319; z2 <- 13.1133; z3 <- 6.68901
w1 <- 0.8389; w2 <- 0.1123; w3 <- 4.68701
v1 <- 3; v2 <- 7; v3 <- 0.6513
u1 <- -0.3721; u2 <- 1.8131; u3 <- 1.22213
i1 <- 3

bindgrads <- function (r1,r2) 
    cbind (rbind(r1,r2), rbind(attr(r1,"gradient"),unlist(attr(r2,"gradient"))))

test1 <- function (fun,...)
    print (bindgrads (numericDeriv(quote(fun(x,...)),"x"),
                      with gradient (x) fun(x,...)))
test1p1 <- function (fun,...)
    print (bindgrads (numericDeriv(quote(fun(x+1,...)),"x"),
                      with gradient (x) fun(x+1,...)))

test1r <- function (fun,...) {
    f <- function (x) { set.seed(179); fun(1,x,...) }
    print (bindgrads (numericDeriv(quote(f(x)),"x"),
                      with gradient (x) f(x)))
}

test2 <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x1"),
                      with gradient (x1) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x2"),
                      with gradient (x2) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),c("x1","x2")),
                      with gradient (x1,x2) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),c("x1","x2")),
      { r <- with gradient (x1) { s <- with gradient (x2) fun(x1,x2,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test2y <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(y1,y3,...)),"y1"),
                      with gradient (y1) fun(y1,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y3,...)),"y3"),
                      with gradient (y3) fun(y1,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y3,...)),c("y1","y3")),
                      with gradient (y1,y3) fun(y1,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y3,...)),c("y1","y3")),
      { r <- with gradient (y1) { s <- with gradient (y3) fun(y1,y3,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test2z <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),"z1"),
                      with gradient (z1) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),"z2"),
                      with gradient (z2) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),c("z1","z2")),
                      with gradient (z1,z2) fun(z1,z2,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,...)),c("z1","z2")),
      { r <- with gradient (z1) { s <- with gradient (z2) fun(z1,z2,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test2u <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(u1,u2,...)),c("u1","u2")),
                      with gradient (u1,u2) fun(u1,u2,...)))
}

test2i <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(i1,x2,...)),"x2"),
                      with gradient (x2) fun(i1,x2,...)))
}

test2r <- function (fun,...) {
    f <- function (x1,x2) { set.seed(179); fun(1,x1,x2,...) }
    print (bindgrads (numericDeriv(quote(f(x1,x2)),c("x1","x2")),
                      with gradient (x1,x2) f(x1,x2)))
}

test3 <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x1"),
                      with gradient (x1) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x2"),
                      with gradient (x2) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),"x3"),
                      with gradient (x3) fun(x1,x2,x3,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,x3,...)),c("x1","x2","x3")),
                      with gradient (x1,x2,x3) fun(x1,x2,x3,...)))
}

test3y <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y1"),
                      with gradient (y1) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y2"),
                      with gradient (y2) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),"y3"),
                      with gradient (y3) fun(y1,y2,y3,...)))
    print (bindgrads (numericDeriv(quote(fun(y1,y2,y3,...)),c("y1","y2","y3")),
                      with gradient (y1,y2,y3) fun(y1,y2,y3,...)))
}

test3z <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z1"),
                      with gradient (z1) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z2"),
                      with gradient (z2) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),"z3"),
                      with gradient (z3) fun(z1,z2,z3,...)))
    print (bindgrads (numericDeriv(quote(fun(z1,z2,z3,...)),c("z1","z2","z3")),
                      with gradient (z1,z2,z3) fun(z1,z2,z3,...)))
}

test3w <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(w1,w2,w3,...)),"w1"),
                      with gradient (w1) fun(w1,w2,w3,...)))
    print (bindgrads (numericDeriv(quote(fun(w1,w2,w3,...)),"w2"),
                      with gradient (w2) fun(w1,w2,w3,...)))
    print (bindgrads (numericDeriv(quote(fun(w1,w2,w3,...)),"w3"),
                      with gradient (w3) fun(w1,w2,w3,...)))
    print (bindgrads (numericDeriv(quote(fun(w1,w2,w3,...)),c("w1","w2","w3")),
                      with gradient (w1,w2,w3) fun(w1,w2,w3,...)))
}

test3u <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(u1,u2,u3,...)),"u1"),
                      with gradient (u1) fun(u1,u2,u3,...)))
    print (bindgrads (numericDeriv(quote(fun(u1,u2,u3,...)),"u2"),
                      with gradient (u2) fun(u1,u2,u3,...)))
    print (bindgrads (numericDeriv(quote(fun(u1,u2,u3,...)),"u3"),
                      with gradient (u3) fun(u1,u2,u3,...)))
    print (bindgrads (numericDeriv(quote(fun(u1,u2,u3,...)),c("u1","u2","u3")),
                      with gradient (u1,u2,u3) fun(u1,u2,u3,...)))
}

test3v <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(v1,v2,v3,...)),"v3"),
                      with gradient (v3) fun(v1,v2,v3,...)))
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
test1(psigamma)
test1p1(psigamma)
test1(psigamma,3)
test1p1(psigamma,3)

test2(atan2)
test2z(atan2)

test2(beta)
test2z(beta)

test2(lbeta)
test2z(lbeta)

test2(dchisq)
test2(dchisq,log=TRUE)

test2(pchisq)
test2(pchisq,log=TRUE)
test2(pchisq,lower=FALSE)
test2(pchisq,log=TRUE,lower=FALSE)

test2(qchisq)
test2u(qchisq,log=TRUE)
test2(qchisq,lower=FALSE)
test2u(qchisq,log=TRUE,lower=FALSE)

test2(dexp)
test2(dexp,log=TRUE)

test2(pexp)
test2(pexp,log=TRUE)
test2(pexp,lower=FALSE)
test2(pexp,log=TRUE,lower=FALSE)

test2(qexp)
test2y(qexp,log=TRUE)
test2(qexp,lower=FALSE)
test2y(qexp,log=TRUE,lower=FALSE)

test1r(rexp)

test2i(dgeom)
test2i(dgeom,log=TRUE)

test2i(pgeom)
test2i(pgeom,log=TRUE)
test2i(pgeom,lower=FALSE)
test2i(pgeom,log=TRUE,lower=FALSE)

test2i(dpois)
test2i(dpois,log=TRUE)

test2i(ppois)
test2i(ppois,log=TRUE)
test2i(ppois,lower=FALSE)
test2i(ppois,log=TRUE,lower=FALSE)

test2(dt)
test2(dt,log=TRUE)
test2z(dt)
test2z(dt,log=TRUE)

test2(pt)
test2(pt,log=TRUE)
test2(pt,lower=FALSE)
test2(pt,log=TRUE,lower=FALSE)
test2z(pt)
test2z(pt,log=TRUE)
test2z(pt,lower=FALSE)
test2z(pt,log=TRUE,lower=FALSE)

test2(qt)
test2u(qt,log=TRUE)
test2(qt,lower=FALSE)
test2u(qt,log=TRUE,lower=FALSE)

test3(dbeta)
test3(dbeta,log=TRUE)
test3w(dbeta)
test3w(dbeta,log=TRUE)

test3(pbeta)
test3(pbeta,log=TRUE)
test3(pbeta,lower=FALSE)
test3(pbeta,log=TRUE,lower=FALSE)

test3(qbeta)
test3u(qbeta,log=TRUE)
test3(qbeta,lower=FALSE)
test3u(qbeta,log=TRUE,lower=FALSE)

test3v(dbinom)
test3v(dbinom,log=TRUE)

test3v(pbinom)
test3v(pbinom,log=TRUE)
test3v(pbinom,lower=FALSE)
test3v(pbinom,log=TRUE,lower=FALSE)

test3(dcauchy)
test3(dcauchy,log=TRUE)
test3z(dcauchy)
test3z(dcauchy,log=TRUE)

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

test3(df)
test3(df,log=TRUE)

test3(pf)
test3(pf,log=TRUE)
test3(pf,lower=FALSE)
test3(pf,log=TRUE,lower=FALSE)

test3(qf)
test3u(qf,log=TRUE)
test3(qf,lower=FALSE)
test3u(qf,log=TRUE,lower=FALSE)

test3(dgamma)
test3(dgamma,log=TRUE)
test3z(dgamma)
test3z(dgamma,log=TRUE)

dgamma_scale <- function (x,a,b,...) dgamma(x,a,scale=b,...)
test3(dgamma_scale)
test3(dgamma_scale,log=TRUE)
test3z(dgamma_scale)
test3z(dgamma_scale,log=TRUE)

test3(pgamma)
test3(pgamma,log=TRUE)
test3(pgamma,lower=FALSE)
test3(pgamma,log=TRUE,lower=FALSE)
test3z(pgamma)
test3z(pgamma,log=TRUE)
test3z(pgamma,lower=FALSE)
test3z(pgamma,log=TRUE,lower=FALSE)

test3(qgamma)
test3u(qgamma,log=TRUE)
test3(qgamma,lower=FALSE)
test3u(qgamma,log=TRUE,lower=FALSE)

test3(dlnorm)
test3(dlnorm,log=TRUE)
test3z(dlnorm)
test3z(dlnorm,log=TRUE)

test3(plnorm)
test3(plnorm,log=TRUE)
test3(plnorm,lower=FALSE)
test3(plnorm,log=TRUE,lower=FALSE)
test3z(plnorm)
test3z(plnorm,log=TRUE)
test3z(plnorm,lower=FALSE)
test3z(plnorm,log=TRUE,lower=FALSE)

test3(qlnorm)
test3y(qlnorm,log=TRUE)
test3(qlnorm,lower=FALSE)
test3y(qlnorm,log=TRUE,lower=FALSE)

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

test3v(dnbinom)
test3v(dnbinom,log=TRUE)

test3v(pnbinom)
test3v(pnbinom,log=TRUE)
test3v(pnbinom,lower=FALSE)
test3v(pnbinom,log=TRUE,lower=FALSE)

dnbinom_mu <- function (x, size, mu, ...) dnbinom(x,size,mu=mu,...)
test3v(dnbinom_mu)
test3v(dnbinom_mu,log=TRUE)

pnbinom_mu <- function (q, size, mu, ...) pnbinom(q,size,mu=mu,...)
test3v(pnbinom_mu)
test3v(pnbinom_mu,log=TRUE)
test3v(pnbinom_mu,lower=FALSE)
test3v(pnbinom_mu,log=TRUE,lower=FALSE)

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

test3w(dunif)
test3w(dunif,log=TRUE)

test3w(punif)
test3w(punif,log=TRUE)
test3w(punif,lower=FALSE)
test3w(punif,log=TRUE,lower=FALSE)

test3w(qunif)
test3y(qunif,log=TRUE)
test3w(qunif,lower=FALSE)
test3y(qunif,log=TRUE,lower=FALSE)

test2r(runif)

test3(dweibull)

test3(pweibull)
test3(pweibull,log=TRUE)
test3(pweibull,lower=FALSE)
test3(pweibull,log=TRUE,lower=FALSE)

test3(qweibull)
test3u(qweibull,log=TRUE)
test3(qweibull,lower=FALSE)
test3u(qweibull,log=TRUE,lower=FALSE)

test2r(rweibull)


# Check gradients of lists.

with gradient (a=3.1) list (a^2, a^3)
with gradient (a=3.1) list (list(a^2), xx=sin(a), yy=99, zz=list(7,a^3))
with gradient (a=3.1) with gradient (b=7.7) list(a*b,a^2*b^2)

with gradient (a=3.1) with gradient (b=100*a) list(a, b, a^2, b^2, a*b, a^2*b^2)

with gradient (a=3.1,b=4.2) list(a,b,a*b)

with gradient (a=3.1,b=4.2) with gradient (c=7.9) list (a*b*c)
with gradient (c=7.9) with gradient (a=3.1,b=4.2) list (a*b*c)

with gradient (a=3.1,b=4.2) with gradient (c=7.9*a) list (a*b*c)
with gradient (c=7.9) with gradient (a=3.1*c,b=4.2) list (a*b*c)

with gradient (a=3.1) { x <- list(xxx=a,yyy=a^2); list(qqq=x,rrr=a^3) }

with gradient (a=3.1, b=4.2) {
  r <- list(abc=a^2,pqr=b^2,xyz=a*b)
  print(gradient_of(r$pqr))
  print(gradient_of(r$abc))
  print(gradient_of(r$xyz))
  r
}


# Test gradients of list elements found with $ and [[.]].

with gradient (a=9) { r <- list(x=a,y=a^2,z=a^3); list(r$z,r$y) }
with gradient (a=9) { r <- list(x=a,y=a^2,z=a^3); list(r[[3]],r[[2]]) }

with gradient (a=9) { r <- list(x=a,y=list(p=a^2,7,q=a^3))
                      list(r$y$q,r$x) }
with gradient (a=9) { r <- list(x=a,y=list(p=a^2,7,q=a^3))
                      list(r[[2]][[3]],r[[1]]) }
with gradient (a=9) { r <- list(x=a,y=list(p=a^2,7,q=a^3))
                      list(r[[c(2,3)]],r[[1]]) }


# Test 'compute gradient'.

f <- function (x) compute gradient (x) x^2 as (123*x) # wrong, see if happens

with gradient (a=12) f(a)
with gradient (a=12) f(a^2)

g <- function (x,y)
  compute gradient (x,y) list(x,y,x*y)
  as (list(8,9,x+y), list(x,y,7))  # wrong, but see if it happens

with gradient (z=9) g(100*z,z+1)


# Test subassign operations.

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$y <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]] <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$y <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]] <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$w <- -a
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[4]] <- -a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$x$w <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]]$w <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$x[[2]] <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]][[2]] <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$x$w <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]][[2]] <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r$x$u <- -a
  r
}

with gradient (a=3.01) {
  r <- list(a=3,x=list(v=8,w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  r[[2]][[4]] <- -a
  r
}

xel <- function (a) a$x
`xel<-` <- function (a,value) { a$x <- value; a }

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  xel(r) <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  xel(r) <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  xel(r) <- -a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=list(w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  xel(xel(r)) <- 1/a;
  r
}

with gradient (a=3.01) {
  r <- list(x=list(w=3*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  xel(xel(r)) <- -a^2
  r
}

elip1 <- function (a,i) a[[i+1]]
`elip1<-` <- function (a,i,value) { a[[i+1]] <- value; a }

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  elip1(r,0) <- 1/a
  r
}

with gradient (a=3.01) {
  r <- list(x=a^2,y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  elip1(r,0) <- NULL
  r
}

with gradient (a=3.01) {
  r <- list(y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  elip1(r,2) <- -a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=list(w=3*a,x=4*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  elip1(elip1(r,0),1) <- 1/a;
  r
}

with gradient (a=3.01) {
  r <- list(x=list(w=3*a),y=a^3,z=a^4)
  print(gradient_of(r))
  cat("--\n")
  elip1(elip1(r,0),1) <- -a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r$y <- a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r[[2]] <- a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r$x <- -a
  r$y <- a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r$x <- -a
  r[[2]] <- a^2
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r <- `$<-` (r, "y", a^2)
  r
}

with gradient (a=3.01) {
  r <- list(x=1,y=2)
  class(r) <- "fred"
  r <- `[[<-` (r, 2, a^2)
  r
}

with gradient (a=3.01) {
  r <- list(x=-1,y=2)
  class(r) <- "fred"
  r$x <- -a
  r <- `$<-` (r, "y", a^2)
  r
}

with gradient (a=3.01) {
  r <- list(x=-1,y=2)
  class(r) <- "fred"
  r$x <- -a
  r <- `[[<-` (r, 2, a^2)
  r
}


# Test gradients w.r.t. list values.

with gradient (a=list(5,55,555)) a

with gradient (a=list(x=5,y=55,z=555)) a$x
with gradient (a=list(x=5,y=55,z=555)) a$y
with gradient (a=list(x=5,y=55,z=555)) a$z

with gradient (a=list(x=5,y=55,z=555)) sqrt(a$y)
with gradient (a=list(x=5,y=55,z=555)) a$z+a$x
with gradient (a=list(x=5,y=55,z=555)) a$z*a$x

with gradient (a=list(x=5,y=55,z=555)) list(a$z,a$x^2, a$x*a$y*a$z)

with gradient (a=list(x=7,y=list(3,z=10))) a
with gradient (a=list(x=7,y=list(3,z=10))) list(a$y$z,a$x,-a$x)
with gradient (a=list(x=7,y=list(3,z=10))) list(a$y,a$x,-a$x)

with gradient (b=list(3,abc=4)) {
  r <- list(x=b,y=b$abc,z=b$abc^2)
  r$y <- NULL
  r
}

with gradient (b=list(3,abc=4)) {
  r <- list(x=b,y=b$abc,z=b$abc^2)
  r$x <- NULL
  r
}

with gradient (b=list(10,bb=list(bbb=3,4),20)) {
  r <- list(a1=7,a2=-b$bb$bbb,a3=b)
  r$a3 <- NULL
  r
}

with gradient (b=list(3,abc=4)) {
  r <- list(x=b,y=b$abc,z=b$abc^2)
  r$y <- b$abc^3
  r
}

with gradient (b=list(3,abc=4)) {
  r <- list(x=b,y=b$abc,z=b$abc^2)
  r$w <- b$abc^3
  r
}


# Test gradients w.r.t. numeric vectors.

with gradient (a=c(5,55,555)) a

with gradient (a=c(x=5,y=55,z=555)) a[["x"]]
with gradient (a=c(x=5,y=55,z=555)) a[["y"]]
with gradient (a=c(x=5,y=55,z=555)) a[["z"]]

with gradient (a=c(x=5,y=55,z=555)) sqrt(a[["y"]])
with gradient (a=c(x=5,y=55,z=555)) a[["z"]]+a[["x"]]
with gradient (a=c(x=5,y=55,z=555)) 2.1*a[["z"]]+3.4*a[["x"]]

with gradient (a=c(9.1,4.1)) {
  b <- numeric(3)
  b[[2]] <- a[[1]]
  b[[1]] <- 3*a[[2]] + b[[2]]
  b[[3]] <- sin(a[[2]])
  b[[5]] <- b[[1]]^2
  b
}

with gradient (a=c(5.1,2.2,3.7)) {
  x <- numeric(2)
  x[[1]] <- a[[2]]^2
  x[[2]] <- a[[3]]
  sin(x)
}

with gradient (a=c(5.1,2.2,3.7),b=c(7.3,6.4)) {
  x <- numeric(2)
  x[[1]] <- a[[2]]^2
  x[[2]] <- a[[3]]*b[[2]]
  sin(x)
}

with gradient (a=c(-1.1,2.3)) {
  x <- numeric(2)
  x[[1]] <- a[[2]]^2
  x[[2]] <- a[[1]]*a[[2]]
  abs(x)
}

with gradient (a=c(1.1,2.3),b=3.4) {
  list (dt(a,5), dt(5,a), dt(a,a), dt(a,b), dt(b,a))
}

with gradient (a=c(1.1,2.3),b=3.4) {
  list (dnorm(a,2,5), dnorm(5,a,2), dnorm(5,2,a), dnorm(a,a,a), 
        dnorm(a,a,b), dnorm(a,b,a), dnorm(b,a,a), dnorm(b,a,b))
}

with gradient (a=c(1.1,2.3)) list (+a, -a)
with gradient (a=c(1.1,2.3)) list (+sin(a), -sin(a))

with gradient (a=c(1.1,2.3),b=c(5.2,3.1)) list (a+b, a-b, a*b, a/b, a^b)
with gradient (a=c(1.1,2.3,3.7),b=c(5.2,3.1)) list (a+b, a-b, a*b, a/b, a^b)
with gradient (a=c(1.1,2.3),b=c(5.2,3.1,1.9)) list (a+b, a-b, a*b, a/b, a^b)
with gradient (a=c(1.1,2.3)) list (1+a, 1-a, a+1, a-1, a+a, a-a)
with gradient (a=c(1.1,2.3)) list (2.1*a, 2.1/a, a*2.1, a/2.1, a*a, a/a)
with gradient (a=c(1.1,2.3)) list (2.1^a, a^2.1, a^a)

with gradient (a=c(1.7,-34.9)) list ((a+2)*(a+1), sin(a)^2+a)
with gradient (a=1.7) list ((a+2)*(a+1), sin(a)^2+a)
with gradient (a=-34.9) list ((a+2)*(a+1), sin(a)^2+a)

with gradient (a=1.7) 
  list (a+3L, a-3L, a*3L, 3L*a, a^3L, a/3L, 3L/a, 3L^a)
with gradient (a=c(1.7,1.7)) 
  list (a+3L, a-3L, a*3L, 3L*a, a^3L, a/3L, 3L/a, 3L^a)
with gradient (a=1.7)
  list (a+3, a-3, a*3, 3*a, a^3, a/3, 3/a, 3^a)
with gradient (a=c(1.7,1.7))
  list (a+3, a-3, a*3, 3*a, a^3, a/3, 3/a, 3^a)

set.seed(179)
with gradient (a=3.1) rexp(1,a)
with gradient (a=0.2) rexp(1,a)
with gradient (a=7.5) rexp(1,a)
with gradient (a=3.1) rexp(1,a^2)
with gradient (a=0.2) rexp(1,a^2)
with gradient (a=7.5) rexp(1,a^2)

set.seed(179)
with gradient (a=c(3.1,0.2,7.5)) rexp(3,a)
with gradient (a=c(3.1,0.2,7.5)) rexp(3,a^2)

set.seed(179)
with gradient (a=3.1,b=2.4) rnorm(1,a,b)
with gradient (a=0.2,b=0.9) rnorm(1,a,b)
with gradient (a=3.1,b=2.4) rnorm(1,cos(a),sin(b)^2)
with gradient (a=0.2,b=0.9) rnorm(1,cos(a),sin(b)^2)
with gradient (a=3.1,b=2.4) rnorm(1,a,3)
with gradient (a=0.2,b=0.9) rnorm(1,a,3)
with gradient (a=3.1,b=2.4) rnorm(1,4,b)
with gradient (a=0.2,b=0.9) rnorm(1,4,b)
with gradient (a=0.7,b=1.8) rnorm(1,a^2,b)
with gradient (a=0.9,b=1.6) rnorm(1,a^2,b)
with gradient (a=0.7,b=1.2) rnorm(1,a^2,b)

set.seed(179)
with gradient (a=c(3.1,0.2),b=c(2.4,0.9)) rnorm(2,a,b)
with gradient (a=c(3.1,0.2),b=c(2.4,0.9)) rnorm(2,cos(a),sin(b)^2)
with gradient (a=c(3.1,0.2),b=c(2.4,0.9)) rnorm(2,a,3)
with gradient (a=c(3.1,0.2),b=c(2.4,0.9)) rnorm(2,4,b)
with gradient (a=c(0.7,0.9),b=c(1.8,1.6,1.2)) rnorm(3,a^2,b)


# Test backpropagation.

with gradient (a=5) 
  with gradient (d=a^2) sin(d)

with gradient (a=5,b=7) 
  with gradient (d=a^2+b,e=a*b) sin(d)+sqrt(e)

with gradient (a=5,b=7) 
  with gradient (d=a^2+b,e=a*b) list(sin(d)+sqrt(e),55)

with gradient (L=list(a=5,b=7))
  with gradient (d=L$a^2+L$b,e=L$a*L$b) sin(d)+sqrt(e)

with gradient (a=5,b=7)
  with gradient (M=list(d=a^2+b,e=a*b)) sin(M$d)+sqrt(M$e)

with gradient (a=5,b=7)
  with gradient (M=list(d=a^2+b,e=a*b)) list(sin(M$d)+sqrt(M$e),55)

with gradient (L=list(a=5,b=7))
  with gradient (M=list(d=L$a^2+L$b,e=L$a*L$b)) sin(M$d)+sqrt(M$e)

with gradient (L=list(a=5,b=7))
  with gradient (d=L$a^2+L$b,e=L$a*L$b) list(sin(d)+sqrt(e),55)

with gradient (L=list(a=5,b=7))
  with gradient (M=list(d=L$a^2+L$b,e=L$a*L$b)) list(sin(M$d)+sqrt(M$e),55)

with gradient (L=list(a=5,b=7))
  track gradient (M=list(d=L$a^2+L$b,e=L$a*L$b)) list(sin(M$d)+sqrt(M$e),55)

with gradient (L=list(a=5,b=7))
  back gradient (M=list(d=L$a^2+L$b,e=L$a*L$b)) list(sin(M$d)+sqrt(M$e),55)

with gradient (L=c(a=5,b=7)) {
  M <- c(d=0,e=0)
  M[[1]] <- L[["a"]]^2+L[["b"]]
  M[[2]] <- L[["a"]]*L[["b"]]
  back gradient (M) {
    c <- c(0,55)
    c[[1]] <- sin(M[["d"]])+sqrt(M[["e"]])
    c
  }
}

with gradient (a=list(4.1,2.7,3.1,5.2)) {
  back gradient (b=list(a[[2]],sin(a[[1]]+a[[4]]),a[[1]]^2)) {
    c <- list(0,0)
    c[[1]] <- b[[1]] + 4*b[[2]]
    c[[2]] <- b[[3]]^2 + b[[2]]
    c
  }
}

with gradient (a=c(4.1,2.7,3.1,5.2)) {
  b <- numeric(3)
  b[[1]] <- a[[2]]
  b[[2]] <- sin(a[[1]]+a[[4]])
  b[[3]] <- a[[1]]^2
  back gradient (b) {
    c <- numeric(2)
    c[[1]] <- b[[1]] + 4*b[[2]]
    c[[2]] <- b[[3]]^2 + b[[2]]
    c
  }
}

