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


# Check consistency of results between with_gradient and numericDeriv.

x <- 0.32739
x1 <- 2.1047; x2 <- 1.9713

bindgrads <- function (r1,r2) 
    cbind (rbind(r1,r2), rbind(attr(r1,"gradient"),attr(r2,"gradient")))

test1 <- function (fun)
    print (bindgrads (numericDeriv(quote(fun(x)),"x"),
                      with_gradient (x) fun(x)))

test2 <- function (fun,...) {
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x1"),
                      with_gradient (x1) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),"x2"),
                      with_gradient (x2) fun(x1,x2,...)))
    print (bindgrads (numericDeriv(quote(fun(x1,x2,...)),c("x1","x2")),
      { r <- with_gradient (x1) { s <- with_gradient (x2) fun(x1,x2,...); 
                                  g2 <<- attr(s,"gradient"); s }
        attr(r,"gradient") <- cbind(g1=attr(r,"gradient"),g2=g2)
        r
      }
    ))
}

test1(sin)

test1(log)

test1(gamma)

test2(atan2)

test2(dexp)

test2(dexp,log=TRUE)

