# Test arithmetic operations on lists.
#
# Added for pqR, 2020 Radford M. Neal.


#gctorture()  # Can enable for testing, but slow

a <- list (x=3, y=c(4,5,-1), z=TRUE)
b <- list (x=list(10,20), y=2, z=3)
c <- list (3.1,q=3.7,list(0.1,0.9))

+a
-a
a+b
a-b
a*b
a/b
a^b
a%%b
a%/%b

floor(c)
ceiling(c)
round(c)
round(a+b,-1)
signif(a+b,1)
trunc(c)

abs(a)
sqrt(b)
sign(a)
exp(b)
expm1(b)
log1p(c)
log(c)
log(c,base=2)
log2(c)
log10(c)
cos(c)
sin(c)
tan(c)
acos(c)
asin(c)
atan(a)
cosh(b)
sinh(b)
tanh(b)
acosh(a)
asinh(a)
atanh(c)
lgamma(a)
gamma(a)
digamma(a)
trigamma(a)


with gradient (x=0.1) list (a=7*x, b=x^2) * 100
with gradient (x=0.1) 100 * list (a=7*x, b=x^2)
with gradient (x=0.1) - list (a=7*x, b=x^2)
with gradient (x=0.1) list (a=3, b=x) + list (a=7*x, b=x^2)
with gradient (x=0.1) log (list (x, x^2, x^3))
with gradient (x=-0.1,y=0.2) abs (list (x, y, x*y))
with gradient (x=-0.1,y=0.2) abs (list (x, y, x*y)) * 10
