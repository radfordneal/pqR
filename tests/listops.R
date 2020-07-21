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
