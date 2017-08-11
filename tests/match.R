# Test possible bugs involving 'match', '%in%', 'duplicated', etc.
#
# Added for pqR, 2017, Radford M. Neal.

a <- c("abc","def","hij")

print(match("def",a))
print(match("xyz",a))
print(match("xyz",a,nomatch=77))
print(match("def",a,incomparables=c("q","def")))

print("def" %in% a)
print("xyz" %in% a)

b <- c("xyz","def","abc","q")

print(match(b,a))
print(match(b,a,nomatch=77))
print(match(b,a,nomatch=77,incomparables=c("q","def")))

print(match(a,b))
print(match(a,b,nomatch=77))
print(match(a,b,nomatch=77,incomparables=c("q","def")))

x <- c(a,a)
y <- c(b,b,b)

print(match(x,b))
print(match(x,y))
print(match(y,x))

print(duplicated(a))
print(duplicated(x))
print(duplicated(b))
print(duplicated(y))

z <- character(0)

print(match(z,a))
print(match(a,z))
print(match(z,z))

print(z %in% a)
print(a %in% z)
print(z %in% z)

print(duplicated(z))
