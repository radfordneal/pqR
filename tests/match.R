# Test possible bugs involving 'match', '%in%', 'duplicated', etc.
# and also which, which.min, and which.max.
#
# Added for pqR, 2017, Radford M. Neal.

# Tests with scalar first operand.

a <- c("abc","def","hij")

print(match("def",a))
print(match("xyz",a))
print(match("xyz",a,nomatch=77))
print(match("def",a,incomparables=c("q","def")))

print("def" %in% a)
print("xyz" %in% a)

# Tests with non-scalar operands.

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

# Tests of 'duplicated'.

print(duplicated(a))
print(duplicated(x))
print(duplicated(b))
print(duplicated(y))

# Tests with zero-length vector.

z <- character(0)

print(match(z,a))
print(match(a,z))
print(match(z,z))

print(z %in% a)
print(a %in% z)
print(z %in% z)

print(duplicated(z))

# Tests of %in% with any, all, or sum.

print(any("def" %in% b))
print(any("pqr" %in% b))
print(all("def" %in% b))
print(all("pqr" %in% b))
print(sum("def" %in% b))
print(sum("pqr" %in% b))

print(any(a %in% y))
print(all(a %in% y))
print(sum(a %in% y))
print(all(b %in% y))

print(any(y %in% a))
print(all(y %in% a))
print(sum(y %in% a))
print(all(y %in% b))

ap <- paste0(a,"Q")

print(any(ap %in% y))
print(all(ap %in% y))
print(sum(ap %in% y))

print(any(y %in% ap))
print(all(y %in% ap))
print(sum(y %in% ap))

# Tests of which.

print(which(c(NA,TRUE,FALSE,FALSE,TRUE,NA,TRUE)))
print(which(c(NA,NA,NA)))
print(which(logical(0)))

# Tests of which.min and which.max.

print(which.min(c(TRUE,TRUE,FALSE,FALSE,TRUE)))
print(which.min(c(NA,TRUE,TRUE,NA,FALSE,FALSE)))
print(which.min(c(NA,TRUE,TRUE,NA,TRUE)))
print(which.min(c(NA,NA,NA)))
print(which.min(logical(0)))

print(which.max(c(FALSE,FALSE,TRUE,TRUE,FALSE)))
print(which.max(c(NA,FALSE,FALSE,NA,TRUE,TRUE)))
print(which.max(c(NA,FALSE,FALSE,NA,FALSE)))
print(which.max(c(NA,NA,NA)))
print(which.max(logical(0)))

print(which.min(c(7L,2L,-1L,8L)))
print(which.min(c(NA,7L,2L,NA,-1L,8L)))
print(which.min(as.integer(c(NA,NA,NA))))
print(which.min(integer(0)))

print(which.max(c(7L,2L,-1L,8L)))
print(which.max(c(NA,7L,2L,NA,-1L,8L)))
print(which.max(as.integer(c(NA,NA,NA))))
print(which.max(integer(0)))

print(which.min(c(7.1,2.1,-1.1,8.1)))
print(which.min(c(NA,7.1,2.1,NA,-1.1,8.1)))
print(which.min(as.integer(c(NA,NA,NA))))
print(which.min(integer(0)))

print(which.max(c(7.1,2.1,-1.1,8.1)))
print(which.max(c(NA,7.1,2.1,NA,-1.1,8.1)))
print(which.max(as.double(c(NA,NA,NA))))
print(which.max(double(0)))

print(which.min(c("55","66","22","33")))

print(which.max(c("55","66","22","33")))
