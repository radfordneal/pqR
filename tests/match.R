# Test possible bugs involving 'match', '%in%', 'duplicated', etc.
#
# Added for pqR, 2017, Radford M. Neal.

a <- c("abc","def","hij")

print(match("def",a))
print(match("xyz",a))
print(match("xyz",a,nomatch=77))
print(match("def",a,incomparables=c("q","def")))

b <- c("xyz","def","abc","q")
print(match(b,a))
print(match(b,a,nomatch=77))
print(match(b,a,nomatch=77,incomparables=c("q","def")))
