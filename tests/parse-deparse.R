# Test parsing and deparsing
#
# Added for pqR, 2015, Radford M. Neal

unary_ops <- 
  expression (`U`, `+`, `-`, `!`, `~`, `?`)

binary_ops <- 
  expression (`B`, `^`, `**`, `:`, `%*%`, `*`, `/`, `+`, `-`,
              `==`, `!=`, `<`, `<=`, `>=`, `>`, `&`, `&&`, `|`, `||`, 
              `~`, `->`, `->>`, `<-`, `<<-`, `:=`, `?`, `=`)

check_deparse_parse <- function (ex)
{
    de1 <- deparse(ex)
    pa1 <- parse(text=de1)[[1]]
    de2 <- deparse(pa1)
    pa2 <- parse(text=de2)[[1]]
    de3 <- deparse(pa2)

    print(c(de1,de2,de3))

    stopifnot (identical(de1,de2))
    stopifnot (identical(de1,de3))
    stopifnot (identical(pa1,pa2))
}

for (i in unary_ops)
    check_deparse_parse (substitute (x(a), list(x=i)))

for (i in binary_ops)
    check_deparse_parse (substitute (y(a,b), list(y=i)))

for (i in unary_ops)
    for (j in binary_ops)
        check_deparse_parse (substitute (x(y(b,c)), list(x=i,y=j)))

for (i in unary_ops)
    for (j in binary_ops)
        check_deparse_parse (substitute (y(x(b),c), list(x=i,y=j)))

for (i in unary_ops)
    for (j in binary_ops)
        check_deparse_parse (substitute (x((y(b,c))), list(x=i,y=j)))

for (i in binary_ops)
    for (j in binary_ops)
        check_deparse_parse (substitute (x(a,(y(b,c))), list(x=i,y=j)))

for (i in unary_ops)
    check_deparse_parse (substitute (x(a)$q, list(x=i)))

for (i in binary_ops)
    check_deparse_parse (substitute (y(a,b)$q, list(y=i)))

for (i in binary_ops)
    for (j in binary_ops)
        check_deparse_parse (substitute (x(a,y(b,c)), list(x=i,y=j)))

