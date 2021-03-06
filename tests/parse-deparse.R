# Test parsing and deparsing, and getParseData
#
# Added for pqR, 2015, 2018, Radford M. Neal


# Check output of getParseData.

options(keep.source=TRUE,keep.parens=FALSE)

expr1 <- parse (text = "y <- 1; fn <- function(x) {
                         z <- (x+y)   +   (2*y)    # A comment
                         y <- y-1
                         if (y > 10 && fn (x+1) <= 1) 1 else z
                       }
                       if (y>1) y <- 3
                       y <- y+1")

pd1 <- getParseData(expr1)[c(-5,-6)]
row.names(pd1) <- NULL 
print(pd1)

for (e in attr(expr1,"srcref")) print(unclass(e))

expr2 <- parse (text = c("y <- 1; fn <- function(x) {",
"                         z <- (x+y)   +   (2*y)    # A comment",
"                         y <- y-1",
"                         if (y > 10 && fn (x+1) <= 1) 1 else z",
"                       }",
"                       if (y>1) y <- 3",
"                       y <- y+1"))

cmp <- c(-5,-6)
stopifnot(identical(getParseData(expr1)[cmp],getParseData(expr2)[cmp]))

for (e in attr(expr1,"srcref")) print(unclass(e))


# Check retention/removal of parentheses.

options(keep.parens=FALSE)
print(as.list (quote (a+b*c)))
print(as.list (quote (a+(b*c))))
print(as.list (quote ((a+b)*c)))
print(as.list (quote ((a^b)^c)))
print(as.list (quote (a^(b^c))))
print(as.list (quote ((a <- b <- c) -> d)))
f <- y ~ (x) + a * (b + c)
print(as.list(f))
print(as.list(f[[3]]))
print(as.list(f[[3]][[3]]))

options(keep.parens=TRUE)
print(as.list (quote (a+b*c)))
print(as.list (quote (a+(b*c))))
print(as.list (quote ((a+b)*c)))
print(as.list (quote ((a^b)^c)))
print(as.list (quote (a^(b^c))))
print(as.list (quote ((a <- b <- c) -> d)))
f <- y ~ (x) + a * (b + c)
print(as.list(f))
print(as.list(f[[3]]))
print(as.list(f[[3]][[3]]))


# Check consistency of parsing and deparsing.

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


for (kp in c(FALSE,TRUE)) {

    options(keep.parens=kp)
    cat("\n\nWith keep.parens set to",kp,"\n")

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

}


options(keep.source=FALSE)

# We have to parse this at run-time so that the keep.source=FALSE
# setting above is in effect.

exprs <- eval (parse (text = "list (
    quote (+b),
    quote (-b),
    quote (!b),
    quote (~b),
    quote (?b),
    quote ((b)),
    quote (a ^ b),
    quote (a ** b),
    quote (a %x% b),
    quote (a * b),
    quote (a / b),
    quote (a + b),
    quote (a - b),
    quote (a == b),
    quote (a != b),
    quote (a < b),
    quote (a <= b),
    quote (a >= b),
    quote (a > b),
    quote (a & b),
    quote (a && b),
    quote (a | b),
    quote (a || b),
    quote (a ~ b),
    quote (a -> b),
    quote (a ->> b),
    quote (a <- b),
    quote (a <<- b),
    quote (a := b),
    quote (a ? b),
    as.call(list(quote(`=`),quote(a),quote(b))),
    quote (a [b]),
    quote (a [[b]]),
    quote (a $ x),
    quote (a @ x),
    quote (a (b)),
    quote (function (x) a),
    quote (if (c) a else b),
    quote (while (a) b),
    quote (repeat a),
    quote (-if (c) a else b),
    quote (-!a),
    quote (a * 2 ^ if (T) 1 else b)
)" ))

for (kp in c(FALSE,TRUE)) {

    options(keep.parens=kp)
    cat("\n\nWith keep.parens set to",kp,"\n")

    cat("\nOne level\n\n");
    for (e in exprs) {
        e1 <- e
        s1 <- deparse(e1)
        e2 <- try(parse(text=s1)[[1]],silent=TRUE)
        if (!identical(e1,e2))
            cat("EXPRESSIONS NOT IDENTICAL: ",
              deparse(e1)," : ",deparse(e2),"\n")
        if (class(e2)=="try-error")
            s2 <- "PARSE ERROR"
        else
            s2 <- deparse(e2)
        if (!identical(s1,s2))
            cat("NOT IDENTICAL: ",s1," : ",s2,"\n")
        else
            cat("IDENTICAL: ",s1," : ",s2,"\n")
    }

    cat("\nWith a replaced by inner expression\n");
    for (e in exprs) {
        for (f in exprs) {
            e1 <- eval(as.call(list(substitute,e,list(a=f))))
            s1 <- deparse(e1)
            e2 <- try(parse(text=s1)[[1]],silent=TRUE)
            if (class(e2)=="try-error")
                s2 <- "PARSE ERROR"
            else {
                if (!kp && !identical(e1,e2))
                    cat("EXPRESSIONS NOT IDENTICAL: ",
                      deparse(e1)," : ",deparse(e2),"\n")
                s2 <- deparse(e2)
            }
            if (!identical(s1,s2))
                cat("NOT IDENTICAL: ",s1," : ",s2,"\n")
        }
    }

    cat("\nWith b replaced by inner expression\n");
    for (e in exprs) {
        for (f in exprs) {
            e1 <- eval(as.call(list(substitute,e,list(b=f))))
            s1 <- deparse(e1)
            e2 <- try(parse(text=s1)[[1]],silent=TRUE)
            if (class(e2)=="try-error")
                s2 <- "PARSE ERROR"
            else {
                if (!kp && !identical(e1,e2))
                    cat("EXPRESSIONS NOT IDENTICAL: ",
                      deparse(e1)," : ",deparse(e2),"\n")
                s2 <- deparse(e2)
            }
            if (!identical(s1,s2))
                cat("NOT IDENTICAL: ",s1," : ",s2,"\n")
        }
    }
}
