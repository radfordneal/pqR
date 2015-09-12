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
    quote ((a = b))[[2]],
    quote (a [b]),
    quote (a [[b]]),
    quote (a $ x),
    quote (a @ x),
    quote (a (b)),
    quote (function (x) a),
    quote (if (c) a else b),
    quote (while (a) b),
    quote (repeat a),
    quote ({ a; b })
)" ))

for (kp in c(FALSE,TRUE)) {

    options(keep.parens=kp)
    cat("\n\nWith keep.parens set to",kp,"\n")

    cat("\nOne operator\n\n");
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

    cat("\nWith a replaced by inner operator\n");
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

    cat("\nWith b replaced by inner operator\n");
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
