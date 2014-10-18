# Test possible bugs involving variable/function lookup for objects
# in global environemnts when local bindings come and go (testing 
# whether several optimizations are correct).
#
# Added for pqR, 2014, Radford M. Neal.

abc <- 11
def <- 22
xyz <- function()33

af <- function (e) { 
    assign ("*", function(a,b)a+b, envir=e)
}

ag <- function (e) { 
    assign ("xyz", function()99, envir=e)
}

fun <- function (abc,`/`,eight) {

    cat("  A\n")
    def <- 2200
    x <- eight/2
    stopifnot(abc==1100)
    stopifnot(def==2200)
    stopifnot(8/2==5482)
    stopifnot(x==5482)
    stopifnot(xyz()==33)

    cat("  B\n")
    ag(environment())
    rm("abc","/","def")
    x <- eight/2
    stopifnot(abc==11)
    stopifnot(def==22)
    stopifnot(8/2==4)
    stopifnot(x==4)
    stopifnot(xyz()==99)

    cat("  C\n")
    abc <- 110000
    `/` <- function (a,b) a^2+b
    af(environment())
    x <- eight*2
    y <- eight/2
    stopifnot(abc==110000)
    stopifnot(8/2==66)
    stopifnot(y==66)
    stopifnot(8*2==10)
    stopifnot(x==10)

    cat("  D\n")
    rm("*")
    x <- eight*2
    stopifnot(8*2==16)
    stopifnot(x==16)
}

fun2 <- function (abc,eight) {

    assign("/",function (a,b) 5400+10*a+b)

    cat("  A\n")
    def <- 2200
    x <- eight/2
    stopifnot(abc==1100)
    stopifnot(def==2200)
    stopifnot(8/2==5482)
    stopifnot(x==5482)
    stopifnot(xyz()==33)

    cat("  B\n")
    ag(environment())
    rm("abc","/","def")
    x <- eight/2
    stopifnot(abc==11)
    stopifnot(def==22)
    stopifnot(8/2==4)
    stopifnot(x==4)
    stopifnot(xyz()==99)

    cat("  C\n")
    abc <- 110000
    `/` <- function (a,b) a^2+b
    af(environment())
    x <- eight*2
    y <- eight/2
    stopifnot(abc==110000)
    stopifnot(8/2==66)
    stopifnot(y==66)
    stopifnot(8*2==10)
    stopifnot(x==10)

    cat("  D\n")
    rm("*")
    x <- eight*2
    stopifnot(8*2==16)
    stopifnot(x==16)
}

cat("Test 1st time\n")
fun (1100, function (a,b) 5400+10*a+b, 8)
fun2 (1100, 8)

cat("Test 2nd time\n")
fun (1100, function (a,b) 5400+10*a+b, 8)
fun2 (1100, 8)
