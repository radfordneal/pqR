## tests of boundary cases in read.table()

## force standard handling for character cols
options(stringsAsFactors=TRUE)

# empty file
file.create("foo1")
try(read.table("foo1")) # fails
read.table("foo1", col.names=LETTERS[1:4])
unlink("foo1")

# header only
cat("head\n", file = "foo2")
read.table("foo2")
try(read.table("foo2", header=TRUE)) # fails in 1.2.3
unlink("foo2")
# header detection
cat("head\n", 1:2, "\n", 3:4, "\n", file = "foo3")
read.table("foo3", header=TRUE)
read.table("foo3", header=TRUE, col.names="V1")
read.table("foo3", header=TRUE, row.names=1)
read.table("foo3", header=TRUE, row.names="row.names")
read.table("foo3", header=TRUE, row.names="head") # fails in 1.2.3

# wrong col.names
try(read.table("foo3", header=TRUE, col.names=letters[1:4]))
unlink("foo3")

# incomplete last line
cat("head\n", 1:2, "\n", 3:4, file = "foo4")
read.table("foo4", header=TRUE)
unlink("foo4")

# blank last line
cat("head\n\n", 1:2, "\n", 3:4, "\n\n", file = "foo5")
read.table("foo5", header=TRUE)

# test of fill
read.table("foo5", header=FALSE, fill=TRUE, blank.lines.skip=FALSE) # fails in 1.2.3
unlink("foo5")

cat("head\n", 1:2, "\n", 3:5, "\n", 6:9, "\n", file = "foo6")
try(read.table("foo6", header=TRUE))
try(read.table("foo6", header=TRUE, fill=TRUE))
read.table("foo6", header=FALSE, fill=TRUE)
unlink("foo6")

# test of type conversion in 1.4.0 and later.
cat("A B C D E F\n",
    "1 1 1.1 1.1+0i NA F abc\n",
    "2 NA NA NA NA NA NA\n",
    "3 1 2 3 NA TRUE def\n",
    sep = "", file = "foo7")
(res <- read.table("foo7"))
sapply(res, typeof)
sapply(res, class)
(res2 <- read.table("foo7",
                    colClasses = c("character", rep("numeric", 2),
                    "complex", "integer", "logical", "character")))
sapply(res2, typeof)
sapply(res2, class)
unlink("foo7")

# should be logical
type.convert(character(0))

# test of comments in data files
cat("# a test file",
    "# line 2",
    "# line 3",
    "# line 4",
    "# line 5",
    "## now the header",
    " a b c",
    "# some more comments",
    "1 2 3",
    "4 5 6# this is the second data row of the file",
    "  # some more comments",
    "7 8 9",
    "# trailing comment\n",
    file= "ex.data", sep="\n")
read.table("ex.data", header = T)
unlink("ex.data")

## comment chars in headers
cat("x1\tx#2\tx3\n1\t2\t2\n2\t3\t3\n", file = "test.dat")
read.table("test.dat", header=T, comment.char="")
unlink("test.dat")

cat('#comment\n\n#another\n#\n#\n',
    'C1\tC2\tC3\n"Panel"\t"Area Examined"\t"# Blemishes"\n',
    '"1"\t"0.8"\t"3"\n', '"2"\t"0.6"\t"2"\n', '"3"\t"0.8"\t"3"\n',
    file = "test.dat", sep="")
read.table("test.dat")
unlink("test.dat")

cat('%comment\n\n%another\n%\n%\n',
    'C1\tC2\tC3\n"Panel"\t"Area Examined"\t"% Blemishes"\n',
    '"1"\t"0.8"\t"3"\n', '"2"\t"0.6"\t"2"\n', '"3"\t"0.8"\t"3"\n',
    file = "test.dat", sep="")
read.table("test.dat", comment.char = "%")
unlink("test.dat")

## test on Windows Unicode file
con <- file(file.path(Sys.getenv("SRCDIR"), "WinUnicode.dat"),
            encoding="UCS-2LE")
scan(con, 0, quiet=TRUE)
close(con)

## tests of allowEscape
x <- "1 2 3 \\ab\\c"
writeLines(x, "test.dat")
readLines("test.dat")
scan("test.dat", "", allowEscapes=TRUE)
scan("test.dat", "", allowEscapes=FALSE)
read.table("test.dat", header=FALSE, allowEscapes=TRUE)
read.table("test.dat", header=FALSE, allowEscapes=FALSE)
x <- c("TEST", 1, 2, "\\b", 4, 5, "\\040", "\\x20",
       "c:\\spencer\\tests",
       "\\t", "\\n", "\\r")
writeLines(x, "test.dat")
read.table("test.dat", allowEscapes=FALSE, header = TRUE)
unlink("test.dat")
## end of tests


## Tests of "seek".  The "pr" argument can be "print" for debugging.

seek_test <- function (file, encoding="", pr=invisible) {

    # Do it with file contents actually all ASCII.

    cat("With ASCII only\n")

    con <- file (file)
    open (con, "w")
    lets <- rep (paste (LETTERS, collapse=""), 100)
    substring(lets[1],3,3) <- ":"
    lets[2] <- paste (letters, collapse="")
    writeLines (lets, con)
    close (con)

    con <- file (file, encoding=encoding)
    open (con, "r+")
    cat ("seekable?", isSeekable(con), "\n")

    r <- readLines (con)
    pr (seek (con, NA))
    stopifnot (identical (r, lets))

    pr (seek (con, 0))
    r <- readLines (con, n=1)
    pr (r)
    pr (L2pos <- seek (con, NA))
    stopifnot (identical (r, lets[1]))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[3]))

    pr (seek (con, L2pos))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))

    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[3]))

    pr (seek (con, L2pos))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))

    cat("--\n")

    uvwxyz <- "uvwyxzuvwyxz..uvwyxzuvwyxz"
    pr (seek (con, L2pos, rw="w"))
    writeLines (rep(uvwxyz,2), con)
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, uvwxyz))

    pr (seek (con, 0))
    r <- readLines (con, n=4)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, c(lets[1],uvwxyz,uvwxyz,lets[4])))

    close (con)
    cat ("OK\n")

    # Do it again with file containing some UTF-8 characters.

    cat("With UTF-8\n")

    con <- file (file)
    open (con, "w", useBytes=TRUE)
    LETRS <- LETTERS
    LETRS[1] <- "\u0100"
    LETRS[5] <- "\u0112"
    lets <- rep (paste (LETRS, collapse=""), 100)
    substring(lets[1],3,3) <- ":"
    letrs <- letters
    letrs[1] <- "\u0101"
    letrs[5] <- "\u0113"
    lets[2] <- paste (letrs, collapse="")
    writeLines (lets, con)
    close (con)

    con <- file (file, encoding=encoding)
    open (con, "r+")
    cat ("seekable?", isSeekable(con), "\n")

    r <- readLines (con)
    pr (seek (con, NA))
    pr(r)
    lets <- r

    pr (seek (con, 0))
    r <- readLines (con, n=1)
    pr (r)
    pr (L2pos <- seek (con, NA))
    stopifnot (identical (r, lets[1]))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[3]))

    pr (seek (con, L2pos))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))

    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[3]))

    pr (seek (con, L2pos))
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, lets[2]))

    cat("--\n")

    uvwxyz <- "\u0101vwy\u0102zuvwyxz..uvwyxzuvwyxz"
    pr (seek (con, L2pos, rw="w"))
    writeLines (rep(uvwxyz,2), con)
    r <- readLines (con, n=1)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, enc2native(uvwxyz)))

    pr (seek (con, 0))
    r <- readLines (con, n=4)
    pr (r)
    pr (seek (con, NA))
    stopifnot (identical (r, 
                c(lets[1],enc2native(uvwxyz),enc2native(uvwxyz),lets[4])))

    close (con)
    cat ("OK\n")
}

tf <- tempfile()

seek_test (tf)

seek_test (tf, "UTF-8")
