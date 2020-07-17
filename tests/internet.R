## These are tests that require socket and internet functionality, and
## a working Internet connection.
## We attempt to test for those.

if(!capabilities()["http/ftp"]) {
    warning("no internet capabilities")
    q()
}

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

# test do_download.
a <- available.packages(contrib.url("http://repos.pqR-project.org"))
colnames(a)
a["ggplot2","License"]

# test url connections on http
zz <- url("http://pqR-project.org/internet.html")
readLines(zz)
close(zz)

## check graceful failure:
try(zz <- url("http://foo.bar", "r"))

# and via read.table, test http # and ftp.

(http_dat <- read.table("http://pqR-project.org/test.dat"))
#(ftp_dat  <- read.table("ftp://pqR-project.org/test.dat"))
#identical(http_dat,ftp_dat)

real_dat <- as.data.frame(matrix(c(
  10.1, 20.1, 30, 40, 50.1,
  20.2, 20.2, 30, 40, 50.2,
  30.3, 20.3, 30, 40, 50.3,
  40.4, 20.4, 30, 40, 50.4,
  50.5, 20.5, 30, 40, 50.5,
  60.6, 20.6, 30, 40, 50.6,
  70.7, 20.7, 30, 40, 50.7,
  80.8, 20.8, 30, 40, 50.8,
  90.9, 20.9, 30, 40, 50.9), nrow=9, ncol=5, byrow=TRUE))
identical(http_dat,real_dat)

## everything from here on is directly over sockets
if(!capabilities("sockets")) stop("no socket capabilities")

# do the same thing via sockets (cut-down httpclient)
httpget <- function (url, port = 80)
{
    urlel <- strsplit(url, "/")[[1]]
    if (urlel[1] != "http:") stop("Not an http:// URL")
    host <- urlel[3]
    rurl <- paste(c("", urlel[-(1:3)]), collapse = "/")
    a <- make.socket(host, port = port)
    on.exit(close.socket(a))
    headreq <- paste("HEAD", rurl, 
      "HTTP/1.0\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, headreq)
    head <- read.socket(a, maxlen = 8000)
    b <- strsplit(head, "\n")[[1]]
    if (length(grep("200 OK", b[1])) == 0) stop(b[1])
    len <- as.numeric(strsplit(grep("Content-Length", b, value = TRUE),
                               ":")[[1]][2])
    getreq <- paste("GET", rurl, 
      "HTTP/1.0\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, getreq)
    junk <- read.socket(a, maxlen = nchar(head))
    data <- ""
    b <- strsplit(c(head, junk), "\n")
    nn <- length(b[[1]])
    if (length(b[[2]]) > nn)
        data <- paste(b[[2]][-(1:nn)], collapse = "\n")
    while (nchar(data) < len) {
        data <- paste(data, read.socket(a, maxlen = len - nchar(data)),
                      sep = "")
    }
    strsplit(data, "\n")[[1]]
}

if(nchar(Sys.getenv("http_proxy")) > 0
   || nchar(Sys.getenv("HTTP_PROXY")) > 0) {
    cat("http proxy is set, so skip test of http over sockets\n")
} else {
    httpget("http://repos.pqR-project.org/test.dat")
}
