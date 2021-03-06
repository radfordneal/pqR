# Test operations done in helper threads or with task merging, against
# saved results and results with multithreading and merging disabled.
#
# Added for pqR, 2014, 2015 Radford M. Neal.

# These tests are designed for pqR, but should run without error in 
# other R implementations, since the option settings should simply
# be ignored.  The number of helper threads used can be set using
# the R_HELPERS environment variable.


# RANDOM TEST PROCEDURE.  Returns a list with scalars a, b, c, d, e, f, g, h, 
# vectors t, u, v, w, and matrices X, Y, Z.  Contains lots of code generated
# randomly with the script given.

do_helpers_tests <- function(len,seed=1)
{
    set.seed(seed)

    gen <- function (n) sample(c(-1,+1),1) * (rpois(n,2) + 1)

    a <- gen(1); b <- gen(1); c <- gen(1); d <- gen(1)
    e <- gen(1); f <- gen(1); g <- gen(1); h <- gen(1)

    t <- gen(len); u <- gen(len); v <- gen(len); w <- gen(len);

    X <- matrix(gen(len*len),len,len)
    Y <- matrix(gen(len*len),len,len)
    Z <- matrix(gen(len*len),len,len)

    # The operations below were randomly generated with the following script:
    #
    # set.seed(1)
    # 
    # svars <- c("a","b","c","d","e","f","g","h")
    # vvars <- c("t","u","v","w")
    # mvars <- c("X","Y","Z")
    # arith <- c("+","-")
    # sp <- "   "
    # 
    # for (i in 1:400) {
    #     P <- runif(1)
    #     Q <- runif(1)
    #     R <- runif(1)
    #     if (P<0.1) {
    #         dst <- sample(svars,1)
    #         if (Q<0.4)
    #             cat(sp,dst,"<- 2 *",dst,"+ sum(",sample(vvars,1),")\n")
    #         else if (Q<0.8)
    #             cat(sp,dst,"<- 2 *",dst,"+ sum(",sample(mvars,1),")\n")
    #         else
    #             cat(sp,dst,"<- 2 *",dst,"+ as.vector( t(",
    #                            sample(vvars,1),") %*%",sample(vvars,1),")\n")
    #     }
    #     else if (P<0.6) {
    #         dst <- sample(vvars,1)
    #         if (Q<0.1)
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,sample(arith,1),"2\n")
    #             else
    #                 cat(sp,dst,"<- 2",sample(arith,1),dst,"\n")
    #         else if (Q<0.2)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- t( t(",dst,") %*%",sample(mvars,1),")\n")
    #             else
    #                 cat(sp,dst,"<-",sample(mvars,1),"%*%",dst,"\n")
    #         else if (Q<0.25)
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,"+ colSums(",sample(mvars,1),")\n")
    #             else
    #                 cat(sp,dst,"<-",dst,"+ rowSums(",sample(mvars,1),")\n")
    #         else if (Q<0.4)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- floor(1 + 0.03 *",dst,")\n")
    #             else
    #                 cat(sp,dst,"<- floor(1 - 0.03 *",dst,")\n")
    #         else if (Q<0.5)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- floor(5 / (1 + exp(-",dst,")))\n")
    #             else
    #                 cat(sp,dst,"<- -floor(5 / (1 + exp(",dst,")))\n")
    #         else if (Q<0.65)
    #             cat(sp,dst,"<- ceiling(log(1 +",dst,"^ 2))\n")
    #         else
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,sample(arith,1),
    #                                   sample(setdiff(vvars,dst),1),"\n")
    #             else
    #                 cat(sp,dst,"<-",sample(setdiff(vvars,dst),1),
    #                                   sample(arith,1),dst,"\n")
    #     }
    #     else {
    #         dst <- sample(mvars,1)
    #         if (Q<0.1)
    #             cat(sp,dst,"<- t(",dst,")\n")
    #         else if (Q<0.2)
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,sample(arith,1),"2\n")
    #             else
    #                 cat(sp,dst,"<- 2",sample(arith,1),dst,"\n")
    #         else if (Q<0.3)
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,sample(arith,1),
    #                                   sample(setdiff(mvars,dst),1),"\n")
    #             else
    #                 cat(sp,dst,"<-",sample(setdiff(mvars,dst),1),
    #                                   sample(arith,1),dst,"\n")
    #         else if (Q<0.4)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- floor(1 + 0.05 *",dst,")\n")
    #             else
    #                 cat(sp,dst,"<- floor(1 - 0.05 *",dst,")\n")
    #         else if (Q<0.65)
    #             cat(sp,dst,"<- ceiling(log(1 +",dst,"^ 2))\n")
    #         else if (Q<0.8)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- floor(5 / (1 + exp(-",dst,")))\n")
    #             else
    #                 cat(sp,dst,"<- -floor(5 / (1 + exp(",dst,")))\n")
    #         else if (Q<0.85)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- crossprod(",dst,")\n")
    #             else
    #                 cat(sp,dst,"<- tcrossprod(",dst,")\n")
    #         else if (Q<0.90)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- crossprod(",dst,",",sample(mvars,1),")\n")
    #             else
    #                 cat(sp,dst,"<- crossprod(",sample(mvars,1),",",dst,")\n")
    #         else if (Q<0.95)
    #             if (R<0.5)
    #                 cat(sp,dst,"<- tcrossprod(",dst,",",sample(mvars,1),")\n")
    #             else
    #                 cat(sp,dst,"<- tcrossprod(",sample(mvars,1),",",dst,")\n")
    #         else
    #             if (R<0.5)
    #                 cat(sp,dst,"<-",dst,"%*%",sample(mvars,1),"\n")
    #             else
    #                 cat(sp,dst,"<-",sample(mvars,1),"%*%",dst,"\n")
    #     }
    # }

    w <- floor(1 - 0.03 * w )
    v <- u + v 
    u <- Z %*% u 
    u <- w - u 
    u <- u + t 
    u <- u - v 
    w <- t - w 
    w <- u - w 
    Z <- t( Z )
    Y <- ceiling(log(1 + Y ^ 2))
    u <- u - 2
    X <- ceiling(log(1 + X ^ 2))
    u <- floor(1 - 0.03 * u )
    w <- w + v 
    w <- floor(1 + 0.03 * w )
    Z <- floor(1 - 0.05 * Z )
    u <- u - t 
    X <- X + 2
    g <- 2 * g + sum( Z )
    v <- -floor(5 / (1 + exp( v )))
    Z <- floor(1 + 0.05 * Z )
    Y <- Y - Z 
    Y <- floor(5 / (1 + exp(- Y )))
    t <- 2 + t 
    Y <- Y %*% X 
    X <- ceiling(log(1 + X ^ 2))
    t <- ceiling(log(1 + t ^ 2))
    e <- 2 * e + sum( Y )
    v <- u + v 
    t <- t - u 
    X <- ceiling(log(1 + X ^ 2))
    v <- ceiling(log(1 + v ^ 2))
    c <- 2 * c + sum( w )
    u <- t + u 
    Z <- floor(1 - 0.05 * Z )
    Z <- floor(1 + 0.05 * Z )
    Z <- -floor(5 / (1 + exp( Z )))
    v <- Z %*% v 
    w <- u - w 
    u <- Y %*% u 
    w <- w + t 
    v <- floor(1 + 0.03 * v )
    v <- t( t( v ) %*% Z )
    w <- t - w 
    X <- X %*% X 
    v <- ceiling(log(1 + v ^ 2))
    w <- w + 2
    u <- floor(1 - 0.03 * u )
    Y <- -floor(5 / (1 + exp( Y )))
    w <- floor(1 - 0.03 * w )
    u <- floor(5 / (1 + exp(- u )))
    w <- -floor(5 / (1 + exp( w )))
    Y <- ceiling(log(1 + Y ^ 2))
    u <- u + colSums( Z )
    w <- floor(1 + 0.03 * w )
    Z <- Z - 2
    u <- u + 2
    t <- t( t( t ) %*% Y )
    Z <- t( Z )
    u <- u + 2
    t <- t + colSums( Z )
    t <- ceiling(log(1 + t ^ 2))
    v <- 2 + v 
    w <- floor(5 / (1 + exp(- w )))
    u <- u + t 
    Y <- floor(5 / (1 + exp(- Y )))
    e <- 2 * e + sum( v )
    w <- -floor(5 / (1 + exp( w )))
    X <- tcrossprod( X )
    Y <- Y %*% Z 
    b <- 2 * b + sum( v )
    Y <- Y %*% Y 
    X <- X + Z 
    u <- 2 - u 
    w <- w + t 
    v <- v - t 
    Y <- floor(5 / (1 + exp(- Y )))
    t <- t + v 
    Y <- Y - 2
    X <- X %*% Z 
    X <- 2 + X 
    Y <- ceiling(log(1 + Y ^ 2))
    X <- floor(1 + 0.05 * X )
    Z <- 2 - Z 
    X <- floor(1 - 0.05 * X )
    Z <- Z %*% Z 
    X <- ceiling(log(1 + X ^ 2))
    Y <- crossprod( Y )
    Y <- Y + 2
    Z <- tcrossprod( Z )
    a <- 2 * a + sum( w )
    Z <- t( Z )
    e <- 2 * e + sum( u )
    t <- floor(1 - 0.03 * t )
    c <- 2 * c + as.vector( t( t ) %*% t )
    Y <- ceiling(log(1 + Y ^ 2))
    a <- 2 * a + sum( u )
    t <- v - t 
    u <- t + u 
    Z <- ceiling(log(1 + Z ^ 2))
    Z <- 2 - Z 
    w <- t( t( w ) %*% X )
    b <- 2 * b + as.vector( t( u ) %*% u )
    t <- ceiling(log(1 + t ^ 2))
    v <- u - v 
    v <- w + v 
    b <- 2 * b + sum( Y )
    w <- floor(1 - 0.03 * w )
    Y <- floor(1 + 0.05 * Y )
    Z <- tcrossprod( Z )
    Y <- ceiling(log(1 + Y ^ 2))
    u <- t( t( u ) %*% Y )
    w <- ceiling(log(1 + w ^ 2))
    X <- floor(1 + 0.05 * X )
    Z <- floor(1 - 0.05 * Z )
    Z <- ceiling(log(1 + Z ^ 2))
    X <- floor(1 + 0.05 * X )
    X <- tcrossprod( X )
    v <- t - v 
    u <- u + t 
    Y <- ceiling(log(1 + Y ^ 2))
    Z <- floor(5 / (1 + exp(- Z )))
    Y <- t( Y )
    f <- 2 * f + sum( Y )
    w <- u + w 
    w <- floor(1 + 0.03 * w )
    Z <- 2 - Z 
    t <- floor(1 - 0.03 * t )
    Y <- floor(1 - 0.05 * Y )
    Y <- ceiling(log(1 + Y ^ 2))
    X <- t( X )
    v <- t( t( v ) %*% X )
    Z <- ceiling(log(1 + Z ^ 2))
    u <- floor(1 - 0.03 * u )
    v <- v - 2
    X <- floor(1 - 0.05 * X )
    Z <- Z - Y 
    Y <- crossprod( Y )
    v <- v - 2
    X <- floor(1 - 0.05 * X )
    Z <- floor(1 - 0.05 * Z )
    w <- floor(1 - 0.03 * w )
    v <- u + v 
    w <- floor(1 + 0.03 * w )
    Y <- 2 - Y 
    w <- floor(5 / (1 + exp(- w )))
    e <- 2 * e + sum( Y )
    w <- w + v 
    Y <- -floor(5 / (1 + exp( Y )))
    h <- 2 * h + sum( Z )
    Z <- ceiling(log(1 + Z ^ 2))
    t <- floor(5 / (1 + exp(- t )))
    v <- v - w 
    v <- v + t 
    X <- ceiling(log(1 + X ^ 2))
    Z <- Z + X 
    u <- ceiling(log(1 + u ^ 2))
    w <- floor(1 + 0.03 * w )
    w <- w - t 
    t <- t( t( t ) %*% Z )
    w <- w + t 
    w <- ceiling(log(1 + w ^ 2))
    X <- ceiling(log(1 + X ^ 2))
    X <- ceiling(log(1 + X ^ 2))
    Z <- Y - Z 
    v <- v + u 
    h <- 2 * h + sum( u )
    v <- ceiling(log(1 + v ^ 2))
    X <- floor(1 - 0.05 * X )
    Z <- floor(5 / (1 + exp(- Z )))
    t <- t + v 
    v <- v - w 
    v <- v - w 
    Y <- t( Y )
    w <- v - w 
    w <- ceiling(log(1 + w ^ 2))
    X <- ceiling(log(1 + X ^ 2))
    u <- floor(5 / (1 + exp(- u )))
    c <- 2 * c + sum( Z )
    X <- t( X )
    b <- 2 * b + sum( t )
    a <- 2 * a + sum( t )
    v <- -floor(5 / (1 + exp( v )))
    g <- 2 * g + sum( X )
    u <- u - 2
    Z <- floor(1 - 0.05 * Z )
    Z <- ceiling(log(1 + Z ^ 2))
    X <- ceiling(log(1 + X ^ 2))
    c <- 2 * c + sum( t )
    t <- t - 2
    X <- X + Y 
    v <- v - 2
    Y <- t( Y )
    w <- t - w 
    Z <- t( Z )
    Y <- Z - Y 
    w <- floor(1 - 0.03 * w )
    X <- ceiling(log(1 + X ^ 2))
    u <- u + v 
    Z <- -floor(5 / (1 + exp( Z )))
    g <- 2 * g + sum( v )
    u <- u + v 
    u <- X %*% u 
    v <- w - v 
    t <- t + v 
    X <- crossprod( X )
    Y <- floor(1 + 0.05 * Y )
    v <- floor(1 - 0.03 * v )
    t <- -floor(5 / (1 + exp( t )))
    u <- t + u 
    w <- w + t 
    v <- ceiling(log(1 + v ^ 2))
    Z <- ceiling(log(1 + Z ^ 2))
    u <- t + u 
    g <- 2 * g + as.vector( t( t ) %*% w )
    v <- floor(1 + 0.03 * v )
    w <- floor(5 / (1 + exp(- w )))
    Y <- Y - 2
    v <- v + colSums( Y )
    u <- -floor(5 / (1 + exp( u )))
    v <- v + w 
    Y <- 2 - Y 
    h <- 2 * h + sum( t )
    t <- t - v 
    u <- 2 - u 
    u <- u + v 
    v <- ceiling(log(1 + v ^ 2))
    Z <- ceiling(log(1 + Z ^ 2))
    t <- -floor(5 / (1 + exp( t )))
    t <- v - t 
    Y <- floor(5 / (1 + exp(- Y )))
    u <- Y %*% u 
    Z <- floor(1 + 0.05 * Z )
    e <- 2 * e + sum( X )
    X <- floor(1 - 0.05 * X )
    Z <- ceiling(log(1 + Z ^ 2))
    X <- X + Z 
    u <- floor(5 / (1 + exp(- u )))
    w <- floor(1 - 0.03 * w )
    w <- t( t( w ) %*% X )
    X <- ceiling(log(1 + X ^ 2))
    X <- 2 - X 
    X <- X %*% X 
    u <- floor(1 + 0.03 * u )
    u <- floor(1 - 0.03 * u )
    X <- Z - X 
    t <- -floor(5 / (1 + exp( t )))
    X <- floor(5 / (1 + exp(- X )))
    u <- floor(1 + 0.03 * u )
    Y <- Y + 2
    b <- 2 * b + sum( Y )
    v <- t + v 
    v <- w - v 
    Z <- crossprod( Z , X )
    Y <- t( Y )
    Y <- Y %*% X 
    w <- 2 + w 
    w <- w - 2
    t <- floor(1 + 0.03 * t )
    d <- 2 * d + as.vector( t( u ) %*% v )
    u <- ceiling(log(1 + u ^ 2))
    Z <- tcrossprod( Z , Z )
    g <- 2 * g + sum( w )
    v <- t( t( v ) %*% Z )
    c <- 2 * c + sum( v )
    w <- floor(1 - 0.03 * w )
    d <- 2 * d + sum( w )
    X <- -floor(5 / (1 + exp( X )))
    X <- -floor(5 / (1 + exp( X )))
    w <- floor(1 - 0.03 * w )
    u <- floor(1 - 0.03 * u )
    w <- 2 + w 
    d <- 2 * d + sum( Y )
    b <- 2 * b + sum( Z )
    Y <- floor(5 / (1 + exp(- Y )))
    Y <- ceiling(log(1 + Y ^ 2))
    w <- t + w 
    X <- ceiling(log(1 + X ^ 2))
    Z <- floor(1 + 0.05 * Z )
    w <- w + 2
    Z <- t( Z )
    Z <- ceiling(log(1 + Z ^ 2))
    w <- t( t( w ) %*% X )
    t <- t( t( t ) %*% Z )
    X <- t( X )
    v <- v + rowSums( X )
    w <- t + w 
    u <- floor(1 - 0.03 * u )
    t <- ceiling(log(1 + t ^ 2))
    w <- floor(1 - 0.03 * w )
    Y <- crossprod( Y )
    X <- ceiling(log(1 + X ^ 2))
    h <- 2 * h + sum( v )
    t <- ceiling(log(1 + t ^ 2))
    Z <- floor(5 / (1 + exp(- Z )))
    X <- floor(1 - 0.05 * X )
    Y <- Y %*% Z 
    t <- t + 2
    u <- 2 - u 
    Y <- floor(1 - 0.05 * Y )
    X <- -floor(5 / (1 + exp( X )))
    v <- v + 2
    Y <- t( Y )
    v <- v - w 
    t <- t - w 
    t <- floor(5 / (1 + exp(- t )))
    X <- ceiling(log(1 + X ^ 2))
    Z <- crossprod( Z )
    c <- 2 * c + sum( u )
    X <- floor(1 + 0.05 * X )
    t <- u - t 
    Y <- Y - 2
    t <- u + t 
    w <- w - t 
    Z <- t( Z )
    Z <- Y - Z 
    t <- 2 - t 
    v <- floor(1 - 0.03 * v )
    f <- 2 * f + sum( Z )
    Z <- crossprod( Z )
    u <- floor(1 + 0.03 * u )
    X <- -floor(5 / (1 + exp( X )))
    Y <- ceiling(log(1 + Y ^ 2))
    Z <- Z %*% X 
    Y <- 2 + Y 
    t <- floor(1 + 0.03 * t )
    Z <- crossprod( Z )
    t <- -floor(5 / (1 + exp( t )))
    e <- 2 * e + as.vector( t( w ) %*% u )
    X <- crossprod( Z , X )
    Z <- t( Z )
    Y <- crossprod( X , Y )
    u <- w - u 
    u <- u + rowSums( Z )
    Y <- tcrossprod( Y )
    Y <- ceiling(log(1 + Y ^ 2))
    t <- t + u 
    a <- 2 * a + sum( t )
    g <- 2 * g + sum( t )
    t <- floor(1 + 0.03 * t )
    v <- v - w 
    Z <- floor(1 + 0.05 * Z )
    X <- ceiling(log(1 + X ^ 2))
    Z <- crossprod( Z , Y )
    Z <- Z %*% X 
    a <- 2 * a + sum( Z )
    g <- 2 * g + sum( Y )
    X <- floor(1 - 0.05 * X )
    t <- u + t 
    Y <- floor(5 / (1 + exp(- Y )))
    u <- floor(1 + 0.03 * u )
    Z <- ceiling(log(1 + Z ^ 2))
    u <- ceiling(log(1 + u ^ 2))
    X <- crossprod( Y , X )
    X <- 2 + X 
    Z <- ceiling(log(1 + Z ^ 2))
    Z <- crossprod( Z , X )
    Z <- ceiling(log(1 + Z ^ 2))
    t <- ceiling(log(1 + t ^ 2))
    Z <- ceiling(log(1 + Z ^ 2))
    X <- floor(5 / (1 + exp(- X )))
    u <- 2 + u 
    u <- u + colSums( X )
    Y <- floor(1 + 0.05 * Y )
    w <- w - t 
    v <- floor(1 - 0.03 * v )
    Y <- tcrossprod( Y , Z )
    e <- 2 * e + sum( Y )
    f <- 2 * f + as.vector( t( u ) %*% u )
    c <- 2 * c + sum( u )
    t <- ceiling(log(1 + t ^ 2))
    Y <- t( Y )
    u <- u - v 
    v <- ceiling(log(1 + v ^ 2))
    Y <- floor(1 - 0.05 * Y )
    t <- t + rowSums( X )
    e <- 2 * e + sum( X )
    t <- floor(1 - 0.03 * t )
    w <- Y %*% w 
    Z <- 2 + Z 
    c <- 2 * c + sum( t )
    v <- ceiling(log(1 + v ^ 2))
    v <- Y %*% v 
    t <- t + rowSums( Y )
    t <- v - t 
    X <- floor(1 - 0.05 * X )
    w <- ceiling(log(1 + w ^ 2))
    Y <- Z - Y 
    w <- ceiling(log(1 + w ^ 2))
    h <- 2 * h + sum( Z )
    Y <- t( Y )
    X <- X + 2
    Z <- ceiling(log(1 + Z ^ 2))
    Z <- ceiling(log(1 + Z ^ 2))
    d <- 2 * d + as.vector( t( t ) %*% t )
    w <- u - w 
    Y <- t( Y )
    u <- Z %*% u 
    X <- floor(1 - 0.05 * X )
    t <- t( t( t ) %*% Z )

    # End of randomly generated operations.

    list(a=a,b=b,c=c,d=d,e=e,f=f,g=g,h=h,t=t,u=u,v=v,w=w,X=X,Y=Y,Z=Z)
}


# Print results of test.

show_results <- function (R)
{
    print(rbind (`sums:`=unlist(lapply(R,sum,na.rm=TRUE)),
                 `NaNs:`=unlist(lapply(R,function(x)sum(is.na(x))))))
}


# Do tests for varying dimensions of vectors and matrices, under various
# conditions of multithreading, holding, and task merging being enabled.

for (len in c(1,2,21,22,127,128,129,201,202,513))
{
    cat("\n\nTESTING WITH DIMENSION",len,"\n\n")

    cat("Tests with helpers disabled:\n")
    options(helpers_disable=TRUE)
    R0 <- do_helpers_tests(len)
    show_results(R0)
    
    cat("\nTests with merging and holding, but no multithreading:\n")
    options(helpers_disable=FALSE)
    options(helpers_no_multithreading=TRUE)
    options(helpers_no_merging=FALSE)
    options(helpers_no_holding=FALSE)
    R <- do_helpers_tests(len)
    show_results(R)
    stopifnot(identical(R,R0))
    
    cat("\nTests with holding and multithreading, but no merging:\n")
    options(helpers_disable=FALSE)
    options(helpers_no_multithreading=FALSE)
    options(helpers_no_merging=TRUE)
    options(helpers_no_holding=FALSE)
    R <- do_helpers_tests(len)
    show_results(R)
    stopifnot(identical(R,R0))
    
    cat("\nTests with merging and multithreading, but no holding:\n")
    options(helpers_disable=FALSE)
    options(helpers_no_multithreading=FALSE)
    options(helpers_no_merging=FALSE)
    options(helpers_no_holding=TRUE)
    R <- do_helpers_tests(len)
    show_results(R)
    stopifnot(identical(R,R0))
    
    cat("\nTests with multithreading, but no merging or holding:\n")
    options(helpers_disable=FALSE)
    options(helpers_no_multithreading=FALSE)
    options(helpers_no_merging=TRUE)
    options(helpers_no_holding=TRUE)
    R <- do_helpers_tests(len)
    show_results(R)
    stopifnot(identical(R,R0))
    
    cat("\nTests with merging, holding, and multithreading:\n")
    options(helpers_disable=FALSE)
    options(helpers_no_multithreading=FALSE)
    options(helpers_no_merging=FALSE)
    options(helpers_no_holding=FALSE)
    R <- do_helpers_tests(len)
    show_results(R)
    stopifnot(identical(R,R0))
}


# TEST THAT WAITS OCCUR WHEN THEY SHOULD.

wait <- invisible  # invisible will wait until its argument has been computed

a <- wait(rep(1/30,100000))
b <- wait(rep(-1/7,100000))

for (f in list(abs,trunc,exp,`-`)) {
    r <- f(wait(b%*%a))
    x <- f(b%*%a)
    g <- x==r
    print(c(x))
    stopifnot(g)
}

for (o in list(`+`,`-`,`*`,`/`,`^`)) {
    r <- o(wait(b%*%a),10)
    x <- o(b%*%a,10)
    g <- x==r
    print(c(x))
    stopifnot(g)
    r <- o(0.9,wait(b%*%a))
    x <- o(0.9,b%*%a)
    g <- x==r
    print(c(x))
    stopifnot(g)
}

nr <- 5000
M <- wait(matrix(1/11,nr,1))

for (f in list(abs,trunc,exp,`-`)) {
    r <- f(wait(.colSums(M,nr,1)))
    x <- f(.colSums(M,nr,1))
    g <- x==r
    print(x)
    stopifnot(g)
}

for (o in list(`+`,`-`,`*`,`/`,`^`)) {
    r <- o(wait(.colSums(M,nr,1)),10)
    x <- o(.colSums(M,nr,1),10)
    g <- x==r
    print(x)
    stopifnot(g)
    r <- o(0.9,wait(.colSums(M,nr,1)))
    x <- o(0.9,.colSums(M,nr,1))
    g <- x==r
    print(x)
    stopifnot(g)
}

nr <- 500
nc <- 1000
M <- wait(matrix(1/3,nr,nc))

for (f in list(abs,trunc,exp,`-`)) {
    r <- f(wait(.colSums(M,nr,nc)))
    x <- f(.colSums(M,nr,nc))
    g <- all(wait(x==r))
    print(x[c(1,nr)])
    stopifnot(g)
}

for (o in list(`+`,`-`,`*`,`/`,`^`)) {
    r <- o(wait(.colSums(M,nr,nc)),10)
    x <- o(.colSums(M,nr,nc),10)
    g <- all(wait(x==r))
    print(x[c(1,nr)])
    stopifnot(g)
    r <- o(0.9,wait(.colSums(M,nr,nc)))
    x <- o(0.9,.colSums(M,nr,nc))
    g <- all(wait(x==r))
    print(x[c(1,nr)])
    stopifnot(g)
}

