# This is a simple demo, illustrating the functionality of spam.
set.seed(14)

nrow <- 5
ncol <- 7
fmat <- matrix(rnorm(nrow*ncol),nrow)
smat <- as.spam(fmat)


smat[1,]
smat[,1] <- 0
as.spam(smat)


ssmat <- smat %*% t(smat)

b <- c(-2:2)

solve(ssmat,b)

cholssmat <- chol(ssmat)


# works also for large matrices:
set.seed(14)

nz <- 1000
nrow <- 1000
ncol <- 1000
smat <- diag.spam(1,nrow,ncol)

smat[cbind(sample(1:(nrow*ncol),size=nz))] <- runif(nz)

smat <- smat %*% t(smat)


b <- rnorm(nz)

smatinvb <- solve(smat,b)

cholssmat <- chol(smat)


# displaying matrices
opar <- par(no.readonly = TRUE)
par(ask=interactive() &&
            (.Device %in% c("X11","GTK","gnome","windows","quartz")))

display(smat, main="'scatterplot'-type display, very efficient")


options(spam.imagesize=prod(smat@dimension)+1)
display(smat, main="'image'-type display, may be slow and heavy")

par(opar)
