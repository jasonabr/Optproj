#gamma =0
setwd("C:/users/jasonab/dropbox/mp")
theta <- c(0,1,1,1);
x <- gen.supp(10000, c(-10, 10))
A <- read.table('inforf.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 0, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
#[-10, 10]

par(mfrow=c(2,1))
d1 <- c(-10, -1.5894, 1.5856, 10, 0.1591, 0.3409, 0.3409, 0.1591); l.phi1 <- 1.511;
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, pch=16, xlim=c(-11, 11), ylim=c(0.1, 0.4), xlab="support", ylab="weight", main=expression(paste(gamma, " = 0"))); 
lines(z, w, "h", lty=1)
plot(x, d, 'l', col="blue", xlim=c(-11,11) ,xlab="z", main=expression(d(x,xi) ))
abline(h=4, lty=2)

par(mfrow=c(2,2))
d1 <- c(-5, -1.6171, 1.6101, 5, 0.1784, 0.3216, 0.3212, 0.1789); l.phi1 <- 1.511;
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, pch=16, xlim=c(-6, 6), ylim=c(0.1, 0.4), xlab="support", ylab="weight", main="[-5, 5]"); 
lines(z, w, "h", lty=1)
x <- gen.supp(10000, c(-5, 5))
A <- read.table('infor5.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 0, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue", xlim=c(-6, 6) ,xlab="z", main=expression(d(x,xi)) )
abline(h=4, lty=2)

d1 <- c(-3, -1.3669, 1.3600, 3, 0.3286, 0.1711, 0.1712, 0.3290); l.phi1 <- 1.511;
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, pch=16, xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="support", ylab="weight", main="[-3, 3]"); 
lines(z, w, "h", lty=1)
x <- gen.supp(10000, c(-3, 3))
A <- read.table('infor3.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 0, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue", xlim=c(-4,4) ,xlab="z", main=expression(d(x,xi) ))
abline(h=4, lty=2)
#LaTeX for design
#$k=1$
#\begin{equation*}
#\xi^*=
#\begin{pmatrix*}
#-3 & -1.0440 & 1.0440 &  3\\ 0.3528 &  0.1471 & 0.1471 & 0.3528
#\end{pmatrix*}
#\end{equation*}
#k=1000, gamma large
#\begin{equation*}
#\xi^*=
#\begin{pmatrix*}
#-3 & -0.9124 & 0.9124 &  3\\ 0.2515 &  0.2484 & 0.2484 & 0.2515
#\end{pmatrix*}
#\end{equation*}

