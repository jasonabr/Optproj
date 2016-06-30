setwd("C:/users/jbradsh1/dropbox/mp")
theta <- c(0,1,1,1);
x <- gen.supp(10000, c(-10, 10))
A <- read.table('infor10.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 1, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue")
abline(h=4)

par(mfrow=c(2,3))
#[-3,3]
# k=1
d1 <- c(-3, -1.3670, 1.3600, 3, 0.3286, 0.1711, 0.1712, 0.3290); l.phi1 <- 1.511;
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, xaxt="n",yaxt="n", pch=16, cex=1,xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);

#k=10
d2 <- c(-3.1, -1.4055, 1.3927, 3.1, 0.3109, 0.1884, 0.1887, 0.3118); l.phi2 <- 1.511;
z <- d2[1:4]; w <- d2[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", , main="k=10"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
#k=50
d3 <- c(-3.2, -0.9137, 0.9136, 3.2, 0.2951, 0.2046, 0.2045, 0.2955)
z <- d3[1:4]; w <- d3[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", , main="k=50"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=.25);
#k=100
d4 <- c(-3, -0.9159, 0.9159, 3, 0.2642, 0.2357, 0.2357, 0.2642)
z <- d4[1:4]; w <- d4[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", , main="k=100"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=.25);
#k=500
d5 <- c(-3, -0.9125, 0.9134, 3, 0.2530, 0.2460, 0.2460, 0.2530)
z <- d5[1:4]; w <- d5[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight",, main="k=500"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=.25);
#k=1000
d6 <- c(-3, -0.9124, 0.9134, 3, 0.2515, 0.2484, 0.2484, 0.2515)
z <- d6[1:4]; w <- d6[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,xlim=c(-4, 4), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", , main="k=1000"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=.25);


#[-5, 5]
par(mfrow=c(2,2))
#k=1
d1 <- c(-5, -1.4849, 1.4849, 5, 0.1940,  0.3058,0.3058, 0.1940)
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-6 ,6), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
#k=10
d2 <- c(-5, -1.506, 1.1554, 5, 0.2350, 0.2650, 0.2650, 0.2350)
z <- d2[1:4]; w <- d2[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-6, 6), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=10"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);
#k=100
d3 <- c(-5, -1.0386, 1.0386, 5, 0.2486, 0.2514, 0.2514, 0.2486)
z <- d3[1:4]; w <- d3[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-6, 6), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=100"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);
#k=1000
d4 <- c(-5, -1.0264, 1.0263, 5, 0.2499, 0.2501, 0.2501, 0.2499)
z <- d4[1:4]; w <- d4[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-6, 6), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1000"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);

par(mfrow=c(2,2))
#k=0.1
d7 <- c(-10, -1.4966, 1.503, 10, 0.1667, 0.333, 0.333, 0.1667)
z <- d7[1:4]; w <- d7[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-11, 11), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);
#k0.001
d8 <- c(-10, -1.2135, 1.2156, 10, 0.2074, 0.2926, 0.2926, 0.2074)
z <- d8[1:4]; w <- d8[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,xlim=c(-11, 11), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=10"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25); 
#k=0.0001
d9<- c(-10, -1.0656, 1.0646, 10, 0.2438, 0.2562, 0.2562, 0.2438)
z <- d9[1:4]; w <- d9[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-11, 11), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=100"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);
#k=0.00001
d10 <- c(-10, -1.0448, 1.0452, 10, 0.2493, 0.2506, 0.2506, 0.2494)
z <- d10[1:4]; w <- d10[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1, xlim=c(-11, 11), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1000"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=0.25);

par(mfrow=c(3,2))
d1 <- c(-3, -1.0440, 1.0440, 3, 0.3528, 0.1471, 0.1471, 0.3528); l.phi1 <- 1.511;
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, xaxt="n",yaxt="n",pch=16, cex=1,   xlim=c(-10.5, 10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="[-3,3]"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
theta <- c(0,1,1,1);
x <- gen.supp(10000, c(-3, 3))
A <- read.table('infor3.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 1, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue", main=expression(d(x,xi)))
abline(h=4, lty=2)

d1 <- c(-5, -1.4849, 1.4849, 5, 0.1940,  0.3058,0.3058, 0.1940)
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, , xaxt="n",yaxt="n",  xlim=c(-10.5 ,10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="[-5,5]"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
setwd("C:/users/jasonab/dropbox/mp")
theta <- c(0,1,1,1);
x <- gen.supp(10000, c(-5, 5))
A <- read.table('infor5.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 1, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue")
abline(h=4, lty=2)

d7 <- c(-10, -1.4966, 1.503, 10, 0.1667, 0.333, 0.333, 0.1667)
z <- d7[1:4]; w <- d7[5:8]; plot(z,w,xaxt="n",yaxt="n", xlim=c(-10.5, 10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="[-10,10]"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
setwd("C:/users/jasonab/dropbox/mp")
theta <- c(0,1,1,1);
x <- gen.supp(10000, c(-10, 10))
A <- read.table('infor10.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  inforx <- infor1(x[i], theta, 1, 1)
  tr[i] <- sum(diag(A.i%*%inforx));
  d[i]<-tr[i];
}
plot(x, d, 'l', col="blue")
abline(h=4, lty=2)

par(mfrow=c(2,2))
#[-10, 10]
#0.1
d1 <- c(-10, -1.5786, 1.5786, 10, 0.1599, 0.3400, 0.3401, 0.1600)
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, , xaxt="n",yaxt="n",  pch=16, cex=1,xlim=c(-10.5 ,10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=0.1"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);

#0.001
d1 <- c(-10, -1.5846, 1.5884, 10, 0.1592, 0.3408, 0.3408, 0.1592)
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, , xaxt="n",yaxt="n", pch=16, cex=1, xlim=c(-10.5 ,10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=0.001"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);
#0.00001
d1 <- c(-10, -1.5894, 1.5854, 10, 0.1591, 0.3409, 0.3408, 0.1592 )
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, , xaxt="n",yaxt="n", pch=16, cex=1, xlim=c(-10.5 ,10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1e-5"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);

#0.0000001
d1 <- c(-10, -1.5894, 1.5856, 10, 0.1591, 0.3409, 0.3408, 0.1592 )
z <- d1[1:4]; w <- d1[5:8]; plot(z,w, , xaxt="n",yaxt="n", pch=16, cex=1, xlim=c(-10.5 ,10.5), ylim=c(0.1, 0.4), xlab="supp", ylab="weight", main="k=1e-7"); lines(z, w, "h")
axis(side=1, at=z); axis(side=2, at=w);



d1 <- c(-3 & -1.0440 & 1.0440 & 3 \\ 0.3528 & 0.0.14710.1471, 0.352)