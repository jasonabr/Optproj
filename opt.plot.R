x <- gen.supp(10000, c(-10, 10))
A <- read.table('inforf.txt'); A <- as.matrix(A);
A.i <- solve(A)
tr <- rep(0, length(x))
d <- rep(0, length(x))
for(i in 1:(length(x))){
  infor <- infor1(x[i], theta, 1, 10)
  tr[i] <- sum(diag(A.i*infor));
  d[i]<-tr[i]-4;
}
plot(x, d, 'l', col="blue")
abline(h=0)

  op.