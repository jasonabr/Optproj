ver.equiv <- function(x,inforo, theta, ex.des, wb){
  infor0 <- diag(0, nrow(wb), ncol(wb))
  temp <- -100000
  nx <- 1;
  np <- length(x);
  result <- rep(-10000, 1, np+1);
  coeff = 1;
  temp_inv <- wb%*%ginv(infor0);
  part <- t(temp_inv)%*%ginv(temp_inv%*%t(wb))
}