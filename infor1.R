infor1 <- function(t, theta, k, ss){
  ssr <- ss^-1; gamma=sqrt(ss*k)
  alpha <- theta[1];
  beta <- theta[2];
  mu <- exp(alpha+beta*t)/(1+exp(alpha+beta*t))^2;
  ft <- c(1,t)
  infor <-diag(1,4,4);
  infor[1:2, 1:2] <- mu*ft%*%t(ft)+ssr*gamma^2*mu^2*ft%*%t(ft);
  infor[1:2, 3:4] <- -ssr*gamma*mu*ft%*%t(ft);
  infor[3:4, 1:2] <- infor[1:2, 3:4];
  infor[3:4, 3:4] <- ssr*ft%*%t(ft);
  return(infor)
}

  
  
