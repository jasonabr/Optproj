gen.supp <- function(step, bound){
  x.step <- (bound[2]-bound[1])/step;
  x.temp <- seq(bound[1], bound[2], x.step);
  x.temp <- t(as.matrix(x.temp))
  return(x.temp)
}

  
  
  