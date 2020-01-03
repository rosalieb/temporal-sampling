

# Build ar-1 time series with various model inputs

build_time_series <- function(mu=0,sigma=2,phi=0.5,shift_size=0.25,shift_time=50,max_time=100){
  
  ar1 <- vector(mode = 'numeric',max_time)
  ar1[1] <- 100
  #shift_time = sample(5:(max_time-5),size = 1)
  for (t in 2:max_time){
    if(t == shift_time){ar1[t-1]=(1-shift_size)*ar1[1]}
    ar1[t] <- ar1[t-1] + phi*rnorm(1,mean=mu,sd = sigma)
  }
  return(ar1) 
}
