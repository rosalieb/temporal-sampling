
# Build ar-1 time series with various model inputs, and a linear transition

build_time_series <- function(mu=0,sigma=0.053,phi=0.404,shift_size=0.81,shift_time=50,max_time=100, shift_duration = 1){
  
  ar1 <- vector(mode = 'numeric',max_time)
  meanXt = 0.899
  ar1[1] <- meanXt
  if(shift_duration !=1) {
    shift_size = shift_size / shift_duration
    shift_time <- rep(shift_time, shift_duration)
    for (i in 2:shift_duration) {
      shift_time[i] <- shift_time[i-1]+1
    }
  }
  
  #shift_time = sample(5:(max_time-5),size = 1)
  for (t in 2:max_time){
    #if(t == shift_time){meanXt = (1-shift_size)*ar1[1]}
    #ar1[t] <- meanXt + phi*(ar1[t-1] - meanXt) + rnorm(1,mean=mu,sd = sigma)
    if(t %in% shift_time){meanXt = meanXt - shift_size;ar1[t]=meanXt}
    else{ ar1[t] <- meanXt + phi*(ar1[t-1] - meanXt) + rnorm(1,mean=mu,sd = sigma)}
  }
  
  ar1 = (ar1-min(ar1))/(max(ar1)-min(ar1))
  return(ar1) 
}

# End of the function