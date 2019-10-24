
# Run a AR-1 time series with a large change forced in the middle

# Set simulation parameters
mu = 0
sigma = 0.1
phi = 1
shift_time = 50
max_time = 100

# Set up vectors
ar1 <- vector(mode = 'numeric',100)
ar1[1] <- 10

# Build time series
for (t in 2:max_time){
  if(t == shift_time){ar1[t-1]=-10}
  ar1[t] <- ar1[t-1] + phi*rnorm(1,mean=mu,sd = sigma)

}

# standarize the data
ar1_s <- (ar1 - min(ar1))/(max(ar1) - min(ar1))


plot(ar1_s,ylim=c(0,1),type='l')
