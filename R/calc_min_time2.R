
calc_min_time = function(param_name,param_value,sampling_approach){

#sigma_level = 20
power_level = 0.8
sample_size = 10
gap_size = 5
prob_correct = 0
#combined_correct = NULL

switch(param_name,
       sigma = {params <- tibble(sigma=rep(param_value,50))},
       phi = {params <- tibble(phi=rep(param_value,50))},
       shift_size = {params <- tibble(shift_size=rep(param_value,50))},
       shift_time = {params <- tibble(shift_time=rep(param_value,100))}
)

params$shift_time = sample(30:70,nrow(params),replace = T)  


time_series <- params %>%
  pmap(build_time_series) 

while (prob_correct<power_level & sample_size<85){
  sample_size = sample_size + gap_size

if (sampling_approach == 'regular'){
  output <- time_series %>% map_dbl(function(df) sample_regular(df,sample_size)$changepoint) %>% mutate(params,detection=.)
}else if (sampling_approach == 'random'){
  output <- time_series %>% map_dbl(function(df) sample_random(df,sample_size)$changepoint) %>% mutate(params,detection=.)
}else if (sampling_approach == 'iterative'){
  output <- time_series %>% map_dbl(function(df) sample_iterative(df,sample_size)$changepoint) %>% mutate(params,detection=.)
}else{
    
  }

  
  # Sample regular for each time series
#  output <- time_series %>%
#    map_dbl(function(df) sample_regular(df,sample_size)$changepoint) %>%
#    mutate(params,detection=.)
  
  names(output) = c('param_name','shift_time','detection')
  output$detection[is.na(output$detection)] <- 0
  output$detection <- ifelse(abs(output$detection-output$shift_time)<5,1,0) # If detected changepoint is within 5 time points, record a successful detection
  
  prob_correct <- output %>%
    group_by(param_name) %>%
    summarize(prob_correct = sum(detection==1)/length(param_name/unique(param_name))) %>%
    select(prob_correct)
    #%>%
  #plot(ylab='Probability correct',xlab=paste(param_name),pch=16,cex.lab=1.3,las=1,ylim=c(0,1),col=1)
  
 # combined_correct <- c(combined_correct,prob_correct$prob_correct)

if (as.numeric(prob_correct) > power_level & gap_size > 1){
    sample_size = sample_size - gap_size
    gap_size = 1
    prob_correct=0
  }


  
} # End of while loop  

if(sample_size>10 & sample_size<75){
  return(as.numeric(sample_size))
}else{
  return(NA)
}

} # End of function
