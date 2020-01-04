

# Find min_time required
# Increase sample size until 0.8 power is achieved...

sample_size = 9
power = 0
result=NULL

for (sigma_value in seq(0.5,6,0.3)){
power=0
  
while(power < 0.8 & sample_size<50){
  
  sample_size = sample_size + 1

  params <- tibble(sigma=rep(sigma_value,100))
  
  output <- params %>%
    pmap(build_time_series) %>%
    map_dbl(function(df) sample_regular(df,sample_size)$changepoint) %>%
    mutate(params,detection=.)

output$detection = ifelse(abs(output$detection-50)<5,1,0)
output$detection[is.na(output$detection)]=FALSE

power = sum(output$detection)/nrow(output)


}

result = rbind(result,c(sigma_value,sample_size))  
#print(paste(sigma_value,':',sample_size))

}
