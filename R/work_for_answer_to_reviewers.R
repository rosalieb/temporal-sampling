x = 1:100

y = -0.000001*x + 1 + rnorm(length(x),0,0.001)

plot(x,y)

vrall <- sample_regular(y, n=length(x), input_vector = T, xcol = 1, DCA_axis = 1, messages = F)

vrall$changepoint



vrsub <- sample_iterative(y, n=10, n2=5, input_vector = T, xcol = 1, DCA_axis = 1, messages = F)

vrsub$changepoint


#pdf(paste0(getwd(),"/Output/Worst_case_scenario_1.pdf"), fonts = "Helvetica", height = 5, width = 8 )
plot(x,y, ylab = "y = -0.000001*x + 1 + rnorm(length(x),0,0.001)")
abline(v=vrsub$changepoint)
points(vrsub$matrix$x ~ vrsub$matrix$index, col="tomato")
points(vrsub$matrix$x ~ vrsub$matrix$index, pch=16, col="tomato")
#dev.off()

summary(lm(vrsub$matrix$x ~ vrsub$matrix$index))

vrsub$changepoint_all$order.found

vrsub$changepoint_all$p.values


# Time serie
y <- build_time_series_complex(shift_duration = 10)
vrall <- sample_regular(y, n=length(x), input_vector = T, xcol = 1, DCA_axis = 1, messages = F)

vrall$changepoint



vrsub <- sample_iterative(y, n=10, n2=5, input_vector = T, xcol = 1, DCA_axis = 1, messages = F)

vrsub$changepoint


#pdf(paste0(getwd(),"/Output/linear_shift.pdf"), fonts = "Helvetica", height = 5, width = 8 )
plot(x,y)
abline(v=vrsub$changepoint)
points(vrsub$matrix$x ~ vrsub$matrix$index, col="tomato")
points(vrsub$matrix$x ~ vrsub$matrix$index, pch=16, col="tomato")
#dev.off()

summary(lm(vrsub$matrix$x ~ vrsub$matrix$index))



source('R/sampling_regular.R')
source('R/sampling_random.R')
source('R/build_time_series.R')
source('R/calc_min_time2.R')
require(tidyverse)

number_sims <- 100
# Find minimum time for different parameter combos

sigma_vec = seq(0.01,0.3,0.02)
phi_vec = seq(-0.8,0.8,0.1)
shift_size_vec = c(0.1,0.4,0.8)

output_theory_approach <- data.frame(expand.grid(sigma_vec,phi_vec,shift_size_vec))
names(output_theory_approach) <- c('sigma_vec', 'phi_vec', 'shift_size_vec')
output_theory_approach$power = 0

#params <- tibble(shift_time = sample(30:70,max(simulation_number),replace = T))

shift_time = sample(30:70,number_sims,replace=T) # Keep shift times constant for each param combo

# Run through params
for (i in 1:nrow(output_theory_approach)){
  
  # Build XX number of time series
  params <- list(sigma=rep(output_theory_approach$sigma_vec[i],number_sims),phi=rep(output_theory_approach$phi_vec[i],number_sims),shift_size=rep(output_theory_approach$shift_size_vec[i],number_sims),shift_time = shift_time)
  
  time_series <- params %>%
    pmap(build_time_series) %>%
    map_dbl(function(df) sample_regular(df,20)$changepoint) 
  
  power <- sum(abs(time_series-shift_time)<5)/number_sims
  
  #print(i)
  output_theory_approach$power[i] <- power
}



write.csv(x = output_theory_approach,file = 'Output/parameter_sensitivity_linear.csv',quote = FALSE )

require(ggplot2)
require(viridis)
ggplot(aes(x=phi_vec,y=sigma_vec,fill=power),data=output_theory_approach) + geom_tile() +
  scale_fill_viridis(discrete=FALSE,begin = 0.1,end=0.9) +
  facet_wrap(~shift_size_vec) +
  labs(fill='Statistical power') +
  xlab("Lag-1 autocorrelation (phi)") +
  ylab("Standard deviation (sigma)") +
  theme_classic(base_size = 14, base_family = "")
