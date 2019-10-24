# compute the tolerance to error against sample size

require(changepoint)

#uncertainty tolerance function
ut <- function(x = NULL, y, sample_size = NULL, type=c("mean","sd"), threshold_error = 1, each = 10) {
  if(!is.vector(y)) stop("y is required and must be a vector")
  if(is.null(x)) x <- 1:length(y)
  if(length(x) != length(y)) stop("x and y must be the same length")
  if(is.null(sample_size)) sample_size <- 3:length(y)
  if(max(sample_size)>length(y))   stop("you cannot sample a greater number than the initial vector y") 
  
  sample_size <- rep(sample_size,each=each)
  
  out_list <- list()
  for (i in 1:length(sample_size)) {
    # Case 1: changepoint in mean
    if("mean" %in% type) {
      # Get the "TRUE" changepoint
      if (i==1) {
        true.cptmean.AMOC.y <- cpt.mean(y, method="AMOC") 
        true.cptmean.AMOC.y <- as.numeric(x[cpts(true.cptmean.AMOC.y)])
        if(length(true.cptmean.AMOC.y)==0) true.cptmean.AMOC.y <- 0
      }
      # Get the changepoint for the subsample
      if (true.cptmean.AMOC.y == 0) cat("No changepoint in mean was detected on the full y serie -- cannot compute the impact of lower sampling.") else
      {
        for (j in 1:length(threshold_error)) {
          # 1. Sample 
          which_samples <- c(1,length(y),sample(2:(length(y)-1), size = sample_size[i],replace = F))
          which_samples <- which_samples[order(which_samples)]
          y2 <- y[which_samples]
          x2 <- x[which_samples]
          
          # 2. Run changepoints 
          cptmean.AMOC.y2 <- cpt.mean(y2, method="AMOC") 
          cptmean.AMOC.y2 <- as.numeric(x2[cpts(cptmean.AMOC.y2)])
          if(length(cptmean.AMOC.y2)==0) cptmean.AMOC.y2 <- 0
          
          # 3. Save outputs
          if (i == 1) 
            out_list$cptmean_out <- data.frame("sample_size"=sample_size[i],
                                                         "threshold_error"=threshold_error[j],
                                                         "cptmean"=ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                         "same_cptmean"=ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)) else
                                                           {
                                                             out_list$cptmean_out <- rbind(out_list$cptmean_out, c(sample_size[i],
                                                                                                                   threshold_error[j],
                                                                                                                   ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                                                                                   ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)))
                                                         }

      }
      
    }
    
    
  }
   
    if("sd" %in% type) {
      # Get the "TRUE" changepoint
      if (i==1) {
        true.cptvar.AMOC.y <- cpt.var(y, method="AMOC") 
        true.cptvar.AMOC.y <- as.numeric(x[cpts(true.cptvar.AMOC.y)])
        if(length(true.cptvar.AMOC.y)==0) true.cptvar.AMOC.y <- 0
      }
      # Get the changepoint for the subsample
      if (true.cptvar.AMOC.y == 0) cat("No changepoint in mean was detected on the full y serie -- cannot compute the impact of lower sampling.") else
      {
        for (j in 1:length(threshold_error)) {
          # 1. Sample 
          which_samples <- c(1,length(y),sample(2:(length(y)-1), size = sample_size[i],replace = F))
          which_samples <- which_samples[order(which_samples)]
          y2 <- y[which_samples]
          x2 <- x[which_samples]
          
          # 2. Run changepoints 
          cptvar.AMOC.y2 <- cpt.var(y2, method="AMOC") 
          cptvar.AMOC.y2 <- as.numeric(x2[cpts(cptvar.AMOC.y2)])
          if(length(cptvar.AMOC.y2)==0) cptvar.AMOC.y2 <- 0
          
          # 3. Save outputs
          if (i == 1) 
            out_list$cptmean_out <- data.frame("sample_size"=sample_size[i],
                                               "threshold_error"=threshold_error[j],
                                               "cptmean"=ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                               "same_cptmean"=ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)) else
                                               {
                                                 out_list$cptmean_out <- rbind(out_list$cptmean_out, c(sample_size[i],
                                                                                                       threshold_error[j],
                                                                                                       ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                                                                                       ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)))
                                               }
          
        }
        
      }
      
      
    }
  
  
  }
  return(out_list)
}

out <-ut(y=y,sample_size=c(10:30), threshold_error = 3)
out
