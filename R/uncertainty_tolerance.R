# compute the tolerance to error against sample size

require(changepoint)

#uncertainty tolerance function
ut <- function(x = NULL, y, sample_size = NULL, type=c("mean","sd"),
               threshold_error = 1, each = 10,
               random_sampling=T,
               regular_sampling=T,
               convergence_sampling=T) {
  if(!is.vector(y)) stop("y is required and must be a vector")
  if(is.null(x)) x <- 1:length(y)
  if(length(x) != length(y)) stop("x and y must be the same length")
  if(is.null(sample_size)) sample_size <- 3:length(y)
  if(max(sample_size)>length(y))   stop("you cannot sample a greater number than the initial vector y") 
  
  
  out_list <- list()
  
  # Get the "TRUE" changepoint in mean
  if("mean" %in% type) {
    true.cptmean.AMOC.y <- cpt.mean(y, method="AMOC") 
    true.cptmean.AMOC.y <- as.numeric(x[cpts(true.cptmean.AMOC.y)])
    if(length(true.cptmean.AMOC.y)==0) true.cptmean.AMOC.y <- 0
    if (true.cptmean.AMOC.y == 0) cat("No changepoint in mean was detected on the full y serie -- cannot compute the impact of lower sampling.")
  }  
  # Get the "TRUE" changepoint in variance
  if("sd" %in% type) {
    true.cptvar.AMOC.y <- cpt.var(y, method="AMOC") 
    true.cptvar.AMOC.y <- as.numeric(x[cpts(true.cptvar.AMOC.y)])
    if(length(true.cptvar.AMOC.y)==0) true.cptvar.AMOC.y <- 0
    if (true.cptvar.AMOC.y == 0) cat("No changepoint in variance was detected on the full y serie -- cannot compute the impact of lower sampling.")
  }  
  
  # I - Regular sampling ####
  if(regular_sampling) {
    for (i in seq_along(unique(sample_size))) {
      # 1. Sample 
      which_samples <- seq(1,length(y), length.out = unique(sample_size)[i])
      which_samples <- round(which_samples)
      which_samples <- which_samples[order(which_samples)]
      y2 <- y[which_samples]
      x2 <- x[which_samples]
      
      # 2.1 Case 1: changepoint in mean
      if("mean" %in% type) {
        
        # Get the changepoint for the subsample
        if (true.cptmean.AMOC.y != 0) 
        {
          for (j in 1:length(threshold_error)) {
            # 2.1.1. Run changepoints 
            cptmean.AMOC.y2 <- cpt.mean(y2, method="AMOC") 
            cptmean.AMOC.y2 <- as.numeric(x2[cpts(cptmean.AMOC.y2)])
            if(length(cptmean.AMOC.y2)==0) cptmean.AMOC.y2 <- 0
            
            # 2.1.2. Save outputs
            if (i == 1 & j == 1) 
              out_list$regular_cptmean <- data.frame("sample_size"=sample_size[i],
                                                 "threshold_error"=threshold_error[j],
                                                 "cptmean"=ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                 "same_cptmean"=ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)) else
                                                 {
                                                   out_list$regular_cptmean <- rbind(out_list$regular_cptmean, c(sample_size[i],
                                                                                                         threshold_error[j],
                                                                                                         ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                                                                         ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)))
                                                 }
            
          }
        }
      }
      
      # 2.2 Case 1: changepoint in variance
      if("sd" %in% type) {
        if (true.cptvar.AMOC.y != 0)
        {
          for (j in 1:length(threshold_error)) {
            # 2.2.1 Run changepoints 
            cptvar.AMOC.y2 <- cpt.var(y2, method="AMOC") 
            cptvar.AMOC.y2 <- as.numeric(x2[cpts(cptvar.AMOC.y2)])
            if(length(cptvar.AMOC.y2)==0) cptvar.AMOC.y2 <- 0
            
            # 2.2.2. Save outputs
            if (i == 1 & j == 1) 
              out_list$regular_cptvar <- data.frame("sample_size"=sample_size[i],
                                                 "threshold_error"=threshold_error[j],
                                                 "cptmean"=ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                                 "same_cptmean"=ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)) else
                                                 {
                                                   out_list$regular_cptvar <- rbind(out_list$regular_cptvar, c(sample_size[i],
                                                                                                         threshold_error[j],
                                                                                                         ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                                                                                         ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)))
                                                 }
            
          }
          
        }
        
        
      }
      
      
    }
  }
  
  # II - Random sampling ####
  if(random_sampling) {
    # 1. Iterations
    sample_size <- rep(sample_size,each=each)
    
    for (i in 1:length(sample_size)) {
      # 2. Case 1: changepoint in mean
      if("mean" %in% type) {
        # Get the changepoint for the subsample
        if (true.cptmean.AMOC.y != 0) {
          for (j in 1:length(threshold_error)) {
            # 2.1. Sample 
            which_samples <- c(1,length(y),sample(2:(length(y)-1), size = c(sample_size[i]-2),replace = F))
            which_samples <- which_samples[order(which_samples)]
            y2 <- y[which_samples]
            x2 <- x[which_samples]
            
            # 2.2. Run changepoints 
            cptmean.AMOC.y2 <- cpt.mean(y2, method="AMOC") 
            cptmean.AMOC.y2 <- as.numeric(x2[cpts(cptmean.AMOC.y2)])
            if(length(cptmean.AMOC.y2)==0) cptmean.AMOC.y2 <- 0
            
            # 2.3. Save outputs
            if (i == 1 & j == 1) 
              out_list$random_cptmean <- data.frame("sample_size"=sample_size[i],
                                                 "threshold_error"=threshold_error[j],
                                                 "cptmean"=ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                 "same_cptmean"=ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)) else
                                                 {
                                                   out_list$random_cptmean <- rbind(out_list$random_cptmean, c(sample_size[i],
                                                                                                         threshold_error[j],
                                                                                                         ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                                                                         ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)))
                                                 }
            
          }
        }
      }
      
      # 3. Case 1: changepoint in mean
      if("sd" %in% type) {
        if (true.cptvar.AMOC.y != 0) {
          for (j in 1:length(threshold_error)) {
            # 3.1. Sample 
            which_samples <- c(1,length(y),sample(2:(length(y)-1), size = c(sample_size[i]-2),replace = F))
            which_samples <- which_samples[order(which_samples)]
            y2 <- y[which_samples]
            x2 <- x[which_samples]
            
            # 3.2. Run changepoints 
            cptvar.AMOC.y2 <- cpt.var(y2, method="AMOC") 
            cptvar.AMOC.y2 <- as.numeric(x2[cpts(cptvar.AMOC.y2)])
            if(length(cptvar.AMOC.y2)==0) cptvar.AMOC.y2 <- 0
            
            # 3.3. Save outputs
            if (i == 1 & j == 1) 
              out_list$random_cptvar <- data.frame("sample_size"=sample_size[i],
                                                 "threshold_error"=threshold_error[j],
                                                 "cptvar"=ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                                 "same_cptvar"=ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)) else
                                                 {
                                                   out_list$random_cptvar <- rbind(out_list$random_cptvar, c(sample_size[i],
                                                                                                         threshold_error[j],
                                                                                                         ifelse(cptvar.AMOC.y2==0,NA,cptvar.AMOC.y2),
                                                                                                         ifelse(abs(cptvar.AMOC.y2-true.cptvar.AMOC.y)<=threshold_error[j], 1, 0)))
                                                 }
            
          }
        }
      }
    }
  }
  
  # III - Convergence sampling ####
  if(convergence_sampling) {
    # Initial vector
    sample_size <- unique(sample_size)
    
    for (i in 1:length(sample_size)) {
      # 1. Sample 
      which_samples <- seq(1,length(y),length.out = sample_size[i])
      which_samples <- round(which_samples)
      which_samples <- which_samples[order(which_samples)]
      y2 <- y[which_samples]
      x2 <- x[which_samples]
      
      which_samples_reset <- which_samples
      
      # 2. Case 1: changepoint in mean
      if("mean" %in% type) {
        if (true.cptmean.AMOC.y != 0) {
          # 2. Run changepoints for the initial subsample
          cptmean.AMOC.y2 <- cpt.mean(y2, method="AMOC") 
          num <- cpts(cptmean.AMOC.y2)
          cptmean.AMOC.y2 <- as.numeric(x2[cpts(cptmean.AMOC.y2)])
          if(length(cptmean.AMOC.y2)==0) cptmean.AMOC.y2 <- 0
          num_reset <- num
          
          for (j in seq_along(threshold_error)) {
            which_samples <- which_samples_reset
            num <- num_reset
            print(paste0(sample_size[i],"-",threshold_error[j]))
            
            # 3. Decision
            t1 = sample_size[i] # The while function doesn't stop sometimes, so stopping the function once we reached the max length of the dataset
            while(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)>threshold_error[j] & t1 < length(y)) {
              t1 = t1 + 1
              # 3.1. Add an extra point
              # random point is a random sample between change point and following sample
              if (length(c(which_samples[num] + 1):c(which_samples[num + 1] - 1))>0) additional_sample <- sample(c(which_samples[num]+1):c(which_samples[num+1]-1), size = 1)
              which_samples <- c(which_samples,additional_sample)
              which_samples <- which_samples[order(which_samples)]
              y2 <- y[which_samples]
              x2 <- x[which_samples]
              
              # 3.2. Run changepoints for the new subsample
              cptmean.AMOC.y2 <- cpt.mean(y2, method="AMOC") 
              num <- cpts(cptmean.AMOC.y2)
              cptmean.AMOC.y2 <- as.numeric(x2[cpts(cptmean.AMOC.y2)])
              if(length(cptmean.AMOC.y2)==0) cptmean.AMOC.y2 <- 0
              #print(additional_sample)
            }
            
            # 4. Save outputs
            if (i == 1 & j == 1) {
              out_list$convergence_cptmean <- data.frame("sample_size"=sample_size[i],
                                                         "threshold_error"=threshold_error[j],
                                                         "number_sample_added" = length(which_samples)-sample_size[i],
                                                         "final_sample_size" = length(which_samples),
                                                         "cptmean"=ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                         "same_cptmean"=ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)) 
              #print(paste0("i and j=",i,"-",j)) 
              } else
                                                         {
                                                           out_list$convergence_cptmean <- rbind(out_list$convergence_cptmean, c(sample_size[i],
                                                                                                                                 threshold_error[j],
                                                                                                                                 length(which_samples)-sample_size[i],
                                                                                                                                 length(which_samples),
                                                                                                                                 ifelse(cptmean.AMOC.y2==0,NA,cptmean.AMOC.y2),
                                                                                                                                 ifelse(abs(cptmean.AMOC.y2-true.cptmean.AMOC.y)<=threshold_error[j], 1, 0)))
                                                         }
          }
            
          
        }
      }
      
    }
  }
  return(out_list)
}

# out <-ut(y=y,sample_size=c(5:30), threshold_error = 0, random_sampling = F, regular_sampling = F)
# out
