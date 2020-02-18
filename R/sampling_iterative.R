#' Iterative sampling of a 1D vector
#'
#' Function to sample a vector iteratively
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @param n2 Initial number of sample, regularly spaced. Default to 5. Will be changed to (n2*2-1) if no changepoints were detected for initial value of n2.
#' @param c Max number of changepoint to look for. Default = 1 (major changepoint).
#' @param messages Print changepoint or not
#' @param input_vector Default input vector = T. If turned to F, allow a data frame
#' @param xcol if input_vector = F, index of columns not to use for ordination.
#' @param DCA_axis which DCA axis to use as vector on which to detect changepoint
#' @keywords sampling
#' @examples 
#'

sample_iterative <- function(x, n, n2 = 5, c=1, messages = T, input_vector = T, xcol = NA, DCA_axis = 1)
  .sample_iterative(x, n, n2, c, messages, input_vector, xcol, DCA_axis)
.sample_iterative <- function(x, n, n2, c, messages, input_vector, xcol, DCA_axis) {
  require(ecp)
  require(zoo)
  require(vegan)
  do_dca <- function(dataset, xcol= NA) {
    data_temp <- dataset[,-c(xcol)]
    data_temp <- data_temp[, colSums(data_temp != 0) > 0]
    data_temp <- decorana(data_temp)
    return(as.data.frame(data_temp$rproj))
  }
  
  x_stable <- x
  
  if(!is.vector(x)&input_vector) stop("x is required and must be a vector, unless input_vector is turned to FALSE. If so, x must be a dataframe.")
  if(input_vector) lastpoint = length(x) else lastpoint = nrow(x)
  if(is.null(n)) n <- lastpoint
  
  # Sample initial number of samples
  which_samples <- seq(1,lastpoint,length.out = n2-2)
  which_samples <- round(which_samples)
  which_samples <- which_samples[order(which_samples)]
  
  # Do DCA to get only one vector if input is a data frame
  if(!input_vector) {
    x <- do_dca(x[which_samples,], xcol)
    x <- x[,DCA_axis]
  }
  
  # transform into matrix to run e.divisive
  x2 <- matrix(c(1:lastpoint,rep(NA,lastpoint)), ncol=2)
  if(input_vector) x2[which_samples,2] <- x[which_samples] else x2[which_samples,2] <- x
  if(any(is.na(x2[,2]))) x2[,2] <- na.locf(x2[,2])
  x2 <- matrix(x2[,2])
  
  temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3:(2+c)]
  
  if(all(is.na(temp_chgpt))) {
    which_samples <- seq(1,2,lastpoint,length.out = n2*2-1)
    which_samples <- round(which_samples)
    which_samples <- which_samples[order(which_samples)]
    
    # Reset x
    x <- x_stable
    
    # Do DCA to get only one vector if input is a data frame
    if(!input_vector) {
      x <- do_dca(x[which_samples,], xcol)
      x <- x[,DCA_axis]
    }
    
    # transform into matrix to run e.divisive
    x2 <- matrix(c(1:lastpoint,rep(NA,lastpoint)), ncol=2)
    if(input_vector) x2[which_samples,2] <- x[which_samples] else x2[which_samples,2] <- x
    if(any(is.na(x2[,2]))) x2[,2] <- na.locf(x2[,2])
    x2 <- matrix(x2[,2])
    
    temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3:(2+c)]
  }
  
  # Now, start adding samples
  c_order = 1
  for(i in 1:(n-n2)) {
    if (c_order<=c && !is.na(temp_chgpt[c_order])) {
      # Here we only want to add sample if we haven't already found the closest changepoint
      if(!round((which_samples[which(which_samples==temp_chgpt[c_order])-1]+which_samples[which(which_samples==temp_chgpt[c_order])])/2) %in% which_samples) {
        which_samples <- c(which_samples, round((which_samples[which(which_samples==temp_chgpt[c_order])-1]+which_samples[which(which_samples==temp_chgpt[c_order])])/2))
        which_samples <- which_samples[order(which_samples)]
        
        # Reset x
        x <- x_stable
        
        # Do DCA to get only one vector if input is a data frame
        if(!input_vector) {
          x <- do_dca(x[which_samples,], xcol)
          x <- x[,DCA_axis]
        }
        
        # transform into matrix to run e.divisive
        x2 <- matrix(c(1:lastpoint,rep(NA,lastpoint)), ncol=2)
        if(input_vector) x2[which_samples,2] <- x[which_samples] else x2[which_samples,2] <- x
        if(any(is.na(x2[,2]))) x2[,2] <- na.locf(x2[,2])
        x2 <- matrix(x2[,2])
        
        temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3:(2+c)]
      } else {
        c_order = c_order + 1
      }
    } 
  }
  
  changepoints <- e.divisive(x2, min.size = 2)
  
  # Print the main changepoint
  if(messages) print(changepoints$order.found[3])
  
  # More elaborated output
                           out_list <- list()
                    out_list$method <- "Iterative"
               out_list$changepoint <- changepoints$order.found[3]
                   out_list$final_n <- length(which_samples)
           out_list$changepoint_all <- changepoints
   if(input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x[which_samples])
  if(!input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x)
  
  invisible(out_list)
} 


