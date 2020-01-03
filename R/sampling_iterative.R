#' Iterative sampling of a 1D vector
#'
#' Function to sample a vector iteratively
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @keywords sampling
#' @examples 
#'

sample_iterative <- function(x, n, messages = T)
  .sample_iterative(x, n, messages)
.sample_iterative <- function(x, n, messages) {
  require(ecp)
  require(zoo)
  
  if(!is.vector(x)) stop("x is required and must be a vector")
  if(is.null(n)) n <- length(x)
  
  # Sample initial number of samples
  n2 = 5
  which_samples <- seq(1,length(x),length.out = n2)
  which_samples <- round(which_samples)
  which_samples <- which_samples[order(which_samples)]
  
  
  # transform into matrix to run e.divisive
  x2 <- matrix(c(1:length(x),rep(NA,length(x))), ncol=2)
  x2[which_samples,2] <- x[which_samples]
  x2[,2] <- na.locf(x2[,2])
  x2 <- matrix(x2[,2])
  
  temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3]
  
  if(is.na(temp_chgpt)) {
    which_samples <- seq(1,2,length(x),length.out = n2*2-1)
    which_samples <- round(which_samples)
    which_samples <- which_samples[order(which_samples)]
    
    # transform into matrix to run e.divisive
    x2 <- matrix(c(1:length(x),rep(NA,length(x))), ncol=2)
    x2[which_samples,2] <- x[which_samples]
    x2[,2] <- na.locf(x2[,2])
    x2 <- matrix(x2[,2])
    
    temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3]
  }
  
  # Now, start adding samples
  for(i in 1:(n-n2)) {
    # Here we only want to add sample if we haven't already found the closest changepoint
    if(!round((which_samples[which(which_samples==temp_chgpt)-1]+which_samples[which(which_samples==temp_chgpt)])/2) %in% which_samples) {
      which_samples <- c(which_samples, round((which_samples[which(which_samples==temp_chgpt)-1]+which_samples[which(which_samples==temp_chgpt)])/2))
      which_samples <- which_samples[order(which_samples)]
      
      # transform into matrix to run e.divisive
      x2 <- matrix(c(1:length(x),rep(NA,length(x))), ncol=2)
      x2[which_samples,2] <- x[which_samples]
      x2[,2] <- na.locf(x2[,2])
      x2 <- matrix(x2[,2])
      
      temp_chgpt <- e.divisive(x2, min.size = 2, sig.lvl = .7)$order.found[3]
    }
  }
  
  changepoints <- e.divisive(x2, min.size = 2)
  
  # Print the main changepoint
  if(messages) print(changepoints$order.found[3])
  
  # More elaborated output
                  out_list <- list()
      out_list$changepoint <- changepoints$order.found[3]
          out_list$final_n <- length(which_samples)
  out_list$changepoint_all <- changepoints
           out_list$matrix <- data.frame("index"=which_samples,"x"=x[which_samples])
  
  invisible(out_list)
} 

test <- sample_iterative(ar1, n = 10)
e.divisive(matrix(test$matrix$x), min.size = 2)$order.found[3]
