#' Random sampling of a 1D vector
#'
#' Function to sample randomly a vector
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @keywords sampling
#' @examples 
#'

sample_random <- function(x, n, messages = T)
  .sample_regular(x, n, messages)
.sample_regular <- function(x, n, messages) {
  require(ecp)
  require(zoo)
  
  if(!is.vector(x)) stop("x is required and must be a vector")
  if(is.null(n)) n <- length(x)
  
  # Sample n samples from initial x
  which_samples <- sample(1:length(x), n)
  which_samples <- which_samples[order(which_samples)]
  
  # transform into matrix to run e.divisive
  x2 <- matrix(c(1:length(x),rep(NA,length(x))), ncol=2)
  x2[which_samples,2] <- x[which_samples]
  x2[,2] <- na.locf(x2[,2])
  x2 <- matrix(x2[,2])
  
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
