#' Regular sampling of a 1D vector
#'
#' Function to sample regularly a vector
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @keywords sampling
#' @examples 
#'

sample_regular <- function(x, n, messages = T)
  .sample_regular(x, n, messages)
.sample_regular <- function(x, n, messages) {
  require(ecp)
  
  if(!is.vector(x)) stop("x is required and must be a vector")
  if(is.null(n)) n <- length(x)
  
  # Sample n samples from initial x
  which_samples <- seq(1,length(x), length.out = n)
  which_samples <- round(which_samples)
  which_samples <- which_samples[order(which_samples)]
  
  x2 <- matrix(x[which_samples], ncol=1)
  
  changepoints <- e.divisive(x2, min.size = 2)
  
  # Print the main changepoint
  if(messages) print(which_samples[changepoints$order.found[3]])
  
  # More elaborated output
  out_list <- list()
      out_list$changepoint <- which_samples[changepoints$order.found[3]]
  out_list$changepoint_all <- changepoints
           out_list$matrix <- data.frame("index"=which_samples,"x"=x2)
  
  invisible(out_list)
           
} 


