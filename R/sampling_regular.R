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

sample_regular <- function(x, n)
  .sample_regular
.sample_regular <- function(x, n) {
  require(ecp)
  
  if(!is.vector(x)) stop("x is required and must be a vector")
  if(is.null(n)) n <- length(x)
  
  # Sample n samples from initial x
  which_samples <- seq(1,length(x), length.out = n)
  which_samples <- round(which_samples)
  which_samples <- which_samples[order(which_samples)]
  
  x2 <- matrix(c(which_samples,x[which_samples]), ncol=2)
  
  return(e.divisive(x2)$estimates[-1])
} 