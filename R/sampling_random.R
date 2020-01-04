#' Random sampling of a 1D vector
#'
#' Function to sample randomly a vector
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @param messages Print changepoint or not
#' @param input_vector Default input vector = T. If turned to F, allow a data frame
#' @param xcol if input_vector = F, index of columns not to use for ordination.
#' @param DCA_axis which DCA axis to use as vector on which to detect changepoint
#' @keywords sampling
#' @examples 
#'

sample_random <- function(x, n, messages = T, input_vector = T, xcol = NA, DCA_axis = 1)
  .sample_random(x, n, messages, input_vector, xcol, DCA_axis)
.sample_random <- function(x, n, messages, input_vector, xcol, DCA_axis) {
  require(ecp)
  require(zoo)
  require(vegan)
  do_dca <- function(dataset, xcol= NA) {
    data_temp <- dataset[,-c(xcol)]
    data_temp <- data_temp[, colSums(data_temp != 0) > 0]
    data_temp <- decorana(data_temp)
    return(as.data.frame(data_temp$rproj))
  }
  
  if(!is.vector(x)&input_vector) stop("x is required and must be a vector, unless input_vector is turned to FALSE. If so, x must be a dataframe.")
  if(input_vector) lastpoint = length(x) else lastpoint = nrow(x)
  if(is.null(n)) n <- lastpoint
  
  # Sample n samples from initial x
  which_samples <- c(1,lastpoint,sample(1:lastpoint, n-2))
  which_samples <- which_samples[order(which_samples)]
  
  # Do DCA to get only one vector if input is a data frame
  if(!input_vector) {
    x <- do_dca(x[which_samples,], xcol)
    x <- x[,DCA_axis]
  }
  
  # transform into matrix to run e.divisive
  x2 <- matrix(c(1:lastpoint,rep(NA,lastpoint)), ncol=2)
  if(input_vector) x2[which_samples,2] <- x[which_samples] else x2[which_samples,2] <- x
  x2[,2] <- na.locf(x2[,2])
  x2 <- matrix(x2[,2])
  
  changepoints <- e.divisive(x2, min.size = 2)
  
  # Print the main changepoint
  if(messages) print(changepoints$order.found[3])
  
  # More elaborated output
                           out_list <- list()
                    out_list$method <- "Random"
               out_list$changepoint <- changepoints$order.found[3]
                   out_list$final_n <- length(which_samples)
           out_list$changepoint_all <- changepoints
   if(input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x[which_samples])
  if(!input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x)  
  invisible(out_list)
} 
