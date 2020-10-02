#' Regular sampling of a 1D vector
#'
#' Function to sample regularly a vector
#'
#' @export
#' @param x Initial vector
#' @param n Number of samples analyzed
#' @param messages Print changepoint or not
#' @param input_vector Default input vector = T. If turned to F, allow a data frame
#' @param xcol if input_vector = F, index of columns not to use for ordination.
#' @param DCA_axis which DCA axis to use as vector on which to detect changepoint
#' @param R number of permutation in the e.divisive function from the package ecp
#' @param sig.lvl significance level in the e.divisive function from the package ecp
#' @keywords sampling
#' @examples 
#'

sample_regular <- function(x, n, messages = T, input_vector = T, xcol = NA, DCA_axis = 1,
                           R = 199, sig.lvl = 0.05)
  .sample_regular(x, n, messages, input_vector, xcol, DCA_axis, R, sig.lvl)
.sample_regular <- function(x, n, messages, input_vector, xcol, DCA_axis, R, sig.lvl) {
  require(ecp)
  require(zoo)
  require(vegan)
  do_dca <- function(dataset, xcol= NA) {
    data_temp <- dataset[,-c(xcol)]
    data_temp <- data_temp[, colSums(data_temp != 0) > 0]
    data_temp <- decorana(data_temp)
    return(as.data.frame(data_temp$rproj))
    #scores.species<-as.data.frame(data_temp$cproj)
  }
  
  if(!is.vector(x)&input_vector) stop("x is required and must be a vector, unless input_vector is turned to FALSE. If so, x must be a dataframe.")
  if(input_vector) lastpoint = length(x) else lastpoint = nrow(x)
  if(is.null(n)) n <- lastpoint
  
  # Sample n samples from initial x
  which_samples <- seq(1,lastpoint, length.out = n)
  which_samples <- round(which_samples)
  which_samples <- which_samples[order(which_samples)]
  
  # Do DCA to get only one vector if input is a data frame
  if(!input_vector) {
    x <- do_dca(x[which_samples,], xcol)
    x <- x[,DCA_axis]
  }
  
  # transform into matrix to run e.divisive
  x2 <- matrix(c(1:lastpoint,rep(NA,lastpoint)), ncol=2)
  if(input_vector)  x2[which_samples,2] <- x[which_samples] else x2[which_samples,2] <- x
  if(any(is.na(x2[,2]))) x2[,2] <- na.locf(x2[,2])
  x2 <- matrix(x2[,2])
  
  changepoints <- e.divisive(x2, min.size = 2, R = R, sig.lvl = sig.lvl)
  if(length(changepoints$p.values)>1) changepoints$p.values <- changepoints$p.values[-length(changepoints$p.values)]
  if(length(changepoints$p.values)>1) changepoints$estimates <- changepoints$estimates[-c(1, length(changepoints$estimates))]
  
  # Print the main changepoint
  if(messages) print(changepoints$order.found[3])
  
  mmessage <- paste0("We used the function e.divisive() in the package ecp to calculate the changepoint(s). ",
                     "The original function let the user choose R (the number of permutation, default = 199) and sig.lev (the significance level, default = 0.05). ",
                     "Herein, the two parameters are: R = ", R, ", and sig.lvl = ", sig.lvl,". ",
                     "p-values in the output are calculated during the permutation process by the e.divisive function. ",
                     "p = 0.005 is the lowest p possible when R = 199, as it means that for each of the 199 permutations,  the energy needed to break the matrix was lower than the energy needed to find the changepoint in the initial configuration. ",
                     "See the e.divisive docmentation for more details.")
  
  # More elaborated output
                           out_list <- list()
                    out_list$method <- "Regular"
               out_list$changepoint <- changepoints$order.found[3]
                   out_list$final_n <- length(which_samples)
           out_list$changepoint_all <- changepoints
                   out_list$message <- mmessage
   if(input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x[which_samples])
  if(!input_vector) out_list$matrix <- data.frame("index"=which_samples,"x"=x)  
  
  invisible(out_list)
           
} 



