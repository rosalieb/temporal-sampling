# GIF of adding changepoint

# load Libraries
library(ggplot2)
#devtools::install_github('thomasp85/gganimate', force = T)
library(gganimate)

getpath4data <- function() {
  if(Sys.getenv("USER")=="Rosalie") return("/Volumes/-/Script R/Data-off-GitHub/temporal-sampling/")
  if(Sys.getenv("USER")=="eastonwhite") return("~/Desktop/Research/soil-temporal-sampling/")
  if(Sys.getenv("USER")!="Rosalie"|Sys.getenv("USER")!="put here your USER") stop("You need to get the data and define their location.")
}

# Read data
vr <- read.delim(paste0(getpath4data(),"VAR10-10-clado.txt"))

# Creating a dataset with all scores 
vr_out_matrix <-  NULL
for (i in 5:12) {
  vrsub <- sample_iterative(vr, n=i, n2=5, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  if (is.null(vr_out_matrix)) vr_out_matrix <- cbind(vrsub$matrix,"n"=i) else vr_out_matrix <- rbind(vr_out_matrix,cbind(vrsub$matrix,"n"=i))
}

vr_out_matrix$age <- vr$AGE[vr_out_matrix$index]
#write.table(vr_out_matrix,"Output/output_iterative_sampling_varese.txt", sep = "\t")


# Plot
p <- ggplot(vr_out_matrix, aes(index, x)) +
  geom_point() +
  theme_bw() +
  # Animation part
  transition_manual(n)

# We then display it as an animation with the gg_animate function:
gg_animate(p)
# not working, I need to look at the error.

# Error: package or namespace load failed for ‘gganimate’ in get(method, envir = home):
#   lazy-load database '/Library/Frameworks/R.framework/Versions/3.5/Resources/library/gganimate/R/gganimate.rdb' is corrupt
# In addition: Warning messages:
#   1: In .registerS3method(fin[i, 1], fin[i, 2], fin[i, 3], fin[i, 4],  :
#                             restarting interrupted promise evaluation
#                           2: In get(method, envir = home) :
#                             restarting interrupted promise evaluation
#                           3: In get(method, envir = home) : internal error -3 in R_decompress1

