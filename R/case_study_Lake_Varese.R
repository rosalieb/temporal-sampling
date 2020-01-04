getpath4data <- function() {
  if(Sys.getenv("USER")=="Rosalie") return("/Volumes/-/Script R/Data-off-GitHub/temporal-sampling/")
  if(Sys.getenv("USER")=="eastonwhite") return("~/Desktop/Research/soil-temporal-sampling/")
  if(Sys.getenv("USER")!="Rosalie"|Sys.getenv("USER")!="put here your USER") stop("You need to get the data and define their location.")
}
# Function to return DCA
library(vegan)
do_dca <- function(dataset, xcol= NA) {
  data_temp <- dataset[,-c(xcol)]
  data_temp <- decorana(data_temp)
  return(as.data.frame(data_temp$rproj))
  #scores.species<-as.data.frame(data_temp$cproj)
}

# Read data
vr <- read.delim(paste0(getpath4data(),"VAR10-10-clado.txt"))

# DCA and changepoint on all data.
# Note that only the most important changepoint is reported
dca_all <- do_dca(vr,1)
cpvar1 <- sample_regular(dca_all$DCA1, nrow(vr), input_vector = T)
cpvar2 <- sample_regular(dca_all$DCA2, nrow(vr))

# Do the subsampling
vrindex <- 1:nrow(vr)

myoutput <- NULL
for (i in 6:30) {
  vrsub <- sample_regular(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput <- c(myoutput, vrsub$method, vrsub$final_n, vrsub$changepoint)
  vrsub <- sample_random(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput <- c(myoutput, vrsub$method, vrsub$final_n, vrsub$changepoint)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput <- c(myoutput, vrsub$method, vrsub$final_n, vrsub$changepoint)
}
myoutput <- data.frame(matrix(myoutput, ncol=3, byrow=T))
myoutput[,2] <- as.numeric(paste(myoutput[,2]))
myoutput[,3] <- as.numeric(paste(myoutput[,3]))
colnames(myoutput) <- c("method", "final_n", "changepoint")
myoutput$diff_real <- myoutput$changepoint-cpvar1$changepoint
myoutput$method_f <- factor(myoutput$method, levels=c('Random', 'Regular', 'Iterative'))

#write.table(myoutput,"Output/output_changepoint_varese.txt", sep = "\t")
library(ggplot2)
library(gridExtra)
p1 <- ggplot(myoutput, aes(final_n,diff_real)) + 
  geom_point(alpha = .3) +
  facet_wrap(~method_f) + theme_bw() +
  labs(title="a",x="", y="Distance to real changepoint")
p2 <- ggplot(myoutput, aes(rep(0, nrow(myoutput)),diff_real))  +
  facet_wrap(~method_f) + 
  geom_violin() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.3) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + theme_bw() +
  labs(title="b",x="", y="Distance to real changepoint")
p3 <- arrangeGrob(p1, p2, nrow = 2)
ggsave("Output/Fig_case_study_Varese.pdf", p3)



plot(scores$DCA1, type="l")


sample_regular(scores$DCA2, 30)
 