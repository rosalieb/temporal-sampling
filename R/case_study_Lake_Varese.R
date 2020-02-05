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
cpvar1 <- sample_regular(vr, xcol = 1, n = nrow(vr), input_vector = F, DCA_axis = 1)
cpvar2 <- sample_regular(vr, xcol = 1, n = nrow(vr), input_vector = F, DCA_axis = 2)

cpvar1_it <- sample_iterative(dca_all$DCA1, n=30, c=3)
cpvar1$changepoint_all$order.found
cpvar1_it$changepoint_all$order.found
sample_regular(vr, xcol = 1, n = nrow(vr), input_vector = F, DCA_axis = 2)

# Do the subsampling
vrindex <- 1:nrow(vr)

#Cpt1
j=3
myoutput_cpt1 <- NULL
for (i in 6:30) {
  vrsub <- sample_regular(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt1 <- c(myoutput_cpt1, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_random(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt1 <- c(myoutput_cpt1, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt1 <- c(myoutput_cpt1, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 6)
  myoutput_cpt1 <- c(myoutput_cpt1, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 7)
  myoutput_cpt1 <- c(myoutput_cpt1, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
}
myoutput_cpt1 <- data.frame(matrix(myoutput_cpt1, ncol=4, byrow=T))
myoutput_cpt1[,2] <- as.numeric(paste(myoutput_cpt1[,2]))
myoutput_cpt1[,3] <- as.numeric(paste(myoutput_cpt1[,3]))
colnames(myoutput_cpt1) <- c("method", "final_n", "changepoint","total_number_found")
myoutput_cpt1$diff_real <- myoutput_cpt1$changepoint-cpvar1$changepoint_all$order.found[j]
myoutput_cpt1$method_f <- factor(myoutput_cpt1$method, levels=c('Random', 'Regular', 'Iterative'))
myoutput_cpt1$n2_init <- rep(c(NA,NA,5,6,7), len=nrow(myoutput_cpt1))

#Cpt2
j=4
myoutput_cpt2 <- NULL
for (i in 6:30) {
  vrsub <- sample_regular(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt2 <- c(myoutput_cpt2, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_random(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt2 <- c(myoutput_cpt2, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt2 <- c(myoutput_cpt2, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 6)
  myoutput_cpt2 <- c(myoutput_cpt2, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 7)
  myoutput_cpt2 <- c(myoutput_cpt2, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
}
myoutput_cpt2 <- data.frame(matrix(myoutput_cpt2, ncol=4, byrow=T))
myoutput_cpt2[,2] <- as.numeric(paste(myoutput_cpt2[,2]))
myoutput_cpt2[,3] <- as.numeric(paste(myoutput_cpt2[,3]))
colnames(myoutput_cpt2) <- c("method", "final_n", "changepoint","total_number_found")
myoutput_cpt2$diff_real <- myoutput_cpt2$changepoint-cpvar1$changepoint_all$order.found[j]
myoutput_cpt2$method_f <- factor(myoutput_cpt2$method, levels=c('Random', 'Regular', 'Iterative'))
myoutput_cpt2$n2_init <- rep(c(NA,NA,5,6,7), len=nrow(myoutput_cpt2))

#Cpt3
j=5 
myoutput_cpt3 <- NULL
for (i in 6:30) {
  vrsub <- sample_regular(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt3 <- c(myoutput_cpt3, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_random(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt3 <- c(myoutput_cpt3, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
  myoutput_cpt3 <- c(myoutput_cpt3, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 6)
  myoutput_cpt3 <- c(myoutput_cpt3, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
  vrsub <- sample_iterative(vr, i, input_vector = F, xcol = 1, DCA_axis = 1, messages = F, n2 = 7)
  myoutput_cpt3 <- c(myoutput_cpt3, vrsub$method, vrsub$final_n, vrsub$changepoint_all$order.found[j], length(vrsub$changepoint_all$estimates)-2)
}
myoutput_cpt3 <- data.frame(matrix(myoutput_cpt3, ncol=4, byrow=T))
myoutput_cpt3[,2] <- as.numeric(paste(myoutput_cpt3[,2]))
myoutput_cpt3[,3] <- as.numeric(paste(myoutput_cpt3[,3]))
colnames(myoutput_cpt3) <- c("method", "final_n", "changepoint","total_number_found")
myoutput_cpt3$diff_real <- myoutput_cpt3$changepoint-cpvar1$changepoint_all$order.found[j]
myoutput_cpt3$method_f <- factor(myoutput_cpt3$method, levels=c('Random', 'Regular', 'Iterative'))
myoutput_cpt3$n2_init <- rep(c(NA,NA,5,6,7), len=nrow(myoutput_cpt3))

myoutput <- rbind(myoutput_cpt1,myoutput_cpt2,myoutput_cpt3)
myoutput$target_cpt <- as.factor(rep(1:3, each=nrow(myoutput_cpt1)))
myoutput$n2_init[is.na(myoutput$n2_init)] <- 5 

#write.table(myoutput,"Output/output_changepoint_varese.txt", sep = "\t")
library(ggplot2)
library(gridExtra)
p1 <- ggplot(myoutput, aes(final_n,diff_real, col=target_cpt, pch=factor(n2_init))) + 
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


p4 <- ggplot(myoutput, aes(final_n,total_number_found))  +
  facet_wrap(~method_f) + 
  geom_point(alpha = .3) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + theme_bw() +
  labs(title="c",x="", y="Number of changepoint found")

grid.arrange(p1, p2, p4, nrow = 3)

