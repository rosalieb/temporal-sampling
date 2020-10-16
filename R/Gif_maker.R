# Script for gif
pacman::p_load(patchwork, stickylabeller, ggplot2, dplyr, wesanderson, reshape, cowplot, vegan, ecp, ggthemes)

# Read data
vr <- read.delim(paste0(getpath4data(),"VAR10-10-clado.txt"))
vr_out_matrix <- read.table(paste0(getwd(),"/Output/output_iterative_sampling_varese.txt"))

index_temp <- vr_out_matrix$index[vr_out_matrix$n==max(vr_out_matrix$n)]
vr_out_order <-
  data.frame("index" = index_temp,
             "age"   = vrall$matrix$age[index_temp],
             "x"     = vrall$matrix$x[index_temp],
             "count" = summary(as.factor(vr_out_matrix$index)))
vr_out_order$count <- abs(vr_out_order$count-max(vr_out_order$count)-1)

# Detect 'true' changepoint
vrall <- sample_regular(vr, n=nrow(vr), input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
vrall$matrix$age <- vr$AGE[vrall$matrix$index]

# Plot 'true' changepoint
preal <- ggplot(vrall$matrix, aes(age, x)) + 
  geom_rect(stat = "identity", 
            mapping=aes(
              xmin=vrall$matrix$age[which(vrall$matrix$index==vrall$changepoint)-1], 
              xmax=vrall$matrix$age[which(vrall$matrix$index==vrall$changepoint)], 
              ymin=-1, ymax=3), col=NA, fill="grey", alpha=0.01) +
  annotate("text", label = paste0("Number of samples = 74"), x = min(vrall$matrix$age), y = 2.8, colour = "black",  hjust=0,vjust=0) +  
  geom_line() + geom_point() + 
  guides(col=FALSE) +
  scale_color_manual(values=mypal) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="Year", y="DCA first component",
       color='samples added\nat iteration t=') +
  labs(subtitle = "A. Full time-series and 'real' changepoint")



# Random sampling
vrrdm <- sample_random(vr, n=10, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
vrrdm$matrix$age <- vr$AGE[vrrdm$matrix$index]
vrrdm$matrix$count <- vr_out_order$count[vr_out_order$age %in% vrrdm$matrix$age]

# Regular sampling
vrreg <- sample_regular(vr, n=10, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
vrreg$matrix$age <- vr$AGE[vrreg$matrix$index]
vrreg$matrix$count <- vr_out_order$count[vr_out_order$age %in% vrreg$matrix$age]

# Plot random
prdm <- ggplot(vrrdm$matrix, aes(age, x, color=as.factor(count))) +  
  geom_line(alpha = .4, col="grey") + geom_point() + guides(col=FALSE) +
  scale_color_manual(values=mypal) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="Year", y="DCA first component",
       color='samples added\nat iteration t=') + 
  geom_rect(stat = "identity", mapping=aes(xmin=vrrdm$matrix$age[which(vrrdm$matrix$index==vrrdm$changepoint)-1], xmax=vrrdm$matrix$age[which(vrrdm$matrix$index==vrrdm$changepoint)], ymin=-1, ymax=3), col=NA, fill="grey", alpha=0.05) +
  annotate("text", label = paste0("Number of samples = 10"), x = min(vrrdm$matrix$age), y = 2.8, colour = "black",  hjust=0,vjust=0) +
  labs(subtitle = "B. Random sampling")

# Plot regular
preg <- ggplot(vrreg$matrix, aes(age, x, color=as.factor(count))) +  geom_line(alpha = .4, col="grey") + geom_point() + guides(col=FALSE) +
  scale_color_manual(values=mypal) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="Year", y="DCA first component",
       color='samples added\nat iteration t=') + 
  geom_rect(stat = "identity", mapping=aes(xmin=vrreg$matrix$age[which(vrreg$matrix$index==vrreg$changepoint)-1], xmax=vrreg$matrix$age[which(vrreg$matrix$index==vrreg$changepoint)], ymin=-1, ymax=3), col=NA, fill="grey", alpha=0.05) +
  annotate("text", label = paste0("Number of samples = 10"), x = min(vrreg$matrix$age), y = 2.8, colour = "black",  hjust=0,vjust=0)  +
  labs(subtitle = "C. Regular sampling")





mypal = colorblind_pal()(8)
mypal = mypal[c(7,2,3,6,8)]
mypal <- wes_palette("GrandBudapest2", max(vr_out_order$count),type = "continuous")
mypal <- wes_palette("Zissou1", max(vr_out_order$count)+5,type = "continuous")
mypal[4] <- "#90b581"
mypal <- mypal[c(1,3,4,7,9)]


# Plot with all data
p_init <- ggplot(vrall$matrix, aes(age, x)) + geom_point(alpha = .3) + geom_line(alpha = .3) +
  theme_bw()  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_rect(mapping=aes(xmin=vrall$matrix$age[which(vrall$matrix$index==vrall$changepoint)-1], xmax=vrall$matrix$age[which(vrall$matrix$index==vrall$changepoint)], ymin=-1, ymax=2), alpha=0.05, fill="grey") +
  labs(subtitle = paste0("b. Full time-series, n = ", nrow(vr)), x="Year", y="DCA first component",
       color='samples added\nat iteration t=') +
  geom_point(vr_out_order, mapping=aes(age, x, color = as.factor(count)), cex=2) +
  geom_text(vr_out_order, mapping=aes(age, x, label =count), vjust = 1.5, nudge_x = 1.5) +
  scale_color_manual(values=mypal) 



# Initial plot, then Adding samples
for (i in 5:12) {
  # Start a new loop only if the final changepoint hasn't been found at the previous iteration.  
  if (i==5 || vrsub$matrix$index[vrsub$matrix$index==vrsub$changepoint] - vrsub$matrix$index[which(vrsub$matrix$index==vrsub$changepoint)-1]>1) {
    
    vrsub <- sample_iterative(vr, n=i, n2=5, input_vector = F, xcol = 1, DCA_axis = 1, messages = F)
    vrsub$matrix$age <- vr$AGE[vrsub$matrix$index]
    vrsub$matrix$count <- vr_out_order$count[vr_out_order$age %in% vrsub$matrix$age]
    
    if (i==5) {
      vrsub1 <- vrsub
      p1 <- ggplot(vrsub$matrix, aes(age, x, color=as.factor(count))) +  geom_line(alpha = .4, col="grey") + geom_point() + guides(col=FALSE) +
        scale_color_manual(values=mypal) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x="Year", y="DCA first component",
             color='samples added\nat iteration t=')
      p2 <- p1
    } else {
      if (i == 6) {
        p2 <- p1 + 
          geom_line(vrsub$matrix, mapping=aes(age, x), alpha = .4, col="grey") + 
          geom_point(vrsub$matrix, mapping=aes(age, x,color=as.factor(count)))   + guides(col=FALSE) 
      } else {
        p2 <- p2 + geom_line(vrsub$matrix, mapping=aes(age, x), alpha = .4, col="grey") + 
          geom_point(vrsub$matrix, mapping=aes(age, x,color=as.factor(count))) + 
          guides(col=FALSE)
      }
    }
    # Save plot
    ggsave( (preal + prdm ) / (preg + (p2 + 
             geom_rect(stat = "identity", mapping=aes(xmin=vrsub$matrix$age[which(vrsub$matrix$index==vrsub$changepoint)-1], xmax=vrsub$matrix$age[which(vrsub$matrix$index==vrsub$changepoint)], ymin=-1, ymax=3), col=NA, fill="grey", alpha=0.05) +
             annotate("text", label = paste0("Number of samples = ",i), x = min(vrsub$matrix$age), y = 2.8, colour = "black",  hjust=0,vjust=0)  +
             labs(subtitle = "D. Iterative sampling") )) , 
           filename = paste0("Output/GIF_adding_sample_case_study/Stacked_",i,".png"), width = 8, height = 6)
    
  }
}
