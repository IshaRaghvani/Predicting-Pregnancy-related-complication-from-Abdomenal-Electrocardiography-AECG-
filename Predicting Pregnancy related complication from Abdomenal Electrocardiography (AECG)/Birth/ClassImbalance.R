#class imbalance analysis for the whole dataset

C<-3

Tdata<-length(no_norm_df$NSP)

n1<-length(which(no_norm_df$NSP=="1"))
n2<-length(which(no_norm_df$NSP=="2"))
n3<-length(which(no_norm_df$NSP=="3"))

imbalanceStat<-abs(((1/C)-(n1/Tdata)))+abs(((1/C)-(n2/Tdata)))+abs(((1/C)-(n3/Tdata)))

cat("Class 1: ",round(n1*100/Tdata,0),"%\n")
cat("Class 2: ",round(n2*100/Tdata,0),"%\n")
cat("Class 3: ",round(n3*100/Tdata,0),"%\n\n")
cat("Imbalance: ",round(imbalanceStat,2))

library(ggplot2)
library(gridExtra)
theme_set(
  theme_bw() +
    theme(legend.title=element_blank())+
    theme(legend.position = "top")
)

