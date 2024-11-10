# calculating nPoints the length of record length variable from end and start instance numbers
# normalizing the recording length sensitive variables, adding them to the data set and dropping original cols
library(psych)
library(dplyr)
df3 <- df2 %>% mutate(nPoints = e - b)

df3 <- df3 %>% mutate(nAC = AC/nPoints)
df3 <- df3 %>% mutate(nFM = FM/nPoints)
df3 <- df3 %>% mutate(nUC = UC/nPoints)
df3 <- df3 %>% mutate(nDL = DL/nPoints)
df3 <- df3 %>% mutate(nDS = DS/nPoints)
df3 <- df3 %>% mutate(nDP = DP/nPoints)
df4 <- subset (df3, select = -c(b, e,  AC, FM, UC, DL, DS, DP))#, A, B, C, D, E, AD, DE, LD, FS, SUSP))
str(df4)


# plotting n points and recording-length-normalized data

theme_set(
  theme_bw() +
    theme(legend.title=element_blank())+
    theme(legend.position = "top")
)

temp<-df3

# nPoints
p=list()
namess<-c('nPoints')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")





# saving normalized and not normalized dataframe for analysis 

library(tidyverse)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm_df_0<- subset (df3, select = -c(b, e,  AC, FM, UC, DL, DS, DP))
norm_df_1 <- as.data.frame(sapply(norm_df_0 %>% select(where(is.numeric)), min_max_norm))
norm_df <- as.data.frame(cbind(norm_df_1, norm_df_0 %>% select(where(is.factor))))

describeBy(norm_df, group=norm_df$NSP)
str(norm_df)

write.csv(norm_df,"norm_df.csv", row.names = FALSE)

no_norm_df_0<- subset (df3, select = -c(b,  e,  nAC,    nFM,    nUC,    nDL,    nDS,    nDP))
no_norm_df<-as.data.frame(cbind(no_norm_df_0 %>% select(where(is.numeric)) ,no_norm_df_0 %>% select(where(is.factor)) ))

#str(no_norm_df)
write.csv(no_norm_df,"no_norm_df.csv", row.names = FALSE)









