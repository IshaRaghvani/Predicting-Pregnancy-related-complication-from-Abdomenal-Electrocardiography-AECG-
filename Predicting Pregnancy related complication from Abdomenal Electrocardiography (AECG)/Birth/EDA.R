# plotting all raw variable histogram
library(ggplot2)
library(gridExtra)
theme_set(
  theme_bw() +
    theme(legend.title=element_blank())+
    theme(legend.position = "top")
)

temp<-df1



# b e
#to show you how the start (b) and end (e) times of the CTG examination are 
#distributed across the three health categories (Normal, Suspect, Pathologic).
p=list()
namess<-c('b','e')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}

#LBE    LB  
p=list()
namess<-c('LBE','LB')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")


#AC FM  UC  
p=list()
namess<-c('AC', 'FM',   'UC')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Variable histogram with NSP Classes")


#   DL  DS  DP  DR
p=list()
namess<-c(  'DL',   'DS',   'DP',   'DR')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")


#ASTV   MSTV    ALTV    MLTV    
p=list()
namess<-c('ASTV',   'MSTV', 'ALTV', 'MLTV')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")



# Mode  Mean    Median  Variance Tendency
p=list()
namess<-c('Mode',   'Mean', 'Median',   'Variance','Tendency')
for (i in 1:length(namess)){
  if(i < length(namess)){
    p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
      geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
  }else{
    p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
      geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
  }
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Variable histogram with NSP Classes")


# Width Min Max     Nmax    Nzeros 
p=list()
namess<-c('Width',  'Min',  'Max','Nmax',   'Nzeros' )
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_histogram(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Variable histogram with NSP Classes")



#A  B   C   D   
p=list()
namess<-c('A',  'B',    'C',    'D')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")


#   E   AD  DE  LD
p=list()
namess<-c('E',  'AD',   'DE',   'LD')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")



#   FS  SUSP
p=list()
namess<-c('FS', 'SUSP')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")


# CLASS NSP
#class imbalance present
p=list()
namess<-c( 'CLASS' ,    'NSP')
for (i in 1:length(namess)){
  p[[i]] <- ggplot(temp, aes_string(x = namess[i])) + 
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))))
}
grid.arrange(grobs=p,ncol=2, nrow=2, top = "Variable histogram with NSP Classes")

# checking if there are exact duplicate columns in the dataset

dup <- ifelse(df1$LBE > df1$LB, 1,ifelse(df1$LBE < df1$LB, 1, 0))
paste0("There are ",sum(dup)," items that are different in the LB anf LBE columns.")

# dropping the variable LBE because it is exact copy of the LB variable

df2<- subset (df1, select = -c(DR, LBE))

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Ensure numeric columns are in the right format
numeric_cols <- names(df1)[sapply(df1, is.numeric)]

# Create boxplots for each numeric attribute
plot_list <- list()
for (col in numeric_cols) {
  if (!is.null(df1[[col]]) && length(unique(df1[[col]])) > 1) {
    p <- ggplot(df1, aes_string(x = col)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(title = paste("Boxplot of", col), x = col, y = "") +
      theme_minimal()
    plot_list[[col]] <- p
  } else {
    cat(paste("Skipping column", col, "due to insufficient data or NULL values.\n"))
  }
}
summary(df1$DR)

# Remove NULL entries from the plot_list
plot_list <- plot_list[!sapply(plot_list, is.null)]

# Check if any plots were successfully created
if (length(plot_list) > 0) {
  # Arrange plots in a grid
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position = "none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    )
  }
  
  # Display the boxplots
  grid_arrange_shared_legend(grobs = plot_list)
} else {
  cat("No valid numeric columns to plot or an error occurred during plot creation.\n")
}















