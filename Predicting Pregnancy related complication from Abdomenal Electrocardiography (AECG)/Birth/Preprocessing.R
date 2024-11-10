library(readxl)
library(httr)
library(tidyr)


url<-'https://archive.ics.uci.edu/ml/machine-learning-databases/00193/CTG.xls'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

rawdata <- read_excel(tf, 3L)
str(rawdata)
head(rawdata)
tail(rawdata)

num_columns <- ncol(rawdata)
print(num_columns)


# dropping incomplete cases, and irrelevant columns after examining head and tail of data


df1<-rawdata[!complete.cases(rawdata),]
df1<-rawdata %>% drop_na()
df1<- subset (df1, select = -c(FileName,Date,SegFile))
df1<-as.data.frame(df1)
head(df1)


# assigning col names, checking for missing data, and data description
library(psych)
names <- c('Tendency', 'A', 'B',    'C',    'D',    'E',    'AD',   'DE',   'LD',   'FS',   'SUSP', 'CLASS', 'NSP')
df1[,names] <- lapply(df1[,names] , factor)

names <- c('AC',    'FM',   'UC',       'DL',   'DS',   'DP',   'DR', 'Nmax', 'Nzeros')
df1[,names] <- lapply(df1[,names] , as.integer)


cat("There are", sum(is.na(df1)), "missing data in the dataset.\n\n")
describe(df1)

# description of categorical variables
library(SmartEDA)
#ExpCTable used to create table that provides summary of categorical variables in dataset
ExpCTable(df1,Target=NULL,clim=5,nlim=15,round=2,bin=NULL,per=F)
ExpCustomStat(df1,Cvar=c("Tendency","A","B","C","D","E","AD","DE","LD","FS","SUSP","CLASS", "NSP"),gpby=FALSE)


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














