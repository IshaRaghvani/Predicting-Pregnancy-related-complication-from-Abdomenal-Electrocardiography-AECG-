
library(Hmisc)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(rlang)

theme_set(
  theme_bw() +
    theme(legend.title=element_blank())+
    theme(legend.position = "top")
)

temp<-no_norm_df

# 'LB','Mode', 'Mean', 'Median', 'Tendency'
p=list()
namess<-c('LB','Mode', 'Mean', 'Median', 'Tendency', 'nPoints')

i=1
for (var in namess){
  
  new_temp = temp %>%
    select(sym(var), NSP)%>%
    group_by(.data[[var]], NSP) %>% 
    mutate(test = n()) %>%
    distinct(.data[[var]], NSP, test) %>%
    group_by(.data[[var]]) %>%
    mutate(pct = test/sum(test)*100)
  
  p[[i]] <- ggplot(new_temp, aes(x= .data[[var]], y = pct)) +
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))), stat = "identity")+
    labs(x = var, y = "Percentage", 
         fill = "NSP Class")
  
  i=i+1
  
}

grid.arrange(grobs=p,ncol=3, nrow=2, top = "Baseline tendencies with NSP Classes")



# 'AC', 'FM',   'Max', 'Nmax'
p=list()
namess<-c('AC', 'FM',   'Max', 'Nmax')
i=1
for (var in namess){
  
  new_temp = temp %>%
    select(sym(var), NSP)%>%
    group_by(.data[[var]], NSP) %>% 
    mutate(test = n()) %>%
    distinct(.data[[var]], NSP, test) %>%
    group_by(.data[[var]]) %>%
    mutate(pct = test/sum(test)*100)
  
  
  p[[i]] <- ggplot(new_temp, aes(x= .data[[var]], y = pct)) +
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))), stat = "identity")+
    labs(x = var, y = "Percentage", 
         fill = "NSP Class")
  
  i=i+1
  
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Accelerative tendencies with NSP Classes")

#   'DL',   'DS',   'DP',   'Min', 'Nzeros'
p=list()
namess<-c('DL', 'DS',   'DP',   'Min', 'Nzeros')
i=1
for (var in namess){
  
  new_temp = temp %>%
    select(sym(var), NSP)%>%
    group_by(.data[[var]], NSP) %>% 
    mutate(test = n()) %>%
    distinct(.data[[var]], NSP, test) %>%
    group_by(.data[[var]]) %>%
    mutate(pct = test/sum(test)*100)
  
  p[[i]] <- ggplot(new_temp, aes(x= .data[[var]], y = pct)) +
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))), stat = "identity")+
    labs(x = var, y = "Percentage", 
         fill = "NSP Class")
  
  i=i+1
  
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Decelerative tendencies with NSP Classes")

#'ASTV',    'MSTV', 'ALTV', 'MLTV', 'Variance','Width'
p=list()
namess<-c('ASTV',   'MSTV', 'ALTV', 'MLTV', 'Variance','Width')
i=1
for (var in namess){
  
  new_temp = temp %>%
    select(sym(var), NSP)%>%
    group_by(.data[[var]], NSP) %>% 
    mutate(test = n()) %>%
    distinct(.data[[var]], NSP, test) %>%
    group_by(.data[[var]]) %>%
    mutate(pct = test/sum(test)*100)
  
  p[[i]] <- ggplot(new_temp, aes(x= .data[[var]], y = pct)) +
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))), stat = "identity")+
    labs(x = var, y = "Percentage", 
         fill = "NSP Class")
  
  i=i+1
  
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Variability tendencies with NSP Classes")

#'UC'
p=list()
namess<-c('UC')
i=1
for (var in namess){
  
  new_temp = temp %>%
    select(sym(var), NSP)%>%
    group_by(.data[[var]], NSP) %>% 
    mutate(test = n()) %>%
    distinct(.data[[var]], NSP, test) %>%
    group_by(.data[[var]]) %>%
    mutate(pct = test/sum(test)*100)
  
  
  
  p[[i]] <- ggplot(new_temp, aes(x= .data[[var]], y = pct)) +
    geom_bar(aes(fill=factor(NSP, levels=c("3", "2", "1"))), stat = "identity")+
    labs(x = var, y = "Percentage", 
         fill = "NSP Class")
  
  i=i+1
  
}
grid.arrange(grobs=p,ncol=3, nrow=2, top = "Uterine contraction tendencies with NSP Classes")

#plotting pairwise colleration for machine variables grouped by function
library(psych)

dataframe0<-no_norm_df

#Multicollinearity and Variable Grouping

#1. Baseline characteristics
pairs.panels(dataframe0[,c('LB','Mean', 'Median', 'Mode', 'Tendency', 'nPoints','NSP')], 
             bg=c("red","yellow","blue")[dataframe0$NSP],
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
#1.Acceleration related characteristics

pairs.panels(dataframe0[,c('AC','FM','Nmax','Max','NSP')], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses  
)
#3. Decelerative characteristics
pairs.panels(dataframe0[,c('DL','DS','DP','Min','Nzeros','NSP')], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses  5,6,7,13,
)
#4. variability characteristics 
pairs.panels(dataframe0[,c('ASTV','ALTV','MSTV','MLTV','Width','Variance','NSP')], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses  5,6,7,13,
)
#5. uterine contraction characteristics. 

pairs.panels(dataframe0[,c('LB','FM','UC','DL','NSP')], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses  5,6,7,13,
)

#Exploring the correlation between the attributes other than class attribute

library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(round(cor(select_if(no_norm_df, is.numeric)),1), method="color", col=col(200),  
         type="lower", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, number.cex= .6,#Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

library(ggcorrplot)


machine_names<-c('nPoints','LB','AC',   'FM',   'UC',   'DL',   'DS',   'DP',   'ASTV', 'MSTV', 'ALTV', 'MLTV', 'Width', 'Min', 'Max',  'Nmax', 'Nzeros',   'Mode', 'Mean', 'Median',   'Variance', 'Tendency','NSP')

machine_data<-subset (no_norm_df, select = machine_names)
names <- c('Tendency','NSP')
machine_data[,names] <- as.data.frame(lapply(machine_data[,names] , as.integer))

morpho_names<-c('A' ,'B',   'C',    'D',    'E',    'AD',   'DE',   'LD',   'FS',   'SUSP',     'CLASS','NSP')

morpho_data<-no_norm_df[,morpho_names]

names <- c('A'  ,'B',   'C',    'D',    'E',    'AD',   'DE',   'LD',   'FS',   'SUSP',     'CLASS','NSP')
morpho_data[,names] <- lapply(morpho_data[,names] , as.integer)

#pairwise.complete.obs spearman
model.matrix(~0+., data=machine_data) %>% 
  cor(use="pairwise.complete.obs", method = 'spearman') %>% 
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2, tl.srt = 90, colors = c( "#1F51FF", "white","#DC143C"))+
  scale_x_discrete(position='top')+
  theme(aspect.ratio=3/4, axis.text.x.top = element_text(angle = 90,hjust = 0,vjust = 0.5))

model.matrix(~0+., data=morpho_data) %>% 
  cor(use="pairwise.complete.obs", method = 'spearman') %>% 
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2, tl.srt = 90, colors = c( "#1F51FF", "white","#DC143C"))+
  scale_x_discrete(position='top')+
  theme( axis.text.x.top = element_text(angle = 90,hjust = 0,vjust = 0.5))

# Function to print class imbalance and plot it
print_class_imbalance <- function(df, class_col) {
  
  # Calculate frequency and percentage
  class_summary <- df %>%
    group_by(!!sym(class_col)) %>%
    summarise(Frequency = n()) %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100)
  
  # Print the class imbalance
  print(class_summary)
  
  # Plot the class imbalance
  ggplot(class_summary, aes(x = as.factor(!!sym(class_col)), y = Frequency, label = round(Percentage, 2))) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(vjust = -0.5) +
    labs(x = "Class", y = "Frequency", title = paste("Class Imbalance for", class_col)) +
    theme_minimal()
}

# Example usage
print_class_imbalance(no_norm_df, "NSP")

# Example: Comparing 'LB' (Baseline Heart Rate) across NSP classes

# Load your dataset (replace 'no_norm_df' with your actual dataset)
data <- no_norm_df

# Convert NSP to a factor if it's not already
data$NSP <- as.factor(data$NSP)

# Create boxplot for Baseline Heart Rate (LB) across NSP classes
ggplot(data, aes(x = NSP, y = LB, fill = NSP)) +
  geom_boxplot() +
  labs(title = "Boxplot of Baseline Heart Rate (LB) by NSP Class",
       x = "NSP Class",
       y = "Baseline Heart Rate (LB)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")  # Optional: Color palette for fill

# Example: Comparing multiple variables across NSP classes

# Reshape data for plotting
library(tidyr)
data_long <- pivot_longer(data, 
                          cols = c(LB, AC, FM, UC, DL),  # Add other variables as needed
                          names_to = "Variable",
                          values_to = "Value")

# Create boxplots for multiple variables
ggplot(data_long, aes(x = NSP, y = Value, fill = NSP)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Boxplots of Various Variables by NSP Class",
       x = "NSP Class",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")  # Optional: Color palette for fill

