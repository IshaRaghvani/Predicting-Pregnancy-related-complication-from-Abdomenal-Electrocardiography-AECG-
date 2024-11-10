install.packages("readxl")  # For reading Excel files
install.packages("pheatmap") # For heatmaps
install.packages("ggplot2")  # For plotting
install.packages("reshape2") # For reshaping data
install.packages("RColorBrewer") # For color palettes

library(readxl)
library(pheatmap)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
# Load data from Excel file
# Set working directory to your R project directory


rawdata <- read_excel("artificial_FINAL.xlsx")

# View the first few rows of the dataset
head(rawdata)
str(rawdata)
num_columns <- ncol(rawdata)
print(num_columns)
df1<-rawdata[!complete.cases(rawdata),]
df1<-rawdata %>% drop_na()
df1<- subset (df1, select = -c(FileName,Date,SegFile))
df1<-as.data.frame(df1)
head(df1)



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load your data (replace 'your_data_file.csv' with your actual file name)
data <- read_excel('artificial_FINAL.xlsx')
head(data)
# Ensure your data includes the 'ASTV' and 'NSP' columns
# For demonstration, let's summarize ASTV values by NSP classification
colnames(data)
# Aggregate the data by NSP and calculate mean ASTV
data_summary <- data %>%
  group_by(NSP) %>%
  summarise(Mean_ASTV = mean(ASTV, na.rm = TRUE))
data_summary <- data %>%
  # Convert NSP to factor to ensure proper ordering in the plot
  mutate(NSP = factor(NSP, levels = c(1, 2, 3), labels = c("Normal", "Suspect", "Pathological"))) %>%
  group_by(NSP) %>%
  summarise(Mean_ASTV = mean(ASTV, na.rm = TRUE), Count = n()) %>%
  ungroup() %>%
  # Calculate percentage of ASTV
  mutate(Percentage_ASTV = (Mean_ASTV / sum(Mean_ASTV)) * 100)

# Create a stacked bar chart
ggplot(data_summary, aes(x = NSP, y = Percentage_ASTV, fill = NSP)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of ASTV by NSP Classification",
       x = "NSP Classification",
       y = "Percentage of ASTV",
       fill = "NSP Classification") +
  theme_minimal()

# Convert NSP to a factor with appropriate labels
data <- data %>%
  mutate(NSP = factor(NSP, levels = c(1, 2, 3), labels = c('Normal', 'Suspect', 'Pathological')))

# Bin UC into categories (e.g., low, medium, high)
data <- data %>%
  mutate(UC_Category = cut(UC,
                           breaks = c(-Inf, 5, 10, Inf),
                           labels = c('Low', 'Medium', 'High')))




