# Set libraries, & import data
library(tidyverse)
top1000 <- read.csv("IMDb top 1000 movies.csv")

# Standardize duration as numeric
top1000$Duration <- as.numeric(gsub(" min","",top1000$Duration))
top1000 <- top1000[-179,] # Remove outlier 

# Save notable values 
meanDuration <- mean(top1000$Duration)
Q1 <- as.numeric(summary(top1000$Duration)[2])
Q3 <- as.numeric(summary(top1000$Duration)[5])
Median <- as.numeric(summary(top1000$Duration)[3])

# Plot histogram of duration
ggplot(top1000,aes(x = Duration))+
  geom_histogram(fill = "skyblue", 
                 color = "black",
                 binwidth = 10)+
  labs(
    title = "IMDb Top 1000 Rated Movie Runtimes",
    y = "Count",
    x = "Duration (min)"
  )+
  geom_vline(
    xintercept = meanDuration,
    linewidth = 0.8,
    linetype = "dashed",
    color = "red"
  )+
  geom_vline(
    xintercept = Q1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black")+
  geom_vline(
    xintercept = Q3,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black")+
  annotate("text", 
           x = meanDuration + 38, 
           y = 147, 
           label = paste(
             "Mean = ",round(meanDuration,1),"min"
           ),
           size = 3,
           color = "red"
           )+
  annotate("text", 
           x = meanDuration + 38, 
           y = 140, 
           label = paste(
             "Q3 = ",round(Q3,1),"min"
           ),
           size = 3,
           color = "black"
  )+
  annotate("text", 
           x = meanDuration - 45, 
           y = 140, 
           label = paste(
             "Q1 = ",round(Q1,1),"min"
           ),
           size = 3,
           color = "black"
  )+
  theme_bw()

# Plot boxplot of duration
ggplot(top1000, 
       aes(x = Duration, y = ""))+
  geom_boxplot(fill = "skyblue",
               color = "black")+
  coord_flip()+
  labs(title = "IMDb Top 1000 Rated Movie Runtimes",
       x = "Duration (min)",
       y = ""
  )+
  stat_summary(fun = mean, 
               geom = "point",
               color = "red",
               size = 2
               )+
  annotate("text",
           x = meanDuration+5,
           y = 1.09,
           label = paste("Mean = ",round(meanDuration,1)),
           color = "red",
           size = 3)+
  annotate("text",
           x = Q1+3,
           y = 0.56,
           label = paste("Q1 = ",round(Q1,1)),
           color = "black",
           size = 3)+
  annotate("text",
           x = Q3-2,
           y = 1.45,
           label = paste("Q3 = ",round(Q3,1)),
           color = "black",
           size = 3)+
  annotate("text",
           x = Median,
           y = 1.46,
           label = paste("Median = ",round(Median,1)),
           color = "black",
           size = 3)+
  theme_bw()
            
