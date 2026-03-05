# Load packages & data
library(tidyverse)
library(corrgram)
library(reshape2)
library(ggcorrplot)
library(GGally)
data <- read.csv("2010_2025_o100PA_League.csv")
data <- as_tibble(data)

#Create perPA values & clean 
data25 <- data %>% 
  filter(Season == 2025) %>%
  mutate(HRpPA = HR / PA) %>%
  mutate(HpPA = H / PA) %>%
  mutate(RBIpPA = RBI / PA) %>% 
  mutate(RpPA = R / PA)%>%
  mutate(WalkRate = BB / PA) %>%
  mutate(KRate = SO / PA)

cleanData <- tibble("Age" = data25$Age, "Stolen Bases" = data25$SB, "HR/PA" = data25$HRpPA, 
                    "Hits/PA" = data25$HpPA, "RBI/PA" = data25$RBIpPA, "R/PA" = data25$RpPA, 
                    "Walk Rate" = data25$WalkRate, "K Rate" = data25$KRate )

#Create Correlation Matrix 
Correlation <- round(cor(cleanData, use = "complete.obs"),2)

# Visual with ggcorr package
ggcorPlot <- ggcorrplot(Correlation,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("red","white","blue"))+
  labs(title = "2025 MLB Correlations",
       subtitle = "Players with atleast 100 plate apperances")
# Visual with corrgram package
corrgramPlot <- corrgram(cleanData,
         order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "MLB Offensive Statistics Correlations, 2025",
         )
# Visual with GGally 
ggallyPlot <- ggpairs(cleanData)+
  labs(title = "MLB Offensive Statistics Correlations",
       subtitle = "2025 Season with at least 100 apperances")+
  theme_bw()

# Linear Model
## Stardarzine data to percent units
cleanData$K_pct <- cleanData$`K Rate`*100
cleanData$H_pct <- cleanData$`Hits/PA`*100
cleanData$Lg <- data25$Lg

## Create Model
model1 <- lm(formula = H_pct ~ K_pct, data = cleanData)
summary(model1)

## Plot 
ggplot(cleanData, aes(K_pct, H_pct))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(
    title = "Hits by Strikeout Percentage",
    subtitle = "2025 MLB Regular Season, Players With at Least 100 PA",
    y = "Hits per Plate Appearence (%)",
    x = "Strikouts per Plate Appearence (%)"
  )+
  theme_bw()
  

