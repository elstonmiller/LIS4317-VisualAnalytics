# Load packages
library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyquant)

# Gather data
Gtrend <- gtrends(keyword = "Copart",
                  geo = "US",
                  time = "2022-01-01 2026-03-25")$interest_over_time #Google Trends data
CRPT <- tq_get("CPRT", 
       get = "stock.prices", 
       from = "2022-01-1",
       periodicity = "weekly") # Copart stock data 
SNP <- tq_get("^GSPC",
              get = "stock.prices",
              from = "2022-01-01",
              periodicity = "weekly") # S&P 500 data 

#Normalize hits and adjusted
CRPT <- CRPT %>% 
  mutate(adjusted_norm = 100 * (adjusted - min(adjusted)) / (max(adjusted) - min(adjusted)))
Gtrend <- Gtrend %>% 
  mutate(hits_norm = 100 * (hits - min(hits)) / (max(hits) - min(hits)))
SNP <- SNP %>% 
  mutate(adjusted_norm = 100 * (adjusted - min(adjusted)) / (max(adjusted) - min(adjusted)))

#plot
ggplot()+
  geom_line(data = CRPT,
            aes(date,adjusted_norm, color = "Stock Price"),
            alpha = 0.6,
            size = 0.9)+
  geom_line(data = Gtrend, 
            aes(date,hits_norm, 
                color = "Google Trends"), 
            alpha = 0.6, 
            size = 0.9)+
  scale_color_manual(
    values = c(
      "Stock Price" = "blue",
      "Google Trends" = "red"))+
  labs(
    title = "Copart Stock Behavior Relative to U.S Google Search Activity",
    subtitle = "100 = max activity in this period, 0 = min activity in this period",
    x = "",
    y = "Growth in Variables",
    color = "")+
  theme_bw()

# Plot snp & copart 
ggplot()+
  geom_line(data = CRPT,
            aes(date,adjusted_norm, color = "CRPT"),
            alpha = 0.6,
            size = 0.9)+
  geom_line(data = SNP,
            aes(date,adjusted_norm, color = "S&P 500"),
            alpha = 0.6,
            size = 0.9)+
  scale_color_manual(
    values = c(
      "CRPT" = "blue",
      "S&P 500" = "grey"))+
  labs(
    title = "Copart Stock Behavior Relative to S&P 500 Index",
    subtitle = "100 = max price in this period, 0 = min price in this period",
    x = "",
    y = "Growth in Price",
    color = "")+
  theme_bw()

