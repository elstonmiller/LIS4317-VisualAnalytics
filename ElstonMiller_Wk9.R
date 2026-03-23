#install packages
library(tidyquant)
library(fredr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(lubridate)

# Gather and clean data 

fredr_set_key("apiKey")


## 10 yr treasury rates
tenyr <- fredr(series_id = "DGS10",
      observation_start = as.Date("2020-03-01"),
      observation_end = as.Date("2026-02-01"),
      frequency = "m")
tenyr <- tenyr %>% 
  select(-realtime_end,-realtime_start) %>%
  mutate(date = format(date,"%Y-%m"))

## unemployment rates
unemployment <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("2020-03-01"),
  frequency = "m")
unemployment <- unemployment %>% 
  select(-realtime_end,-realtime_start) %>%
  mutate(date = format(date,"%Y-%m"))

## S&P 500
snp500 <- tq_get("^GSPC",
                 from = "2020-03-01",
                 to = "2026-02-03")
### Clean S&P data
snp500 <- snp500 %>% 
  select(-open,-high,-low,-close,-volume) %>% # remove extra columns
  mutate(month = format(date, "%Y-%m")) %>% # Create a date column in line with data from FRED
  group_by(month) %>% # Group data by month to ensure only data from the first trading day of the month is saved
  slice(1) %>% # isolate the first trading day of the month
  ungroup() %>% # ungroup to bring all the first days together
  select(-date) %>% #remove the full date, because we have month which matches FRED 
  rename(date = month) %>% #rename month to date to match FRED data 
  arrange(date) %>% # Ensure data is ordered chronologically after everything we've done
  mutate(change = adjusted / lag(adjusted) - 1) # Calculate a month by month change % 

# Merge data into one df
df <- snp500 %>% 
  left_join(tenyr, by = "date") %>%
  left_join(unemployment, by = "date")
df <- df %>% 
  select(-series_id.x,-series_id.y) %>%
  rename(TenYearYield = value.x,
         UnemploymentRt = value.y,
         GSPC = adjusted) %>%
  select(-symbol)

# Plot 
ggplot(df, aes(TenYearYield, UnemploymentRt, color = change))+
  geom_point(alpha = 0.8)+
  geom_smooth(method = "loess")+
  scale_color_viridis_c(option = "E")+
  labs(title = "S&P 500 Behavior Against Ten Year Yield and Unemployment Rate",
       subtitle = "Monthly Observations between March 2020 and Feb 2026",
       y = "US Unemployment Rate",
       x = "10 Year U.S Tresury Yield",
       color = "S&P 500\nmonthly change")


# Extra practice - time series with multiple variables
rm(df,snp500,tenyr,unemployment)
Tech <- tq_get("XLK",
                 from = "1998-01-01",
                 to = "2026-02-03")
Health <- tq_get("XLV",
               from = "1998-01-01",
               to = "2026-02-03") 
Finance <- tq_get("XLF",
               from = "1998-01-01",
               to = "2026-02-03")
Consumer <- tq_get("XLP",
               from = "1998-01-01",
               to = "2026-02-03")
Altria <- tq_get("MO",
               from = "1998-12-22",
               to = "2026-02-03")
GSPC <- tq_get("^GSPC",
               from = "1998-12-22",
               to = "2026-02-03")
# Prep Data
prep <- function(df){
  df %>% 
    select(date, adjusted) %>% #isolate the only two variables we're measuring, date and adjusted price
    arrange(date) %>% # ensure data is still ordered by date
    mutate(year = year(date)) %>% #create column year to hold the isolated years
    group_by(year) %>% # create a special groups for each years data 
    slice_tail(n = 1) %>% # isolate the prices on the final trading day of the year
    ungroup() %>% # ungroup to bring these sliced values together
    mutate(change = adjusted / lag(adjusted) - 1) %>% #create a column, change, which calculates the percentage change from the previous year
    select(year, change) # select the two variables we will work with, yearly change and it's corresponding year
}
dfs <- list(Tech,Health,Finance,Consumer,Altria,GSPC) # create a list of the dfs to use lapply
cleandfs <- lapply(dfs, prep)
#Retrieve cleaned dataframes
Tech_clean <- cleandfs[[1]]
Health_clean <- cleandfs[[2]]
Finance_clean <- cleandfs[[3]]
Consumer_clean <- cleandfs[[4]]
Altria_clean <- cleandfs[[5]]
GSPC_clean <- cleandfs[[6]]

#Remove fluff 
rm(Altria,Consumer,Health,Tech,Finance,GSPC,cleandfs,dfs,prep)


# Merge into one tibble
full_data <- GSPC_clean %>% 
  rename(SP500 = change) %>%
  left_join(Tech_clean %>% rename(Tech = change), by = "year") %>%
  left_join(Health_clean %>% rename(Health = change), by = "year") %>%
  left_join(Finance_clean %>% rename(Finance = change), by = "year") %>%
  left_join(Consumer_clean %>% rename(Consumer = change), by = "year") %>%
  left_join(Altria_clean %>% rename(Tobacco = change), by = "year")
#put data into long format to plot
full_data_long <- full_data %>% 
  pivot_longer(
    cols = -year,
    names_to = "Sector",
    values_to = "YearlyReturn"
  )

# Plot 
ggplot(full_data_long, aes(year,YearlyReturn,color = Sector))+
  geom_line(size = 1.08)+
  facet_wrap(~Sector, ncol = 3)+
  scale_color_manual(values = c(
    "SP500" = "black",
    "Consumer" = "blue",
    "Finance" = "purple",
    "Health" = "forestgreen",
    "Tech" = "maroon",
    "Tobacco" = "orange"
    ))+
  theme(legend.position = "none")+
  labs(
    title = "Yearly Returns by Sector",
    subtitle = "1999 - 2026",
    y = "Yearly Return",
    x = ""
  )
