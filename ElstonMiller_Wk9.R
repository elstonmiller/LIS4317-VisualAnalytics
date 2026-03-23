#install packages
library(tidyquant)
library(fredr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# Gather and clean data 

fredr_set_key("156a20b4f49aa5c6be5225a3808b659a")


## 10 yr treasury rates
tenyr <- fredr(series_id = "DGS10",
      observation_start = as.Date("1962-01-01"),
      observation_end = as.Date("2026-02-01"),
      frequency = "m")
tenyr <- tenyr %>% 
  select(-realtime_end,-realtime_start) %>%
  mutate(date = format(date,"%Y-%m"))

## unemployment rates
unemployment <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1962-01-01"),
  frequency = "m")
unemployment <- unemployment %>% 
  select(-realtime_end,-realtime_start) %>%
  mutate(date = format(date,"%Y-%m"))

## S&P 500
snp500 <- tq_get("^GSPC",
                 from = "1962-01-01",
                 to = "2026-02-03")
snp500 <- snp500 %>% 
  select(-open,-high,-low,-close,-volume) %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-date) %>% 
  rename(date = month) %>% 
  arrange(date) %>% 
  mutate(change = adjusted / lag(adjusted) - 1)

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

# Explore lagged data as well 
df <- df %>% 
  mutate(
    laggedtenyear = lag(TenYearYield),
    laggedunemployment = lag(UnemploymentRt)
)
# Plot 
ggplot(df, aes(laggedtenyear, laggedunemployment, color = change))+
  geom_point(alpha = 0.8)+
  scale_color_viridis_c(option = "E")+
  labs(title = "S&P 500 Monthly Movement Against Unemployment Rate and Ten Year Yield",
       subtitle = "Monthly Observations between Feb 1962 and Feb 2026",
       y = "US Unemployment Rate",
       x = "10 Year U.S Tresury Yield",
       color = "S&P 500\nmonthly change")

# Load in more data for line comparison 
rm(df,snp500,tenyr,unemployment)
Tech <- tq_get("XLK",
                 from = "1962-01-01",
                 to = "2026-02-03")
Health <- tq_get("XLV",
               from = "1962-01-01",
               to = "2026-02-03") 
Finance <- tq_get("XLF",
               from = "1962-01-01",
               to = "2026-02-03")
Consumer <- tq_get("XLP",
               from = "1962-01-01",
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
    select(date, adjusted) %>%
    arrange(date) %>%
    mutate(year = year(date)) %>%               # extract year
    group_by(year) %>%
    slice_tail(n = 1) %>%                      # take last trading day of year
    ungroup() %>%
    mutate(change = adjusted / lag(adjusted) - 1) %>%
    select(year, change)
}
dfs <- list(Tech,Health,Finance,Consumer,Altria,GSPC)
cleandfs <- lapply(dfs, prep)
Tech_clean <- cleandfs[[1]]
Health_clean <- cleandfs[[2]]
Finance_clean <- cleandfs[[3]]
Consumer_clean <- cleandfs[[4]]
Altria_clean <- cleandfs[[5]]
GSPC_clean <- cleandfs[[6]]

rm(Altria,Consumer,Health,Tech,Finance,GSPC,cleandfs,dfs,prep)


# Merge into one tibble
full_data <- GSPC_clean %>% 
  rename(SP500 = change) %>%
  left_join(Tech_clean %>% rename(Tech = change), by = "quarter") %>%
  left_join(Health_clean %>% rename(Health = change), by = "quarter") %>%
  left_join(Finance_clean %>% rename(Finance = change), by = "quarter") %>%
  left_join(Consumer_clean %>% rename(Consumer = change), by = "quarter") %>%
  left_join(Altria_clean %>% rename(Tobacco = change), by = "quarter")

full_data_long <- full_data %>% 
  pivot_longer(
    cols = -date,
    names_to = "sector",
    values_to = "monthlyReturn"
  )

# Plot 
ggplot(full_data_long, aes(date,monthlyReturn,color = sector))+
  geom_line()
