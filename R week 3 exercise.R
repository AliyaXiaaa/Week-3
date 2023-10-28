#2) Import the dataset called “Natural disasters (EMDAT)”: 

#remove everything in your environment and setting up directory
setwd("~/Desktop")
rm(list = ls())

#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, 
  kableExtra,
  flextable,
  skimr) 

#importing data in .csv format
EMDAT <- read.csv("EMDAT.csv", header = TRUE)

# 3) Inspect the data briefly and identify its structure
options(scipen = 999)
skim(EMDAT)

# 4) Select the variables that capture the information related to deaths, injuries, homelessness caused by all disasters. You can rename the variables. 
EMDAT_selected <- EMDAT %>%
  select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injured = injured_all_disasters, homeless = homeless_all_disasters)

view(EMDAT_selected)
glimpse(EMDAT_selected)

# 5) Create three tables showing the highest averages of deaths, injuries, 
#and homelessness (e.g. top 10)

#calculate the average
averages <- EMDAT_selected %>%
  group_by(Entity) %>%
  summarise(
    avg_deaths = mean(deaths, na.rm = TRUE),
    avg_injured = mean(injured, na.rm = TRUE),
    avg_homeless = mean(homeless, na.rm = TRUE)
  )

# Create tables for the top 10 averages
#创建一个top_10_deaths的新df，倒序，只选取前10，给标题
top_10_deaths <- averages %>%
  arrange(desc(avg_deaths)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Deaths")
#新df用斑马纹，选取经典格式，不占full width
top_10_deaths %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

#injured
top_10_injured <- averages %>%
    arrange(desc(avg_injured)) %>%
    head(10) %>%
    kable(caption = "Top 10 Countries by Average Injured")

top_10_injured %>%
    kable_styling("striped") %>%
    kable_classic(full_width = FALSE)

#homelss
top_10_homeless <- averages %>%
  arrange(desc(avg_homeless)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Homeless")

top_10_homeless %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

# 6) Create a new binary variable in the original dataset to
#show whether the number of deaths by all disasters is higher than 500 in a given year
#mutate一个新列，叫做high_death，如果death大于500，则1，小于500，则0/
df <- df %>% mutate(high_death = ifelse(deaths > 500, 1, 0))

view(df)
# 7) Reshape the dataset (selected version) and save it as a separate dataset in your repository
#names_from = Year: 这里指定了长格式数据中,Year 列中的每一个唯一值（如2000年，2001年等）都将成为 df_wide 中的新列
df_wide <- df %>%
  pivot_wider(
    names_from = Year,      
    values_from = c(deaths, injuries, homelessness, high_death) 
  )
saveRDS(df_wide, "df_wide.rds")



