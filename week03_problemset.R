##Week03 Lab Session -- Problem Set Solutions -- SOCS0100

#1) Create a new public repository for the project (e.g. README.md, scripts)

#Follow the steps below. 

#Open your web browser and go to the GitHub website: https://github.com
#Log in to your GitHub account if you're not already logged in.
#Once you're logged in, follow these steps to create a new public repository named "week03" with a README.md file:
#Click the "+" icon in the upper right corner of the GitHub website.
#Select "New repository" from the dropdown menu.
#On the "Create a new repository" page, fill in the following information:
#Repository name: week03
#Choose the visibility (public) for your repository. 
#Optionally, you can add a description.
#You can choose to initialize the repository with a README file by checking the "Add a README file" option.
#After filling out the information, click the "Create repository" button.
#Once the repository is created, you'll be taken to the repository's main page on GitHub.
#To clone the repository to your computer, 
#click the green "Code" button and make sure "HTTPS" is selected. Copy the URL provided.
#Now, open your terminal or command prompt on your computer.
#Clone the GitHub repository to your computer using the following command, 
#replacing <repository_url> with the URL you copied
#git clone <repository_url> -- This will create a local copy of the "week03" repository on your computer.

#2) Import the dataset called “Natural disasters (EMDAT)”: 

#remove everything in your environment and setting up directory
setwd("~/Desktop")
rm(list = ls())

#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
    kableExtra,#table
  flextable, #table
  skimr) #a broad overview of data frame 

#options(scipen=999) 

#importing data in .csv format

data <- read.csv("EMDAT.csv", header = TRUE)

# 3) Inspect the data briefly and identify its structure
#tip: to avoid all scientific notation (e numbers) by setting options(scipen=999) at the top of the script
skim(data)

# 4) Select the variables that capture the information related to 
#deaths, injuries, homelessness caused by all disasters. You can rename the variables. 

df <- data %>% select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, country = Entity)

glimpse(df)

# 5) Create three tables showing the highest averages of deaths, injuries, 
#and homelessness (e.g. top 10)

# Calculate the averages
averages <- df %>%
  filter(!country %in% c("World", "Soviet Union")) %>%  # Remove "World" and "Soviet Union"
  group_by(country) %>%
  summarise(
    avg_deaths = mean(deaths, na.rm = TRUE),
    avg_injuries = mean(injuries, na.rm = TRUE),
    avg_homelessness = mean(homelessness, na.rm = TRUE)
  )

# Create tables for the top 10 averages
top_10_deaths <- averages %>%
  arrange(desc(avg_deaths)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Deaths")
# Apply formatting
top_10_deaths %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

top_10_injuries <- averages %>%
  arrange(desc(avg_injuries)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Injuries")
top_10_injuries %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

top_10_homelessness <- averages %>%
  arrange(desc(avg_homelessness)) %>%
  head(10) %>%
  kable(caption = "Top 10 Countries by Average Homelessness")
top_10_homelessness %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)

# 6) Create a new binary variable in the original dataset to
#show whether the number of deaths by all disasters is higher than 500 in a given year

df <- df %>% mutate(high_death = ifelse(deaths > 500, 1, 0))

# 7) Reshape the dataset (selected version) and save it as a separate dataset in your repository
df_wide <- df %>%
  pivot_wider(
    names_from = Year,       # Specify the columns to pivot
    values_from = c(deaths, injuries, homelessness, high_death)  # Specify the values columns
  )
# Save the df_wide data frame as a separate R data set
saveRDS(df_wide, "df_wide.rds")