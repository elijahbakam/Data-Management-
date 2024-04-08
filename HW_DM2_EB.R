### Data Management II Homework ###
### Elijah Bakam 2/5/2024 ###


# Exercise 1 
# Load in my data
wbdata <- read_csv("/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Training Data Fall 23/world_bank_data_education.csv")
names(wbdata)[5:55] <- c(1970:2020) # helps get rid of the funky year system the wb has
# A) Reshape the data to a tidy dataset with a country-year unit of analysis
# When working with variables that have more than one word, and are separated by spaces, use
# backticks or back quotes ().

# First we are going to make the data long
long_wb <- wbdata %>%
  pivot_longer(names_to = "Year",
               values_to = "Values",
               c("1970":"2020"))


# Now lets try to widen it
wide_wb <- long_wb%>%
  pivot_wider(
    names_from = Series,
    values_from = Values, id_cols=c(Year, `Country Name`) # the id_cols function makes my variables columns
  )

# Now, I'll want to rename the variables and remove the NULL column that was created
wide_wb <- wide_wb %>%
  select(-5)
  
wide_wb <- wide_wb %>%
  rename("BL_sec_all" = "Barro-Lee: Average years of secondary schooling, age 15-19, total",
         "BL_sec_fem" = "Barro-Lee: Average years of secondary schooling, age 15-19, female")
# Last thing to make this data tidy is to make my year values only numeric values

# Assuming your data frame is named 'wbdata_wide' and you want to keep rows up to row number 'n'
n <- 13668  # Replace with the row number you want to keep

wide_wb <- wide_wb[1:n, ] # All my NA rows or mising rows were ones after a certain row so this removed all rows following that point


wide_wb <- wide_wb%>%
  rename("country" = "Country Name",
         "year" = "Year")

# Exercise 2 
# A
wdi_data <- read_csv("/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Training Data Fall 23/world_bank_data_literacy_rates.csv")

# Now we need to make this data tidy and hopefully do it in a better way than last time
wdi_data <- wdi_data %>%
 filter(!row_number()%in%533:537)

# Same row thing as last time, to get rid of non-country observations
n <- 646
wdi_data <- wdi_data[1:n,]

# Remove columns I don't want and rename some others
wdi_data <- wdi_data %>%
  select(-`Series Code`, -`Country Code`) %>%
  rename(country = `Country Name`,
         series = `Series Name`)

names(wdi_data)[5:65] <- c(1960:2020) # renames the year columns to get rid of the funky naming system they had
# Now I will make the data longer 
long_wdi <- wdi_data %>%
  pivot_longer(names_to = "year",
               values_to = "value",
               c("1960":"2018"))

# I have 2 straggling weird columns to get rid of
long_wdi <- long_wdi %>%
  select(-`1960 [YR1960]`, -`1961 [YR1961]`)



# Now, it's time to make the data wider once again 
wide_wdi <- long_wdi %>%
  pivot_wider(names_from = series, # I thiknI'm finally starting to understand this process. It's pulling the new column names from my 2 series variables
              values_from = value, id_cols=c(year, country)) # and here it's expanding my existing values column to match

# Remove column I don't need
wide_wdi <- wide_wdi%>%
  select(-5)

# Time to rename the variables, last step!
wide_wdi <- wide_wdi %>%
  rename("WDI_mortality_all" = "Mortality rate, under-5 (per 1,000 live births)",
         "WDI_mortality_fem" = "Mortality rate, under-5, female (per 1,000 live births)")

# Exercise 3
# Merge both datasets
complete_dat <- full_join(wide_wdi, wide_wb, by=c("year", "country"))

# To rename columns, I could've done this as well
# separate(col = year, into = c(NA,"year"), sep = "X") 

# Except for separating my "X" I would've done by " " because I want everything past the space in my OG
# Year column to be gone

# For missing values, like the Barro-Lee is only measured every 5 years
# You can ask R to imput and "predict" the value totals for each year in between
# Online just ask for code to imput values in between years in a dataset

# To remove dots and turn them to NA we can try this
complete_dat$WDI_mortality_all <- ifelse(complete_dat$WDI_mortality_all== "..", NA, complete_dat$WDI_mortality_all)
# Then to make ot numeric we do this
complete_dat$WDI_mortality_all <- as.numeric(complete_dat$WDI_mortality_all) # Can check by clicking the blue arrow dropdown to see if the var has been changed to numeric












































