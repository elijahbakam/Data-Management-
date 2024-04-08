### Data Management 3 Homework ###
### Elijah Bakam ###
### 2/23/2024 ###
### Using IPE Dataset ###

# Firstly, I'm going to load in my IPE data
dat <- readRDS("/Users/elijahbakam/Downloads/dataverse_files/4. Graham_Tucker_IPE_v5.RDS")

# Exercise 1 
# 1a Collapse your country-year data to get global averages by year a single variable. 
# 1b Get global averages by year for multiple variables.

# First, I'm going to select a few variables to work with and whittle down the years
dat1 <- dat%>%
  select(year,
         country,
         ccode,
         gwno,
         gen_ps_F_ES,
         gen_ps_ES,
         yr_pri_F_1519_BL_ES,
         yr_pri_1519_BL_ES,
         sec_gdp_ES,
         edu_gdp_ES) %>%
  filter(year %in% 2000:2018)

# Exercise 1 
# 1a Collapse your country-year data to get global averages by year a single variable. 
# 1b Get global averages by year for multiple variables.

dat1a <- dat1 %>%
  group_by(year) %>% # Give us a year by year look 
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T)) # Tells R which variable to fidn the average of 

# 1b Get global averages by year for multiple variables.
dat1b <- dat1 %>%
  group_by(year) %>% # Give us a year by year look 
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T),
            ggen_ps_F_ES_mean = mean(gen_ps_F_ES, na.rm = T),
            edu_gdp_ES_mean = mean(edu_gdp_ES, na.rm = T))

# Exercise 2 Choose a region or continent and find the regional averages by year for multiple variables.
regional_mean <- dat1 %>%
  mutate(region = countrycode(gwno, "gwn", "region")) %>% # this code matches up gwno codes to their regional calls
  group_by(region, year) %>% # Give us a year by year look 
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T),
            ggen_ps_F_ES_mean = mean(gen_ps_F_ES, na.rm = T),
            edu_gdp_ES_mean = mean(edu_gdp_ES, na.rm = T))

n <- 133  # Replace with the row number you want to keep
regional_mean <- regional_mean[1:n, ] # All my NA rows or mising rows were ones after a certain row so this removed all rows following that point

# Exercise 3 Find the regional averages by decade.
decade_mean <- regional_mean %>%
  mutate(decade = year - year %% 10) %>% # the remainder function groups things by decade
  group_by(region, decade) %>%
  select(-2)

decade_mean <- dat1 %>%
  mutate(region = countrycode(gwno, "gwn", "region"),
         decade = year - year %% 10) %>% # this code matches up gwno codes to their regional calls
  group_by(region, decade) %>% # Give us a year by year look 
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T),
            ggen_ps_F_ES_mean = mean(gen_ps_F_ES, na.rm = T),
            edu_gdp_ES_mean = mean(edu_gdp_ES, na.rm = T))
# Same thing, get rid of the NA observations
n <- 14
decade_mean <- decade_mean[1:n, ]

# Exercises 4-10 Choose a set of countries within your region. Take out one cross-sectional year so each country has one value. 
# Create one dataframe with the global, regional, and local values for your variables.
# Make my vector with the countries I want 
selected <- c("France", "Cameroon", "Mali", "Senegal", "Cambodia", "Brazil")

final_dat <- dat1%>%
  filter(year== 2002) %>%
  mutate(region = countrycode(gwno, "gwn", "region")) 


final_dat <- final_dat%>%
  mutate(region = countrycode(gwno, "gwn", "region")) %>%
  filter(country %in% selected) %>%
  select(-3,
         -4,
         -6,
         -7,
         -8) # Getting rid of columns I no longer need 

# Okay now for the region  set
regions <- c("Sub-Saharan Africa", "Latin America & Caribbean", "Europe & Central Asia") # I will try to do the same thing here to create a subset of just regions for 2002


add_dat <- dat1%>%
  mutate(region = countrycode(gwno, "gwn", "region")) # first we need to add the regions to the original dataset

  
add_dat <- add_dat%>%
  filter(year==2002,
         region %in% regions) %>%
  group_by(region, year) %>%
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T),
            ggen_ps_F_ES_mean = mean(gen_ps_F_ES, na.rm = T),
            edu_gdp_ES_mean = mean(edu_gdp_ES, na.rm = T)) #now i have regions serving as my "countries" instead with their averages showing for each variable just like in my subset of countries set

add_dat <- add_dat%>%
  rename("country" = "region")

add_dat <- add_dat %>%
  rename("gen_ps_ES_mean_reg" = "gen_ps_ES_mean",
         "ggen_ps_F_ES_mean_reg" = "ggen_ps_F_ES_mean",
         "edu_gdp_ES_mean_reg" = "edu_gdp_ES_mean") # Want to add specification for when I merge the sets, that this is a regional average and not just a 2002 total like teh countries are

# Okay now we can merge with fulljoin
merged <- full_join(final_dat,add_dat, by = "country") 

merged <- merged%>%
  select(-7,
         -8) # purging certain columns
    
merged <- merged %>%
  mutate(year.x = ifelse(is.na(year.x), 2002, year.x))%>% # had to replace the NA's that came up for the regions for some reason with our confirmed year sample of 2002
  rename("year" = "year.x")

merged <- merged %>%
  mutate(year = ifelse(year == 2022, 2002, year)) # slight snaffu that was tripping me up because I realized after the fact I accidentally coded the NA's to change to 2022, not 2002
  
# Okay almost done, now I'll need to create a 3rd dataset to merge with just the global average for 2002 and then add it on
global <- dat1%>%
  filter(year==2002)

global <- global%>%
  group_by(year) %>%
  summarise(gen_ps_ES_mean = mean(gen_ps_ES, na.rm = T),
            ggen_ps_F_ES_mean = mean(gen_ps_F_ES, na.rm = T),
            edu_gdp_ES_mean = mean(edu_gdp_ES, na.rm = T)) # Now since I only want a global average for these, I can just group by year
  
# I realized, I don't have a location name for this average so I'll add a column to the left of my data
# I'll also rename the variables so we know they're global means
global <- global %>%
  mutate(country = "Global") %>%
  select(country, everything()) %>%
  rename("gen_ps_ES_mean_glo" = "gen_ps_ES_mean",
         "ggen_ps_F_ES_mean_glo" = "ggen_ps_F_ES_mean",
         "edu_gdp_ES_mean_glo" = "edu_gdp_ES_mean")

# Now I can simply merge that to the other data by country
merged1 <- full_join(merged, global, by = "country")

# THen, I will repeat the process of filling in a year for the NA global brought
# And rename my 'country' column to "location' since that fits better
# And get rid of the wonky names + delete a column
merged1 <- merged1 %>%
  mutate(year.x = ifelse(is.na(year.x), 2002, year.x))%>% # had to replace the NA's that came up for the regions for some reason with our confirmed year sample of 2002
  rename("year" = "year.x",
         "location" = "country") %>%
  select(-1,
         -10)
  
#### END OF HOMEWORK ####
































































































