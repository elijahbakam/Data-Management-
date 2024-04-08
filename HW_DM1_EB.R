###### Data Management 1 Homework #####
##### Elijah Bakam 10/2/2023 ######

# 1) Load in the .rds file of the IDC 2021 powersharing data: “Training Data Summer 2022/IDC_training_2021.rds”
data <- readRDS("Training Data Fall 23/IDC_training_2021.rds")
# 2) Complete step B in a single command, piped together. Create a subset of the data that includes only the following countries, years, and variables:
# 1. Countries: U.S., China, Russia, France
# 2. Variables: country, gwno, year, subed_IDC, subtax_IDC, subpolice_IDC, auton_IDC, stconst_IDC
# 3. Years: 2015-2018
countries <- c("United States of America", "China", "France", "Russia (Soviet Union)")
data2 <- data %>%
  filter(country %in% countries) %>%
  select(gwno,
         country,
         year,
         subed_IDC,
         subtax_IDC,
         subpolice_IDC,
         auton_IDC,
         stconst_IDC) %>%
  subset(year %in% 2015:2018)
# 3) Save this smaller dataset as “Minipowersharing_YOURNAME.rds”. Ideally, specify a filepath in your save command so that it saves straight to your homework submission folder in google drive.
saveRDS(data2, "Elijah Bakam/Minipowersharing_ELIJAHBAKAM.rds")
# 4) Using the full dataset again, create a new variable, “subpower_additive” that is the sum of subed_IDC, subtax_IDC, 
# and subpolice_IDC. This index should take a value of N/A if any of the three component indicators is missing.
data3 <- data %>%
  mutate(subpower_additive = subed_IDC + subtax_IDC + subpolice_IDC)
# Bonus: Create a new version of the index, “subpower_additive_nm”, that assumes subed_IDC, sub- tax_IDC, and 
# subpolice_IDC take a value of 0 if they are missing. This version of the index should have no missing values.
data_bonus <- data %>%
  mutate(subpower_additive_nm = subed_IDC+ subtax_IDC + subpolice_IDC)
data_bonus$subpower_additive_nm[is.na(data_bonus$subpower_additive_nm)] <- 0 # make sure that it will take a value of 0 if it has mising values
# 5) Use summarise() or summarise_at() to answer the following:
# 1.What is the mean value of your first subpower index (subpower_additive) in the entire sample of countries,
# across the years 2010 through 2019? Hint: This should be one value. 2.What about in the year 2019 only?
summary <- data3 %>%
  subset(year %in% 2010:2019) %>%
  summarise(mean_sa= mean(subpower_additive, na.rm=TRUE)) # 1.408199

summary2 <- data3 %>%
  filter(year==2019) %>%
  summarise(mean_sa=mean(subpower_additive, na.rm=T)) # 1.416185

summary3 <- data_bonus%>%
  summarise(mean_sanm=mean(subpower_additive_nm, na.rm=T)) # 1.048962

# triple bonus

data3 <- data3 %>%
  filter(year==2019)
 # Couldn't rally figure this one out


















































