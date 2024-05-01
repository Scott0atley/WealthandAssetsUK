
library(tidyverse)
library(survey)
library(lodown)
library(mitools)
library(convey)
library(pbapply)
library(haven)
library(dplyr)
library(ineq)
library(reldist)
library(ggthemes)
library(ggplot2)


### Set Up Directory Structure

setwd("G:/Stata data and do/UK Wealth and Assets/UKDA-7215-stata/stata/stata13_se/Data")
raw.dir <- "G:/Stata data and do/UK Wealth and Assets/UKDA-7215-stata/stata/stata13_se/Data"

# For ease, rename all datasets into a uniform format by reading the data into R. All household datasets renamed using the prefix hholdX and all individual datasets renamed indX. 

hhold1 <- read_dta("was_wave_1_hhold_eul_final_jan_2020.dta")

hhold2 <- read_dta("was_wave_2_hhold_eul_feb_2020.dta")

hhold3 <- read_dta("was_wave_3_hh_eul_march_2020.dta")

hhold4 <- read_dta("was_wave_4_hhold_eul_march_2020.dta")

hhold5 <- read_dta("was_wave_5_hhold_eul_sept_2020.dta")

hhold6 <- read_dta("was_round_5_hhold_eul_feb_20.dta")

hhold7 <- read_dta("was_round_6_hhold_eul_april_2022.dta")

hhold8 <- read_dta("was_round_7_hhold_eul_march_2022.dta")

ind1 <- read_dta("was_wave_1_person_eul_nov_2020.dta")

ind2 <- read_dta("was_wave_2_person_eul_nov_2020.dta")

ind3 <- read_dta("was_wave_3_person_eul_oct_2020.dta")

ind4 <- read_dta("was_wave_4_person_eul_oct_2020.dta")

ind5 <- read_dta("was_wave_5_person_eul_oct_2020.dta")

ind6 <- read_dta("was_round_5_person_eul_oct_2020.dta")

ind7 <- read_dta("was_round_6_person_eul_april_2022.dta")

ind8 <- read_dta("was_round_7_person_eul_june_2022.dta")


# Rename variables within each dataset to conform to same structure here  TotWlth = TotWlthW1 etc...

### Wave 1
### Household
hhold1  <- hhold1  %>% 
  rename(HValue = HValueW1, VOther = DVOPrValW1, TotMort = TotMortW1, VMortP = OthMortW1, Tenure = Ten1W1, Case = CaseW1, Region = GORW1)
redhhold1 <- hhold1 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)
### Individual
ind1 <- ind1 %>%
  rename(Age = DVAge17W1, Case = CaseW1, Person = PersonW1)
redind1 <- ind1 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)
### Merge Household and Individual datasets into a Merged dataset using the unique identifier 'Case'
merged_data_1 <- merge(redhhold1, redind1, by = "Case")
merged_data_1 <- mutate(merged_data_1, Wave = 1)


### Wave 2
hhold2  <- hhold2  %>% 
  rename(HValue = HValueW2, TotMort = TotMortW2, VOther = DVHseValW2, VMortP = OthMortW2, Tenure = Ten1W2, Case = CASEW2, Region = GORW2)
redhhold2 <- hhold2 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind2 <- ind2 %>%
  rename(Age = DVAge17W2, Case = CaseW2, Person = PersonW2)
redind2 <- ind2 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_2 <- merge(redhhold2, redind2, by = "Case")
merged_data_2 <- mutate(merged_data_2, Wave = 2)


### Wave 3
hhold3  <- hhold3  %>% 
  rename(HValue = HValueW3, TotMort = TotMortW3, VOther = DVHseValW3_sum, VMortP = OthMortW3_sum, Tenure = Ten1W3, Case = CASEW3, Region = GORW3)
redhhold3 <- hhold3 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind3 <- ind3 %>%
  rename(Age = DVAge17w3, Case = CASEW3, Person = personW3)
redind3 <- ind3 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_3 <- merge(redhhold3, redind3, by = "Case")
merged_data_3 <- mutate(merged_data_3, Wave = 3)


### Wave4
hhold4  <- hhold4  %>% 
  rename(HValue = HValueW4, TotMort = TotMortW4, VOther = DVHseValW4_sum, VMortP = OthMortW4_sum, Tenure = Ten1W4, Case = CASEW4)

redhhold4 <- hhold4 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind4 <- ind4 %>%
  rename(Age = DVAge17w4, Case = CASEW4, Person = Personw4, Region = GORW4)
redind4 <- ind4 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person, Region)

merged_data_4 <- merge(redhhold4, redind4, by = "Case")
merged_data_4 <- mutate(merged_data_4, Wave = 4)


### Wave5
hhold5  <- hhold5  %>% 
  rename(HValue = HValueW5, TotMort = TotmortW5, VOther = DVHseValW5_sum, VMortP = OthMortW5_sum, Tenure = Ten1W5, Case = CASEW5, Region = GORW5)
redhhold5 <- hhold5 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind5 <- ind5 %>%
  rename(Age = DVAge17W5, Case = CASEW5, Person = PersonW5)
redind5 <- ind5 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_5 <- merge(redhhold5, redind5, by = "Case")
merged_data_5 <- mutate(merged_data_5, Wave = 5)


### Wave6
hhold6  <- hhold6  %>% 
  rename(HValue = HValueW5, TotMort = TotMortR5, VOther = DVHseValR5_sum, VMortP = OthMortR5_sum, Tenure = Ten1W5, Case = CASER5, Region = GORR5)
redhhold6 <- hhold6 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind6 <- ind6 %>%
  rename(Age = DVAge17R5, Case = CASER5)

redind6 <- ind6 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age)


merged_data_6 <- merge(redhhold6, redind6, by = "Case")
merged_data_6 <- mutate(merged_data_5, Wave = 6)


### Wave7
hhold7  <- hhold7  %>% 
  rename(HValue = HValueW6, TotMort = TotMortR6, VOther = DVHseValR6_sum, VMortP = OthMortR6_sum, Tenure = Ten1W6, Case = CASER6, Region = GORR6)
redhhold7 <- hhold7 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind7 <- ind7 %>%
  rename(Age = DVAge17R6, Case = CASER6, Person = PersonW6)
redind7 <- ind7 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_7 <- merge(redhhold7, redind7, by = "Case")
merged_data_7 <- mutate(merged_data_7, Wave = 7)


### Wave8
hhold8  <- hhold8  %>% 
  rename(HValue = hvaluer7, TotMort = TotmortR7, VOther = DVHseValR7_sum, VMortP = OthMortR7_sum, Tenure = ten1r7, Case = CASER7, Weight = R7xshhwgt, Region = gorr7)
redhhold8 <- hhold8 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure, Region)

ind8 <- ind8 %>%
  rename(Age = DVAge17R7, Case = CASER7, Person = personr7)
redind8 <- ind8 %>%
  select(Case, Age, Person)

merged_data_8 <- merge(redhhold8, redind8, by = "Case")
merged_data_8 <- mutate(merged_data_8, Wave = 8)


#### save sample data as .csv file starting w

write.csv(merged_data_1, "merged_data_1.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_2, "merged_data_2.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_3, "merged_data_3.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_4, "merged_data_4.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_5, "merged_data_5.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_6, "merged_data_6.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_7, "merged_data_7.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_8, "merged_data_8.csv", row.names=FALSE, quote=FALSE) 



# Write the Data once more as a .csv file to use later for merging or pulling as single files...
waves <- seq(1,8, 1)
for(i in 1:length(waves)){
  print(waves[i])
  data <- read.csv(paste0("merged_data_", waves[i], ".csv")) %>%
    bind_rows(.id = "m") %>%
    select(Case, HValue, VOther, TotMort, VMortP, Tenure, Wave, Age, Person, Region)
  write.csv(data, paste0("testwaves", waves[i], "wkg.csv"))
  
}

### Merge data into a final merged dataset...

data_list <- list()

for (i in waves) {
  data <- read.csv(paste0("testwaves", i, "wkg.csv"))
  data_list[[i]] <- data
}

merged_data_final <- do.call(rbind, data_list)


### Re-code and generate Wealth related Varaibles for investigation

merged_data_final <- merged_data_final %>%
  mutate(housevalue = ifelse(HValue %in% c(-6, -7, -8, -9), NA, HValue))

merged_data_final <- merged_data_final %>%
  mutate(othervalue = ifelse(VOther %in% c(-6, -7, -8, -9), NA, VOther))

merged_data_final <- merged_data_final %>%
  mutate(total.wealth = housevalue + othervalue)

summary(merged_data_final$total.wealth)

merged_data_final <- merged_data_final %>%
  mutate(othermortvalue = ifelse(VMortP %in% c(-6, -7, -8, -9), NA, VMortP))

summary(merged_data_final$othermortvalue)

merged_data_final <- merged_data_final %>%
  mutate(totalmortvalue = ifelse(TotMort %in% c(-6, -7, -8, -9), NA, TotMort))

summary(merged_data_final$totalmortvalue)


merged_data_final <- merged_data_final %>%
  mutate(net.wealth = total.wealth - (totalmortvalue+othermortvalue))

summary(merged_data_final$net.wealth)

merged_data_final <- merged_data_final %>%
  mutate(HTenure = ifelse(Tenure %in% c(-6, -7, -8, -9), NA, Tenure))

merged_data_final$BTenure <- ifelse(merged_data_final$HTenure == 1, 1,
                            ifelse(merged_data_final$HTenure == 2, 1, 2))


### Now all variables have been transformed into appropriate format, create final data set to contain all appropriate variables with complete cases at all variables...


vars_to_include <- c("Case", "Person", "Wave", "Age", "housevalue", "othervalue", 
                     "total.wealth", "othermortvalue", "totalmortvalue", 
                     "net.wealth", "BTenure", "Region")


final_subset <- merged_data_final[, vars_to_include]

complete_cases <- complete.cases(final_subset)

df_complete <- final_subset[complete_cases, ]



region_labels <- c("North East", "North West", "Yorkshire & Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland")  

df_complete$Region <- factor(df_complete$Region, levels = unique(df_complete$Region), labels = region_labels)

home_labels <- c("Own Home", "Do Not Own Home")  

df_complete$BTenure <- factor(df_complete$BTenure, levels = unique(df_complete$BTenure), labels = home_labels)


### Mean and Median Home Value across Regions for all Waves

# Calculate mean house value
mean_housevalue <- aggregate(housevalue ~ Region + Wave, data = df_complete, FUN = mean)

# Calculate median house value
median_housevalue <- aggregate(housevalue ~ Region + Wave, data = df_complete, FUN = median)

# Merge mean and median data frames
mean_median_housevalue <- merge(mean_housevalue, median_housevalue, by = c("Region", "Wave"), suffixes = c("_mean", "_median"))

# View the resulting data frame
print(mean_median_housevalue)


### Graphs for the mean and median housevalue by region across each wave...
ggplot(mean_housevalue, aes(x = Wave, y = housevalue, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(x = "Wave", y = "Mean House Value", title = "Mean House Value by Region over Each Wave") +
  theme_minimal()

# Plot median house value by region over each wave
ggplot(median_housevalue, aes(x = Wave, y = housevalue, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(x = "Wave", y = "Median House Value", title = "Median House Value by Region over Each Wave") +
  theme_minimal()




### Create a dataframe for all important inequality statistics by Wave
# Calculate mean and median house price for each wave
house_stats <- aggregate(housevalue ~ Wave, data = df_complete, FUN = function(x) c(mean = mean(x), median = median(x)))

# Calculate mean net wealth per wave
mean_net_wealth <- aggregate(net.wealth ~ Wave, data = df_complete, FUN = mean)

# Calculate mean total wealth per wave
mean_total_wealth <- aggregate(total.wealth ~ Wave, data = df_complete, FUN = mean)

# Calculate Gini coefficient of net wealth per wave (using 'ineq' package)
# Get unique waves
waves <- unique(df_complete$Wave)

# Calculate Gini coefficients for each wave
gini_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave
  wave_data <- df_complete[df_complete$Wave == waves[i], ]
  
  # Calculate Gini coefficient for mean net wealth
  gini_coefficients[i] <- Gini(wave_data$net.wealth)
}

# Print or use the Gini coefficients as needed
print(gini_coefficients)

# Create a dataframe for Gini coefficients
gini_df <- data.frame(Wave = waves, Gini_Net_Wealth = gini_coefficients)

# Calculate Gini coefficients for each wave conditional on Home Ownership (BTenure) being equal to 1
ginihome_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave where BTenure = 1
  wave_data <- df_complete[df_complete$Wave == waves[i] & df_complete$BTenure == "Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  ginihome_coefficients[i] <- Gini(wave_data$net.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(ginihome_coefficients)

# Calculate Gini coefficients for each wave conditional on Home Ownership (BTenure) being equal to 1
gininohome_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave where BTenure = 1
  wave_data <- df_complete[df_complete$Wave == waves[i] & df_complete$BTenure == "Do Not Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  gininohome_coefficients[i] <- Gini(wave_data$net.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(gininohome_coefficients)

# Calculate Gini coefficients for each wave conditional on Home Ownership (BTenure) being equal to 1
ginitotalhome_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave where BTenure = 1
  wave_data <- df_complete[df_complete$Wave == waves[i] & df_complete$BTenure == "Own Home", ]
  
  # Calculate Gini coefficient for total wealth
  ginitotalhome_coefficients[i] <- Gini(wave_data$total.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(ginitotalhome_coefficients)

# Calculate Gini coefficients for each wave conditional on Home Ownership (BTenure) being equal to 1
ginitotalnohome_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave where BTenure = 1
  wave_data <- df_complete[df_complete$Wave == waves[i] & df_complete$BTenure == "Do Not Own Home", ]
  
  # Calculate Gini coefficient for total wealth
  ginitotalnohome_coefficients[i] <- Gini(wave_data$total.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(ginitotalnohome_coefficients)

# Calculate 90/10 ratio for each wave
quantiles <- aggregate(net.wealth ~ Wave, data = df_complete, FUN = function(x) quantile(x, c(0.1, 0.9)))
net_ratio_90_10 <- quantiles$net.wealth[,"90%"] / quantiles$net.wealth[,"10%"]

# Combine all statistics into one dataframe
stats_df <- data.frame(Wave = unique(df_complete$Wave),
                       Mean_House_Price = house_stats$housevalue[, "mean"],
                       Median_House_Price = house_stats$housevalue[, "median"],
                       Mean_Net_Wealth = mean_net_wealth$net.wealth,
                       Mean_Total_Wealth = mean_total_wealth$total.wealth,
                       Gini_Net_Wealth = gini_coefficients,
                       Gini_Net_Home_Ownership = ginihome_coefficients,
                       Gini_Net_No_Home_Ownership = gininohome_coefficients,
                       Gini_Total_Home_Ownership = ginitotalhome_coefficients,
                       Gini_Total_No_Home_Ownership = ginitotalnohome_coefficients, 
                       Net_Ratio_90_10 = net_ratio_90_10)

# View the resulting dataframe
print(stats_df)







### The same as above but broken down by Region as well...
# Create a dataframe for all important inequality statistics by Region and Wave

# Calculate mean and median house price for each Region and Wave
house_stats_region <- aggregate(housevalue ~ Region + Wave, data = df_complete, FUN = function(x) c(mean = mean(x), median = median(x)))

# Calculate mean net wealth per Region and Wave
mean_net_wealth_region <- aggregate(net.wealth ~ Region + Wave, data = df_complete, FUN = mean)

# Calculate total wealth per Region and Wave
total_wealth_region <- aggregate(total.wealth ~ Region + Wave, data = df_complete, FUN = mean)

# Calculate Gini coefficient of net wealth per Region and Wave
gini_coefficients_region <- numeric(nrow(house_stats_region))

# Iterate through each row of house_stats_region
for (i in seq_len(nrow(house_stats_region))) {
  # Filter data for the current Region and Wave
  region_wave_data <- df_complete[df_complete$Region == house_stats_region[i, "Region"] & df_complete$Wave == house_stats_region[i, "Wave"], ]
  
  # Calculate Gini coefficient for net wealth
  gini_coefficients_region[i] <- Gini(region_wave_data$net.wealth)
}

# Calculate Gini coefficient of net wealth per Region and Wave, conditional on BTenure = "Own Home"
gini_coefficients_region_home <- numeric(nrow(house_stats_region))

# Iterate through each row of house_stats_region
for (i in seq_len(nrow(house_stats_region))) {
  # Filter data for the current Region and Wave where BTenure = "Own Home"
  region_wave_data <- df_complete[df_complete$Region == house_stats_region[i, "Region"] & 
                                    df_complete$Wave == house_stats_region[i, "Wave"] & 
                                    df_complete$BTenure == "Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  gini_coefficients_region_home[i] <- Gini(region_wave_data$net.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(gini_coefficients_region_home)

# Calculate Gini coefficient of net wealth per Region and Wave, conditional on BTenure = "Own Home"
gini_coefficients_region_no_home <- numeric(nrow(house_stats_region))

# Iterate through each row of house_stats_region
for (i in seq_len(nrow(house_stats_region))) {
  # Filter data for the current Region and Wave where BTenure = "Own Home"
  region_wave_data <- df_complete[df_complete$Region == house_stats_region[i, "Region"] & 
                                    df_complete$Wave == house_stats_region[i, "Wave"] & 
                                    df_complete$BTenure == "Do Not Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  gini_coefficients_region_no_home[i] <- Gini(region_wave_data$net.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(gini_coefficients_region_no_home)


# Calculate Gini coefficient of net wealth per Region and Wave, conditional on BTenure = "Own Home"
gini_coefficients_region_home_total_home <- numeric(nrow(house_stats_region))

# Iterate through each row of house_stats_region
for (i in seq_len(nrow(house_stats_region))) {
  # Filter data for the current Region and Wave where BTenure = "Own Home"
  region_wave_data <- df_complete[df_complete$Region == house_stats_region[i, "Region"] & 
                                    df_complete$Wave == house_stats_region[i, "Wave"] & 
                                    df_complete$BTenure == "Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  gini_coefficients_region_home_total_home[i] <- Gini(region_wave_data$total.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(gini_coefficients_region_home_total_home)


# Calculate Gini coefficient of net wealth per Region and Wave, conditional on BTenure = "Own Home"
gini_coefficients_region_home_total_no_home <- numeric(nrow(house_stats_region))

# Iterate through each row of house_stats_region
for (i in seq_len(nrow(house_stats_region))) {
  # Filter data for the current Region and Wave where BTenure = "Own Home"
  region_wave_data <- df_complete[df_complete$Region == house_stats_region[i, "Region"] & 
                                    df_complete$Wave == house_stats_region[i, "Wave"] & 
                                    df_complete$BTenure == "Do Not Own Home", ]
  
  # Calculate Gini coefficient for net wealth
  gini_coefficients_region_home_total_no_home[i] <- Gini(region_wave_data$total.wealth)
}

# Print or use the conditional Gini coefficients as needed
print(gini_coefficients_region_home_total_no_home)


# Calculate 90/10 ratio for each Region and Wave
quantiles_region <- aggregate(net.wealth ~ Region + Wave, data = df_complete, FUN = function(x) quantile(x, c(0.1, 0.9)))
ratio_90_10_region <- quantiles_region$net.wealth[,"90%"] / quantiles_region$net.wealth[,"10%"]

# Combine all statistics into one dataframe
stats_df_region <- data.frame(Region = house_stats_region$Region,
                              Wave = house_stats_region$Wave,
                              Mean_House_Price = house_stats_region$housevalue[, "mean"],
                              Median_House_Price = house_stats_region$housevalue[, "median"],
                              Mean_Net_Wealth = mean_net_wealth_region$net.wealth,
                              Mean_Total_Wealth = total_wealth_region$total.wealth,
                              Gini_Net_Wealth = gini_coefficients_region,
                              Gini_Net_Home_Ownership = gini_coefficients_region_home,
                              Gini_Net_No_Home_Ownership = gini_coefficients_region_no_home,
                              Gini_Total_Home_Ownership = gini_coefficients_region_home_total_home,
                              Gini_Total_No_Home_Ownership = gini_coefficients_region_home_total_no_home, 
                              Net_Ratio_90_10 = ratio_90_10_region)

# View the resulting dataframe
print(stats_df_region)






### More Graphs - Bit redundant but they serve a niche purpose... 

# Plot the time series line graph
# Create plot_data dataframe
plot_data <- data.frame(Wave = rep(unique(stats_df$Wave), each = nrow(stats_df)),
                        Gini_Net_Wealth = rep(stats_df$Gini_Net_Wealth, times = nrow(stats_df)),
                        Mean_Net_Wealth = rep(stats_df$Mean_Net_Wealth, times = nrow(stats_df)))

# Simple scatter plot of Mean_Net_Wealth by Wave
ggplot(stats_df, aes(x = Wave, y = Mean_Net_Wealth)) +
  geom_point(color = "#1F77B4", alpha = 0.8, size = 3) +
  labs(x = "Wave", y = "Mean Net Wealth", 
       title = "Mean Net Wealth by Wave") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Simple scatter plot of Gini_Net_Wealth by Wave
ggplot(stats_df, aes(x = Wave, y = Gini_Net_Wealth)) +
  geom_point(color = "#1F77B4", alpha = 0.8, size = 3) +
  labs(x = "Wave", y = "Gini Coefficient", 
       title = "Gini Coefficient by Wave") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Plot waves vs Gini coefficient with mean net wealth represented by point size

ggplot(stats_df, aes(x = Mean_Net_Wealth, y = Gini_Net_Wealth, size = Mean_Net_Wealth)) +
  geom_point(color = "#1F77B4", alpha = 0.8, shape = 21, fill = "#FF7F0E") +
  scale_size_continuous(range = c(3, 12)) +
  labs(x = "Mean Net Wealth", y = "Gini Coefficient", 
       title = "Gini Coefficient vs Mean Net Wealth",
       subtitle = "Point size corresponds to Mean Net Wealth") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


### Lorenz Curve
# Calculate cumulative proportion of net wealth for the entire dataset
cum_prop_wealth <- cumsum(sort(df_complete$net.wealth)) / sum(df_complete$net.wealth)

# Create a dataframe to store cumulative proportion of net wealth
lorenz_curve_data <- data.frame(
  cum_prop_individuals = seq(0, 1, length.out = length(cum_prop_wealth)),
  cum_prop_wealth = cum_prop_wealth
)

# Plot Lorenz curve
ggplot(data = lorenz_curve_data, aes(x = cum_prop_individuals, y = cum_prop_wealth)) +
  geom_line(color = "blue") +
  labs(
    title = "Lorenz Curve for Net Wealth Across All Waves",
    x = "Cumulative Proportion of Individuals",
    y = "Cumulative Proportion of Net Wealth"
  ) +
  theme_minimal()


### Lornez Curves for Each Wave

# Create an empty vector to store Lorenz curves
lorenz_curves <- list()

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave
  wave_data <- df_complete[df_complete$Wave == waves[i], ]
  
  # Calculate cumulative proportion of net wealth
  cum_prop_wealth <- cumsum(sort(wave_data$net.wealth)) / sum(wave_data$net.wealth)
  
  # Calculate cumulative proportion of individuals
  cum_prop_individuals <- seq(0, 1, length.out = nrow(wave_data))
  
  # Store the Lorenz curve for the current wave
  lorenz_curves[[i]] <- data.frame(cum_prop_individuals, cum_prop_wealth)
}

# Plot Lorenz curves for each wave
par(mfrow = c(3, 3))  # Adjust rows and columns as needed
for (i in seq_along(waves)) {
  plot(lorenz_curves[[i]], type = "l", main = paste("Wave", waves[i]), xlab = "Cumulative Proportion of Individuals", ylab = "Cumulative Proportion of Net Wealth")
}



### Quantile plots by net.wealth
# Create an empty list to store boxplot data for each wave
boxplot_data <- list()

# Iterate through each wave
for (wave in waves) {
  # Subset data for the current wave
  wave_data <- df_complete[df_complete$Wave == wave, ]
  
  # Calculate quantiles of net wealth
  quantiles <- quantile(wave_data$net.wealth, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  
  # Create a dataframe to store quantiles and net wealth
  quantile_df <- data.frame(
    quantile = seq(0, 100, by = 10),
    net_wealth = quantiles,
    wave = wave
  )
  
  # Append the data to the list
  boxplot_data[[as.character(wave)]] <- quantile_df
}

# Plot boxplots for each wave
par(mfrow = c(3, 3))  # Adjust rows and columns as needed
for (i in seq_along(waves)) {
  boxplot(boxplot_data[[as.character(waves[i])]]$net_wealth ~ boxplot_data[[as.character(waves[i])]]$quantile, 
          main = paste("Wave", waves[i]), xlab = "Quantile", ylab = "Net Wealth", 
          col = "lightblue", border = "blue")
}




# Calculate quantiles of net wealth for the entire dataset
quantiles <- quantile(df_complete$net.wealth, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Create a dataframe to store quantiles and net wealth
quantile_df <- data.frame(
  quantile = seq(0, 100, by = 10),
  net_wealth = quantiles
)

# Plot boxplot for all waves together
boxplot(net.wealth ~ cut(df_complete$net.wealth, breaks = quantiles), 
        data = df_complete,
        main = "Net Wealth Distribution Across All Waves", 
        xlab = "Quantile", 
        ylab = "Net Wealth",
        col = "lightblue", 
        border = "blue")




