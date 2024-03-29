
library(tidyverse)
library(survey)
library(lodown)
library(mitools)
library(convey)
library(pbapply)
library(haven)
library(dplyr)
library(ineq)


setwd("G:/Stata data and do/UK Wealth and Assets/UKDA-7215-stata/stata/stata13_se/Data")
raw.dir <- "G:/Stata data and do/UK Wealth and Assets/UKDA-7215-stata/stata/stata13_se/Data"

# rename all datasets here
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

# rename variables within each dataset to conform to same structure here  TotWlth = TotWlthW1, 
### Wave 1
hhold1  <- hhold1  %>% 
  rename(HValue = HValueW1, VOther = DVOPrValW1, TotMort = TotMortW1, VMortP = OthMortW1, Tenure = Ten1W1, Case = CaseW1)
redhhold1 <- hhold1 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind1 <- ind1 %>%
  rename(Age = DVAge17W1, Case = CaseW1, Person = PersonW1)
redind1 <- ind1 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_1 <- merge(redhhold1, redind1, by = "Case")
merged_data_1 <- mutate(merged_data_1, Wave = 1)


### Wave 2
hhold2  <- hhold2  %>% 
  rename(HValue = HValueW2, TotMort = TotMortW2, VOther = DVHseValW2, VMortP = OthMortW2, Tenure = Ten1W2, Case = CASEW2)
redhhold2 <- hhold2 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind2 <- ind2 %>%
  rename(Age = DVAge17W2, Case = CaseW2, Person = PersonW2)
redind2 <- ind2 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_2 <- merge(redhhold2, redind2, by = "Case")
merged_data_2 <- mutate(merged_data_2, Wave = 2)


### wave 3
hhold3  <- hhold3  %>% 
  rename(HValue = HValueW3, TotMort = TotMortW3, VOther = DVHseValW3_sum, VMortP = OthMortW3_sum, Tenure = Ten1W3, Case = CASEW3)
redhhold3 <- hhold3 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

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
  rename(Age = DVAge17w4, Case = CASEW4, Person = Personw4)
redind4 <- ind4 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_4 <- merge(redhhold4, redind4, by = "Case")
merged_data_4 <- mutate(merged_data_4, Wave = 4)


### Wave5
hhold5  <- hhold5  %>% 
  rename(HValue = HValueW5, TotMort = TotmortW5, VOther = DVHseValW5_sum, VMortP = OthMortW5_sum, Tenure = Ten1W5, Case = CASEW5)
redhhold5 <- hhold5 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind5 <- ind5 %>%
  rename(Age = DVAge17W5, Case = CASEW5, Person = PersonW5)
redind5 <- ind5 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_5 <- merge(redhhold5, redind5, by = "Case")
merged_data_5 <- mutate(merged_data_5, Wave = 5)


### Wave6
hhold6  <- hhold6  %>% 
  rename(HValue = HValueW5, TotMort = TotMortR5, VOther = DVHseValR5_sum, VMortP = OthMortR5_sum, Tenure = Ten1W5, Case = CASER5)
redhhold6 <- hhold6 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind6 <- ind6 %>%
  rename(Age = DVAge17R5, Case = CASER5)
redind6 <- ind6 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age)

merged_data_6 <- merge(redhhold6, redind6, by = "Case")
merged_data_6 <- mutate(merged_data_5, Wave = 6)


### Wave7
hhold7  <- hhold7  %>% 
  rename(HValue = HValueW6, TotMort = TotMortR6, VOther = DVHseValR6_sum, VMortP = OthMortR6_sum, Tenure = Ten1W6, Case = CASER6)
redhhold7 <- hhold7 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind7 <- ind7 %>%
  rename(Age = DVAge17R6, Case = CASER6, Person = PersonW6)
redind7 <- ind7 %>%
  bind_rows(.id = "m") %>%
  select(Case, Age, Person)

merged_data_7 <- merge(redhhold7, redind7, by = "Case")
merged_data_7 <- mutate(merged_data_7, Wave = 7)


### Wave8
hhold8  <- hhold8  %>% 
  rename(HValue = hvaluer7, TotMort = TotmortR7, VOther = DVHseValR7_sum, VMortP = OthMortR7_sum, Tenure = ten1r7, Case = CASER7, Weight = R7xshhwgt)
redhhold8 <- hhold8 %>%
  bind_rows(.id = "m") %>%
  select(Case, HValue, VOther, TotMort, VMortP, Tenure)

ind8 <- ind8 %>%
  rename(Age = DVAge17R7, Case = CASER7, Person = personr7)
redind8 <- ind8 %>%
  select(Case, Age, Person)

merged_data_8 <- merge(redhhold8, redind8, by = "Case")
merged_data_8 <- mutate(merged_data_8, Wave = 8)




# save sample data as .csv file starting with waves 1 + 2...

write.csv(merged_data_1, "merged_data_1.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_2, "merged_data_2.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_3, "merged_data_3.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_4, "merged_data_4.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_5, "merged_data_5.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_6, "merged_data_6.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_7, "merged_data_7.csv", row.names=FALSE, quote=FALSE) 

write.csv(merged_data_8, "merged_data_8.csv", row.names=FALSE, quote=FALSE) 


#reduced data in loop...
waves <- seq(1,8, 1)
for(i in 1:length(waves)){
  print(waves[i])
  data <- read.csv(paste0("merged_data_", waves[i], ".csv")) %>%
    bind_rows(.id = "m") %>%
    select(Case, HValue, VOther, TotMort, VMortP, Tenure, Wave, Age)
  write.csv(data, paste0("testwaves", waves[i], "wkg.csv"))
  
}

### One way to Merge...

data_list <- list()

for (i in waves) {
  data <- read.csv(paste0("testwaves", i, "wkg.csv"))
  data_list[[i]] <- data
}

merged_data_final <- do.call(rbind, data_list)


### Generate Wealth Variables

merged_data_final <- merged_data_final %>%
  mutate(housevalue = ifelse(HValue == -7, NA, HValue))

merged_data_final <- merged_data_final %>%
  mutate(othervalue = ifelse(VOther == -7, NA, VOther))

merged_data_final <- merged_data_final %>%
  mutate(total.wealth = housevalue + othervalue)

summary(merged_data_final$total.wealth)

merged_data_final <- merged_data_final %>%
  mutate(othermortvalue = ifelse(VMortP == -7, NA, VMortP))

summary(merged_data_final$othermortvalue)

merged_data_final <- merged_data_final %>%
  mutate(totalmortvalue = ifelse(TotMort == -7, NA, TotMort))

summary(merged_data_final$totalmortvalue)


merged_data_final <- merged_data_final %>%
  mutate(net.wealth = total.wealth - (totalmortvalue+othermortvalue))

summary(merged_data_final$net.wealth)


# Test Plot histogram of net wealth by wave
filtered_data1 <- merged_data_final$net.wealth[merged_data_final$Wave == 1]

summary(filtered_data1)

# Customized histogram
hist(filtered_data1, 
     breaks = 800, # Number of bins
     col = "skyblue", # Color of bars
     main = "Histogram of Net Wealth", 
     xlab = "Net Wealth", 
     ylab = "Frequency",
     xlim = c(-300000, 1000000))


filtered_data2 <- merged_data_final$net.wealth[merged_data_final$Wave == 2]

summary(filtered_data2)


# Customized histogram
hist(filtered_data2, 
     breaks = 800, # Number of bins
     col = "skyblue", # Color of bars
     main = "Histogram of Net Wealth", 
     xlab = "Net Wealth", 
     ylab = "Frequency",
     xlim = c(-500000, 1000000))



# Create empty vectors to store Gini coefficients and variances for each wave
gini_coefficients <- numeric(length(waves))
gini_variances <- numeric(length(waves))

# Bootstrap function to estimate variance
bootstrap_gini <- function(data, B = 1000) {
  ginis <- numeric(B)
  n <- length(data)
  for (i in 1:B) {
    boot_sample <- sample(data, replace = TRUE)
    ginis[i] <- Gini(boot_sample)
  }
  return(var(ginis))
}

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave
  wave_data <- merged_data_final[merged_data_final$Wave == waves[i], ]
  
  # Calculate Gini coefficient for total wealth
  gini_coefficients[i] <- Gini(wave_data$net.wealth)
  
  # Estimate variance of Gini coefficient using bootstrapping
  gini_variances[i] <- bootstrap_gini(wave_data$net.wealth)
}

# Print or use the Gini coefficients and variances as needed
print(gini_coefficients)

print(gini_variances)


### Time series plot test

# Calculate average net wealth per wave ... waves arent working
# Create empty list to store mean net wealth for each wave
mean_net_wealth_list <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave
  wave_data <- merged_data_final[merged_data_final$Wave == waves[i], ]
  
  # Calculate mean net wealth for the current wave
  mean_net_wealth_list[[i]] <- mean(wave_data$net.wealth, na.rm = TRUE)
}

# Print the list of mean net wealth for each wave
print(mean_net_wealth_list)


### 90/10 is not working properly...
# Create empty list to store 90/10 ratio for each wave
p90_p10_ratio_list <- numeric(length(mean_net_wealth_list))

# Iterate through each wave
for (i in seq_along(mean_net_wealth_list)) {
  # Filter data for the current wave
  wave_data <- merged_data_final[merged_data_final$Wave == waves[i], ]
  
  # Calculate 90/10 ratio for the current wave
  p90_p10_ratio_list[i] <- quantile(wave_data$net.wealth, probs = 0.9, na.rm = TRUE) /
    quantile(wave_data$net.wealth, probs = 0.1, na.rm = TRUE)
}

# Print the result
print(p90_p10_ratio_list)








# Plot the time series line graph
# Create a data frame with waves, Gini coefficients, and mean net wealth
plot_data <- data.frame(Wave = unlist(waves),
                        Gini_Coefficient = unlist(gini_coefficients),
                        Mean_Net_Wealth = unlist(mean_net_wealth_list))

ggplot(plot_data, aes(x = Wave)) +
  geom_line(aes(y = Mean_Net_Wealth, color = "Mean Net Wealth")) +
  labs(x = "Wave", y = "Value", title = "Time Series of Mean Net Wealth") +
  scale_color_manual(values = c("red"), 
                     labels = c("Mean Net Wealth")) +
  theme_minimal()

ggplot(plot_data, aes(x = Wave)) +
  geom_line(aes(y = Gini_Coefficient, color = "Gini Coefficient")) +
  labs(x = "Wave", y = "Value", title = "Time Series of Gini Coefficient") +
  scale_color_manual(values = c("red"), 
                     labels = c("MGini Coefficient")) +
  theme_minimal()

# Plot waves vs Gini coefficient with mean net wealth represented by point size
plot(unlist(waves), unlist(gini_coefficients), 
     xlab = "Wave", ylab = "Gini Coefficient", 
     main = "Gini Coefficient vs Wave with Mean Net Wealth",
     pch = 16, col = "blue", cex = unlist(mean_net_wealth_list) / max(unlist(mean_net_wealth_list)) * 5)









### Create net HMR wealth (home equity) gini (conditional on homeownership)
# Create an empty vector to store Gini coefficients for each wave
netHMRgini_coefficients <- numeric(length(waves))

# Iterate through each wave
for (i in seq_along(waves)) {
  # Filter data for the current wave and homeowners
  wave_data <- merged_data_final[merged_data_final$Wave == waves[i] & merged_data_final$Tenure == "1", ]
  
  # Calculate net home equity wealth
  wave_data <- transform(wave_data, net_home_equity_wealth = HValue - TotMort)
  
  # Calculate Gini coefficient for net home equity wealth
  netHMRgini <- tryCatch({
    Gini(wave_data$net_home_equity_wealth)
  }, error = function(e) {
    print(paste("Error calculating Gini coefficient for Wave", waves[i]))
    print(e)
    NA  # Set NaN or NA for the Gini coefficient
  })
  
  # Store the Gini coefficient for the current wave
  netHMRgini_coefficients[i] <- netHMRgini
}

# Print or use the Gini coefficients as needed
print(netHMRgini_coefficients)



###
gini_coefficients <- as.list(gini_coefficients)

results <- data.frame(Wave = waves,
                      networth.gini_est = unlist(lapply(gini_coefficients, '[[', 'coefficients')),
                      networth.gini_se = unlist(lapply(gini_coefficients, '[[', 'variance')))


# Print the lengths of waves and gini_coefficients
print(length(waves))
print(length(gini_coefficients))

# Loop through each element of gini_coefficients and check its structure
for (i in seq_along(gini_coefficients)) {
  cat("Checking element", i, "\n")
  print(str(gini_coefficients[[i]]))
}

# Continue with your existing code
results <- data.frame(Wave = waves,
                      networth.gini_est = gini_coefficients,
                      networth.gini_se = gini_variances)

print(results)



