#Install Dependencies
library(tidyverse)

#Load the DataSet
hospital <- read_csv("Data/Hospitcal_Coverage.csv")

#Convert ZIP and Fips to character
hospital <- hospital |>
  mutate(
    `Zip Code` = as.character(`Zip Code`),
    `Fips Code` = as.character(`Fips Code`)
  )

#Replace empty strings with NA
hospital <- hospital |>
  mutate(
    across(where(is.character), ~ na_if(.x, ""))
  )

#Inspect columns
colnames(hospital)

# Convert Week Ending to Date
hospital$`Week Ending` <- as.Date(hospital$`Week Ending`)

# Convert CCN to character (hospital ID)
hospital$CCN <- as.character(hospital$CCN)

# Convert Certified Bed Count to numeric
hospital$`Certified Bed Count` <- as.numeric(hospital$`Certified Bed Count`)

# Convert Days at 100% to numeric
hospital$`Days at 100%` <- as.numeric(hospital$`Days at 100%`)

# Convert Percentage of Required Fields Reported to numeric
hospital$`Percentage of Required Fields Reported` <- as.numeric(hospital$`Percentage of Required Fields Reported`)

# Convert the big block of obvious numeric columns
numeric_cols <- c(
  "inpatient_beds", "all_adult_hospital_inpatient_beds",
  "all_pediatric_inpatient_beds", "inpatient_beds_used",
  "all_adult_hospital_inpatient_bed_occupied",
  "all_pediatric_inpatient_bed_occupied", "total_icu_beds",
  "total_staffed_adult_icu_beds", "total_staffed_pediatric_icu_beds",
  "icu_beds_used", "staffed_adult_icu_bed_occupancy",
  "staffed_pediatric_icu_bed_occupancy",
  "total_adult_patients_hospitalized_confirmed_covid",
  "total_pediatric_patients_hospitalized_confirmed_covid",
  "staffed_icu_adult_patients_confirmed_covid",
  "staffed_icu_pediatric_patients_confirmed_covid",
  "previous_day_admission_adult_covid_confirmed",
  "previous_day_admission_adult_covid_confirmed_all",
  "previous_day_admission_pediatric_covid_confirmed",
  "previous_day_admission_peds_covid_confirmed_all",
  "total_patients_hospitalized_confirmed_influenza",
  "previous_day_admission_influenza_confirmed",
  "icu_patients_confirmed_influenza",
  "n95_respirators_days_available",
  "on_hand_supply_of_surgical_masks_in_days",
  "on_hand_supply_of_eye_protection_in_days",
  "on_hand_supply_of_single_use_surgical_gowns_in_days",
  "on_hand_supply_of_gloves_in_days"
)

hospital[numeric_cols] <- lapply(hospital[numeric_cols], as.numeric)

# Convert logical (Yes/No) supply/ability columns
logical_cols <- c(
  "able_to_maintain_n95_masks",
  "able_to_maintain_surgical_masks",
  "able_to_maintain_eye_protection",
  "able_to_maintain_single_use_gowns",
  "able_to_maintain_gloves"
)

#Cleaned Dataset
head(hospital)

# Save cleaned dataset
write_csv(hospital, "Data/cleaned_Hospital_Coverage.csv")

hospital[logical_cols] <- lapply(hospital[logical_cols], as.logical)