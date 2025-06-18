# ===================================================================
# Create Reproducible Dataset for Symbolic Incentives Paper
# 
# Description:
# This script reads the raw Stata data file ('00_Phase1_clean.dta'),
# selects only the variables necessary to reproduce the analyses in
# Supplementary Tables S1-S10, renames them for clarity, and
# saves the result as a clean CSV file for reviewers.
#
# Output: reproducible_dataset.csv
# ===================================================================

# 1. Load necessary libraries
# -------------------------------------------------------------------
# install.packages(c("haven", "dplyr", "readr")) # Uncomment to install
library(haven)
library(dplyr)
library(readr)

# 2. Load Raw Data
# -------------------------------------------------------------------
# Read the original Stata file
raw_data <- read_dta("Data check/00_Phase1_clean.dta")

# 3. Select and Rename Variables
# -------------------------------------------------------------------
# Identify all variables needed for Tables S1-S10
clean_data <- raw_data %>%
  select(
    # Core Identifiers & Treatment
    ID,
    TRT,
    wave,
    
    # Demographics & Controls
    female,
    age,
    race_arc,
    bloodtype,
    pastplasma,
    pre_avdon2_no_dup,
    median_income_zip,
    US_region,
    urban,
    county_covid_incidence,
    majority_dem,
    
    # Sample Selection Criteria
    open,
    nophone,
    
    # Outcome Variables (DVs)
    appt_24h,
    appt_48h,
    appt_7d,
    appt_ever,
    donation_p_13d,
    post_donation_p_ever,
    unsubscribe,
    post_donation_any_count_no_dup
  ) %>%
  # Rename columns for clarity and ease of use
  rename(
    participant_id = ID,
    treatment_group = TRT,
    is_female = female,
    race = race_arc,
    prior_plasma_donation = pastplasma,
    avg_annual_donations = pre_avdon2_no_dup,
    zip_median_income = median_income_zip,
    is_urban = urban,
    county_covid_per_100k = county_covid_incidence,
    is_majority_dem_county = majority_dem,
    email_opened = open,
    no_phone_contact = nophone,
    appointment_within_24h = appt_24h,
    appointment_within_48h = appt_48h,
    appointment_within_7d = appt_7d,
    appointment_anytime = appt_ever,
    donated_within_13d = donation_p_13d,
    donated_anytime = post_donation_p_ever,
    total_donations_post_treatment = post_donation_any_count_no_dup
  )

# 4. Pre-process and Create New Variables
# -------------------------------------------------------------------
# Create factors and new variables needed for the analysis tables
final_data <- clean_data %>%
  mutate(
    # Create a clear factor for the treatment group
    treatment_group = factor(ifelse(treatment_group == 1, "Symbolic Incentives", "Control"),
                             levels = c("Control", "Symbolic Incentives")),
    
    # Create a flag for the main "Study Sample" (used in most tables)
    is_study_sample = (email_opened == 1 & no_phone_contact == 1),
    
    # Create categorical versions of variables for easier grouping
    race = factor(case_when(
      race == 1 ~ "Asian",
      race == 2 ~ "Black",
      race == 3 ~ "Hispanic",
      race == 5 ~ "Other",
      race == 6 ~ "White",
      TRUE ~ "Missing/Other"
    )),
    
    bloodtype_group = factor(case_when(
      bloodtype == 1 ~ "A+", bloodtype == 2 ~ "A-",
      bloodtype == 3 ~ "AB+", bloodtype == 4 ~ "AB-",
      bloodtype == 5 ~ "B+", bloodtype == 6 ~ "B-",
      bloodtype == 7 ~ "O+", bloodtype == 8 ~ "O-",
      TRUE ~ "Unknown"
    )),
    
    us_region = factor(case_when(
      US_region == 1 ~ "Midwest",
      US_region == 2 ~ "Northeast",
      US_region == 3 ~ "South",
      US_region == 4 ~ "West",
      TRUE ~ "Unknown"
    )),
    
    # Create binary indicator for AB blood type (for Table S1)
    is_ab_blood_type = bloodtype %in% c(3, 4),
    
    # Create median split for annual donations (for Table S9)
    high_prior_donations = avg_annual_donations > median(avg_annual_donations, na.rm = TRUE),
    
    # Create the Intention-Behavior Gap variable (for Table S10)
    # 1 = Gap (Appt made, no donation), 0 = No Gap (Appt made, donation occurred)
    intention_behavior_gap = case_when(
      appointment_within_48h == 1 & donated_anytime == 0 ~ 1,
      appointment_within_48h == 1 & donated_anytime == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Select only the final, clean columns
  select(
    participant_id, treatment_group, wave, is_study_sample,
    is_female, age, race, bloodtype_group, is_ab_blood_type,
    prior_plasma_donation, avg_annual_donations, high_prior_donations,
    zip_median_income, us_region, is_urban, county_covid_per_100k, is_majority_dem_county,
    email_opened, no_phone_contact,
    appointment_within_24h, appointment_within_48h, appointment_within_7d, appointment_anytime,
    donated_within_13d, donated_anytime, intention_behavior_gap,
    unsubscribe, total_donations_post_treatment
  )

# 5. Save the Clean Data to CSV
# -------------------------------------------------------------------
write_csv(final_data, 'reproducible_dataset.csv')

# Confirmation message
print("Successfully created 'reproducible_dataset.csv'")
print(paste("The dataset has", nrow(final_data), "rows and", ncol(final_data), "columns."))
