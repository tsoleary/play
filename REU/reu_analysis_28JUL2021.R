# ------------------------------------------------------------------------------
# Lauren & Isha -- Data analysis for their acclimation project
# July 28, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(betareg)
library(emmeans)

# Load data --------------------------------------------------------------------

# Acclimation data
acc_dat <- read_csv("REU/REU_AcclimationData - Sheet1 (1) (2).csv")

# Recovery data
rec_dat <- read_csv("REU/Recovery Temp REU Embryo Survival  (1).csv") %>%
  # Clean space in St. Kitts
  mutate(Genotype = str_remove_all(Genotype, " ")) 

# Run analyses -----------------------------------------------------------------

# Acclimation data -- recovery all at 25°C -----

# Acclimation & Region
region_beta_reg_acc <- betareg(Survival ~ 
                                 Region + Acclimation + Region:Acclimation,
                               data = acc_dat)
# Print results
joint_tests(region_beta_reg_acc)

# Acclimation & Genotype
geno_beta_reg_acc <- betareg(Survival ~ 
                               Genotype + Acclimation + Genotype:Acclimation,
                             data = acc_dat)
# Print results
joint_tests(geno_beta_reg_acc)

# Recovery data -- variable post heat shock recovery temperatures -----

# NEED to transcform Survival data for a beta regression
# Recovery data survival includes 0 values!
# rec_dat <- rec_dat %>%
#   mutate(Survival = ifelse(Survival == 0, Survival + 0.025, Survival))

# Acclimation & Region 
region_beta_reg_rec <- betareg(Survival ~ 
                                 Recovery + Region + Acclimation + 
                                 Region:Acclimation:Recovery, 
                               data = rec_dat)
# Print results
joint_tests(region_beta_reg_rec)

# Acclimation & Genotype
geno_beta_reg_rec <- betareg(Survival ~ 
                               Recovery + Genotype + Acclimation + 
                               Genotype:Acclimation:Recovery,
                             data = rec_dat)
# Print results
joint_tests(geno_beta_reg_rec)
