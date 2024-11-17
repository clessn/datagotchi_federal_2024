# attitudes


## issue_aboriginals ----------------------------------------------------




## issue_immigration -----------------------------------------------------




## issue_refugee ---------------------------------------------------------




## issue_gun_control -----------------------------------------------------




## issue_economy ---------------------------------------------------------




## issue_health ----------------------------------------------------------




## issue_unions ----------------------------------------------------------




## issue_quebec -----------------------------------------------------------




## issue_french ----------------------------------------------------------




## issue_religious_accommodation -----------------------------------------




## issue_sex_ed ----------------------------------------------------------




## issue_carbon_tax ------------------------------------------------------




## issue_oil -------------------------------------------------------------




## issue_climate_change --------------------------------------------------




## issue_lgbtq+ ----------------------------------------------------------




## issue_israel ----------------------------------------------------------




## issue_ukraine ---------------------------------------------------------

table(data_raw$issue_ukraine)
attributes(data_raw$issue_ukraine)
data_clean$iss_help_ukraine <- NA
data_clean$iss_help_ukraine[data_raw$issue_ukraine == 1] <- 0
data_clean$iss_help_ukraine[data_raw$issue_ukraine == 2] <- 0.25
data_clean$iss_help_ukraine[data_raw$issue_ukraine == 3] <- 0.5
data_clean$iss_help_ukraine[data_raw$issue_ukraine == 4] <- 0.75
data_clean$iss_help_ukraine[data_raw$issue_ukraine == 5] <- 1
table(data_clean$iss_help_ukraine)
  
## issue_mi --------------------------------------------------------------

table(data_raw$issue_mi)
attributes(data_raw$issue_mi)
data_clean$iss_most_imp <- (data_raw$issue_mi)
table(data_clean$iss_most_imp)

# issue_economy_curren ----------------------------------------------------

table(data_raw$issue_economy_curren)
attributes(data_raw$issue_economy_curren)
data_clean$iss_good_econo <- NA
data_clean$iss_good_econo[data_raw$issue_economy_curren == 1] <- 0
data_clean$iss_good_econo[data_raw$issue_economy_curren == 2] <- 0.33
data_clean$iss_good_econo[data_raw$issue_economy_curren == 3] <- 0.66
data_clean$iss_good_econo[data_raw$issue_economy_curren == 4] <- 1
table(data_clean$iss_good_econo)

# issue_distinction_us ----------------------------------------------------

table(data_raw$issue_distinction_us)
attributes(data_raw$issue_distinction_us)
data_clean$iss_prodistinct_us <- NA
data_clean$iss_prodistinct_us[data_raw$issue_distinction_us == 1] <- 1
data_clean$iss_prodistinct_us[data_raw$issue_distinction_us == 2] <- 0.66
data_clean$iss_prodistinct_us[data_raw$issue_distinction_us == 3] <- 0.33
data_clean$iss_prodistinct_us[data_raw$issue_distinction_us == 4] <- 0
table(data_clean$iss_prodistinct_us)

# issue_nationalanthem ----------------------------------------------------

table(data_raw$issue_nationalanthem)
data_clean$iss_nationalanthem_bilingual <- NA
data_clean$iss_nationalanthem_bilingual[data_raw$issue_nationalanthem == 1] <- 0
data_clean$iss_nationalanthem_bilingual[data_raw$issue_nationalanthem == 2] <- 0.33
data_clean$iss_nationalanthem_bilingual[data_raw$issue_nationalanthem == 3] <- 0.66
data_clean$iss_nationalanthem_bilingual[data_raw$issue_nationalanthem == 4] <- 1
table(data_clean$iss_nationalanthem_bilingual)


# issue_immig_illegal -----------------------------------------------------

table(data_raw$issue_immig_illegal)
attributes(data_raw$issue_immig_illegal)
data_clean$iss_concern_illimmig <- NA
data_clean$iss_concern_illimmig[data_raw$issue_immig_illegal == 4] <- 0
data_clean$iss_concern_illimmig[data_raw$issue_immig_illegal == 5] <- 0.33
data_clean$iss_concern_illimmig[data_raw$issue_immig_illegal == 6] <- 0.66
data_clean$iss_concern_illimmig[data_raw$issue_immig_illegal == 3] <- 1
table(data_clean$iss_concern_illimmig)

# issue_immig_house -------------------------------------------------------

table(data_raw$issue_immig_house)
data_clean$iss_immig_nohouse <- NA
data_clean$iss_immig_nohouse[data_raw$issue_immig_house == 1] <- 0
data_clean$iss_immig_nohouse[data_raw$issue_immig_house == 2] <- 0.33
data_clean$iss_immig_nohouse[data_raw$issue_immig_house == 3] <- 0.66
data_clean$iss_immig_nohouse[data_raw$issue_immig_house == 4] <- 1
table(data_clean$iss_immig_nohouse)

# issue_gouv_house -------------------------------------------------------

table(data_raw$issue_gouv_house)
data_clean$iss_gouv_act_house <- NA
data_clean$iss_gouv_act_house[data_raw$issue_immig_house == 1] <- 0
data_clean$iss_gouv_act_house[data_raw$issue_immig_house == 2] <- 0.33
data_clean$iss_gouv_act_house[data_raw$issue_immig_house == 3] <- 0.66
data_clean$iss_gouv_act_house[data_raw$issue_immig_house == 4] <- 1
table(data_clean$iss_gouv_act_house)

# issue_bilingual ---------------------------------------------------------

table(data_raw$issue_bilingual)
data_clean$iss_bilingual_can <- NA
data_clean$iss_bilingual_can[data_raw$issue_bilingual == 1] <- 0
data_clean$iss_bilingual_can[data_raw$issue_bilingual == 2] <- 0.33
data_clean$iss_bilingual_can[data_raw$issue_bilingual == 3] <- 0.66
data_clean$iss_bilingual_can[data_raw$issue_bilingual == 4] <- 1
table(data_clean$iss_bilingual_can)

# issue_welfare -----------------------------------------------------------

table(data_raw$issue_welfare)
attributes(data_raw$issue_welfare)
data_clean$iss_welfare_notdeserve <- NA
data_clean$iss_welfare_notdeserve[data_raw$issue_welfare == 1] <- 1
data_clean$iss_welfare_notdeserve[data_raw$issue_welfare == 2] <- 0.66
data_clean$iss_welfare_notdeserve[data_raw$issue_welfare == 3] <- 0.33
data_clean$iss_welfare_notdeserve[data_raw$issue_welfare == 4] <- 0
table(data_clean$iss_welfare_notdeserve)

# issue_tax ---------------------------------------------------------------

table(data_raw$issue_tax)
data_clean$iss_gouv_nomoretax <- NA
data_clean$iss_gouv_nomoretax[data_raw$issue_tax == 1] <- 0
data_clean$iss_gouv_nomoretax[data_raw$issue_tax == 2] <- 0.33
data_clean$iss_gouv_nomoretax[data_raw$issue_tax == 3] <- 0.66
data_clean$iss_gouv_nomoretax[data_raw$issue_tax == 4] <- 1
table(data_clean$iss_gouv_nomoretax)
