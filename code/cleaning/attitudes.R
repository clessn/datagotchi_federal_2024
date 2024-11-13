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




## issue_mi --------------------------------------------------------------





# issue_nationalanthem ----------------------------------------------------

table(data_raw$issue_nationalanthem)
data_clean$issue_nationalanthem_bilingual <- NA
data_clean$issue_nationalanthem_bilingual[data_raw$issue_nationalanthem == 1] <- 0
data_clean$issue_nationalanthem_bilingual[data_raw$issue_nationalanthem == 2] <- 0.33
data_clean$issue_nationalanthem_bilingual[data_raw$issue_nationalanthem == 3] <- 0.66
data_clean$issue_nationalanthem_bilingual[data_raw$issue_nationalanthem == 4] <- 1
table(data_clean$issue_nationalanthem_bilingual)



