# attitudes


## issue_aboriginals ----------------------------------------------------
table(data_raw$issue_aboriginals)
attributes(data_raw$issue_aboriginals)
data_clean$iss_pro_reconcil_aboriginals <- NA
data_clean$iss_pro_reconcil_aboriginals[data_raw$issue_aboriginals == 1] <- 0
data_clean$iss_pro_reconcil_aboriginals[data_raw$issue_aboriginals == 2] <- 0.25
data_clean$iss_pro_reconcil_aboriginals[data_raw$issue_aboriginals == 3] <- 0.5
data_clean$iss_pro_reconcil_aboriginals[data_raw$issue_aboriginals == 4] <- 0.75
data_clean$iss_pro_reconcil_aboriginals[data_raw$issue_aboriginals == 5] <- 1
table(data_clean$iss_pro_reconcil_aboriginals)

## issue_immigration -----------------------------------------------------
table(data_raw$issue_immigration)
attributes(data_raw$issue_immigration)
data_clean$iss_pro_immigration <- NA
data_clean$iss_pro_immigration[data_raw$issue_immigration == 1] <- 0
data_clean$iss_pro_immigration[data_raw$issue_immigration == 2] <- 0.25
data_clean$iss_pro_immigration[data_raw$issue_immigration == 3] <- 0.5
data_clean$iss_pro_immigration[data_raw$issue_immigration == 4] <- 0.75
data_clean$iss_pro_immigration[data_raw$issue_immigration == 5] <- 1
table(data_clean$iss_pro_immigration)

## issue_refugee ---------------------------------------------------------
table(data_raw$issue_refugee)
attributes(data_raw$issue_refugee)
data_clean$iss_pro_bordercontrol <- NA
data_clean$iss_pro_bordercontrol[data_raw$issue_refugee == 1] <- 0
data_clean$iss_pro_bordercontrol[data_raw$issue_refugee == 2] <- 0.33
data_clean$iss_pro_bordercontrol[data_raw$issue_refugee == 3] <- 0.66
data_clean$iss_pro_bordercontrol[data_raw$issue_refugee == 4] <- 1
table(data_clean$iss_pro_bordercontrol)

## issue_gun_control -----------------------------------------------------
table(data_raw$issue_gun_control)
attributes(data_raw$issue_gun_control)
data_clean$iss_pro_guncontrol <- NA
data_clean$iss_pro_guncontrol[data_raw$issue_gun_control == 1] <- 0
data_clean$iss_pro_guncontrol[data_raw$issue_gun_control == 2] <- 0.25
data_clean$iss_pro_guncontrol[data_raw$issue_gun_control == 3] <- 0.5
data_clean$iss_pro_guncontrol[data_raw$issue_gun_control == 4] <- 0.75
data_clean$iss_pro_guncontrol[data_raw$issue_gun_control == 5] <- 1
table(data_clean$iss_pro_guncontrol)

## issue_economy ---------------------------------------------------------
table(data_raw$issue_economy)
attributes(data_raw$issue_economy)
data_clean$iss_reducedeficit <- NA
data_clean$iss_reducedeficit[data_raw$issue_immigration == 1] <- 0
data_clean$iss_reducedeficit[data_raw$issue_immigration == 2] <- 0.33
data_clean$iss_reducedeficit[data_raw$issue_immigration == 3] <- 0.66
data_clean$iss_reducedeficit[data_raw$issue_immigration == 4] <- 1
table(data_clean$iss_reducedeficit)

## issue_health ----------------------------------------------------------
table(data_raw$issue_health)
attributes(data_raw$issue_health)
data_clean$iss_pro_privatehealth <- NA
data_clean$iss_pro_privatehealth[data_raw$issue_health == 1] <- 0
data_clean$iss_pro_privatehealth[data_raw$issue_health == 2] <- 0.25
data_clean$iss_pro_privatehealth[data_raw$issue_health == 3] <- 0.5
data_clean$iss_pro_privatehealth[data_raw$issue_health == 4] <- 0.75
data_clean$iss_pro_privatehealth[data_raw$issue_health == 5] <- 1
table(data_clean$iss_pro_privatehealth)

## issue_unions ----------------------------------------------------------
table(data_raw$issue_unions)
attributes(data_raw$issue_unions)
data_clean$iss_pro_unions <- NA
data_clean$iss_pro_unions[data_raw$issue_unions == 1] <- 0
data_clean$iss_pro_unions[data_raw$issue_unions == 2] <- 0.25
data_clean$iss_pro_unions[data_raw$issue_unions == 3] <- 0.5
data_clean$iss_pro_unions[data_raw$issue_unions == 4] <- 0.75
data_clean$iss_pro_unions[data_raw$issue_unions == 5] <- 1
table(data_clean$iss_pro_unions)

## issue_quebec -----------------------------------------------------------
table(data_raw$issue_quebec)
attributes(data_raw$issue_quebec)
data_clean$iss_pro_qc_independance <- NA
data_clean$iss_pro_qc_independance[data_raw$issue_quebec == 1] <- 0
data_clean$iss_pro_qc_independance[data_raw$issue_quebec == 2] <- 0.33
data_clean$iss_pro_qc_independance[data_raw$issue_quebec == 3] <- 0.66
data_clean$iss_pro_qc_independance[data_raw$issue_quebec == 4] <- 1
table(data_clean$iss_pro_qc_independance)

## issue_french ----------------------------------------------------------
table(data_raw$issue_french)
attributes(data_raw$issue_french)
data_clean$issue_french <- NA
data_clean$issue_french[data_raw$issue_french == 1] <- 0
data_clean$issue_french[data_raw$issue_french == 2] <- 0.25
data_clean$issue_french[data_raw$issue_french == 3] <- 0.5
data_clean$issue_french[data_raw$issue_french == 4] <- 0.75
data_clean$issue_french[data_raw$issue_french == 5] <- 1
table(data_clean$issue_french)



## issue_religious_accommodation -----------------------------------------
table(data_raw$issue_religious_acco)
attributes(data_raw$issue_religious_acco)
data_clean$issue_religious_acco <- NA
data_clean$issue_religious_acco[data_raw$issue_religious_acco == 1] <- 0
data_clean$issue_religious_acco[data_raw$issue_religious_acco == 2] <- 0.33
data_clean$issue_religious_acco[data_raw$issue_religious_acco == 3] <- 0.66
data_clean$issue_religious_acco[data_raw$issue_religious_acco == 4] <- 1
table(data_clean$issue_religious_acco)



## issue_sex_ed ----------------------------------------------------------
table(data_raw$issue_sex_ed)
attributes(data_raw$issue_sex_ed)
data_clean$issue_sex_ed <- NA
data_clean$issue_sex_ed[data_raw$issue_sex_ed == 1] <- 0
data_clean$issue_sex_ed[data_raw$issue_sex_ed == 2] <- 0.33
data_clean$issue_sex_ed[data_raw$issue_sex_ed == 3] <- 0.66
data_clean$issue_sex_ed[data_raw$issue_sex_ed == 4] <- 1
table(data_clean$issue_sex_ed)



## issue_carbon_tax ------------------------------------------------------
table(data_raw$issue_carbon_tax)
attributes(data_raw$issue_carbon_tax)
data_clean$issue_carbon_tax <- NA
data_clean$issue_carbon_tax[data_raw$issue_carbon_tax == 1] <- 0
data_clean$issue_carbon_tax[data_raw$issue_carbon_tax == 2] <- 0.33
data_clean$issue_carbon_tax[data_raw$issue_carbon_tax == 3] <- 0.66
data_clean$issue_carbon_tax[data_raw$issue_carbon_tax == 4] <- 1
table(data_clean$issue_carbon_tax)



## issue_oil -------------------------------------------------------------
table(data_raw$issue_oil)
attributes(data_raw$issue_oil)
data_clean$issue_oil <- NA
data_clean$issue_oil[data_raw$issue_oil == 1] <- 0
data_clean$issue_oil[data_raw$issue_oil == 2] <- 0.25
data_clean$issue_oil[data_raw$issue_oil == 3] <- 0.5
data_clean$issue_oil[data_raw$issue_oil == 4] <- 0.75
data_clean$issue_oil[data_raw$issue_oil == 5] <- 1
table(data_clean$issue_oil)


## issue_climate_change --------------------------------------------------
table(data_raw$issue_climate_change)
attributes(data_raw$issue_climate_change)
data_clean$issue_climate_change <- NA
data_clean$issue_climate_change[data_raw$issue_climate_change == 1] <- 0
data_clean$issue_climate_change[data_raw$issue_climate_change == 2] <- 0.33
data_clean$issue_climate_change[data_raw$issue_climate_change == 3] <- 0.66
data_clean$issue_climate_change[data_raw$issue_climate_change == 4] <- 1
table(data_clean$issue_climate_change)




## issue_lgbtq+ ----------------------------------------------------------
table(data_raw$issue_lgbtq_)
attributes(data_raw$issue_lgbtq_)
data_clean$issue_lgbtq_ <- NA
data_clean$issue_lgbtq_[data_raw$issue_lgbtq_ == 1] <- 0
data_clean$issue_lgbtq_[data_raw$issue_lgbtq_ == 2] <- 0.25
data_clean$issue_lgbtq_[data_raw$issue_lgbtq_ == 3] <- 0.5
data_clean$issue_lgbtq_[data_raw$issue_lgbtq_ == 4] <- 0.75
data_clean$issue_lgbtq_[data_raw$issue_lgbtq_ == 5] <- 1
table(data_clean$issue_lgbtq_)



## issue_israel ----------------------------------------------------------
table(data_raw$issue_israel)
attributes(data_raw$issue_israel)
data_clean$issue_israel <- NA
data_clean$issue_israel[data_raw$issue_israel == 1] <- 0
data_clean$issue_israel[data_raw$issue_israel == 2] <- 0.25
data_clean$issue_israel[data_raw$issue_israel == 3] <- 0.5
data_clean$issue_israel[data_raw$issue_israel == 4] <- 0.75
data_clean$issue_israel[data_raw$issue_israel == 5] <- 1
table(data_clean$issue_israel)



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
