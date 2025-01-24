# attitudes

## issue_aboriginals ----------------------------------------------------
table(data_raw$issue_aboriginals)
attributes(data_raw$issue_aboriginals)
data_clean$iss_proReconcilAboriginals <- NA
data_clean$iss_proReconcilAboriginals[data_raw$issue_aboriginals == 1] <- 0
data_clean$iss_proReconcilAboriginals[data_raw$issue_aboriginals == 2] <- 0.25
data_clean$iss_proReconcilAboriginals[data_raw$issue_aboriginals == 3] <- 0.5
data_clean$iss_proReconcilAboriginals[data_raw$issue_aboriginals == 4] <- 0.75
data_clean$iss_proReconcilAboriginals[data_raw$issue_aboriginals == 5] <- 1
table(data_clean$iss_proReconcilAboriginals)

## issue_immigration -----------------------------------------------------
table(data_raw$issue_immigration)
attributes(data_raw$issue_immigration)
data_clean$iss_proImmigration <- NA
data_clean$iss_proImmigration[data_raw$issue_immigration == 1] <- 0
data_clean$iss_proImmigration[data_raw$issue_immigration == 2] <- 0.25
data_clean$iss_proImmigration[data_raw$issue_immigration == 3] <- 0.5
data_clean$iss_proImmigration[data_raw$issue_immigration == 4] <- 0.75
data_clean$iss_proImmigration[data_raw$issue_immigration == 5] <- 1
table(data_clean$iss_proImmigration)

## issue_refugee ---------------------------------------------------------
table(data_raw$issue_refugee)
attributes(data_raw$issue_refugee)
data_clean$iss_proBorderControl <- NA
data_clean$iss_proBorderControl[data_raw$issue_refugee == 1] <- 0
data_clean$iss_proBorderControl[data_raw$issue_refugee == 2] <- 0.33
data_clean$iss_proBorderControl[data_raw$issue_refugee == 3] <- 0.66
data_clean$iss_proBorderControl[data_raw$issue_refugee == 4] <- 1
table(data_clean$iss_proBorderControl)

## issue_gun_control -----------------------------------------------------
table(data_raw$issue_gun_control)
attributes(data_raw$issue_gun_control)
data_clean$iss_proGunControl <- NA
data_clean$iss_proGunControl[data_raw$issue_gun_control == 1] <- 0
data_clean$iss_proGunControl[data_raw$issue_gun_control == 2] <- 0.25
data_clean$iss_proGunControl[data_raw$issue_gun_control == 3] <- 0.5
data_clean$iss_proGunControl[data_raw$issue_gun_control == 4] <- 0.75
data_clean$iss_proGunControl[data_raw$issue_gun_control == 5] <- 1
table(data_clean$iss_proGunControl)

## issue_economy ---------------------------------------------------------
table(data_raw$issue_economy)
attributes(data_raw$issue_economy)
data_clean$iss_reduceDeficit <- NA
data_clean$iss_reduceDeficit[data_raw$issue_immigration == 1] <- 0
data_clean$iss_reduceDeficit[data_raw$issue_immigration == 2] <- 0.33
data_clean$iss_reduceDeficit[data_raw$issue_immigration == 3] <- 0.66
data_clean$iss_reduceDeficit[data_raw$issue_immigration == 4] <- 1
table(data_clean$iss_reduceDeficit)

## issue_health ----------------------------------------------------------
table(data_raw$issue_health)
attributes(data_raw$issue_health)
data_clean$iss_proPrivateHealth <- NA
data_clean$iss_proPrivateHealth[data_raw$issue_health == 1] <- 0
data_clean$iss_proPrivateHealth[data_raw$issue_health == 2] <- 0.25
data_clean$iss_proPrivateHealth[data_raw$issue_health == 3] <- 0.5
data_clean$iss_proPrivateHealth[data_raw$issue_health == 4] <- 0.75
data_clean$iss_proPrivateHealth[data_raw$issue_health == 5] <- 1
table(data_clean$iss_proPrivateHealth)

## issue_unions ----------------------------------------------------------
table(data_raw$issue_unions)
attributes(data_raw$issue_unions)
data_clean$iss_proUnions <- NA
data_clean$iss_proUnions[data_raw$issue_unions == 1] <- 0
data_clean$iss_proUnions[data_raw$issue_unions == 2] <- 0.25
data_clean$iss_proUnions[data_raw$issue_unions == 3] <- 0.5
data_clean$iss_proUnions[data_raw$issue_unions == 4] <- 0.75
data_clean$iss_proUnions[data_raw$issue_unions == 5] <- 1
table(data_clean$iss_proUnions)

## issue_quebec -----------------------------------------------------------
table(data_raw$issue_quebec)
attributes(data_raw$issue_quebec)
data_clean$iss_proQcIndependance <- NA
data_clean$iss_proQcIndependance[data_raw$issue_quebec == 1] <- 0
data_clean$iss_proQcIndependance[data_raw$issue_quebec == 2] <- 0.33
data_clean$iss_proQcIndependance[data_raw$issue_quebec == 3] <- 0.66
data_clean$iss_proQcIndependance[data_raw$issue_quebec == 4] <- 1
table(data_clean$iss_proQcIndependance)

## issue_french ----------------------------------------------------------
table(data_raw$issue_french)
attributes(data_raw$issue_french)
data_clean$iss_protectFrench <- NA
data_clean$iss_protectFrench[data_raw$issue_french == 1] <- 0
data_clean$iss_protectFrench[data_raw$issue_french == 2] <- 0.25
data_clean$iss_protectFrench[data_raw$issue_french == 3] <- 0.5
data_clean$iss_protectFrench[data_raw$issue_french == 4] <- 0.75
data_clean$iss_protectFrench[data_raw$issue_french == 5] <- 1
table(data_clean$iss_protectFrench)

## issue_religious_accommodation -----------------------------------------
table(data_raw$issue_religious_acco)
attributes(data_raw$issue_religious_acco)
data_clean$iss_religiousAccomodations <- NA
data_clean$iss_religiousAccomodations[data_raw$issue_religious_acco == 1] <- 0
data_clean$iss_religiousAccomodations[data_raw$issue_religious_acco == 2] <- 0.33
data_clean$iss_religiousAccomodations[data_raw$issue_religious_acco == 3] <- 0.66
data_clean$iss_religiousAccomodations[data_raw$issue_religious_acco == 4] <- 1
table(data_clean$iss_religiousAccomodations)

## issue_sex_ed ----------------------------------------------------------
table(data_raw$issue_sex_ed)
attributes(data_raw$issue_sex_ed)
data_clean$iss_proSexEduc <- NA
data_clean$iss_proSexEduc[data_raw$issue_sex_ed == 1] <- 0
data_clean$iss_proSexEduc[data_raw$issue_sex_ed == 2] <- 0.33
data_clean$iss_proSexEduc[data_raw$issue_sex_ed == 3] <- 0.66
data_clean$iss_proSexEduc[data_raw$issue_sex_ed == 4] <- 1
table(data_clean$iss_proSexEduc)

## issue_carbon_tax ------------------------------------------------------
table(data_raw$issue_carbon_tax)
attributes(data_raw$issue_carbon_tax)
data_clean$iss_proCarbonTax <- NA
data_clean$iss_proCarbonTax[data_raw$issue_carbon_tax == 1] <- 0
data_clean$iss_proCarbonTax[data_raw$issue_carbon_tax == 2] <- 0.33
data_clean$iss_proCarbonTax[data_raw$issue_carbon_tax == 3] <- 0.66
data_clean$iss_proCarbonTax[data_raw$issue_carbon_tax == 4] <- 1
table(data_clean$iss_proCarbonTax)

## issue_oil -------------------------------------------------------------
table(data_raw$issue_oil)
attributes(data_raw$issue_oil)
data_clean$iss_supportOil <- NA
data_clean$iss_supportOil[data_raw$issue_oil == 1] <- 0
data_clean$iss_supportOil[data_raw$issue_oil == 2] <- 0.25
data_clean$iss_supportOil[data_raw$issue_oil == 3] <- 0.5
data_clean$iss_supportOil[data_raw$issue_oil == 4] <- 0.75
data_clean$iss_supportOil[data_raw$issue_oil == 5] <- 1
table(data_clean$iss_supportOil)

## issue_climate_change --------------------------------------------------
table(data_raw$issue_climate_change)
attributes(data_raw$issue_climate_change)
data_clean$iss_climateChangeExaggerated <- NA
data_clean$iss_climateChangeExaggerated[data_raw$issue_climate_change == 1] <- 0
data_clean$iss_climateChangeExaggerated[data_raw$issue_climate_change == 2] <- 0.33
data_clean$iss_climateChangeExaggerated[data_raw$issue_climate_change == 3] <- 0.66
data_clean$iss_climateChangeExaggerated[data_raw$issue_climate_change == 4] <- 1
table(data_clean$iss_climateChangeExaggerated)

## issue_lgbtq+ ----------------------------------------------------------
table(data_raw$issue_lgbtq_)
attributes(data_raw$issue_lgbtq_)
data_clean$iss_proLgbtqRights <- NA
data_clean$iss_proLgbtqRights[data_raw$issue_lgbtq_ == 1] <- 0
data_clean$iss_proLgbtqRights[data_raw$issue_lgbtq_ == 2] <- 0.25
data_clean$iss_proLgbtqRights[data_raw$issue_lgbtq_ == 3] <- 0.5
data_clean$iss_proLgbtqRights[data_raw$issue_lgbtq_ == 4] <- 0.75
data_clean$iss_proLgbtqRights[data_raw$issue_lgbtq_ == 5] <- 1
table(data_clean$iss_proLgbtqRights)

## issue_israel ----------------------------------------------------------
table(data_raw$issue_israel)
attributes(data_raw$issue_israel)
data_clean$iss_proIsrael <- NA
data_clean$iss_proIsrael[data_raw$issue_israel == 1] <- 0
data_clean$iss_proIsrael[data_raw$issue_israel == 2] <- 0.25
data_clean$iss_proIsrael[data_raw$issue_israel == 3] <- 0.5
data_clean$iss_proIsrael[data_raw$issue_israel == 4] <- 0.75
data_clean$iss_proIsrael[data_raw$issue_israel == 5] <- 1
table(data_clean$iss_proIsrael)

## issue_ukraine ---------------------------------------------------------

table(data_raw$issue_ukraine)
attributes(data_raw$issue_ukraine)
data_clean$iss_helpUkraine <- NA
data_clean$iss_helpUkraine[data_raw$issue_ukraine == 1] <- 0
data_clean$iss_helpUkraine[data_raw$issue_ukraine == 2] <- 0.25
data_clean$iss_helpUkraine[data_raw$issue_ukraine == 3] <- 0.5
data_clean$iss_helpUkraine[data_raw$issue_ukraine == 4] <- 0.75
data_clean$iss_helpUkraine[data_raw$issue_ukraine == 5] <- 1
table(data_clean$iss_helpUkraine)
  
## issue_mi --------------------------------------------------------------

table(data_raw$issue_mi)
attributes(data_raw$issue_mi)
data_clean$iss_mostImportant <- (data_raw$issue_mi)
table(data_clean$iss_mostImportant)

# issue_economy_curren ----------------------------------------------------

table(data_raw$issue_economy_curren)
attributes(data_raw$issue_economy_curren)
data_clean$iss_goodEcon <- NA
data_clean$iss_goodEcon[data_raw$issue_economy_curren == 1] <- 0
data_clean$iss_goodEcon[data_raw$issue_economy_curren == 2] <- 0.33
data_clean$iss_goodEcon[data_raw$issue_economy_curren == 3] <- 0.66
data_clean$iss_goodEcon[data_raw$issue_economy_curren == 4] <- 1
table(data_clean$iss_goodEcon)

# issue_distinction_us ----------------------------------------------------

table(data_raw$issue_distinction_us)
attributes(data_raw$issue_distinction_us)
data_clean$iss_proDistinctUSA <- NA
data_clean$iss_proDistinctUSA[data_raw$issue_distinction_us == 1] <- 1
data_clean$iss_proDistinctUSA[data_raw$issue_distinction_us == 2] <- 0.66
data_clean$iss_proDistinctUSA[data_raw$issue_distinction_us == 3] <- 0.33
data_clean$iss_proDistinctUSA[data_raw$issue_distinction_us == 4] <- 0
table(data_clean$iss_proDistinctUSA)

# issue_nationalanthem ----------------------------------------------------

table(data_raw$issue_nationalanthem)
data_clean$iss_anthemBilingual <- NA
data_clean$iss_anthemBilingual[data_raw$issue_nationalanthem == 1] <- 0
data_clean$iss_anthemBilingual[data_raw$issue_nationalanthem == 2] <- 0.33
data_clean$iss_anthemBilingual[data_raw$issue_nationalanthem == 3] <- 0.66
data_clean$iss_anthemBilingual[data_raw$issue_nationalanthem == 4] <- 1
table(data_clean$iss_anthemBilingual)

# issue_immig_illegal -----------------------------------------------------

table(data_raw$issue_immig_illegal)
attributes(data_raw$issue_immig_illegal)
data_clean$iss_concernImmigIllegal <- NA
data_clean$iss_concernImmigIllegal[data_raw$issue_immig_illegal == 4] <- 0
data_clean$iss_concernImmigIllegal[data_raw$issue_immig_illegal == 5] <- 0.33
data_clean$iss_concernImmigIllegal[data_raw$issue_immig_illegal == 6] <- 0.66
data_clean$iss_concernImmigIllegal[data_raw$issue_immig_illegal == 3] <- 1
table(data_clean$iss_concernImmigIllegal)

# issue_immig_house -------------------------------------------------------

table(data_raw$issue_immig_house)
data_clean$iss_immigNoHouse <- NA
data_clean$iss_immigNoHouse[data_raw$issue_immig_house == 1] <- 0
data_clean$iss_immigNoHouse[data_raw$issue_immig_house == 2] <- 0.33
data_clean$iss_immigNoHouse[data_raw$issue_immig_house == 3] <- 0.66
data_clean$iss_immigNoHouse[data_raw$issue_immig_house == 4] <- 1
table(data_clean$iss_immigNoHouse)

# issue_gouv_house -------------------------------------------------------

table(data_raw$issue_gouv_house)
data_clean$iss_gouvActHouse <- NA
data_clean$iss_gouvActHouse[data_raw$issue_immig_house == 1] <- 0
data_clean$iss_gouvActHouse[data_raw$issue_immig_house == 2] <- 0.33
data_clean$iss_gouvActHouse[data_raw$issue_immig_house == 3] <- 0.66
data_clean$iss_gouvActHouse[data_raw$issue_immig_house == 4] <- 1
table(data_clean$iss_gouvActHouse)

# issue_bilingual ---------------------------------------------------------

table(data_raw$issue_bilingual)
data_clean$iss_bilingualCan <- NA
data_clean$iss_bilingualCan[data_raw$issue_bilingual == 1] <- 0
data_clean$iss_bilingualCan[data_raw$issue_bilingual == 2] <- 0.33
data_clean$iss_bilingualCan[data_raw$issue_bilingual == 3] <- 0.66
data_clean$iss_bilingualCan[data_raw$issue_bilingual == 4] <- 1
table(data_clean$iss_bilingualCan)

# issue_welfare -----------------------------------------------------------

table(data_raw$issue_welfare)
attributes(data_raw$issue_welfare)
data_clean$iss_welfareNotDeserve <- NA
data_clean$iss_welfareNotDeserve[data_raw$issue_welfare == 1] <- 1
data_clean$iss_welfareNotDeserve[data_raw$issue_welfare == 2] <- 0.66
data_clean$iss_welfareNotDeserve[data_raw$issue_welfare == 3] <- 0.33
data_clean$iss_welfareNotDeserve[data_raw$issue_welfare == 4] <- 0
table(data_clean$iss_welfareNotDeserve)

# issue_tax ---------------------------------------------------------------

table(data_raw$issue_tax)
data_clean$iss_gouvNoMoreTax <- NA
data_clean$iss_gouvNoMoreTax[data_raw$issue_tax == 1] <- 0
data_clean$iss_gouvNoMoreTax[data_raw$issue_tax == 2] <- 0.33
data_clean$iss_gouvNoMoreTax[data_raw$issue_tax == 3] <- 0.66
data_clean$iss_gouvNoMoreTax[data_raw$issue_tax == 4] <- 1
table(data_clean$iss_gouvNoMoreTax)