# attitudes

## issue_aboriginals ----------------------------------------------------
table(Data_raw$issue_aboriginals)
attributes(Data_raw$issue_aboriginals)
Data_clean$issue_proReconcilAboriginals <- NA
Data_clean$issue_proReconcilAboriginals[Data_raw$issue_aboriginals == 1] <- 0
Data_clean$issue_proReconcilAboriginals[Data_raw$issue_aboriginals == 2] <- 0.25
Data_clean$issue_proReconcilAboriginals[Data_raw$issue_aboriginals == 3] <- 0.5
Data_clean$issue_proReconcilAboriginals[Data_raw$issue_aboriginals == 4] <- 0.75
Data_clean$issue_proReconcilAboriginals[Data_raw$issue_aboriginals == 5] <- 1
table(Data_clean$issue_proReconcilAboriginals)

## issue_immigration -----------------------------------------------------
table(Data_raw$issue_immigration)
attributes(Data_raw$issue_immigration)
Data_clean$issue_proImmigration <- NA
Data_clean$issue_proImmigration[Data_raw$issue_immigration == 1] <- 0
Data_clean$issue_proImmigration[Data_raw$issue_immigration == 2] <- 0.25
Data_clean$issue_proImmigration[Data_raw$issue_immigration == 3] <- 0.5
Data_clean$issue_proImmigration[Data_raw$issue_immigration == 4] <- 0.75
Data_clean$issue_proImmigration[Data_raw$issue_immigration == 5] <- 1
table(Data_clean$issue_proImmigration)

## issue_refugee ---------------------------------------------------------
table(Data_raw$issue_refugee)
attributes(Data_raw$issue_refugee)
Data_clean$issue_proBorderControl <- NA
Data_clean$issue_proBorderControl[Data_raw$issue_refugee == 1] <- 0
Data_clean$issue_proBorderControl[Data_raw$issue_refugee == 2] <- 0.33
Data_clean$issue_proBorderControl[Data_raw$issue_refugee == 3] <- 0.66
Data_clean$issue_proBorderControl[Data_raw$issue_refugee == 4] <- 1
table(Data_clean$issue_proBorderControl)

## issue_gun_control -----------------------------------------------------
table(Data_raw$issue_gun_control)
attributes(Data_raw$issue_gun_control)
Data_clean$issue_proGunControl <- NA
Data_clean$issue_proGunControl[Data_raw$issue_gun_control == 1] <- 0
Data_clean$issue_proGunControl[Data_raw$issue_gun_control == 2] <- 0.25
Data_clean$issue_proGunControl[Data_raw$issue_gun_control == 3] <- 0.5
Data_clean$issue_proGunControl[Data_raw$issue_gun_control == 4] <- 0.75
Data_clean$issue_proGunControl[Data_raw$issue_gun_control == 5] <- 1
table(Data_clean$issue_proGunControl)

## issue_economy ---------------------------------------------------------
table(Data_raw$issue_economy)
attributes(Data_raw$issue_economy)
Data_clean$issue_reduceDeficit <- NA
Data_clean$issue_reduceDeficit[Data_raw$issue_immigration == 1] <- 0
Data_clean$issue_reduceDeficit[Data_raw$issue_immigration == 2] <- 0.33
Data_clean$issue_reduceDeficit[Data_raw$issue_immigration == 3] <- 0.66
Data_clean$issue_reduceDeficit[Data_raw$issue_immigration == 4] <- 1
table(Data_clean$issue_reduceDeficit)

## issue_health ----------------------------------------------------------
table(Data_raw$issue_health)
attributes(Data_raw$issue_health)
Data_clean$issue_proPrivateHealth <- NA
Data_clean$issue_proPrivateHealth[Data_raw$issue_health == 1] <- 0
Data_clean$issue_proPrivateHealth[Data_raw$issue_health == 2] <- 0.25
Data_clean$issue_proPrivateHealth[Data_raw$issue_health == 3] <- 0.5
Data_clean$issue_proPrivateHealth[Data_raw$issue_health == 4] <- 0.75
Data_clean$issue_proPrivateHealth[Data_raw$issue_health == 5] <- 1
table(Data_clean$issue_proPrivateHealth)

## issue_unions ----------------------------------------------------------
table(Data_raw$issue_unions)
attributes(Data_raw$issue_unions)
Data_clean$issue_proUnions <- NA
Data_clean$issue_proUnions[Data_raw$issue_unions == 1] <- 0
Data_clean$issue_proUnions[Data_raw$issue_unions == 2] <- 0.25
Data_clean$issue_proUnions[Data_raw$issue_unions == 3] <- 0.5
Data_clean$issue_proUnions[Data_raw$issue_unions == 4] <- 0.75
Data_clean$issue_proUnions[Data_raw$issue_unions == 5] <- 1
table(Data_clean$issue_proUnions)

## issue_quebec -----------------------------------------------------------
table(Data_raw$issue_quebec)
attributes(Data_raw$issue_quebec)
Data_clean$issue_proQcIndependance <- NA
Data_clean$issue_proQcIndependance[Data_raw$issue_quebec == 1] <- 0
Data_clean$issue_proQcIndependance[Data_raw$issue_quebec == 2] <- 0.33
Data_clean$issue_proQcIndependance[Data_raw$issue_quebec == 3] <- 0.66
Data_clean$issue_proQcIndependance[Data_raw$issue_quebec == 4] <- 1
table(Data_clean$issue_proQcIndependance)

## issue_french ----------------------------------------------------------
table(Data_raw$issue_french)
attributes(Data_raw$issue_french)
Data_clean$issue_protectFrench <- NA
Data_clean$issue_protectFrench[Data_raw$issue_french == 1] <- 0
Data_clean$issue_protectFrench[Data_raw$issue_french == 2] <- 0.25
Data_clean$issue_protectFrench[Data_raw$issue_french == 3] <- 0.5
Data_clean$issue_protectFrench[Data_raw$issue_french == 4] <- 0.75
Data_clean$issue_protectFrench[Data_raw$issue_french == 5] <- 1
table(Data_clean$issue_protectFrench)

## issue_religious_accommodation -----------------------------------------
table(Data_raw$issue_religious_acco)
attributes(Data_raw$issue_religious_acco)
Data_clean$isue_religiousAccomodations <- NA
Data_clean$isue_religiousAccomodations[Data_raw$issue_religious_acco == 1] <- 0
Data_clean$isue_religiousAccomodations[Data_raw$issue_religious_acco == 2] <- 0.33
Data_clean$isue_religiousAccomodations[Data_raw$issue_religious_acco == 3] <- 0.66
Data_clean$isue_religiousAccomodations[Data_raw$issue_religious_acco == 4] <- 1
table(Data_clean$issue_religiousAccomodations)

## issue_sex_ed ----------------------------------------------------------
table(Data_raw$issue_sex_ed)
attributes(Data_raw$issue_sex_ed)
Data_clean$issue_proSexEduc <- NA
Data_clean$issue_proSexEduc[Data_raw$issue_sex_ed == 1] <- 0
Data_clean$issue_proSexEduc[Data_raw$issue_sex_ed == 2] <- 0.33
Data_clean$issue_proSexEduc[Data_raw$issue_sex_ed == 3] <- 0.66
Data_clean$issue_proSexEduc[Data_raw$issue_sex_ed == 4] <- 1
table(Data_clean$issue_proSexEduc)

## issue_carbon_tax ------------------------------------------------------
table(Data_raw$issue_carbon_tax)
attributes(Data_raw$issue_carbon_tax)
Data_clean$issue_proCarbonTax <- NA
Data_clean$issue_proCarbonTax[Data_raw$issue_carbon_tax == 1] <- 0
Data_clean$issue_proCarbonTax[Data_raw$issue_carbon_tax == 2] <- 0.33
Data_clean$issue_proCarbonTax[Data_raw$issue_carbon_tax == 3] <- 0.66
Data_clean$issue_proCarbonTax[Data_raw$issue_carbon_tax == 4] <- 1
table(Data_clean$isue_proCarbonTax)

## issue_oil -------------------------------------------------------------
table(Data_raw$issue_oil)
attributes(Data_raw$issue_oil)
Data_clean$issue_supportOil <- NA
Data_clean$issue_supportOil[Data_raw$issue_oil == 1] <- 0
Data_clean$issue_supportOil[Data_raw$issue_oil == 2] <- 0.25
Data_clean$issue_supportOil[Data_raw$issue_oil == 3] <- 0.5
Data_clean$issue_supportOil[Data_raw$issue_oil == 4] <- 0.75
Data_clean$issue_supportOil[Data_raw$issue_oil == 5] <- 1
table(Data_clean$issue_supportOil)

## issue_climate_change --------------------------------------------------
table(Data_raw$issue_climate_change)
attributes(Data_raw$issue_climate_change)
Data_clean$issue_climateChangeExaggerated <- NA
Data_clean$issue_climateChangeExaggerated[Data_raw$issue_climate_change == 1] <- 0
Data_clean$issue_climateChangeExaggerated[Data_raw$issue_climate_change == 2] <- 0.33
Data_clean$issue_climateChangeExaggerated[Data_raw$issue_climate_change == 3] <- 0.66
Data_clean$issue_climateChangeExaggerated[Data_raw$issue_climate_change == 4] <- 1
table(Data_clean$issue_climateChangeExaggerated)

## issue_lgbtq+ ----------------------------------------------------------
table(Data_raw$issue_lgbtq_)
attributes(Data_raw$issue_lgbtq_)
Data_clean$issue_proLgbtqRights <- NA
Data_clean$issue_proLgbtqRights[Data_raw$issue_lgbtq_ == 1] <- 0
Data_clean$issue_proLgbtqRights[Data_raw$issue_lgbtq_ == 2] <- 0.25
Data_clean$issue_proLgbtqRights[Data_raw$issue_lgbtq_ == 3] <- 0.5
Data_clean$issue_proLgbtqRights[Data_raw$issue_lgbtq_ == 4] <- 0.75
Data_clean$issue_proLgbtqRights[Data_raw$issue_lgbtq_ == 5] <- 1
table(Data_clean$issue_proLgbtqRights)

## issue_israel ----------------------------------------------------------
table(Data_raw$issue_israel)
attributes(Data_raw$issue_israel)
Data_clean$issue_proIsrael <- NA
Data_clean$issue_proIsrael[Data_raw$issue_israel == 1] <- 0
Data_clean$issue_proIsrael[Data_raw$issue_israel == 2] <- 0.25
Data_clean$issue_proIsrael[Data_raw$issue_israel == 3] <- 0.5
Data_clean$issue_proIsrael[Data_raw$issue_israel == 4] <- 0.75
Data_clean$issue_proIsrael[Data_raw$issue_israel == 5] <- 1
table(Data_clean$issue_proIsrael)

## issue_ukraine ---------------------------------------------------------

table(Data_raw$issue_ukraine)
attributes(Data_raw$issue_ukraine)
Data_clean$issue_helpUkraine <- NA
Data_clean$issue_helpUkraine[Data_raw$issue_ukraine == 1] <- 0
Data_clean$issue_helpUkraine[Data_raw$issue_ukraine == 2] <- 0.25
Data_clean$issue_helpUkraine[Data_raw$issue_ukraine == 3] <- 0.5
Data_clean$issue_helpUkraine[Data_raw$issue_ukraine == 4] <- 0.75
Data_clean$issue_helpUkraine[Data_raw$issue_ukraine == 5] <- 1
table(Data_clean$issue_helpUkraine)
  
## issue_mi --------------------------------------------------------------

table(Data_raw$issue_mi)
attributes(Data_raw$issue_mi)
Data_clean$issue_mostImportant <- (Data_raw$issue_mi)
table(Data_clean$issue_mostImportant)

# issue_economy_curren ----------------------------------------------------

table(Data_raw$issue_economy_curren)
attributes(Data_raw$issue_economy_curren)
Data_clean$issue_goodEcon <- NA
Data_clean$issue_goodEcon[Data_raw$issue_economy_curren == 1] <- 0
Data_clean$issue_goodEcon[Data_raw$issue_economy_curren == 2] <- 0.33
Data_clean$issue_goodEcon[Data_raw$issue_economy_curren == 3] <- 0.66
Data_clean$issue_goodEcon[Data_raw$issue_economy_curren == 4] <- 1
table(Data_clean$issue_goodEcon)

# issue_distinction_us ----------------------------------------------------

table(Data_raw$issue_distinction_us)
attributes(Data_raw$issue_distinction_us)
Data_clean$issue_proDistinctUSA <- NA
Data_clean$issue_proDistinctUSA[Data_raw$issue_distinction_us == 1] <- 1
Data_clean$issue_proDistinctUSA[Data_raw$issue_distinction_us == 2] <- 0.66
Data_clean$issue_proDistinctUSA[Data_raw$issue_distinction_us == 3] <- 0.33
Data_clean$issue_proDistinctUSA[Data_raw$issue_distinction_us == 4] <- 0
table(Data_clean$issue_proDistinctUSA)

# issue_nationalanthem ----------------------------------------------------

table(Data_raw$issue_nationalanthem)
Data_clean$issue_anthemBilingual <- NA
Data_clean$issue_anthemBilingual[Data_raw$issue_nationalanthem == 1] <- 0
Data_clean$issue_anthemBilingual[Data_raw$issue_nationalanthem == 2] <- 0.33
Data_clean$issue_anthemBilingual[Data_raw$issue_nationalanthem == 3] <- 0.66
Data_clean$issue_anthemBilingual[Data_raw$issue_nationalanthem == 4] <- 1
table(Data_clean$issue_anthemBilingual)

# issue_immig_illegal -----------------------------------------------------

table(Data_raw$issue_immig_illegal)
attributes(Data_raw$issue_immig_illegal)
Data_clean$issue_concernImmigIllegal <- NA
Data_clean$issue_concernImmigIllegal[Data_raw$issue_immig_illegal == 4] <- 0
Data_clean$issue_concernImmigIllegal[Data_raw$issue_immig_illegal == 5] <- 0.33
Data_clean$issue_concernImmigIllegal[Data_raw$issue_immig_illegal == 6] <- 0.66
Data_clean$issue_concernImmigIllegal[Data_raw$issue_immig_illegal == 3] <- 1
table(Data_clean$issue_concernImmigIllegal)

# issue_immig_house -------------------------------------------------------

table(Data_raw$issue_immig_house)
Data_clean$issue_immigNoHouse <- NA
Data_clean$issue_immigNoHouse[Data_raw$issue_immig_house == 1] <- 0
Data_clean$issue_immigNoHouse[Data_raw$issue_immig_house == 2] <- 0.33
Data_clean$issue_immigNoHouse[Data_raw$issue_immig_house == 3] <- 0.66
Data_clean$issue_immigNoHouse[Data_raw$issue_immig_house == 4] <- 1
table(Data_clean$issue_immigNoHouse)

# issue_gouv_house -------------------------------------------------------

table(Data_raw$issue_gouv_house)
Data_clean$issue_gouvActHouse <- NA
Data_clean$issue_gouvActHouse[Data_raw$issue_immig_house == 1] <- 0
Data_clean$issue_gouvActHouse[Data_raw$issue_immig_house == 2] <- 0.33
Data_clean$issue_gouvActHouse[Data_raw$issue_immig_house == 3] <- 0.66
Data_clean$issue_gouvActHouse[Data_raw$issue_immig_house == 4] <- 1
table(Data_clean$issue_gouvActHouse)

# issue_bilingual ---------------------------------------------------------

table(Data_raw$issue_bilingual)
Data_clean$issue_bilingualCan <- NA
Data_clean$issue_bilingualCan[Data_raw$issue_bilingual == 1] <- 0
Data_clean$issue_bilingualCan[Data_raw$issue_bilingual == 2] <- 0.33
Data_clean$issue_bilingualCan[Data_raw$issue_bilingual == 3] <- 0.66
Data_clean$issue_bilingualCan[Data_raw$issue_bilingual == 4] <- 1
table(Data_clean$issue_bilingualCan)

# issue_welfare -----------------------------------------------------------

table(Data_raw$issue_welfare)
attributes(Data_raw$issue_welfare)
Data_clean$issue_welfareNotDeserve <- NA
Data_clean$issue_welfareNotDeserve[Data_raw$issue_welfare == 1] <- 1
Data_clean$issue_welfareNotDeserve[Data_raw$issue_welfare == 2] <- 0.66
Data_clean$issue_welfareNotDeserve[Data_raw$issue_welfare == 3] <- 0.33
Data_clean$issue_welfareNotDeserve[Data_raw$issue_welfare == 4] <- 0
table(Data_clean$issue_welfareNotDeserve)

# issue_tax ---------------------------------------------------------------

table(Data_raw$issue_tax)
Data_clean$issue_gouvNoMoreTax <- NA
Data_clean$issue_gouvNoMoreTax[Data_raw$issue_tax == 1] <- 0
Data_clean$issue_gouvNoMoreTax[Data_raw$issue_tax == 2] <- 0.33
Data_clean$issue_gouvNoMoreTax[Data_raw$issue_tax == 3] <- 0.66
Data_clean$issue_gouvNoMoreTax[Data_raw$issue_tax == 4] <- 1
table(Data_clean$issue_gouvNoMoreTax)