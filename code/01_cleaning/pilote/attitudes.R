# attitudes

## issue_aboriginals ----------------------------------------------------
table(DataRaw$issue_aboriginals)
attributes(DataRaw$issue_aboriginals)
DataClean$issue_proReconcilAboriginals <- NA
DataClean$issue_proReconcilAboriginals[DataRaw$issue_aboriginals == 1] <- 0
DataClean$issue_proReconcilAboriginals[DataRaw$issue_aboriginals == 2] <- 0.25
DataClean$issue_proReconcilAboriginals[DataRaw$issue_aboriginals == 3] <- 0.5
DataClean$issue_proReconcilAboriginals[DataRaw$issue_aboriginals == 4] <- 0.75
DataClean$issue_proReconcilAboriginals[DataRaw$issue_aboriginals == 5] <- 1
table(DataClean$issue_proReconcilAboriginals)

## issue_immigration -----------------------------------------------------
table(DataRaw$issue_immigration)
attributes(DataRaw$issue_immigration)
DataClean$issue_proImmigration <- NA
DataClean$issue_proImmigration[DataRaw$issue_immigration == 1] <- 0
DataClean$issue_proImmigration[DataRaw$issue_immigration == 2] <- 0.25
DataClean$issue_proImmigration[DataRaw$issue_immigration == 3] <- 0.5
DataClean$issue_proImmigration[DataRaw$issue_immigration == 4] <- 0.75
DataClean$issue_proImmigration[DataRaw$issue_immigration == 5] <- 1
table(DataClean$issue_proImmigration)

## issue_refugee ---------------------------------------------------------
table(DataRaw$issue_refugee)
attributes(DataRaw$issue_refugee)
DataClean$issue_proBorderControl <- NA
DataClean$issue_proBorderControl[DataRaw$issue_refugee == 1] <- 0
DataClean$issue_proBorderControl[DataRaw$issue_refugee == 2] <- 0.33
DataClean$issue_proBorderControl[DataRaw$issue_refugee == 3] <- 0.66
DataClean$issue_proBorderControl[DataRaw$issue_refugee == 4] <- 1
table(DataClean$issue_proBorderControl)

## issue_gun_control -----------------------------------------------------
table(DataRaw$issue_gun_control)
attributes(DataRaw$issue_gun_control)
DataClean$issue_proGunControl <- NA
DataClean$issue_proGunControl[DataRaw$issue_gun_control == 1] <- 0
DataClean$issue_proGunControl[DataRaw$issue_gun_control == 2] <- 0.25
DataClean$issue_proGunControl[DataRaw$issue_gun_control == 3] <- 0.5
DataClean$issue_proGunControl[DataRaw$issue_gun_control == 4] <- 0.75
DataClean$issue_proGunControl[DataRaw$issue_gun_control == 5] <- 1
table(DataClean$issue_proGunControl)

## issue_economy ---------------------------------------------------------
table(DataRaw$issue_economy)
attributes(DataRaw$issue_economy)
DataClean$issue_reduceDeficit <- NA
DataClean$issue_reduceDeficit[DataRaw$issue_immigration == 1] <- 0
DataClean$issue_reduceDeficit[DataRaw$issue_immigration == 2] <- 0.33
DataClean$issue_reduceDeficit[DataRaw$issue_immigration == 3] <- 0.66
DataClean$issue_reduceDeficit[DataRaw$issue_immigration == 4] <- 1
table(DataClean$issue_reduceDeficit)

## issue_health ----------------------------------------------------------
table(DataRaw$issue_health)
attributes(DataRaw$issue_health)
DataClean$issue_proPrivateHealth <- NA
DataClean$issue_proPrivateHealth[DataRaw$issue_health == 1] <- 0
DataClean$issue_proPrivateHealth[DataRaw$issue_health == 2] <- 0.25
DataClean$issue_proPrivateHealth[DataRaw$issue_health == 3] <- 0.5
DataClean$issue_proPrivateHealth[DataRaw$issue_health == 4] <- 0.75
DataClean$issue_proPrivateHealth[DataRaw$issue_health == 5] <- 1
table(DataClean$issue_proPrivateHealth)

## issue_unions ----------------------------------------------------------
table(DataRaw$issue_unions)
attributes(DataRaw$issue_unions)
DataClean$issue_proUnions <- NA
DataClean$issue_proUnions[DataRaw$issue_unions == 1] <- 0
DataClean$issue_proUnions[DataRaw$issue_unions == 2] <- 0.25
DataClean$issue_proUnions[DataRaw$issue_unions == 3] <- 0.5
DataClean$issue_proUnions[DataRaw$issue_unions == 4] <- 0.75
DataClean$issue_proUnions[DataRaw$issue_unions == 5] <- 1
table(DataClean$issue_proUnions)

## issue_quebec -----------------------------------------------------------
table(DataRaw$issue_quebec)
attributes(DataRaw$issue_quebec)
DataClean$issue_proQcIndependance <- NA
DataClean$issue_proQcIndependance[DataRaw$issue_quebec == 1] <- 0
DataClean$issue_proQcIndependance[DataRaw$issue_quebec == 2] <- 0.33
DataClean$issue_proQcIndependance[DataRaw$issue_quebec == 3] <- 0.66
DataClean$issue_proQcIndependance[DataRaw$issue_quebec == 4] <- 1
table(DataClean$issue_proQcIndependance)

## issue_french ----------------------------------------------------------
table(DataRaw$issue_french)
attributes(DataRaw$issue_french)
DataClean$issue_protectFrench <- NA
DataClean$issue_protectFrench[DataRaw$issue_french == 1] <- 0
DataClean$issue_protectFrench[DataRaw$issue_french == 2] <- 0.25
DataClean$issue_protectFrench[DataRaw$issue_french == 3] <- 0.5
DataClean$issue_protectFrench[DataRaw$issue_french == 4] <- 0.75
DataClean$issue_protectFrench[DataRaw$issue_french == 5] <- 1
table(DataClean$issue_protectFrench)

## issue_religious_accommodation -----------------------------------------
table(DataRaw$issue_religious_acco)
attributes(DataRaw$issue_religious_acco)
DataClean$isue_religiousAccomodations <- NA
DataClean$isue_religiousAccomodations[DataRaw$issue_religious_acco == 1] <- 0
DataClean$isue_religiousAccomodations[DataRaw$issue_religious_acco == 2] <- 0.33
DataClean$isue_religiousAccomodations[DataRaw$issue_religious_acco == 3] <- 0.66
DataClean$isue_religiousAccomodations[DataRaw$issue_religious_acco == 4] <- 1
table(DataClean$issue_religiousAccomodations)

## issue_sex_ed ----------------------------------------------------------
table(DataRaw$issue_sex_ed)
attributes(DataRaw$issue_sex_ed)
DataClean$issue_proSexEduc <- NA
DataClean$issue_proSexEduc[DataRaw$issue_sex_ed == 1] <- 0
DataClean$issue_proSexEduc[DataRaw$issue_sex_ed == 2] <- 0.33
DataClean$issue_proSexEduc[DataRaw$issue_sex_ed == 3] <- 0.66
DataClean$issue_proSexEduc[DataRaw$issue_sex_ed == 4] <- 1
table(DataClean$issue_proSexEduc)

## issue_carbon_tax ------------------------------------------------------
table(DataRaw$issue_carbon_tax)
attributes(DataRaw$issue_carbon_tax)
DataClean$issue_proCarbonTax <- NA
DataClean$issue_proCarbonTax[DataRaw$issue_carbon_tax == 1] <- 0
DataClean$issue_proCarbonTax[DataRaw$issue_carbon_tax == 2] <- 0.33
DataClean$issue_proCarbonTax[DataRaw$issue_carbon_tax == 3] <- 0.66
DataClean$issue_proCarbonTax[DataRaw$issue_carbon_tax == 4] <- 1
table(DataClean$isue_proCarbonTax)

## issue_oil -------------------------------------------------------------
table(DataRaw$issue_oil)
attributes(DataRaw$issue_oil)
DataClean$issue_supportOil <- NA
DataClean$issue_supportOil[DataRaw$issue_oil == 1] <- 0
DataClean$issue_supportOil[DataRaw$issue_oil == 2] <- 0.25
DataClean$issue_supportOil[DataRaw$issue_oil == 3] <- 0.5
DataClean$issue_supportOil[DataRaw$issue_oil == 4] <- 0.75
DataClean$issue_supportOil[DataRaw$issue_oil == 5] <- 1
table(DataClean$issue_supportOil)

## issue_climate_change --------------------------------------------------
table(DataRaw$issue_climate_change)
attributes(DataRaw$issue_climate_change)
DataClean$issue_climateChangeExaggerated <- NA
DataClean$issue_climateChangeExaggerated[DataRaw$issue_climate_change == 1] <- 0
DataClean$issue_climateChangeExaggerated[DataRaw$issue_climate_change == 2] <- 0.33
DataClean$issue_climateChangeExaggerated[DataRaw$issue_climate_change == 3] <- 0.66
DataClean$issue_climateChangeExaggerated[DataRaw$issue_climate_change == 4] <- 1
table(DataClean$issue_climateChangeExaggerated)

## issue_lgbtq+ ----------------------------------------------------------
table(DataRaw$issue_lgbtq_)
attributes(DataRaw$issue_lgbtq_)
DataClean$issue_proLgbtqRights <- NA
DataClean$issue_proLgbtqRights[DataRaw$issue_lgbtq_ == 1] <- 0
DataClean$issue_proLgbtqRights[DataRaw$issue_lgbtq_ == 2] <- 0.25
DataClean$issue_proLgbtqRights[DataRaw$issue_lgbtq_ == 3] <- 0.5
DataClean$issue_proLgbtqRights[DataRaw$issue_lgbtq_ == 4] <- 0.75
DataClean$issue_proLgbtqRights[DataRaw$issue_lgbtq_ == 5] <- 1
table(DataClean$issue_proLgbtqRights)

## issue_israel ----------------------------------------------------------
table(DataRaw$issue_israel)
attributes(DataRaw$issue_israel)
DataClean$issue_proIsrael <- NA
DataClean$issue_proIsrael[DataRaw$issue_israel == 1] <- 0
DataClean$issue_proIsrael[DataRaw$issue_israel == 2] <- 0.25
DataClean$issue_proIsrael[DataRaw$issue_israel == 3] <- 0.5
DataClean$issue_proIsrael[DataRaw$issue_israel == 4] <- 0.75
DataClean$issue_proIsrael[DataRaw$issue_israel == 5] <- 1
table(DataClean$issue_proIsrael)

## issue_ukraine ---------------------------------------------------------

table(DataRaw$issue_ukraine)
attributes(DataRaw$issue_ukraine)
DataClean$issue_helpUkraine <- NA
DataClean$issue_helpUkraine[DataRaw$issue_ukraine == 1] <- 0
DataClean$issue_helpUkraine[DataRaw$issue_ukraine == 2] <- 0.25
DataClean$issue_helpUkraine[DataRaw$issue_ukraine == 3] <- 0.5
DataClean$issue_helpUkraine[DataRaw$issue_ukraine == 4] <- 0.75
DataClean$issue_helpUkraine[DataRaw$issue_ukraine == 5] <- 1
table(DataClean$issue_helpUkraine)
  
## issue_mi --------------------------------------------------------------

table(DataRaw$issue_mi)
attributes(DataRaw$issue_mi)
DataClean$issue_mostImportant <- (DataRaw$issue_mi)
table(DataClean$issue_mostImportant)

# issue_economy_curren ----------------------------------------------------

table(DataRaw$issue_economy_curren)
attributes(DataRaw$issue_economy_curren)
DataClean$issue_goodEcon <- NA
DataClean$issue_goodEcon[DataRaw$issue_economy_curren == 1] <- 0
DataClean$issue_goodEcon[DataRaw$issue_economy_curren == 2] <- 0.33
DataClean$issue_goodEcon[DataRaw$issue_economy_curren == 3] <- 0.66
DataClean$issue_goodEcon[DataRaw$issue_economy_curren == 4] <- 1
table(DataClean$issue_goodEcon)

# issue_distinction_us ----------------------------------------------------

table(DataRaw$issue_distinction_us)
attributes(DataRaw$issue_distinction_us)
DataClean$issue_proDistinctUSA <- NA
DataClean$issue_proDistinctUSA[DataRaw$issue_distinction_us == 1] <- 1
DataClean$issue_proDistinctUSA[DataRaw$issue_distinction_us == 2] <- 0.66
DataClean$issue_proDistinctUSA[DataRaw$issue_distinction_us == 3] <- 0.33
DataClean$issue_proDistinctUSA[DataRaw$issue_distinction_us == 4] <- 0
table(DataClean$issue_proDistinctUSA)

# issue_nationalanthem ----------------------------------------------------

table(DataRaw$issue_nationalanthem)
DataClean$issue_anthemBilingual <- NA
DataClean$issue_anthemBilingual[DataRaw$issue_nationalanthem == 1] <- 0
DataClean$issue_anthemBilingual[DataRaw$issue_nationalanthem == 2] <- 0.33
DataClean$issue_anthemBilingual[DataRaw$issue_nationalanthem == 3] <- 0.66
DataClean$issue_anthemBilingual[DataRaw$issue_nationalanthem == 4] <- 1
table(DataClean$issue_anthemBilingual)

# issue_immig_illegal -----------------------------------------------------

table(DataRaw$issue_immig_illegal)
attributes(DataRaw$issue_immig_illegal)
DataClean$issue_concernImmigIllegal <- NA
DataClean$issue_concernImmigIllegal[DataRaw$issue_immig_illegal == 4] <- 0
DataClean$issue_concernImmigIllegal[DataRaw$issue_immig_illegal == 5] <- 0.33
DataClean$issue_concernImmigIllegal[DataRaw$issue_immig_illegal == 6] <- 0.66
DataClean$issue_concernImmigIllegal[DataRaw$issue_immig_illegal == 3] <- 1
table(DataClean$issue_concernImmigIllegal)

# issue_immig_house -------------------------------------------------------

table(DataRaw$issue_immig_house)
DataClean$issue_immigNoHouse <- NA
DataClean$issue_immigNoHouse[DataRaw$issue_immig_house == 1] <- 0
DataClean$issue_immigNoHouse[DataRaw$issue_immig_house == 2] <- 0.33
DataClean$issue_immigNoHouse[DataRaw$issue_immig_house == 3] <- 0.66
DataClean$issue_immigNoHouse[DataRaw$issue_immig_house == 4] <- 1
table(DataClean$issue_immigNoHouse)

# issue_gouv_house -------------------------------------------------------

table(DataRaw$issue_gouv_house)
DataClean$issue_gouvActHouse <- NA
DataClean$issue_gouvActHouse[DataRaw$issue_immig_house == 1] <- 0
DataClean$issue_gouvActHouse[DataRaw$issue_immig_house == 2] <- 0.33
DataClean$issue_gouvActHouse[DataRaw$issue_immig_house == 3] <- 0.66
DataClean$issue_gouvActHouse[DataRaw$issue_immig_house == 4] <- 1
table(DataClean$issue_gouvActHouse)

# issue_bilingual ---------------------------------------------------------

table(DataRaw$issue_bilingual)
DataClean$issue_bilingualCan <- NA
DataClean$issue_bilingualCan[DataRaw$issue_bilingual == 1] <- 0
DataClean$issue_bilingualCan[DataRaw$issue_bilingual == 2] <- 0.33
DataClean$issue_bilingualCan[DataRaw$issue_bilingual == 3] <- 0.66
DataClean$issue_bilingualCan[DataRaw$issue_bilingual == 4] <- 1
table(DataClean$issue_bilingualCan)

# issue_welfare -----------------------------------------------------------

table(DataRaw$issue_welfare)
attributes(DataRaw$issue_welfare)
DataClean$issue_welfareNotDeserve <- NA
DataClean$issue_welfareNotDeserve[DataRaw$issue_welfare == 1] <- 1
DataClean$issue_welfareNotDeserve[DataRaw$issue_welfare == 2] <- 0.66
DataClean$issue_welfareNotDeserve[DataRaw$issue_welfare == 3] <- 0.33
DataClean$issue_welfareNotDeserve[DataRaw$issue_welfare == 4] <- 0
table(DataClean$issue_welfareNotDeserve)

# issue_tax ---------------------------------------------------------------

table(DataRaw$issue_tax)
DataClean$issue_gouvNoMoreTax <- NA
DataClean$issue_gouvNoMoreTax[DataRaw$issue_tax == 1] <- 0
DataClean$issue_gouvNoMoreTax[DataRaw$issue_tax == 2] <- 0.33
DataClean$issue_gouvNoMoreTax[DataRaw$issue_tax == 3] <- 0.66
DataClean$issue_gouvNoMoreTax[DataRaw$issue_tax == 4] <- 1
table(DataClean$issue_gouvNoMoreTax)