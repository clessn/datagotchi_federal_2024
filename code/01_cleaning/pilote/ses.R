# SES

## gender ----------------------------------------------------------------------

attributes(DataRaw$ses_gender)
table(DataRaw$ses_gender)
DataClean$ses_gender <- NA
DataClean$ses_gender[DataRaw$ses_gender == 1] <- "male"
DataClean$ses_gender[DataRaw$ses_gender == 2] <- "female"
DataClean$ses_gender[DataRaw$ses_gender == 3] <- "trans_man"
DataClean$ses_gender[DataRaw$ses_gender == 4] <- "trans_woman"
DataClean$ses_gender[DataRaw$ses_gender == 5] <- "non_binary"
DataClean$ses_gender[DataRaw$ses_gender == 6] <- "queer"
DataClean$ses_gender[DataRaw$ses_gender == 7] <- "agender"
DataClean$ses_gender <- factor(DataClean$ses_gender)
table(DataClean$ses_gender)

# gender_female ----------------------------------------------------------------

DataClean$ses_genderMale <- NA
DataClean$ses_genderMale[DataRaw$ses_gender == 1] <- 1
DataClean$ses_genderMale[DataRaw$ses_gender != 1] <- 0
table(DataClean$ses_genderMale)

# gender_female ----------------------------------------------------------------

DataClean$ses_genderFemale <- NA
DataClean$ses_genderFemale[DataRaw$ses_gender == 2] <- 1
DataClean$ses_genderFemale[DataRaw$ses_gender != 2] <- 0
table(DataClean$ses_genderFemale)

## age--------------------------------------------------------------------------

attributes(DataRaw$ses_age)
table(DataRaw$ses_age)
DataClean$ses_age <- NA
DataClean$ses_age <- DataRaw$ses_age
table(DataClean$ses_age)

DataClean$ses_ageGroup5Years <- NA
DataClean$ses_ageGroup5Years[DataRaw$ses_age == 18 | DataRaw$ses_age == 19] <- "18_19"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 20 & DataRaw$ses_age < 25] <- "20_24"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 25 & DataRaw$ses_age < 30] <- "25_29"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 30 & DataRaw$ses_age < 35] <- "30_34"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 35 & DataRaw$ses_age < 40] <- "35_39"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 40 & DataRaw$ses_age < 45] <- "40-44"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 45 & DataRaw$ses_age < 50] <- "45_49"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 50 & DataRaw$ses_age < 55] <- "50_54"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 55 & DataRaw$ses_age < 60] <- "55_59"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 60 & DataRaw$ses_age < 65] <- "60_64"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 65 & DataRaw$ses_age < 70] <- "65_69"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 70 & DataRaw$ses_age < 75] <- "70_74"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 75 & DataRaw$ses_age < 80] <- "75_79"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 80 & DataRaw$ses_age < 85] <- "80_84"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 85 & DataRaw$ses_age < 90] <- "85_89"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 90 & DataRaw$ses_age < 95] <- "90_94"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 95 & DataRaw$ses_age < 100] <- "95_99"
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 100] <- "100+"
DataClean$ses_ageGroup5Years <- factor(DataClean$ses_ageGroup5Years, levels = c("18_19",
                                                                        "20_24",
                                                                        "25_29",
                                                                        "30_34",
                                                                        "35_39",
                                                                        "40_44",
                                                                        "45_49",
                                                                        "50_54",
                                                                        "55_59",
                                                                        "60_64",
                                                                        "65_69",
                                                                        "70_74",
                                                                        "75_79",
                                                                        "80_84",
                                                                        "85_89",
                                                                        "90_94",
                                                                        "95_99",
                                                                        "100+"))
table(DataClean$ses_ageGroup5Years)

table(DataRaw$ses_age)

DataClean$ses_age_4Cat <- NA
DataClean$ses_age_4Cat[DataRaw$ses_age < 25 & DataRaw$ses_age > 18] <- "18_24"
DataClean$ses_age_4Cat[DataRaw$ses_age >= 25 & DataRaw$ses_age < 45] <- "25_44"
DataClean$ses_age_4Cat[DataRaw$ses_age >= 45 & DataRaw$ses_age < 65] <- "45_64"
DataClean$ses_age_4Cat[DataRaw$ses_age >= 65] <- "65+"
DataClean$ses_age_4Cat <- factor(DataClean$ses_age_4Cat, levels = c("18_24",
                                                                        "25_44",
                                                                        "45_64",
                                                                        "65+"))
table(DataClean$ses_age_4Cat)



## region ----------------------------------------------------------------------

attributes(DataRaw$ses_region)
table(DataRaw$ses_region)
DataClean$ses_province <- NA
DataClean$ses_province[DataRaw$ses_region == 1] <- "AB"
DataClean$ses_province[DataRaw$ses_region == 2] <- "BC"
DataClean$ses_province[DataRaw$ses_region == 3] <- "MB"
DataClean$ses_province[DataRaw$ses_region == 4] <- "NB"
DataClean$ses_province[DataRaw$ses_region == 5] <- "NL"
DataClean$ses_province[DataRaw$ses_region == 6] <- "NT"
DataClean$ses_province[DataRaw$ses_region == 7] <- "NS"
DataClean$ses_province[DataRaw$ses_region == 8] <- "NU"
DataClean$ses_province[DataRaw$ses_region == 9] <- "ON"
DataClean$ses_province[DataRaw$ses_region == 10] <- "PE"
DataClean$ses_province[DataRaw$ses_region == 11] <- "QC"
DataClean$ses_province[DataRaw$ses_region == 12] <- "SK"
DataClean$ses_province[DataRaw$ses_region == 13] <- "YT"
DataClean$ses_province <- factor(DataClean$ses_province, levels = c("AB",
                                                                      "BC",
                                                                      "MB",
                                                                      "NB",
                                                                      "NL",
                                                                      "NT",
                                                                      "NS",
                                                                      "NU",
                                                                      "ON",
                                                                      "PE",
                                                                      "QC",
                                                                      "SK",
                                                                      "YT"))
table(DataClean$ses_province)

# region ----------------------------------------------------------------------

DataClean$ses_region <- NA
DataClean$ses_region[DataRaw$ses_region == 1 |
                        DataRaw$ses_region == 3 |
                        DataRaw$ses_region == 12] <- "prairie"
DataClean$ses_region[DataRaw$ses_region == 2] <- "british_columbia"
DataClean$ses_region[DataRaw$ses_region == 5 |
                        DataRaw$ses_region == 7 |
                        DataRaw$ses_region == 10] <- "atlantic"
DataClean$ses_region[DataRaw$ses_region == 9] <- "ontario"
DataClean$ses_region[DataRaw$ses_region == 11] <- "quebec"
DataClean$ses_region[DataRaw$ses_region == 6 |
                        DataRaw$ses_region == 8 |
                        DataRaw$ses_region == 13] <- "territories"
DataClean$ses_region <- factor(DataClean$ses_region, levels = c("prairie",
                                                                "british_columbia",
                                                                "atlantic",
                                                                "ontario",
                                                                "quebec",
                                                                "territories"))
table(DataClean$ses_region)

# region qc -------------------------------------------------------------------

table(DataRaw$ses_region)
DataClean$ses_regionQc <- NA
DataClean$ses_regionQc[DataRaw$ses_region == 11] <- 1
DataClean$ses_regionQc[DataRaw$ses_region != 11] <- 0
table(DataClean$ses_regionQc)


## postal_code -----------------------------------------------------------------

attributes(DataRaw$ses_postal_code)
table(DataRaw$ses_postal_code)
DataClean$ses_postalCode <- NA
DataClean$ses_postalCode <- DataRaw$ses_postal_code
table(DataClean$ses_postalCode)

## language --------------------------------------------------------------------

attributes(DataRaw$ses_language)
table(DataRaw$ses_language)
DataClean$ses_language <- NA
DataClean$ses_language[DataRaw$ses_language == 1] <- "english"
DataClean$ses_language[DataRaw$ses_language == 2] <- "french"
DataClean$ses_language[DataRaw$ses_language == 3] <- "other"
DataClean$ses_language <- factor(DataClean$ses_language, levels = c("english",
                                                                      "french",
                                                                      "other"))
table(DataClean$ses_language)


## religion --------------------------------------------------------------------

attributes(DataRaw$ses_religion)
table(DataRaw$ses_religion)
DataClean$ses_religion <- NA
DataClean$ses_religion[DataRaw$ses_religion == 1] <- "agnostic"
DataClean$ses_religion[DataRaw$ses_religion == 2] <- "atheist"
DataClean$ses_religion[DataRaw$ses_religion == 3] <- "buddhist"
DataClean$ses_religion[DataRaw$ses_religion == 4] <- "catholic"
DataClean$ses_religion[DataRaw$ses_religion == 5] <- "orthodox_christian"
DataClean$ses_religion[DataRaw$ses_religion == 6] <- "hindu"
DataClean$ses_religion[DataRaw$ses_religion == 7] <- "muslim"
DataClean$ses_religion[DataRaw$ses_religion == 8] <- "jew"
DataClean$ses_religion[DataRaw$ses_religion == 9] <- "protestant"
DataClean$ses_religion[DataRaw$ses_religion == 10] <- "sikh"
DataClean$ses_religion[DataRaw$ses_religion == 11] <- "evangelical"
DataClean$ses_religion[DataRaw$ses_religion == 12] <- "other"
DataClean$ses_religion <- factor(DataClean$ses_religion, levels = c("agnostic",
                                                                      "atheist",
                                                                      "buddhist",
                                                                      "catholic",
                                                                      "orthodox_christian",
                                                                      "hindu",
                                                                      "muslim",
                                                                      "jew",
                                                                      "protestant",
                                                                      "sikh",
                                                                      "evangelical",
                                                                      "other"))
table(DataRaw$ses_religion)
DataClean$ses_religionBigFive <- NA
DataClean$ses_religionBigFive[DataRaw$ses_religion %in% c(4, 5, 9, 11)] <- "christian"
DataClean$ses_religionBigFive[DataRaw$ses_religion == 7] <- "muslim"
DataClean$ses_religionBigFive[DataRaw$ses_religion == 8] <- "jew"
DataClean$ses_religionBigFive[DataRaw$ses_religion == 6] <- "hindu"
DataClean$ses_religionBigFive[DataRaw$ses_religion == 3] <- "buddhist"
DataClean$ses_religionBigFive[DataRaw$ses_religion %in% c(1, 2)] <- "agnostic/atheist"
DataClean$ses_religionBigFive[DataRaw$ses_religion == 12 | 
                                 DataRaw$ses_religion == 10] <- "other"
DataClean$ses_religionBigFive <- factor(DataClean$ses_religionBigFive)
table(DataClean$ses_religionBigFive)


attributes(DataRaw$ses_religion)
table(DataRaw$ses_religion, useNA = "ifany")
DataClean$ses_religion_bin <- NA
DataClean$ses_religion_bin[DataRaw$ses_religion == 1 | DataRaw$ses_religion == 2] <- 0
DataClean$ses_religion_bin[DataRaw$ses_religion != 1 & DataRaw$ses_religion != 2 & !is.na(DataRaw$ses_religion)] <- 1
table(DataClean$ses_religion_bin)

## religiosity -------------------------------------------------------------

table(DataRaw$ses_religiosity_1)
DataClean$ses_religiosity <- NA
DataClean$ses_religiosity <- DataRaw$ses_religiosity_1/100
table(DataClean$ses_religiosity)


## education ---------------------------------------------------------------

attributes(DataRaw$ses_education)
table(DataRaw$ses_education)
DataClean$ses_educ <- NA
DataClean$ses_educ[DataRaw$ses_education == 1] <- "no_schooling"
DataClean$ses_educ[DataRaw$ses_education == 2] <- "elementary_school"
DataClean$ses_educ[DataRaw$ses_education == 3] <- "high_school"
DataClean$ses_educ[DataRaw$ses_education == 4] <- "technical_community_cegep"
DataClean$ses_educ[DataRaw$ses_education == 5] <- "bachelor"
DataClean$ses_educ[DataRaw$ses_education == 6] <- "masters"
DataClean$ses_educ[DataRaw$ses_education == 7] <- "doctorate"
DataClean$ses_educ <- factor(DataClean$ses_educ, levels = c("no_schooling",
                                                                        "elementary_school",
                                                                        "high_school",
                                                                        "technical_community_cegep",
                                                                        "bachelor",
                                                                        "masters",
                                                                        "doctorate"))
table(DataClean$ses_educ)

DataClean$ses_educ_5Cat <- NA
DataClean$ses_educ_5Cat[DataRaw$ses_education == 1 | DataRaw$ses_education == 2] <- "educBHS"
DataClean$ses_educ_5Cat[DataRaw$ses_education == 3] <- "educHS"
DataClean$ses_educ_5Cat[DataRaw$ses_education == 4] <- "educPostHS"
DataClean$ses_educ_5Cat[DataRaw$ses_education == 5 ]<- "educUnivBac"
DataClean$ses_educ_5Cat[DataRaw$ses_education == 6 | DataRaw$ses_education == 7] <- "educUnivSup"
DataClean$ses_educ_5Cat <- factor(DataClean$ses_educ_5Cat, levels = c("educBHS",
                                                                        "educHS",
                                                                        "educPostHS",
                                                                        "educUnivBac",
                                                                        "educUnivSup"))
table(DataClean$ses_educ_5Cat)

DataClean$ses_educ_3Cat <- NA
DataClean$ses_educ_3Cat[DataRaw$ses_education == 1 | DataRaw$ses_education == 2 | DataRaw$ses_education == 3] <- "educBHS"
DataClean$ses_educ_3Cat[DataRaw$ses_education == 4] <- "educPostHS"
DataClean$ses_educ_3Cat[DataRaw$ses_education == 5 | DataRaw$ses_education == 6 | DataRaw$ses_education == 7]<- "educUniv"
DataClean$ses_educ_3Cat <- factor(DataClean$ses_educ_3Cat, levels = c("educBHS",
                                                                        "educPostHS",
                                                                        "educUniv"))

table(DataClean$educ_5Cat)


## income ----------------------------------------------------------------

attributes(DataRaw$ses_income)
table(DataRaw$ses_income)
DataClean$ses_income <- NA
DataClean$ses_income[DataRaw$ses_income == 1] <- "no_income"
DataClean$ses_income[DataRaw$ses_income == 2] <- "1_to_30000"
DataClean$ses_income[DataRaw$ses_income == 3] <- "30001_to_60000"
DataClean$ses_income[DataRaw$ses_income == 4] <- "60001_to_90000"
DataClean$ses_income[DataRaw$ses_income == 5] <- "90001_to_110000"
DataClean$ses_income[DataRaw$ses_income == 6] <- "110001_to_150000"
DataClean$ses_income[DataRaw$ses_income == 7] <- "150001_to_200000"
DataClean$ses_income[DataRaw$ses_income == 8] <- "more_than_200000"
DataClean$ses_income <- factor(DataClean$ses_income, levels = c("no_income",
                                                                  "1_to_30000",
                                                                  "30001_to_60000",
                                                                  "60001_to_90000",
                                                                  "90001_to_110000",
                                                                  "110001_to_150000",
                                                                  "150001_to_200000",
                                                                  "more_than_200000"))
table(DataClean$ses_income)

DataClean$ses_income3Cat <- NA
DataClean$ses_income3Cat[DataRaw$ses_income == 1 | DataRaw$ses_income == 2] <- "Low"
DataClean$ses_income3Cat[DataRaw$ses_income == 3 | DataRaw$ses_income == 4 | DataRaw$ses_income == 5 | DataRaw$ses_income == 6] <- "Mid"
DataClean$ses_income3Cat[DataRaw$ses_income == 7 | DataRaw$ses_income == 8] <- "High"
DataClean$ses_income3Cat <- factor(DataClean$ses_income3Cat)
table(DataClean$ses_income3Cat)

attributes(DataRaw$ses_income)
table(DataRaw$ses_income)
DataClean$ses_incomeCensus <- NA
DataClean$ses_incomeCensus[DataRaw$ses_income == 1] <- "no_income"
DataClean$ses_incomeCensus[DataRaw$ses_income == 2] <- "1_to_30000"
DataClean$ses_incomeCensus[DataRaw$ses_income == 3] <- "30001_to_60000"
DataClean$ses_incomeCensus[DataRaw$ses_income == 4] <- "60001_to_90000"
DataClean$ses_incomeCensus[DataRaw$ses_income == 5] <- "90001_to_110000"
DataClean$ses_incomeCensus[DataRaw$ses_income == 6] <- "110001_to_150000"
DataClean$ses_incomeCensus[DataRaw$ses_income %in% c(7, 8)] <- "more_than_150000"

DataClean$ses_incomeCensus <- factor(DataClean$ses_incomeCensus, levels = c("no_income",
                                                                  "1_to_30000",
                                                                  "30001_to_60000",
                                                                  "60001_to_90000",
                                                                  "90001_to_110000",
                                                                  "110001_to_150000",
                                                                  "more_than_150000"))
table(DataClean$ses_incomeCensus)

## bilingualism-------------------------------------------------------------

attributes(DataRaw$ses_bilingual_2)
table(DataRaw$ses_bilingual_1)

DataClean$ses_englishSkills <- NA
DataClean$ses_englishSkills[DataRaw$ses_bilingual_1 == 4] <- "Full proficiency"
DataClean$ses_englishSkills[DataRaw$ses_bilingual_1 == 3] <- "Conversational level"
DataClean$ses_englishSkills[DataRaw$ses_bilingual_1 == 2] <- "Basic level"
DataClean$ses_englishSkills[DataRaw$ses_bilingual_1 == 1] <- "No proficiency"
DataClean$ses_englishSkills <- factor(DataClean$ses_englishSkills, levels = c("Full proficiency",
                                                                  "Conversational level",
                                                                  "Basic level",
                                                                  "No proficiency"))
table(DataClean$ses_englishSkills)
  
  
  
  
DataClean$ses_frenchSkills <- NA
DataClean$ses_frenchSkills[DataRaw$ses_bilingual_2 == 4] <- "Full proficiency"
DataClean$ses_frenchSkills[DataRaw$ses_bilingual_2 == 3] <- "Conversational level"
DataClean$ses_frenchSkills[DataRaw$ses_bilingual_2 == 2] <- "Basic level"
DataClean$ses_frenchSkills[DataRaw$ses_bilingual_2 == 1] <- "No proficiency"
DataClean$ses_frenchSkills <- factor(DataClean$ses_frenchSkills, levels = c("Full proficiency",
                                                                          "Conversational level",
                                                                          "Basic level",
                                                                          "No proficiency"))
table(DataClean$ses_frenchSkills)




## environment -------------------------------------------------------------

attributes(DataRaw$ses_environment) 
table(DataRaw$ses_environment)

DataClean$ses_rurality <- NA # P-e changer le nom de cette variable pour quelque chose de plus parlant
DataClean$ses_rurality[DataRaw$ses_environment == 1] <- "urban"
DataClean$ses_rurality[DataRaw$ses_environment == 2] <- "suburban"
DataClean$ses_rurality[DataRaw$ses_environment == 3] <- "rural"
DataClean$ses_rurality <- factor(DataClean$ses_rurality, levels = c("urban",
                                                                            "suburban",
                                                                            "rural"))
table(DataClean$ses_rurality)



## status ------------------------------------------------------------------

attributes(DataRaw$ses_status)
table(DataRaw$ses_status)

DataClean$ses_matStatus <- NA
DataClean$ses_matStatus[DataRaw$ses_status == 1] <- "single"
DataClean$ses_matStatus[DataRaw$ses_status == 2] <- "married"
DataClean$ses_matStatus[DataRaw$ses_status == 3] <- "common_law_relationship"
DataClean$ses_matStatus[DataRaw$ses_status == 4] <- "widower_widow"
DataClean$ses_matStatus[DataRaw$ses_status == 5] <- "divorced_separated"
DataClean$ses_matStatus <- factor(DataClean$ses_matStatus, levels = c("single",
                                                                  "married",
                                                                  "common_law_relationship",
                                                                  "widower_widow",
                                                                  "divorced_separated"))
table(DataClean$ses_matStatus)


## owner -------------------------------------------------------------------

attributes(DataRaw$ses_owner)
table(DataRaw$ses_owner)

DataClean$ses_owner <- NA
DataClean$ses_owner[DataRaw$ses_owner == 1] <- "owner"
DataClean$ses_owner[DataRaw$ses_owner == 2] <- "tenant"
DataClean$ses_owner[DataRaw$ses_owner == 3] <- "neither"
DataClean$ses_owner <- factor(DataClean$ses_owner, levels = c("owner",
                                                                  "tenant",
                                                                  "neither"))
table(DataClean$ses_owner)


## kids --------------------------------------------------------------------

attributes(DataRaw$ses_kids)
table(DataRaw$ses_kids)

DataClean$ses_householdComp <- NA
DataClean$ses_householdComp[DataRaw$ses_kids == 1] <- "partner_no_children"
DataClean$ses_householdComp[DataRaw$ses_kids == 2] <- "partner_with_children"
DataClean$ses_householdComp[DataRaw$ses_kids == 3] <- "no_partner_no_children"
DataClean$ses_householdComp[DataRaw$ses_kids == 4] <- "no_partner_with_children"
DataClean$ses_householdComp[DataRaw$ses_kids == 5] <- "roommates"
DataClean$ses_householdComp[DataRaw$ses_kids == 6] <- "other"
DataClean$ses_householdComp <- factor(DataClean$ses_householdComp, levels = c("partner_no_children",
                                                                                "partner_with_children",
                                                                                "no_partner_no_children",
                                                                                "no_partner_with_children",
                                                                                "roommates",
                                                                                "other"))
table(DataClean$ses_householdComp)


## ses_occupation --------------------------------------------------------------

table(DataRaw$ses_occupation)
attributes(DataRaw$ses_occupation)
DataClean$ses_occupation <- NA
DataClean$ses_occupation[DataRaw$ses_occupation == 1] <- "paid_employment"
DataClean$ses_occupation[DataRaw$ses_occupation == 2] <- "self_employment"
DataClean$ses_occupation[DataRaw$ses_occupation == 3] <- "student"
DataClean$ses_occupation[DataRaw$ses_occupation == 4] <- "retired"
DataClean$ses_occupation[DataRaw$ses_occupation == 5] <- "looking_for_work"
DataClean$ses_occupation[DataRaw$ses_occupation == 6] <- "unemployed"
DataClean$ses_occupation[DataRaw$ses_occupation == 7] <- "other"
DataClean$ses_occupation <- factor(DataClean$ses_occupation, levels = c("paid_employment",
                                                                          "self_employment",
                                                                          "student",
                                                                          "retired",
                                                                          "looking_for_work",
                                                                          "unemployed",
                                                                          "other"))

table(DataRaw$ses_occupation)
attributes(DataRaw$ses_occupation)
# Initialisation de la variable
DataClean$ses_occupation_5Cat<- NA

# Attribution des valeurs corrigées
DataClean$ses_occupation_5Cat[DataRaw$ses_occupation == 1 | DataRaw$ses_occupation == 2] <- "employed"
DataClean$ses_occupation_5Cat[DataRaw$ses_occupation == 5 | DataRaw$ses_occupation == 6] <- "unemployed"
DataClean$ses_occupation_5Cat[DataRaw$ses_occupation == 4] <- "retired"
DataClean$ses_occupation_5Cat[DataRaw$ses_occupation == 3] <- "student"
DataClean$ses_occupation_5Cat[DataRaw$ses_occupation == 7] <- "other"
table(DataClean$ses_occupation_5Cat)
# Conversion en facteur avec les niveaux ordonnés
DataClean$ses_occupation_5Cat <- factor(DataClean$ses_occupation_5Cat, 
                                            levels = c("employed", 
                                                       "unemployed", 
                                                       "retired", 
                                                       "student", 
                                                       "other"))

# Vérification des résultats
table(DataClean$ses_occupation_5Cat)

## SES (enfant) -------------------------------------------------------------------

attributes(DataRaw$ses_children)
table(DataRaw$ses_children)
DataRaw$ses_children <- as.numeric(DataRaw$ses_children)
DataClean$ses_children <- NA
DataClean$ses_children[DataRaw$ses_children == 0] <- 0
DataClean$ses_children[DataRaw$ses_children == 1] <- 1
DataClean$ses_children[DataRaw$ses_children == 2] <- 2
DataClean$ses_children[DataRaw$ses_children == 3] <- 3
DataClean$ses_children[DataRaw$ses_children == 4] <- 4
DataClean$ses_children[DataRaw$ses_children >= 5] <- 5
DataClean$ses_children <- factor(DataClean$ses_children, levels = c(0,
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      4,
                                                                      5),
                                  ordered = TRUE)
table(DataClean$ses_children)



## ethnicity -------------------------------------------------------------
attributes(DataRaw$ses_ethnicity)
table(DataRaw$ses_ethnicity)
DataClean$ses_ethnicity <- NA
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 1] <- "white"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 2] <- "black"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 3] <- "indigenous"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 4] <- "asian"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 5] <- "hispanic"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 6] <- "arab"
DataClean$ses_ethnicity[DataRaw$ses_ethnicity == 7] <- "other"
DataClean$ses_ethnicity <- factor(DataClean$ses_ethnicity, levels = c("white",
                                                                        "black",
                                                                        "indigenous",
                                                                        "asian",
                                                                        "hispanic",
                                                                        "arab",
                                                                        "other"))
table(DataClean$ses_ethnicity, useNA = "ifany")

DataClean$ses_ethnicityWhite <- NA
DataClean$ses_ethnicityWhite[DataRaw$ses_ethnicity == 1] <- 1
DataClean$ses_ethnicityWhite[DataRaw$ses_ethnicity != 1] <- 0
table(DataClean$ses_ethnicityWhite)

DataClean$ses_ethnicityWB <- NA
DataClean$ses_ethnicityWB[DataRaw$ses_ethnicity == 1] <- "white"
DataClean$ses_ethnicityWB[DataRaw$ses_ethnicity == 2] <- "black"
DataClean$ses_ethnicityWB[(DataRaw$ses_ethnicity %in% c(3, 4, 5, 6, 7))] <- "other"
DataClean$ses_ethnicityWB <- factor(DataClean$ses_ethnicityWB)
DataClean$ses_ethnicityWB <- relevel(DataClean$ses_ethnicityWB, ref = "white")
table(DataClean$ses_ethnicityWB, useNA = "ifany")


labels <- stringr::str_trim(names(attributes(DataRaw$ses_ethnicity)$labels))
json_data <- list(
  `1` = labels[1],
  `0` = labels[-1]
)

jsonlite::toJSON(json_data, pretty = TRUE)



## orientation -----------------------------------------------------------
attributes(DataRaw$ses_orientation)
table(DataRaw$ses_orientation)
DataClean$ses_sexOrientation <- NA
DataClean$ses_sexOrientation[DataRaw$ses_orientation == 1] <- "heterosexual"
DataClean$ses_sexOrientation[DataRaw$ses_orientation == 2] <- "gay"
DataClean$ses_sexOrientation[DataRaw$ses_orientation == 3] <- "bisexual"
DataClean$ses_sexOrientation[DataRaw$ses_orientation == 4] <- "other"
DataClean$ses_sexOrientation <- factor(DataClean$ses_sexOrientation, levels = c("heterosexual",
                                                                                        "gay",
                                                                                        "bisexual",
                                                                                        "other"))
table(DataClean$ses_sexOrientation)

## heterosexual

DataClean$ses_sexOrientationHetero <- NA
DataClean$ses_sexOrientationHetero[DataRaw$ses_orientation == 1] <- 1
DataClean$ses_sexOrientationHetero[DataRaw$ses_orientation != 1] <- 0
table(DataClean$ses_sexOrientationHetero)



## parent ----------------------------------------------------------------

attributes(DataRaw$ses_parent)
table(DataRaw$ses_parent)
DataClean$ses_parentImmigrant <- NA
DataClean$ses_parentImmigrant[DataRaw$ses_parent == 1] <- 1
DataClean$ses_parentImmigrant[DataRaw$ses_parent == 2] <- 0
table(DataClean$ses_parentImmigrant)


## immigrant -------------------------------------------------------------

attributes(DataRaw$ses_immigrant)
table(DataRaw$ses_immigrant)
DataClean$ses_immigrant <- NA
DataClean$ses_immigrant[DataRaw$ses_immigrant == 1] <- 0
DataClean$ses_immigrant[DataRaw$ses_immigrant != 1] <- 1
table(DataClean$ses_immigrant)

## dwelling --------------------------------------------------------------
attributes(DataRaw$ses_dwelling)
table(DataRaw$ses_dwelling, useNA = "ifany")

DataClean$ses_dwelling <- NA
DataClean$ses_dwelling[DataRaw$ses_dwelling == 1] <- "apartment_complex"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 2] <- "loft"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 3] <- "condominium"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 4] <- "high_rise_apartment"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 5] <- "stand_alone_house"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 6] <- "townhouse"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 7] <- "duplex"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 8] <- "cooperative_housing"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 9] <- "social_or_public_housing"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 10] <- "mobile_home"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 11] <- "other"
DataClean$ses_dwelling <- factor(DataClean$ses_dwelling, levels = c("apartment_complex",
                                                                              "loft",
                                                                              "condominium",
                                                                              "high_rise_apartment",
                                                                              "stand_alone_house",
                                                                              "townhouse",
                                                                              "duplex",
                                                                              "cooperative_housing",
                                                                              "social_or_public_housing",
                                                                              "mobile_home",
                                                                              "other"))
table(DataClean$ses_dwelling, useNA = "ifany")

attributes(DataRaw$ses_dwelling)
table(DataRaw$ses_dwelling)

# Initialisation
DataClean$ses_dwelling_cat <- NA

# Regroupement des codes existants
# 1,2,3,8,9,11 -> "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 1] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 2] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 3] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 8] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 9] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 11] <- "other"

# 4 -> "high_rise_apartment"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 4] <- "high_rise_apartment"

# 5 -> "stand_alone_house"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 5] <- "stand_alone_house"

# 6 -> "townhouse"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 6] <- "townhouse"

# 7 -> "duplex"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 7] <- "duplex"

# 10 -> "mobile_home"
DataClean$ses_dwelling_cat[DataRaw$ses_dwelling == 10] <- "mobile_home"

# On définit l'ordre final des facteurs
DataClean$ses_dwelling_cat <- factor(
  DataClean$ses_dwelling_cat,
  levels = c("stand_alone_house",
             "townhouse",
             "duplex",
             "apartment_complex",
             "high_rise_apartment",
             "mobile_home",
            "other")
)
table(DataClean$ses_dwelling_cat, useNA = "ifany")

## bin

DataClean$ses_dwellingApp <- NA
DataClean$ses_dwellingApp[DataRaw$ses_dwelling == 1] <- 1
DataClean$ses_dwellingApp[DataRaw$ses_dwelling != 1] <- 0
table(DataClean$ses_dwellingApp)

DataClean$ses_dwellingLoft <- NA
DataClean$ses_dwellingLoft[DataRaw$ses_dwelling == 2] <- 1
DataClean$ses_dwellingLoft[DataRaw$ses_dwelling != 2] <- 0
table(DataClean$ses_dwellingLoft)

DataClean$ses_dwellingCondo <- NA
DataClean$ses_dwellingCondo[DataRaw$ses_dwelling == 3] <- 1
DataClean$ses_dwellingCondo[DataRaw$ses_dwelling != 3] <- 0
table(DataClean$ses_dwellingCondo)

DataClean$ses_dwellingTour <- NA
DataClean$ses_dwellingTour[DataRaw$ses_dwelling == 4] <- 1
DataClean$ses_dwellingTour[DataRaw$ses_dwelling != 4] <- 0
table(DataClean$ses_dwellingTour)

DataClean$ses_dwellingDetachedHouse <- NA
DataClean$ses_dwellingDetachedHouse[DataRaw$ses_dwelling == 5] <- 1
DataClean$ses_dwellingDetachedHouse[DataRaw$ses_dwelling != 5] <- 0
table(DataClean$ses_dwellingDetachedHouse)

DataClean$ses_dwellingTownhouse <- NA
DataClean$ses_dwellingTownhouse[DataRaw$ses_dwelling == 6] <- 1
DataClean$ses_dwellingTownhouse[DataRaw$ses_dwelling != 6] <- 0
table(DataClean$ses_dwellingTownhouse)

DataClean$ses_dwellingDuplex <- NA
DataClean$ses_dwellingDuplex[DataRaw$ses_dwelling == 7] <- 1
DataClean$ses_dwellingDuplex[DataRaw$ses_dwelling != 7] <- 0
table(DataClean$ses_dwellingDuplex)

DataClean$ses_dwellingCoop <- NA
DataClean$ses_dwellingCoop[DataRaw$ses_dwelling == 8] <- 1
DataClean$ses_dwellingCoop[DataRaw$ses_dwelling != 8] <- 0
table(DataClean$ses_dwellingCoop)

DataClean$ses_dwellingHLM <- NA
DataClean$ses_dwellingHLM[DataRaw$ses_dwelling == 9] <- 1
DataClean$ses_dwellingHLM[DataRaw$ses_dwelling != 9] <- 0
table(DataClean$ses_dwellingHLM)

DataClean$ses_dwellingMobile <- NA
DataClean$ses_dwellingMobile[DataRaw$ses_dwelling == 10] <- 1
DataClean$ses_dwellingMobile[DataRaw$ses_dwelling != 10] <- 0
table(DataClean$ses_dwellingMobile)

DataClean$ses_dwellingOther <- NA
DataClean$ses_dwellingOther[DataRaw$ses_dwelling == 11] <- 1
DataClean$ses_dwellingOther[DataRaw$ses_dwelling != 11] <- 0
table(DataClean$ses_dwellingOther)


