# SES

## gender ----------------------------------------------------------------------

attributes(Data_raw$ses_gender)
table(Data_raw$ses_gender)
Data_clean$ses_gender <- NA
Data_clean$ses_gender[Data_raw$ses_gender == 1] <- "male"
Data_clean$ses_gender[Data_raw$ses_gender == 2] <- "female"
Data_clean$ses_gender[Data_raw$ses_gender == 3] <- "trans_man"
Data_clean$ses_gender[Data_raw$ses_gender == 4] <- "trans_woman"
Data_clean$ses_gender[Data_raw$ses_gender == 5] <- "non_binary"
Data_clean$ses_gender[Data_raw$ses_gender == 6] <- "queer"
Data_clean$ses_gender[Data_raw$ses_gender == 7] <- "agender"
Data_clean$ses_gender <- factor(Data_clean$ses_gender)
table(Data_clean$ses_gender)

# gender_female ----------------------------------------------------------------

Data_clean$ses_genderFemale <- NA
Data_clean$ses_genderFemale[Data_raw$ses_gender == 2] <- 1
Data_clean$ses_genderFemale[Data_raw$ses_gender != 2] <- 0
table(Data_clean$ses_genderFemale)

## age--------------------------------------------------------------------------

attributes(Data_raw$ses_age)
table(Data_raw$ses_age)
Data_clean$ses_age <- NA
Data_clean$ses_age <- Data_raw$ses_age
table(Data_clean$ses_age)

Data_clean$ses_ageGroup5Years <- NA
Data_clean$ses_ageGroup5Years[Data_raw$ses_age == 18 | Data_raw$ses_age == 19] <- "18_19"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 20 & Data_raw$ses_age < 25] <- "20_24"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 25 & Data_raw$ses_age < 30] <- "25_29"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 30 & Data_raw$ses_age < 35] <- "30_34"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 35 & Data_raw$ses_age < 40] <- "35_39"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 40 & Data_raw$ses_age < 45] <- "40-44"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 45 & Data_raw$ses_age < 50] <- "45_49"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 50 & Data_raw$ses_age < 55] <- "50_54"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 55 & Data_raw$ses_age < 60] <- "55_59"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 60 & Data_raw$ses_age < 65] <- "60_64"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 65 & Data_raw$ses_age < 70] <- "65_69"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 70 & Data_raw$ses_age < 75] <- "70_74"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 75 & Data_raw$ses_age < 80] <- "75_79"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 80 & Data_raw$ses_age < 85] <- "80_84"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 85 & Data_raw$ses_age < 90] <- "85_89"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 90 & Data_raw$ses_age < 95] <- "90_94"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 95 & Data_raw$ses_age < 100] <- "95_99"
Data_clean$ses_ageGroup5Years[Data_raw$ses_age >= 100] <- "100+"
Data_clean$ses_ageGroup5Years <- factor(Data_clean$ses_ageGroup5Years, levels = c("18_19",
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
table(Data_clean$ses_ageGroup5Years)

Data_clean$ses_age_4Cat <- NA
Data_clean$ses_age_4Cat[Data_raw$ses_age < 25 & Data_raw$ses_age > 18] <- "18_24"
Data_clean$ses_age_4Cat[Data_raw$ses_age >= 25 & Data_raw$ses_age < 45] <- "25_44"
Data_clean$ses_age_4Cat[Data_raw$ses_age >= 45 & Data_raw$ses_age < 65] <- "45_64"
Data_clean$ses_age_4Cat[Data_raw$ses_age >= 65] <- "65+"
Data_clean$ses_age_4Cat <- factor(Data_clean$ses_age_group, levels = c("18_24",
                                                                        "25_44",
                                                                        "45_64",
                                                                        "65+"))
table(Data_clean$ses_age_4Cat)



## region ----------------------------------------------------------------------

attributes(Data_raw$ses_region)
table(Data_raw$ses_region)
Data_clean$ses_province <- NA
Data_clean$ses_province[Data_raw$ses_region == 1] <- "AB"
Data_clean$ses_province[Data_raw$ses_region == 2] <- "BC"
Data_clean$ses_province[Data_raw$ses_region == 3] <- "MB"
Data_clean$ses_province[Data_raw$ses_region == 4] <- "NB"
Data_clean$ses_province[Data_raw$ses_region == 5] <- "NL"
Data_clean$ses_province[Data_raw$ses_region == 6] <- "NT"
Data_clean$ses_province[Data_raw$ses_region == 7] <- "NS"
Data_clean$ses_province[Data_raw$ses_region == 8] <- "NU"
Data_clean$ses_province[Data_raw$ses_region == 9] <- "ON"
Data_clean$ses_province[Data_raw$ses_region == 10] <- "PE"
Data_clean$ses_province[Data_raw$ses_region == 11] <- "QC"
Data_clean$ses_province[Data_raw$ses_region == 12] <- "SK"
Data_clean$ses_province[Data_raw$ses_region == 13] <- "YT"
Data_clean$ses_province <- factor(Data_clean$ses_province, levels = c("AB",
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
table(Data_clean$ses_province)

# region ----------------------------------------------------------------------

Data_clean$ses_region <- NA
Data_clean$ses_region[Data_raw$ses_region == 1 |
                        Data_raw$ses_region == 3 |
                        Data_raw$ses_region == 12] <- "prairie"
Data_clean$ses_region[Data_raw$ses_region == 2] <- "british_columbia"
Data_clean$ses_region[Data_raw$ses_region == 5 |
                        Data_raw$ses_region == 7 |
                        Data_raw$ses_region == 10] <- "atlantic"
Data_clean$ses_region[Data_raw$ses_region == 9] <- "ontario"
Data_clean$ses_region[Data_raw$ses_region == 11] <- "quebec"
Data_clean$ses_region[Data_raw$ses_region == 6 |
                        Data_raw$ses_region == 8 |
                        Data_raw$ses_region == 13] <- "territories"
table(Data_clean$ses_region)

# region qc -------------------------------------------------------------------

table(Data_raw$ses_region)
Data_clean$ses_regionQc <- NA
Data_clean$ses_regionQc[Data_raw$ses_region == 11] <- 1
Data_clean$ses_regionQc[Data_raw$ses_region != 11] <- 0
table(Data_clean$ses_regionQc)


## postal_code -----------------------------------------------------------------

attributes(Data_raw$ses_postal_code)
table(Data_raw$ses_postal_code)
Data_clean$ses_postalCode <- NA
Data_clean$ses_postalCode <- Data_raw$ses_postal_code
table(Data_clean$ses_postalCode)

## language --------------------------------------------------------------------

attributes(Data_raw$ses_language)
table(Data_raw$ses_language)
Data_clean$ses_language <- NA
Data_clean$ses_language[Data_raw$ses_language == 1] <- "english"
Data_clean$ses_language[Data_raw$ses_language == 2] <- "french"
Data_clean$ses_language[Data_raw$ses_language == 3] <- "other"
Data_clean$ses_language <- factor(Data_clean$ses_language, levels = c("english",
                                                                      "french",
                                                                      "other"))
table(Data_clean$ses_language)


## religion --------------------------------------------------------------------

attributes(Data_raw$ses_religion)
table(Data_raw$ses_religion)
Data_clean$ses_religion <- NA
Data_clean$ses_religion[Data_raw$ses_religion == 1] <- "agnostic"
Data_clean$ses_religion[Data_raw$ses_religion == 2] <- "atheist"
Data_clean$ses_religion[Data_raw$ses_religion == 3] <- "buddhist"
Data_clean$ses_religion[Data_raw$ses_religion == 4] <- "catholic"
Data_clean$ses_religion[Data_raw$ses_religion == 5] <- "orthodox_christian"
Data_clean$ses_religion[Data_raw$ses_religion == 6] <- "hindu"
Data_clean$ses_religion[Data_raw$ses_religion == 7] <- "muslim"
Data_clean$ses_religion[Data_raw$ses_religion == 8] <- "jew"
Data_clean$ses_religion[Data_raw$ses_religion == 9] <- "protestant"
Data_clean$ses_religion[Data_raw$ses_religion == 10] <- "sikh"
Data_clean$ses_religion[Data_raw$ses_religion == 11] <- "evangelical"
Data_clean$ses_religion[Data_raw$ses_religion == 12] <- "other"
Data_clean$ses_religion <- factor(Data_clean$ses_religion, levels = c("agnostic",
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
table(Data_clean$ses_religion)
Data_clean$ses_religionBigFive <- NA
Data_clean$ses_religionBigFive[Data_raw$ses_religion %in% c(4, 5, 9, 11)] <- "christian"
Data_clean$ses_religionBigFive[Data_raw$ses_religion == 7] <- "muslim"
Data_clean$ses_religionBigFive[Data_raw$ses_religion == 8] <- "jew"
Data_clean$ses_religionBigFive[Data_raw$ses_religion == 6] <- "hindu"
Data_clean$ses_religionBigFive[Data_raw$ses_religion == 3] <- "buddhist"
Data_clean$ses_religionBigFive[Data_raw$ses_religion %in% c(1, 2)] <- "agnostic/atheist"
Data_clean$ses_religionBigFive[Data_raw$ses_religion == 12 | 
                                 Data_raw$ses_religion == 10] <- "other"
Data_clean$ses_religionBigFive <- factor(Data_clean$ses_religionBigFive)
table(Data_clean$ses_religionBigFive)

## religiosity -------------------------------------------------------------

table(Data_raw$ses_religiosity_1)
Data_clean$ses_religiosity <- NA
Data_clean$ses_religiosity <- Data_raw$ses_religiosity_1/100
table(Data_clean$ses_religiosity)


## education ---------------------------------------------------------------

attributes(Data_raw$ses_education)
table(Data_raw$ses_education)
Data_clean$ses_educ <- NA
Data_clean$ses_educ[Data_raw$ses_education == 1] <- "no_schooling"
Data_clean$ses_educ[Data_raw$ses_education == 2] <- "elementary_school"
Data_clean$ses_educ[Data_raw$ses_education == 3] <- "high_school"
Data_clean$ses_educ[Data_raw$ses_education == 4] <- "technical_community_cegep"
Data_clean$ses_educ[Data_raw$ses_education == 5] <- "bachelor"
Data_clean$ses_educ[Data_raw$ses_education == 6] <- "masters"
Data_clean$ses_educ[Data_raw$ses_education == 7] <- "doctorate"
Data_clean$ses_educ <- factor(Data_clean$ses_educ, levels = c("no_schooling",
                                                                        "elementary_school",
                                                                        "high_school",
                                                                        "technical_community_cegep",
                                                                        "bachelor",
                                                                        "masters",
                                                                        "doctorate"))
table(Data_clean$ses_educ)

Data_clean$ses_educ_5Cat <- NA
Data_clean$ses_educ_5Cat[Data_raw$ses_education == 1 | Data_raw$ses_education == 2] <- "educBHS"
Data_clean$ses_educ_5Cat[Data_raw$ses_education == 3] <- "educHS"
Data_clean$ses_educ_5Cat[Data_raw$ses_education == 4] <- "educPostHS"
Data_clean$ses_educ_5Cat[Data_raw$ses_education == 5 ]<- "educUnivBac"
Data_clean$ses_educ_5Cat[Data_raw$ses_education == 6 | Data_raw$ses_education == 7] <- "educUnivSup"
Data_clean$ses_educ_5Cat <- factor(Data_clean$ses_educ_5Cat, levels = c("educBHS",
                                                                        "educHS",
                                                                        "educPostHS",
                                                                        "educUnivBac",
                                                                        "educUnivSup"))
table(Data_clean$ses_educ_5Cat)

Data_clean$ses_educ_3Cat <- NA
Data_clean$ses_educ_3Cat[Data_raw$ses_education == 1 | Data_raw$ses_education == 2 | Data_raw$ses_education == 3] <- "educBUniv"
Data_clean$ses_educ_3Cat[Data_raw$ses_education == 4] <- "educPostHS"
Data_clean$ses_educ_3Cat[Data_raw$ses_education == 5 | Data_raw$ses_education == 6 | Data_raw$ses_education == 7]<- "educUniv"
Data_clean$ses_educ_3Cat <- factor(Data_clean$ses_educ_3Cat, levels = c("educBUniv",
                                                                        "educPostHS",
                                                                        "educUniv"))

table(Data_clean$educ_5Cat)


## income ----------------------------------------------------------------

attributes(Data_raw$ses_income)
table(Data_raw$ses_income)
Data_clean$ses_income <- NA
Data_clean$ses_income[Data_raw$ses_income == 1] <- "no_income"
Data_clean$ses_income[Data_raw$ses_income == 2] <- "1_to_30000"
Data_clean$ses_income[Data_raw$ses_income == 3] <- "30001_to_60000"
Data_clean$ses_income[Data_raw$ses_income == 4] <- "60001_to_90000"
Data_clean$ses_income[Data_raw$ses_income == 5] <- "90001_to_110000"
Data_clean$ses_income[Data_raw$ses_income == 6] <- "110001_to_150000"
Data_clean$ses_income[Data_raw$ses_income == 7] <- "150001_to_200000"
Data_clean$ses_income[Data_raw$ses_income == 8] <- "more_than_200000"
Data_clean$ses_income <- factor(Data_clean$ses_income, levels = c("no_income",
                                                                  "1_to_30000",
                                                                  "30001_to_60000",
                                                                  "60001_to_90000",
                                                                  "90001_to_110000",
                                                                  "110001_to_150000",
                                                                  "150001_to_200000",
                                                                  "more_than_200000"))
table(Data_clean$ses_income)

Data_clean$ses_income_3Cat <- NA
Data_clean$ses_income_3Cat[Data_raw$ses_income == 1 | Data_raw$se_income == 2] <- "incomeLow"
Data_clean$ses_income_3Cat[Data_raw$ses_income == 3 | Data_raw$se_income == 4 | Data_raw$se_income == 5 | Data_raw$se_income == 6] <- "incomeMid"
Data_clean$ses_income_3Cat[Data_raw$ses_income == 7 | Data_raw$se_income == 8] <- "incomeHigh"
table(Data_clean$$ses_income3Cat)

attributes(Data_raw$ses_income)
table(Data_raw$ses_income)
Data_clean$ses_incomeCensus <- NA
Data_clean$ses_incomeCensus[Data_raw$ses_income == 1] <- "no_income"
Data_clean$ses_incomeCensus[Data_raw$ses_income == 2] <- "1_to_30000"
Data_clean$ses_incomeCensus[Data_raw$ses_income == 3] <- "30001_to_60000"
Data_clean$ses_incomeCensus[Data_raw$ses_income == 4] <- "60001_to_90000"
Data_clean$ses_incomeCensus[Data_raw$ses_income == 5] <- "90001_to_110000"
Data_clean$ses_incomeCensus[Data_raw$ses_income == 6] <- "110001_to_150000"
Data_clean$ses_incomeCensus[Data_raw$ses_income %in% c(7, 8)] <- "more_than_150000"

Data_clean$ses_incomeCensus <- factor(Data_clean$ses_incomeCensus, levels = c("no_income",
                                                                  "1_to_30000",
                                                                  "30001_to_60000",
                                                                  "60001_to_90000",
                                                                  "90001_to_110000",
                                                                  "110001_to_150000",
                                                                  "more_than_150000"))
table(Data_clean$ses_incomeCensus)

## bilingualism-------------------------------------------------------------

attributes(Data_raw$ses_bilingual_2)
table(Data_raw$ses_bilingual_1)

Data_clean$ses_englishSkills <- NA
Data_clean$ses_englishSkills[Data_raw$ses_bilingual_1 == 4] <- "Full proficiency"
Data_clean$ses_englishSkills[Data_raw$ses_bilingual_1 == 3] <- "Conversational level"
Data_clean$ses_englishSkills[Data_raw$ses_bilingual_1 == 2] <- "Basic level"
Data_clean$ses_englishSkills[Data_raw$ses_bilingual_1 == 1] <- "No proficiency"
Data_clean$ses_englishSkills <- factor(Data_clean$ses_englishSkills, levels = c("Full proficiency",
                                                                  "Conversational level",
                                                                  "Basic level",
                                                                  "No proficiency"))
table(Data_clean$ses_englishSkills)
  
  
  
  
Data_clean$ses_frenchSkills <- NA
Data_clean$ses_frenchSkills[Data_raw$ses_bilingual_2 == 4] <- "Full proficiency"
Data_clean$ses_frenchSkills[Data_raw$ses_bilingual_2 == 3] <- "Conversational level"
Data_clean$ses_frenchSkills[Data_raw$ses_bilingual_2 == 2] <- "Basic level"
Data_clean$ses_frenchSkills[Data_raw$ses_bilingual_2 == 1] <- "No proficiency"
Data_clean$ses_frenchSkills <- factor(Data_clean$ses_frenchSkills, levels = c("Full proficiency",
                                                                          "Conversational level",
                                                                          "Basic level",
                                                                          "No proficiency"))
table(Data_clean$ses_frenchSkills)




## environment -------------------------------------------------------------

attributes(Data_raw$ses_environment) 
table(Data_raw$ses_environment)

Data_clean$ses_rurality <- NA # P-e changer le nom de cette variable pour quelque chose de plus parlant
Data_clean$ses_rurality[Data_raw$ses_environment == 1] <- "urban"
Data_clean$ses_rurality[Data_raw$ses_environment == 2] <- "suburban"
Data_clean$ses_rurality[Data_raw$ses_environment == 3] <- "rural"
Data_clean$ses_rurality <- factor(Data_clean$ses_rurality, levels = c("urban",
                                                                            "suburban",
                                                                            "rural"))
table(Data_clean$ses_rurality)



## status ------------------------------------------------------------------

attributes(Data_raw$ses_status)
table(Data_raw$ses_status)

Data_clean$ses_matStatus <- NA
Data_clean$ses_matStatus[Data_raw$ses_status == 1] <- "single"
Data_clean$ses_matStatus[Data_raw$ses_status == 2] <- "married"
Data_clean$ses_matStatus[Data_raw$ses_status == 3] <- "common_law_relationship"
Data_clean$ses_matStatus[Data_raw$ses_status == 4] <- "widower_widow"
Data_clean$ses_matStatus[Data_raw$ses_status == 5] <- "divorced_separated"
Data_clean$ses_matStatus <- factor(Data_clean$ses_matStatus, levels = c("single",
                                                                  "married",
                                                                  "common_law_relationship",
                                                                  "widower_widow",
                                                                  "divorced_separated"))
table(Data_clean$ses_matStatus)


## owner -------------------------------------------------------------------

attributes(Data_raw$ses_owner)
table(Data_raw$ses_owner)

Data_clean$ses_owner <- NA
Data_clean$ses_owner[Data_raw$ses_owner == 1] <- "owner"
Data_clean$ses_owner[Data_raw$ses_owner == 2] <- "tenant"
Data_clean$ses_owner[Data_raw$ses_owner == 3] <- "neither"
Data_clean$ses_owner <- factor(Data_clean$ses_owner, levels = c("owner",
                                                                  "tenant",
                                                                  "neither"))
table(Data_clean$ses_owner)


## kids --------------------------------------------------------------------

attributes(Data_raw$ses_kids)
table(Data_raw$ses_kids)

Data_clean$ses_householdComp <- NA
Data_clean$ses_householdComp[Data_raw$ses_kids == 1] <- "partner_no_children"
Data_clean$ses_householdComp[Data_raw$ses_kids == 2] <- "partner_with_children"
Data_clean$ses_householdComp[Data_raw$ses_kids == 3] <- "no_partner_no_children"
Data_clean$ses_householdComp[Data_raw$ses_kids == 4] <- "no_partner_with_children"
Data_clean$ses_householdComp[Data_raw$ses_kids == 5] <- "roommates"
Data_clean$ses_householdComp[Data_raw$ses_kids == 6] <- "other"
Data_clean$ses_householdComp <- factor(Data_clean$ses_householdComp, levels = c("partner_no_children",
                                                                                "partner_with_children",
                                                                                "no_partner_no_children",
                                                                                "no_partner_with_children",
                                                                                "roommates",
                                                                                "other"))
table(Data_clean$ses_householdComp)


## ses_occupation --------------------------------------------------------------

table(Data_raw$ses_occupation)
attributes(Data_raw$ses_occupation)
Data_clean$ses_occupation <- NA
Data_clean$ses_occupation[Data_raw$ses_occupation == 1] <- "paid_employment"
Data_clean$ses_occupation[Data_raw$ses_occupation == 2] <- "self_employment"
Data_clean$ses_occupation[Data_raw$ses_occupation == 3] <- "student"
Data_clean$ses_occupation[Data_raw$ses_occupation == 4] <- "retired"
Data_clean$ses_occupation[Data_raw$ses_occupation == 5] <- "looking_for_work"
Data_clean$ses_occupation[Data_raw$ses_occupation == 6] <- "unemployed"
Data_clean$ses_occupation[Data_raw$ses_occupation == 7] <- "other"
Data_clean$ses_occupation <- factor(Data_clean$ses_occupation, levels = c("paid_employment",
                                                                          "self_employment",
                                                                          "student",
                                                                          "retired",
                                                                          "looking_for_work",
                                                                          "unemployed",
                                                                          "other"))
# Initialisation de la variable
Data_clean$ses_occupation_5Cat<- NA

# Attribution des valeurs corrigées
Data_clean$ses_occupation_5Cat[Data_raw$ses_occupation == 1 | Data_raw$ses_occupation == 2] <- "employed"
Data_clean$ses_occupation_5Cat[Data_raw$ses_occupation == 5 | Data_raw$ses_occupation == 6] <- "unemployed"
Data_clean$ses_occupation_5Cat[Data_raw$ses_occupation == 4] <- "retired"
Data_clean$ses_occupation_5Cat[Data_raw$ses_occupation == 3] <- "student"
Data_clean$ses_occupation_5Cat[Data_raw$ses_occupation == 7] <- "other"

# Conversion en facteur avec les niveaux ordonnés
Data_clean$ses_occupation_5Cat <- factor(Data_clean$ses_occupation_grouped, 
                                            levels = c("employed", 
                                                       "unemployed", 
                                                       "retired", 
                                                       "student", 
                                                       "other"))

# Vérification des résultats
table(Data_clean$ses_occupation_5Cat)

## SES (enfant) -------------------------------------------------------------------

attributes(Data_raw$ses_children)
table(Data_raw$ses_children)
Data_raw$ses_children <- as.numeric(Data_raw$ses_children)
Data_clean$ses_children <- NA
Data_clean$ses_children[Data_raw$ses_children == 0] <- 0
Data_clean$ses_children[Data_raw$ses_children == 1] <- 1
Data_clean$ses_children[Data_raw$ses_children == 2] <- 2
Data_clean$ses_children[Data_raw$ses_children == 3] <- 3
Data_clean$ses_children[Data_raw$ses_children == 4] <- 4
Data_clean$ses_children[Data_raw$ses_children >= 5] <- 5
Data_clean$ses_children <- factor(Data_clean$ses_children, levels = c(0,
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      4,
                                                                      5),
                                  ordered = TRUE)
table(Data_clean$ses_children)



## ethnicity -------------------------------------------------------------
attributes(Data_raw$ses_ethnicity)
table(Data_raw$ses_ethnicity)
Data_clean$ses_ethnicity <- NA
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 1] <- "white"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 2] <- "black"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 3] <- "indigenous"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 4] <- "asian"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 5] <- "hispanic"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 6] <- "arab"
Data_clean$ses_ethnicity[Data_raw$ses_ethnicity == 7] <- "other"
Data_clean$ses_ethnicity <- factor(Data_clean$ses_ethnicity, levels = c("white",
                                                                        "black",
                                                                        "indigenous",
                                                                        "asian",
                                                                        "hispanic",
                                                                        "arab",
                                                                        "other"))
table(Data_clean$ses_ethnicity)

Data_clean$ses_ethnicityWhite <- NA
Data_clean$ses_ethnicityWhite[Data_raw$ses_ethnicity == 1] <- 1
Data_clean$ses_ethnicityWhite[Data_raw$ses_ethnicity != 1] <- 0
table(Data_clean$ses_ethnicityWhite)

Data_clean$ses_ethnicityWB <- NA
Data_clean$ses_ethnicityWB[Data_raw$ses_ethnicity == 1] <- "white"
Data_clean$ses_ethnicityWB[Data_raw$ses_ethnicity == 2] <- "black"
Data_clean$ses_ethnicityWB[!(Data_raw$ses_ethnicity %in% c(1, 2))] <- "other"
Data_clean$ses_ethnicityWB <- factor(Data_clean$ses_ethnicityWB)
Data_clean$ses_ethnicityWB <- relevel(Data_clean$ses_ethnicityWB, ref = "white")
table(Data_clean$ses_ethnicityWB)


labels <- stringr::str_trim(names(attributes(Data_raw$ses_ethnicity)$labels))
json_data <- list(
  `1` = labels[1],
  `0` = labels[-1]
)
jsonlite::toJSON(json_data, pretty = TRUE)



## orientation -----------------------------------------------------------
attributes(Data_raw$ses_orientation)
table(Data_raw$ses_orientation)
Data_clean$ses_sexOrientation <- NA
Data_clean$ses_sexOrientation[Data_raw$ses_orientation == 1] <- "heterosexual"
Data_clean$ses_sexOrientation[Data_raw$ses_orientation == 2] <- "gay"
Data_clean$ses_sexOrientation[Data_raw$ses_orientation == 3] <- "bisexual"
Data_clean$ses_sexOrientation[Data_raw$ses_orientation == 4] <- "other"
Data_clean$ses_sexOrientation <- factor(Data_clean$ses_sexOrientation, levels = c("heterosexual",
                                                                                        "gay",
                                                                                        "bisexual",
                                                                                        "other"))
table(Data_clean$ses_sexOrientation)

## heterosexual

Data_clean$ses_sexOrientationHetero <- NA
Data_clean$ses_sexOrientationHetero[Data_raw$ses_orientation == 1] <- 1
Data_clean$ses_sexOrientationHetero[Data_raw$ses_orientation != 1] <- 0
table(Data_clean$ses_sexOrientationHetero)



## parent ----------------------------------------------------------------

attributes(Data_raw$ses_parent)
table(Data_raw$ses_parent)
Data_clean$ses_parentImmigrant <- NA
Data_clean$ses_parentImmigrant[Data_raw$ses_parent == 1] <- 1
Data_clean$ses_parentImmigrant[Data_raw$ses_parent != 1] <- 0
table(Data_clean$ses_parentImmigrant)


## immigrant -------------------------------------------------------------

attributes(Data_raw$ses_immigrant)
table(Data_raw$ses_immigrant)
Data_clean$ses_immigrant <- NA
Data_clean$ses_immigrant[Data_raw$ses_immigrant == 1] <- 1
Data_clean$ses_immigrant[Data_raw$ses_immigrant != 1] <- 0
table(Data_clean$ses_immigrant)

## dwelling --------------------------------------------------------------
attributes(Data_raw$ses_dwelling)
table(Data_raw$ses_dwelling)
Data_clean$ses_dwelling <- NA
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 1] <- "apartment_complex"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 2] <- "loft"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 3] <- "condominium"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 4] <- "high_rise_apartment"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 5] <- "stand_alone_house"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 6] <- "townhouse"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 7] <- "duplex"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 8] <- "cooperative_housing"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 9] <- "social_or_public_housing"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 10] <- "mobile_home"
Data_clean$ses_dwelling[Data_raw$ses_dwelling == 11] <- "other"
Data_clean$ses_dwelling <- factor(Data_clean$ses_dwelling, levels = c("apartment_complex",
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
table(Data_clean$ses_dwelling)

attributes(Data_raw$ses_dwelling)
table(Data_raw$ses_dwelling)

# Initialisation
Data_clean$ses_dwelling_grouped <- NA

# Regroupement des codes existants
# 1,2,3,8,9,11 -> "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 1] <- "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 2] <- "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 3] <- "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 8] <- "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 9] <- "apartment_complex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 11] <- "other"

# 4 -> "high_rise_apartment"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 4] <- "high_rise_apartment"

# 5 -> "stand_alone_house"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 5] <- "stand_alone_house"

# 6 -> "townhouse"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 6] <- "townhouse"

# 7 -> "duplex"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 7] <- "duplex"

# 10 -> "mobile_home"
Data_clean$ses_dwelling_cat[Data_raw$ses_dwelling == 10] <- "mobile_home"

# On définit l'ordre final des facteurs
Data_clean$ses_dwelling_gcat <- factor(
  Data_clean$ses_dwelling_cat,
  levels = c("stand_alone_house",
             "townhouse",
             "duplex",
             "apartment_complex",
             "high_rise_apartment",
             "mobile_home",
            "other")
)
table(Data_clean$ses_dwelling_cat)

