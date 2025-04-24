# SES
## Postal code

table(DataRaw$postal_code)

DataClean$ses_postalCode <- DataRaw$postal_code

table(DataClean$ses_postalCode)
## Sex ----------------------------------------------------------------------

attributes(df$ses_sex)
table(df$ses_sex)


#data clean

DataClean$ses_gender <- NA
DataClean$ses_gender[DataRaw$ses_sex == 2] <- "male"
DataClean$ses_gender[DataRaw$ses_sex == 1] <- "female"
DataClean$ses_gender[DataRaw$ses_sex == 3] <- "non_binary"
DataClean$ses_gender[DataRaw$ses_sex == 4] <- "queer"
DataClean$ses_gender[DataRaw$ses_sex == 5] <- "agender"
DataClean$ses_gender <- factor(DataClean$ses_gender)
table(DataClean$ses_gender)

# gender_male ----------------------------------------------------------------

DataClean$ses_genderMale <- NA
DataClean$ses_genderMale[DataRaw$ses_sex == 2] <- 1
DataClean$ses_genderMale[DataRaw$ses_sex != 2] <- 0
table(DataClean$ses_genderMale)

# gender_female ----------------------------------------------------------------

DataClean$ses_genderFemale <- NA
DataClean$ses_genderFemale[DataRaw$ses_sex == 1] <- 1
DataClean$ses_genderFemale[DataRaw$ses_sex != 1] <- 0
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
DataClean$ses_ageGroup5Years[DataRaw$ses_age >= 40 & DataRaw$ses_age < 45] <- "40_44"
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

# region ----------------------------------------------------------------------
attributes(DataRaw$ses_province)
table(DataRaw$ses_province)

DataClean$ses_province <- NA
DataClean$ses_province[DataRaw$ses_province == 1] <- "Alberta"
DataClean$ses_province[DataRaw$ses_province == 2] <- "British Columbia"
DataClean$ses_province[DataRaw$ses_province == 3] <- "Manitoba"
DataClean$ses_province[DataRaw$ses_province == 4] <- "New Brunswick"
DataClean$ses_province[DataRaw$ses_province == 5] <- "Newfoundland and Labrador"
DataClean$ses_province[DataRaw$ses_province == 6] <- "Nova Scotia"
DataClean$ses_province[DataRaw$ses_province == 7] <- "Northwest Territories"
DataClean$ses_province[DataRaw$ses_province == 8] <- "Nunavut"
DataClean$ses_province[DataRaw$ses_province == 9] <- "Ontario"
DataClean$ses_province[DataRaw$ses_province == 10] <- "Prince Edward Island"
DataClean$ses_province[DataRaw$ses_province == 11] <- "Quebec"
DataClean$ses_province[DataRaw$ses_province == 12] <- "Saskatchewan"
DataClean$ses_province[DataRaw$ses_province == 13] <- "Yukon"
table(DataClean$ses_province)

# Définir la correspondance entre provinces et régions
DataClean$ses_region <- NA  # Créer la nouvelle variable

# Attribuer les régions correspondantes
DataClean$ses_region[DataRaw$ses_province %in% c(1, 12, 3)] <- "prairie"
DataClean$ses_region[DataRaw$ses_province == 2] <- "british_columbia"
DataClean$ses_region[DataRaw$ses_province %in% c(4, 5, 6, 10)] <- "atlantic"
DataClean$ses_region[DataRaw$ses_province == 9] <- "ontario"
DataClean$ses_region[DataRaw$ses_province == 11] <- "quebec"
DataClean$ses_region[DataRaw$ses_province %in% c(7, 8, 13)] <- "territories"

# Convertir en facteur avec les niveaux spécifiés
DataClean$ses_region <- factor(DataClean$ses_region, 
                               levels = c("prairie", 
                                          "british_columbia",
                                          "atlantic",
                                          "ontario", 
                                          "quebec",
                                          "territories"))

# Vérifier le résultat
table(DataClean$ses_region)


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




## education ---------------------------------------------------------------

attributes(DataRaw$ses_educ)
table(DataRaw$ses_educ)
# Créer la nouvelle variable
DataClean$ses_education <- NA

# Attribuer les catégories correspondantes
DataClean$ses_education[DataRaw$ses_educ == 1] <- "no_schooling"
DataClean$ses_education[DataRaw$ses_educ == 4] <- "elementary_school"
DataClean$ses_education[DataRaw$ses_educ == 5] <- "high_school"
DataClean$ses_education[DataRaw$ses_educ == 6] <- "technical_community_cegep"
DataClean$ses_education[DataRaw$ses_educ == 7] <- "bachelor"
DataClean$ses_education[DataRaw$ses_educ == 8] <- "masters"
DataClean$ses_education[DataRaw$ses_educ == 9] <- "doctorate"

# Convertir en facteur avec les niveaux spécifiés
DataClean$ses_education <- factor(DataClean$ses_education, 
                             levels = c("no_schooling",
                                        "elementary_school",
                                        "high_school",
                                        "technical_community_cegep",
                                        "bachelor",
                                        "masters",
                                        "doctorate"))

# Vérifier le résultat
table(DataClean$ses_education)

## income ----------------------------------------------------------------

attributes(DataRaw$ses_income)
table(DataRaw$ses_income)
DataClean$ses_income <- NA

# Attribuer les catégories correspondantes
DataClean$ses_income[DataRaw$ses_income == 1] <- "no_income"
DataClean$ses_income[DataRaw$ses_income == 4] <- "1_to_30000"
DataClean$ses_income[DataRaw$ses_income == 5] <- "30001_to_60000"
DataClean$ses_income[DataRaw$ses_income == 6] <- "60001_to_90000"
DataClean$ses_income[DataRaw$ses_income == 7] <- "90001_to_110000"
DataClean$ses_income[DataRaw$ses_income == 8] <- "110001_to_150000"
DataClean$ses_income[DataRaw$ses_income == 9] <- "150001_to_200000"
DataClean$ses_income[DataRaw$ses_income == 10] <- "more_than_200000"

# Convertir en facteur avec les niveaux spécifiés
DataClean$ses_income <- factor(DataClean$ses_income, 
                               levels = c("no_income",
                                          "1_to_30000",
                                          "30001_to_60000",
                                          "60001_to_90000",
                                          "90001_to_110000",
                                          "110001_to_150000",
                                          "150001_to_200000",
                                          "more_than_200000"))

# Vérifier le résultat
table(DataClean$ses_income)

## Dwelling - type ------------------------------------------------------------

attributes(DataRaw$ses_dwelling)
table(DataRaw$ses_dwelling)

# Créer la nouvelle variable

DataClean$ses_dwelling <- NA
# Attribuer les catégories correspondantes
DataClean$ses_dwelling[DataRaw$ses_dwelling == 1] <- "high_rise_apartment"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 13] <- "apartment_fewer_than_5_storeys"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 14] <- "condo"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 15] <- "co-op"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 16] <- "public_housing"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 17] <- "duplex"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 18] <- "loft"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 19] <- "townhome"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 20] <- "detached_house"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 21] <- "mobile_house"
DataClean$ses_dwelling[DataRaw$ses_dwelling == 22] <- "other"
table(DataClean$ses_dwelling)


