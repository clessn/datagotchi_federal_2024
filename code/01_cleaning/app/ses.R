# SES
## Postal code

table(DataRaw$postal_code)

DataClean$ses_postalCode <- DataRaw$postal_code

table(DataClean$ses_postalCode)
## gender ----------------------------------------------------------------------


attributes(DataRaw$gender)
table(DataRaw$gender)


#data clean

DataClean$ses_gender <- NA
DataClean$ses_gender[DataRaw$gender == "Man"] <- "male"
DataClean$ses_gender[DataRaw$gender == "Woman"] <- "female"
DataClean$ses_gender[DataRaw$gender == "Trans man"] <- "trans_man"
DataClean$ses_gender[DataRaw$gender == "Trans woman"] <- "trans_woman"
DataClean$ses_gender[DataRaw$gender == "Non-binary"] <- "non_binary"
DataClean$ses_gender[DataRaw$gender == "Queer"] <- "queer"
DataClean$ses_gender[DataRaw$gender == "Agender"] <- "agender"
DataClean$ses_gender <- factor(DataClean$ses_gender)
table(DataClean$ses_gender)

# gender_male ----------------------------------------------------------------

DataClean$ses_genderMale <- NA
DataClean$ses_genderMale[DataRaw$gender == "Man"] <- 1
DataClean$ses_genderMale[DataRaw$gender != "Man"] <- 0
table(DataClean$ses_genderMale)

# gender_female ----------------------------------------------------------------

DataClean$ses_genderFemale <- NA
DataClean$ses_genderFemale[DataRaw$gender == "Woman"] <- 1
DataClean$ses_genderFemale[DataRaw$gender != "Woman"] <- 0
table(DataClean$ses_genderFemale)

## age--------------------------------------------------------------------------

attributes(DataRaw$age)
table(DataRaw$age)
DataClean$ses_age <- NA
DataClean$ses_age <- DataRaw$age
table(DataClean$ses_age)

DataClean$ses_ageGroup5Years <- NA
DataClean$ses_ageGroup5Years[DataRaw$age == 18 | DataRaw$age == 19] <- "18_19"
DataClean$ses_ageGroup5Years[DataRaw$age >= 20 & DataRaw$age < 25] <- "20_24"
DataClean$ses_ageGroup5Years[DataRaw$age >= 25 & DataRaw$age < 30] <- "25_29"
DataClean$ses_ageGroup5Years[DataRaw$age >= 30 & DataRaw$age < 35] <- "30_34"
DataClean$ses_ageGroup5Years[DataRaw$age >= 35 & DataRaw$age < 40] <- "35_39"
DataClean$ses_ageGroup5Years[DataRaw$age >= 40 & DataRaw$age < 45] <- "40_44"
DataClean$ses_ageGroup5Years[DataRaw$age >= 45 & DataRaw$age < 50] <- "45_49"
DataClean$ses_ageGroup5Years[DataRaw$age >= 50 & DataRaw$age < 55] <- "50_54"
DataClean$ses_ageGroup5Years[DataRaw$age >= 55 & DataRaw$age < 60] <- "55_59"
DataClean$ses_ageGroup5Years[DataRaw$age >= 60 & DataRaw$age < 65] <- "60_64"
DataClean$ses_ageGroup5Years[DataRaw$age >= 65 & DataRaw$age < 70] <- "65_69"
DataClean$ses_ageGroup5Years[DataRaw$age >= 70 & DataRaw$age < 75] <- "70_74"
DataClean$ses_ageGroup5Years[DataRaw$age >= 75 & DataRaw$age < 80] <- "75_79"
DataClean$ses_ageGroup5Years[DataRaw$age >= 80 & DataRaw$age < 85] <- "80_84"
DataClean$ses_ageGroup5Years[DataRaw$age >= 85 & DataRaw$age < 90] <- "85_89"
DataClean$ses_ageGroup5Years[DataRaw$age >= 90 & DataRaw$age < 95] <- "90_94"
DataClean$ses_ageGroup5Years[DataRaw$age >= 95 & DataRaw$age < 100] <- "95_99"
DataClean$ses_ageGroup5Years[DataRaw$age >= 100] <- "100+"
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
DataClean$ses_age_4Cat[DataRaw$age < 25 & DataRaw$age > 18] <- "18_24"
DataClean$ses_age_4Cat[DataRaw$age >= 25 & DataRaw$age < 45] <- "25_44"
DataClean$ses_age_4Cat[DataRaw$age >= 45 & DataRaw$age < 65] <- "45_64"
DataClean$ses_age_4Cat[DataRaw$age >= 65] <- "65+"
DataClean$ses_age_4Cat <- factor(DataClean$ses_age_4Cat, levels = c("18_24",
                                                                        "25_44",
                                                                        "45_64",
                                                                        "65+"))
table(DataClean$ses_age_4Cat)



## region ----------------------------------------------------------------------

# Fonction pour convertir les codes postaux en provinces avec distinction NT/NU
postal_to_province <- function(postal_codes) {
  # S'assurer que les codes sont en majuscules et nettoyer les espaces
  postal_codes <- toupper(trimws(postal_codes))
  
  # Extraire le premier caractère (lettre)
  first_char <- substr(postal_codes, 1, 1)
  
  # Extraire les 3 premiers caractères pour distinguer NT et NU
  first_three <- substr(postal_codes, 1, 3)
  
  # Table de correspondance première lettre -> province
  provinces <- character(length(first_char))
  
  # Assigner les provinces selon la première lettre
  provinces[first_char == "A"] <- "NL"  # Newfoundland and Labrador
  provinces[first_char == "B"] <- "NS"  # Nova Scotia
  provinces[first_char == "C"] <- "PE"  # Prince Edward Island
  provinces[first_char == "E"] <- "NB"  # New Brunswick
  provinces[first_char %in% c("G", "H", "J")] <- "QC"  # Quebec
  provinces[first_char %in% c("K", "L", "M", "N", "P")] <- "ON"  # Ontario
  provinces[first_char == "R"] <- "MB"  # Manitoba
  provinces[first_char == "S"] <- "SK"  # Saskatchewan
  provinces[first_char == "T"] <- "AB"  # Alberta
  provinces[first_char == "V"] <- "BC"  # British Columbia
  provinces[first_char == "Y"] <- "YT"  # Yukon
  
  # Distinction NT/NU basée sur les 3 premiers caractères
  # NT: X0E, X0G, X1A (Yellowknife)
  provinces[first_three %in% c("X0E", "X0G", "X1A")] <- "NT"
  
  # NU: X0A, X0B, X0C
  provinces[first_three %in% c("X0A", "X0B", "X0C")] <- "NU"
  
  # Si X mais pas dans les codes connus ci-dessus, on laisse NA
  provinces[first_char == "X" & !(first_three %in% c("X0A", "X0B", "X0C", "X0E", "X0G", "X1A"))] <- NA
  
  return(provinces)
}


table(DataRaw$postal_code)
# 1. Appliquer la fonction aux données brutes pour obtenir les provinces
DataClean$ses_province <- postal_to_province(DataRaw$postal_code)

# 2. Convertir en facteur avec les niveaux spécifiés
DataClean$ses_province <- factor(DataClean$ses_province, 
                               levels = c("AB", "BC", "MB", "NB", "NL", 
                                         "NT", "NS", "NU", "ON", "PE", 
                                         "QC", "SK", "YT"))

# 3. Vérifier la distribution
table(DataClean$ses_province, useNA = "ifany")
table(DataClean$ses_province)

# region ----------------------------------------------------------------------

# Définir la correspondance entre provinces et régions
DataClean$ses_region <- NA  # Créer la nouvelle variable

# Attribuer les régions correspondantes
DataClean$ses_region[DataClean$ses_province %in% c("AB", "SK", "MB")] <- "prairie"
DataClean$ses_region[DataClean$ses_province == "BC"] <- "british_columbia"
DataClean$ses_region[DataClean$ses_province %in% c("NB", "NL", "NS", "PE")] <- "atlantic"
DataClean$ses_region[DataClean$ses_province == "ON"] <- "ontario"
DataClean$ses_region[DataClean$ses_province == "QC"] <- "quebec"
DataClean$ses_region[DataClean$ses_province %in% c("NT", "NU", "YT")] <- "territories"

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

# region qc -------------------------------------------------------------------

table(DataClean$ses_region)

DataClean$ses_regionQc <- NA
DataClean$ses_regionQc[DataClean$ses_region == "quebec"] <- 1
DataClean$ses_regionQc[DataClean$ses_region != "quebec"] <- 0
table(DataClean$ses_regionQc)


## postal_code -----------------------------------------------------------------

attributes(DataRaw$postal_code)
table(DataRaw$postal_code)
DataClean$ses_postalCode <- NA
DataClean$ses_postalCode <- DataRaw$postal_code
table(DataClean$ses_postalCode)

## language --------------------------------------------------------------------

attributes(DataRaw$language)
table(DataRaw$language)
DataClean$ses_language <- NA
DataClean$ses_language[DataRaw$language == "English"] <- "english"
DataClean$ses_language[DataRaw$language == "French"] <- "french"
DataClean$ses_language[DataRaw$language == "Other"] <- "other"
DataClean$ses_language <- factor(DataClean$ses_language, levels = c("english",
                                                                      "french",
                                                                      "other"))
table(DataClean$ses_language)


## religion --------------------------------------------------------------------




## education ---------------------------------------------------------------

attributes(DataRaw$education)
table(DataRaw$education)
# Créer la nouvelle variable
DataClean$ses_educ <- NA

# Attribuer les catégories correspondantes
DataClean$ses_educ[DataRaw$education == "No schooling"] <- "no_schooling"
DataClean$ses_educ[DataRaw$education == "Elementary school"] <- "elementary_school"
DataClean$ses_educ[DataRaw$education == "High school"] <- "high_school"
DataClean$ses_educ[DataRaw$education == "Technical, community college, CEGEP or Classical college"] <- "technical_community_cegep"
DataClean$ses_educ[DataRaw$education == "Bachelor’s degree"] <- "bachelor"
DataClean$ses_educ[DataRaw$education == "Master’s degree"] <- "masters"
DataClean$ses_educ[DataRaw$education == "Doctorate"] <- "doctorate"

# Convertir en facteur avec les niveaux spécifiés
DataClean$ses_educ <- factor(DataClean$ses_educ, 
                             levels = c("no_schooling",
                                        "elementary_school",
                                        "high_school",
                                        "technical_community_cegep",
                                        "bachelor",
                                        "masters",
                                        "doctorate"))

# Vérifier le résultat
table(DataClean$ses_educ)


DataClean$ses_educ_5Cat <- NA
DataClean$ses_educ_5Cat[DataClean$ses_educ == "no_schooling" | DataClean$ses_educ == "elementary_school"] <- "educBHS"
DataClean$ses_educ_5Cat[DataClean$ses_educ == "high_school"] <- "educHS"
DataClean$ses_educ_5Cat[DataClean$ses_educ == "technical_community_cegep"] <- "educPostHS"
DataClean$ses_educ_5Cat[DataClean$ses_educ == "bachelor" ]<- "educUnivBac"
DataClean$ses_educ_5Cat[DataClean$ses_educ == "masters" | DataClean$ses_educ == "doctorate"] <- "educUnivSup"
DataClean$ses_educ_5Cat <- factor(DataClean$ses_educ_5Cat, levels = c("educBHS",
                                                                        "educHS",
                                                                        "educPostHS",
                                                                        "educUnivBac",
                                                                        "educUnivSup"))
table(DataClean$ses_educ_5Cat)

DataClean$ses_educ_3Cat <- NA
DataClean$ses_educ_3Cat[DataClean$ses_educ == "no_schooling" | DataClean$ses_educ == "elementary_school" | DataClean$ses_educ == "high_school"] <- "educBHS"
DataClean$ses_educ_3Cat[DataClean$ses_educ == "technical_community_cegep"] <- "educPostHS"
DataClean$ses_educ_3Cat[DataClean$ses_educ == "masters" | DataClean$ses_educ == "doctorate" | DataClean$ses_educ == "bachelor"]<- "educUniv"
DataClean$ses_educ_3Cat <- factor(DataClean$ses_educ_3Cat, levels = c("educBHS",
                                                                        "educPostHS",
                                                                        "educUniv"))

table(DataClean$ses_educ_3Cat)


## income ----------------------------------------------------------------

attributes(DataRaw$ses_income)
table(DataRaw$income)
DataClean$ses_income <- NA

# Attribuer les catégories correspondantes
DataClean$ses_income[DataRaw$income == "No income"] <- "no_income"
DataClean$ses_income[DataRaw$income == "$ 1 to $ 30 000"] <- "1_to_30000"
DataClean$ses_income[DataRaw$income == "$ 30 001 to $ 60 000"] <- "30001_to_60000"
DataClean$ses_income[DataRaw$income == "$ 60 001 to $ 90 000"] <- "60001_to_90000"
DataClean$ses_income[DataRaw$income == "$ 90 001 to $ 110 000"] <- "90001_to_110000"
DataClean$ses_income[DataRaw$income == "$ 110 001 to $ 150 000"] <- "110001_to_150000"
DataClean$ses_income[DataRaw$income == "$ 150 001 to $ 200 000"] <- "150001_to_200000"
DataClean$ses_income[DataRaw$income == "More than $ 200 000"] <- "more_than_200000"

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

DataClean$ses_income3Cat <- NA

# Utiliser les catégories de revenu au lieu des valeurs numériques
DataClean$ses_income3Cat[DataClean$ses_income %in% c("no_income", "1_to_30000")] <- "Low"
DataClean$ses_income3Cat[DataClean$ses_income %in% c("30001_to_60000", "60001_to_90000", "90001_to_110000", "110001_to_150000")] <- "Mid"
DataClean$ses_income3Cat[DataClean$ses_income %in% c("150001_to_200000", "more_than_200000")] <- "High"

# Convertir en facteur
DataClean$ses_income3Cat <- factor(DataClean$ses_income3Cat)

# Vérifier le résultat
table(DataClean$ses_income3Cat)

attributes(DataRaw$ses_income)
DataClean$ses_incomeCensus <- NA

# Utiliser les catégories de revenu de la variable income originale
DataClean$ses_incomeCensus[DataRaw$income == "No income"] <- "no_income"
DataClean$ses_incomeCensus[DataRaw$income == "$ 1 to $ 30 000"] <- "1_to_30000"
DataClean$ses_incomeCensus[DataRaw$income == "$ 30 001 to $ 60 000"] <- "30001_to_60000"
DataClean$ses_incomeCensus[DataRaw$income == "$ 60 001 to $ 90 000"] <- "60001_to_90000"
DataClean$ses_incomeCensus[DataRaw$income == "$ 90 001 to $ 110 000"] <- "90001_to_110000"
DataClean$ses_incomeCensus[DataRaw$income == "$ 110 001 to $ 150 000"] <- "110001_to_150000"
DataClean$ses_incomeCensus[DataRaw$income %in% c("$ 150 001 to $ 200 000", "More than $ 200 000")] <- "more_than_150000"

# Convertir en facteur avec les niveaux spécifiés
DataClean$ses_incomeCensus <- factor(DataClean$ses_incomeCensus, 
                                     levels = c("no_income",
                                                "1_to_30000",
                                                "30001_to_60000",
                                                "60001_to_90000",
                                                "90001_to_110000",
                                                "110001_to_150000",
                                                "more_than_150000"))

# Vérifier le résultat
table(DataClean$ses_incomeCensus)

## bilingualism-------------------------------------------------------------


## environment -------------------------------------------------------------


## status ------------------------------------------------------------------


## owner -------------------------------------------------------------------


## kids --------------------------------------------------------------------




## ses_occupation --------------------------------------------------------------



## SES (enfant) -------------------------------------------------------------------



## ethnicity -------------------------------------------------------------

table(DataRaw$ethnicity)
correspondance <- c(
  "Aboriginal" = "indigenous",
  "Arab" = "arab",
  "Asian" = "asian",
  "Black" = "black",
  "Hispanic" = "hispanic",
  "Other" = "other",
  "White" = "white"
)

# Application de la transformation
DataClean$ses_ethnicity <- correspondance[DataRaw$ethnicity]

# Conversion en facteur avec les niveaux spécifiés
DataClean$ses_ethnicity <- factor(DataClean$ses_ethnicity, 
                                   levels = c("white", "black", "indigenous", 
                                              "asian", "hispanic", "arab", "other"))

# Vérification du résultat
table(DataClean$ses_ethnicity, useNA = "ifany")

DataClean$ses_ethnicityWhite <- NA
DataClean$ses_ethnicityWhite[DataRaw$ethnicity == "White"] <- 1
DataClean$ses_ethnicityWhite[DataRaw$ethnicity != "White"] <- 0
table(DataClean$ses_ethnicityWhite)

DataClean$ses_ethnicityWB <- NA
DataClean$ses_ethnicityWB[DataRaw$ethnicity == "White"] <- "white"
DataClean$ses_ethnicityWB[DataRaw$ethnicity == "Black"] <- "black"
DataClean$ses_ethnicityWB[(DataRaw$ethnicity %in% c("Aboriginal", 
"Asian", "Hispanic", "Arab", "Other"))] <- "other"
DataClean$ses_ethnicityWB <- factor(DataClean$ses_ethnicityWB)
DataClean$ses_ethnicityWB <- relevel(DataClean$ses_ethnicityWB, ref = "white")
table(DataClean$ses_ethnicityWB, useNA = "ifany")


labels <- stringr::str_trim(names(attributes(DataRaw$ethnicity)$labels))
json_data <- list(
  `1` = labels[1],
  `0` = labels[-1]
)

jsonlite::toJSON(json_data, pretty = TRUE)



## orientation -----------------------------------------------------------

table(DataRaw$sexual_orientation)
DataClean$ses_sexOrientation <- NA
DataClean$ses_sexOrientation[DataRaw$sexual_orientation == "Heterosexual"] <- "heterosexual"
DataClean$ses_sexOrientation[DataRaw$sexual_orientation == "Homosexual"] <- "gay"
DataClean$ses_sexOrientation[DataRaw$sexual_orientation == "Bisexual"] <- "bisexual"
DataClean$ses_sexOrientation[DataRaw$sexual_orientation == "Other"] <- "other"

DataClean$ses_sexOrientation <- factor(DataClean$ses_sexOrientation, levels = c("heterosexual",
                                                                                        "gay",
                                                                                        "bisexual",
                                                                                        "other"))
table(DataClean$ses_sexOrientation)

## heterosexual

DataClean$ses_sexOrientationHetero <- NA
DataClean$ses_sexOrientationHetero[DataRaw$sexual_orientation == "Heterosexual"] <- 1
DataClean$ses_sexOrientationHetero[DataRaw$sexual_orientation != "Heterosexual"] <- 0
table(DataClean$ses_sexOrientationHetero)



## parent ----------------------------------------------------------------


## immigrant -------------------------------------------------------------


table(DataRaw$birthplace)
# Créer la variable ses_immigrant (initialement NA)
DataClean$ses_immigrant <- NA

# Convertir les codes de pays en format standardisé (tous en majuscules)
# On extrait d'abord le code du pays du format JSON
birthplace_codes <- gsub('.*"code":"([^"]+)".*', "\\1", DataRaw$birthplace)
birthplace_codes <- toupper(birthplace_codes)

# Identifier les codes canadiens (CA, CA-QC)
canadian_codes <- c("CA", "CA-QC")

# Assigner 0 aux personnes nées au Canada (non-immigrants)
DataClean$ses_immigrant[birthplace_codes %in% canadian_codes] <- 0

# Assigner 1 aux personnes nées à l'étranger (immigrants)
DataClean$ses_immigrant[!birthplace_codes %in% canadian_codes] <- 1


# Vérifier le résultat
table(DataClean$ses_immigrant, useNA = "ifany")

## dwelling --------------------------------------------------------------

table(DataRaw$dwelling, useNA = "ifany")

# Création de la correspondance entre les catégories originales et les nouvelles catégories
DataClean$ses_dwelling <- NA  # Initialisation

# Transformation avec R de base
DataClean$ses_dwelling[DataRaw$dwelling == "Apartment in a building that has fewer than five storeys"] <- "apartment_complex"
DataClean$ses_dwelling[DataRaw$dwelling == "Loft"] <- "loft"
DataClean$ses_dwelling[DataRaw$dwelling == "Condo"] <- "condominium"
DataClean$ses_dwelling[DataRaw$dwelling == "High-rise apartment"] <- "high_rise_apartment"
DataClean$ses_dwelling[DataRaw$dwelling == "Detached house"] <- "stand_alone_house"
DataClean$ses_dwelling[DataRaw$dwelling == "Townhome"] <- "townhouse"
DataClean$ses_dwelling[DataRaw$dwelling == "Duplex"] <- "duplex"
DataClean$ses_dwelling[DataRaw$dwelling == "Co-op"] <- "cooperative_housing"
DataClean$ses_dwelling[DataRaw$dwelling == "Public housing"] <- "social_or_public_housing"
DataClean$ses_dwelling[DataRaw$dwelling == "Mobile house (boat, van, etc.)"] <- "mobile_home"
DataClean$ses_dwelling[DataRaw$dwelling == "Other" | is.na(DataRaw$dwelling)] <- "other"

# Conversion en facteur avec les niveaux spécifiés
DataClean$ses_dwelling <- factor(
  DataClean$ses_dwelling,
  levels = c(
    "apartment_complex",
    "loft",
    "condominium",
    "high_rise_apartment",
    "stand_alone_house",
    "townhouse",
    "duplex",
    "cooperative_housing",
    "social_or_public_housing",
    "mobile_home",
    "other"
  )
)

# Vérification avec table
table(DataClean$ses_dwelling, useNA = "ifany")


table(DataRaw$dwelling)

# Initialisation
DataClean$ses_dwelling_cat <- NA

# Regroupement selon les nouvelles catégories de DataRaw
# "Apartment in a building that has fewer than five storeys", "Co-op", "Condo", "Loft", "Public housing" -> "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Apartment in a building that has fewer than five storeys"] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Co-op"] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Condo"] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Loft"] <- "apartment_complex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Public housing"] <- "apartment_complex"

# "High-rise apartment" -> "high_rise_apartment"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "High-rise apartment"] <- "high_rise_apartment"

# "Detached house" -> "stand_alone_house"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Detached house"] <- "stand_alone_house"

# "Townhome" -> "townhouse"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Townhome"] <- "townhouse"

# "Duplex" -> "duplex"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Duplex"] <- "duplex"

# "Mobile house (boat, van, etc.)" -> "mobile_home"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Mobile house (boat, van, etc.)"] <- "mobile_home"

# "Other" -> "other"
DataClean$ses_dwelling_cat[DataRaw$dwelling == "Other"] <- "other"

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

## Variables binaires
DataClean$ses_dwellingApp <- NA
DataClean$ses_dwellingApp[DataRaw$dwelling == "Apartment in a building that has fewer than five storeys"] <- 1
DataClean$ses_dwellingApp[DataRaw$dwelling != "Apartment in a building that has fewer than five storeys"] <- 0
table(DataClean$ses_dwellingApp)

DataClean$ses_dwellingLoft <- NA
DataClean$ses_dwellingLoft[DataRaw$dwelling == "Loft"] <- 1
DataClean$ses_dwellingLoft[DataRaw$dwelling != "Loft"] <- 0
table(DataClean$ses_dwellingLoft)

DataClean$ses_dwellingCondo <- NA
DataClean$ses_dwellingCondo[DataRaw$dwelling == "Condo"] <- 1
DataClean$ses_dwellingCondo[DataRaw$dwelling != "Condo"] <- 0
table(DataClean$ses_dwellingCondo)

DataClean$ses_dwellingTour <- NA
DataClean$ses_dwellingTour[DataRaw$dwelling == "High-rise apartment"] <- 1
DataClean$ses_dwellingTour[DataRaw$dwelling != "High-rise apartment"] <- 0
table(DataClean$ses_dwellingTour)

DataClean$ses_dwellingDetachedHouse <- NA
DataClean$ses_dwellingDetachedHouse[DataRaw$dwelling == "Detached house"] <- 1
DataClean$ses_dwellingDetachedHouse[DataRaw$dwelling != "Detached house"] <- 0
table(DataClean$ses_dwellingDetachedHouse)

DataClean$ses_dwellingTownhouse <- NA
DataClean$ses_dwellingTownhouse[DataRaw$dwelling == "Townhome"] <- 1
DataClean$ses_dwellingTownhouse[DataRaw$dwelling != "Townhome"] <- 0
table(DataClean$ses_dwellingTownhouse)

DataClean$ses_dwellingDuplex <- NA
DataClean$ses_dwellingDuplex[DataRaw$dwelling == "Duplex"] <- 1
DataClean$ses_dwellingDuplex[DataRaw$dwelling != "Duplex"] <- 0
table(DataClean$ses_dwellingDuplex)

DataClean$ses_dwellingCoop <- NA
DataClean$ses_dwellingCoop[DataRaw$dwelling == "Co-op"] <- 1
DataClean$ses_dwellingCoop[DataRaw$dwelling != "Co-op"] <- 0
table(DataClean$ses_dwellingCoop)

DataClean$ses_dwellingHLM <- NA
DataClean$ses_dwellingHLM[DataRaw$dwelling == "Public housing"] <- 1
DataClean$ses_dwellingHLM[DataRaw$dwelling != "Public housing"] <- 0
table(DataClean$ses_dwellingHLM)

DataClean$ses_dwellingMobile <- NA
DataClean$ses_dwellingMobile[DataRaw$dwelling == "Mobile house (boat, van, etc.)"] <- 1
DataClean$ses_dwellingMobile[DataRaw$dwelling != "Mobile house (boat, van, etc.)"] <- 0
table(DataClean$ses_dwellingMobile)

DataClean$ses_dwellingOther <- NA
DataClean$ses_dwellingOther[DataRaw$dwelling == "Other"] <- 1
DataClean$ses_dwellingOther[DataRaw$dwelling != "Other"] <- 0
table(DataClean$ses_dwellingOther)


# Date

table(DataRaw$X_time)

# Nettoyage de la variable de date X_time
# On suppose que X_time est déjà au format de date "YYYY-MM-DD"

# Vérification du format actuel
print("Format de X_time avant transformation:")
print(class(DataRaw$X_time))
print(head(DataRaw$X_time))

# Création de la variable de date nettoyée dans DataClean
# Si X_time est déjà un objet Date, on le copie simplement
if (inherits(DataRaw$X_time, "Date")) {
  DataClean$date <- DataRaw$X_time
} else {
  # Si X_time est une chaîne de caractères, on la convertit en Date
  DataClean$date <- as.Date(DataRaw$X_time)
}

# Vérification
print("Résumé de la variable date nettoyée:")
print(class(DataClean$date))
print(summary(DataClean$date))
print(table(DataClean$date, useNA = "always"))
