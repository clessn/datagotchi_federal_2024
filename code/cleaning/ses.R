# SES

## gender ----------------------------------------------------------------------

attributes(data_raw$ses_gender)
table(data_raw$ses_gender)
data_clean$ses_gender_factor <- NA
data_clean$ses_gender_factor[data_raw$ses_gender == 1] <- "male"
data_clean$ses_gender_factor[data_raw$ses_gender == 2] <- "female"
data_clean$ses_gender_factor[data_raw$ses_gender == 3] <- "trans_man"
data_clean$ses_gender_factor[data_raw$ses_gender == 4] <- "trans_woman"
data_clean$ses_gender_factor[data_raw$ses_gender == 5] <- "non_binary"
data_clean$ses_gender_factor[data_raw$ses_gender == 6] <- "queer"
data_clean$ses_gender_factor[data_raw$ses_gender == 7] <- "agender"
data_clean$ses_gender_factor <- factor(data_clean$ses_gender_factor)
table(data_clean$ses_gender_factor)

## age--------------------------------------------------------------------------

attributes(data_raw$ses_age)
table(data_raw$ses_age)
data_clean$ses_age <- NA
data_clean$ses_age <- data_raw$ses_age
table(data_clean$ses_age)

data_clean$ses_age_group_by_5 <- NA
data_clean$ses_age_group_by_5[data_raw$ses_age == 18 | data_raw$ses_age == 19] <- "18_19"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 20 & data_raw$ses_age < 25] <- "20_24"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 25 & data_raw$ses_age < 30] <- "25_29"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 30 & data_raw$ses_age < 35] <- "30_34"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 35 & data_raw$ses_age < 40] <- "35_39"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 40 & data_raw$ses_age < 45] <- "40-44"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 45 & data_raw$ses_age < 50] <- "45_49"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 50 & data_raw$ses_age < 55] <- "50_54"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 55 & data_raw$ses_age < 60] <- "55_59"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 60 & data_raw$ses_age < 65] <- "60_64"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 65 & data_raw$ses_age < 70] <- "65_69"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 70 & data_raw$ses_age < 75] <- "70_74"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 75 & data_raw$ses_age < 80] <- "75_79"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 80 & data_raw$ses_age < 85] <- "80_84"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 85 & data_raw$ses_age < 90] <- "85_89"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 90 & data_raw$ses_age < 95] <- "90_94"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 95 & data_raw$ses_age < 100] <- "95_99"
data_clean$ses_age_group_by_5[data_raw$ses_age >= 100] <- "100+"
data_clean$ses_age_group_by_5 <- factor(data_clean$ses_age_group_by_5, levels = c("18_19",
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
table(data_clean$ses_age_group_by_5)

data_clean$ses_age_group <- NA
data_clean$ses_age_group[data_raw$ses_age < 25 & data_raw$ses_age > 18] <- "18_24"
data_clean$ses_age_group[data_raw$ses_age >= 25 & data_raw$ses_age < 45] <- "25_44"
data_clean$ses_age_group[data_raw$ses_age >= 45 & data_raw$ses_age < 65] <- "45_64"
data_clean$ses_age_group[data_raw$ses_age >= 65] <- "65+"
data_clean$ses_age_group <- factor(data_clean$ses_age_group, levels = c("18_24",
                                                                        "25_44",
                                                                        "45_64",
                                                                        "65+"))
table(data_clean$ses_age_group)



## region ----------------------------------------------------------------------

attributes(data_raw$ses_region)
table(data_raw$ses_region)
data_clean$ses_province <- NA
data_clean$ses_province[data_raw$ses_region == 1] <- "AB"
data_clean$ses_province[data_raw$ses_region == 2] <- "BC"
data_clean$ses_province[data_raw$ses_region == 3] <- "MB"
data_clean$ses_province[data_raw$ses_region == 4] <- "NB"
data_clean$ses_province[data_raw$ses_region == 5] <- "NL"
data_clean$ses_province[data_raw$ses_region == 6] <- "NT"
data_clean$ses_province[data_raw$ses_region == 7] <- "NS"
data_clean$ses_province[data_raw$ses_region == 8] <- "NU"
data_clean$ses_province[data_raw$ses_region == 9] <- "ON"
data_clean$ses_province[data_raw$ses_region == 10] <- "PE"
data_clean$ses_province[data_raw$ses_region == 11] <- "QC"
data_clean$ses_province[data_raw$ses_region == 12] <- "SK"
data_clean$ses_province[data_raw$ses_region == 13] <- "YT"
data_clean$ses_province <- factor(data_clean$ses_province, levels = c("AB",
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
table(data_clean$ses_province)

data_clean$ses_region <- NA
data_clean$ses_region[data_raw$ses_region == 1 |
                        data_raw$ses_region == 3 |
                        data_raw$ses_region == 12] <- "prairie"
data_clean$ses_region[data_raw$ses_region == 2] <- "british_columbia"
data_clean$ses_region[data_raw$ses_region == 5 |
                        data_raw$ses_region == 7 |
                        data_raw$ses_region == 10] <- "atlantic"
data_clean$ses_region[data_raw$ses_region == 9] <- "ontario"
data_clean$ses_region[data_raw$ses_region == 11] <- "quebec"
data_clean$ses_region[data_raw$ses_region == 6 |
                        data_raw$ses_region == 8 |
                        data_raw$ses_region == 13] <- "territories"
table(data_clean$ses_region)

## postal_code -----------------------------------------------------------------

attributes(data_raw$ses_postal_code)
table(data_raw$ses_postal_code)
data_clean$ses_postal_code <- NA
data_clean$ses_postal_code <- data_raw$ses_postal_code
table(data_clean$ses_postal_code)

## language --------------------------------------------------------------------

attributes(data_raw$ses_language)
table(data_raw$ses_language)
data_clean$ses_language <- NA
data_clean$ses_language[data_raw$ses_language == 1] <- "english"
data_clean$ses_language[data_raw$ses_language == 2] <- "french"
data_clean$ses_language[data_raw$ses_language == 3] <- "other"
data_clean$ses_language <- factor(data_clean$ses_language, levels = c("english",
                                                                      "french",
                                                                      "other"))
table(data_clean$ses_language)


## religion --------------------------------------------------------------------

attributes(data_raw$ses_religion)
table(data_raw$ses_religion)
data_clean$ses_religion <- NA
data_clean$ses_religion[data_raw$ses_religion == 1] <- "agnostic"
data_clean$ses_religion[data_raw$ses_religion == 2] <- "atheist"
data_clean$ses_religion[data_raw$ses_religion == 3] <- "buddhist"
data_clean$ses_religion[data_raw$ses_religion == 4] <- "catholic"
data_clean$ses_religion[data_raw$ses_religion == 5] <- "orthodox_christian"
data_clean$ses_religion[data_raw$ses_religion == 6] <- "hindu"
data_clean$ses_religion[data_raw$ses_religion == 7] <- "muslim"
data_clean$ses_religion[data_raw$ses_religion == 8] <- "jew"
data_clean$ses_religion[data_raw$ses_religion == 9] <- "protestant"
data_clean$ses_religion[data_raw$ses_religion == 10] <- "sikh"
data_clean$ses_religion[data_raw$ses_religion == 11] <- "evangelical"
data_clean$ses_religion[data_raw$ses_religion == 12] <- "other"
data_clean$ses_religion <- factor(data_clean$ses_religion, levels = c("agnostic",
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
table(data_clean$ses_religion)

## religiosity -------------------------------------------------------------



## education ---------------------------------------------------------------



## income ----------------------------------------------------------------



## environment -------------------------------------------------------------



## status ------------------------------------------------------------------



## owner -------------------------------------------------------------------



## kids --------------------------------------------------------------------



## ses_occupation --------------------------------------------------------------

table(data_raw$ses_occupation)
attributes(data_raw$ses_occupation)
data_clean$ses_occupation <- NA
data_clean$ses_occupation[data_raw$ses_occupation == 1] <- "paid_employment"
data_clean$ses_occupation[data_raw$ses_occupation == 2] <- "self_employment"
data_clean$ses_occupation[data_raw$ses_occupation == 3] <- "student"
data_clean$ses_occupation[data_raw$ses_occupation == 4] <- "retired"
data_clean$ses_occupation[data_raw$ses_occupation == 5] <- "looking_for_work"
data_clean$ses_occupation[data_raw$ses_occupation == 6] <- "unemployed"
data_clean$ses_occupation[data_raw$ses_occupation == 7] <- "other"
data_clean$ses_occupation <- factor(data_clean$ses_occupation, levels = c("paid_employment",
                                                                          "self_employment",
                                                                          "student",
                                                                          "retired",
                                                                          "looking_for_work",
                                                                          "unemployed",
                                                                          "other"))
table(data_clean$ses_occupation)



## SES (enfant) -------------------------------------------------------------------

attributes(data_raw$ses_children)
table(data_raw$ses_children)
data_raw$ses_children <- as.numeric(data_raw$ses_children)
data_clean$ses_children <- NA
data_clean$ses_children[data_raw$ses_children == 0] <- "0"
data_clean$ses_children[data_raw$ses_children == 2] <- "1"
data_clean$ses_children[data_raw$ses_children == 3] <- "2"
data_clean$ses_children[data_raw$ses_children == 4] <- "3"
data_clean$ses_children[data_raw$ses_children == 5] <- "4"
data_clean$ses_children[data_raw$ses_children >= 6] <- "5+"
data_clean$ses_children <- factor(data_clean$ses_children, levels = c("0",
                                                                      "1",
                                                                      "2",
                                                                      "3",
                                                                      "4",
                                                                      "5+"),
                                  ordered = TRUE)
table(data_clean$ses_children)



## ethnicity -------------------------------------------------------------
attributes(data_raw$ses_ethnicity)
table(data_raw$ses_ethnicity)
data_clean$ses_ethnicity <- NA
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 1] <- "white"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 2] <- "black"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 3] <- "indigenous"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 4] <- "asian"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 5] <- "hispanic"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 6] <- "arab"
data_clean$ses_ethnicity[data_raw$ses_ethnicity == 7] <- "other"
data_clean$ses_ethnicity <- factor(data_clean$ses_ethnicity, levels = c("white",
                                                                        "black",
                                                                        "indigenous",
                                                                        "asian",
                                                                        "hispanic",
                                                                        "arab",
                                                                        "other"))
table(data_clean$ses_ethnicity)

data_clean$ses_ethnicity_white <- NA
data_clean$ses_ethnicity_white[data_raw$ses_ethnicity == 1] <- 1
data_clean$ses_ethnicity_white[data_raw$ses_ethnicity != 1] <- 0
table(data_clean$ses_ethnicity_white)

data_clean$ses_ethnicity_white_black <- NA
data_clean$ses_ethnicity_white_black[data_raw$ses_ethnicity == 1] <- "white"
data_clean$ses_ethnicity_white_black[data_raw$ses_ethnicity == 2] <- "black"
data_clean$ses_ethnicity_white_black[!(data_raw$ses_ethnicity %in% c(1, 2))] <- "other"
data_clean$ses_ethnicity_white_black <- factor(data_clean$ses_ethnicity_white_black)
data_clean$ses_ethnicity_white_black <- relevel(data_clean$ses_ethnicity_white_black, ref = "white")
table(data_clean$ses_ethnicity_white_black)


labels <- stringr::str_trim(names(attributes(data_raw$ses_ethnicity)$labels))
json_data <- list(
  `1` = labels[1],
  `0` = labels[-1]
)
jsonlite::toJSON(json_data, pretty = TRUE)



## orientation -----------------------------------------------------------
attributes(data_raw$ses_orientation)
table(data_raw$ses_orientation)
data_clean$ses_orientation_factor <- NA
data_clean$ses_orientation_factor[data_raw$ses_orientation == 1] <- "heterosexual"
data_clean$ses_orientation_factor[data_raw$ses_orientation == 2] <- "gay"
data_clean$ses_orientation_factor[data_raw$ses_orientation == 3] <- "bisexual"
data_clean$ses_orientation_factor[data_raw$ses_orientation == 4] <- "other"
data_clean$ses_orientation_factor <- factor(data_clean$ses_orientation_factor, levels = c("heterosexual",
                                                                                        "gay",
                                                                                        "bisexual",
                                                                                        "other"))
table(data_clean$ses_orientation_factor)

## heterosexual

data_clean$ses_orientation_heterosexual <- NA
data_clean$ses_orientation_heterosexual[data_raw$ses_orientation == 1] <- 1
data_clean$ses_orientation_heterosexual[data_raw$ses_orientation != 1] <- 0
table(data_clean$ses_orientation_heterosexual)



## parent ----------------------------------------------------------------

attributes(data_raw$ses_parent)
table(data_raw$ses_parent)
data_clean$ses_parent_immigrant <- NA
data_clean$ses_parent_immigrant[data_raw$ses_parent == 1] <- 1
data_clean$ses_parent_immigrant[data_raw$ses_parent != 1] <- 0
table(data_clean$ses_parent_immigrant)


## immigrant -------------------------------------------------------------

attributes(data_raw$ses_immigrant)
table(data_raw$ses_immigrant)
data_clean$ses_immigrant <- NA
data_clean$ses_immigrant[data_raw$ses_immigrant == 1] <- 1
data_clean$ses_immigrant[data_raw$ses_immigrant != 1] <- 0
table(data_clean$ses_immigrant)

## dwelling --------------------------------------------------------------
attributes(data_raw$ses_dwelling)
table(data_raw$ses_dwelling)
data_clean$ses_dwelling <- NA
data_clean$ses_dwelling[data_raw$ses_dwelling == 1] <- "apartment_complex"
data_clean$ses_dwelling[data_raw$ses_dwelling == 2] <- "loft"
data_clean$ses_dwelling[data_raw$ses_dwelling == 3] <- "condominium"
data_clean$ses_dwelling[data_raw$ses_dwelling == 4] <- "high_rise_apartment"
data_clean$ses_dwelling[data_raw$ses_dwelling == 5] <- "stand_alone_house"
data_clean$ses_dwelling[data_raw$ses_dwelling == 6] <- "townhouse"
data_clean$ses_dwelling[data_raw$ses_dwelling == 7] <- "duplex"
data_clean$ses_dwelling[data_raw$ses_dwelling == 8] <- "cooperative_housing"
data_clean$ses_dwelling[data_raw$ses_dwelling == 9] <- "social_or_public_housing"
data_clean$ses_dwelling[data_raw$ses_dwelling == 10] <- "mobile_home"
data_clean$ses_dwelling[data_raw$ses_dwelling == 11] <- "other"
data_clean$ses_dwelling <- factor(data_clean$ses_dwelling, levels = c("apartment_complex",
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
table(data_clean$ses_dwelling)

