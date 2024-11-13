# SES

## gender ----------------------------------------------------------------------



## age--------------------------------------------------------------------------



## region ----------------------------------------------------------------------



## postal_code -----------------------------------------------------------------



## language --------------------------------------------------------------------



## religion --------------------------------------------------------------------



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

