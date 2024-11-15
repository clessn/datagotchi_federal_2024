# lifestyle

## exercise --------------------------------------------------------------

table(data_raw$exercise)
data_clean$exercise_clean <- NA
data_clean$exercise_clean[data_raw$exercise == 1] <- 1
data_clean$exercise_clean[data_raw$exercise == 2] <- 2
data_clean$exercise_clean[data_raw$exercise == 3] <- 3
data_clean$exercise_clean[data_raw$exercise == 4] <- 4
data_clean$exercise_clean[data_raw$exercise == 5] <- 5
data_clean$exercise_clean[data_raw$exercise == 6] <- 6
data_clean$exercise_clean[data_raw$exercise == 7] <- 7
data_clean$exercise_clean[data_raw$exercise == 8] <- 8
table(data_clean$exercise_clean)


## activity_1 ------------------------------------------------------------

table(data_raw$activity_1) 
data_clean$lifestyle_fishing_freq <- NA
data_raw$activity_1 <- as.numeric(data_raw$activity_1)
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 1] <- 1
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 2] <- 2
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 3] <- 3
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 4] <- 4
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 5] <- 5
table(data_clean$lifestyle_fishing_freq)

## factor

table(data_clean$lifestyle_fishing_freq)
data_clean$lifestyle_fishing_freq_factor <- NA
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 1] <- "never"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 2] <- "almost_never"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 3] <- "sometimes"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 4] <- "often"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 5] <- "very_often"
data_clean$lifestyle_fishing_freq_factor <- factor(data_clean$lifestyle_fishing_freq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_fishing_freq_factor)

## numeric

data_clean$lifestyle_fishing_freq_numeric <- NA 
data_clean$lifestyle_fishing_freq_numeric <- (data_raw$activity_1 - 1) / 4
table(data_clean$lifestyle_fishing_freq_numeric)

## bin

data_clean$lifestyle_fishing_bin <- NA
data_clean$lifestyle_fishing_bin[data_raw$activity_1  == 1] <- 0
data_clean$lifestyle_fishing_bin[data_raw$activity_1  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_fishing_bin) 

## activity_2 ------------------------------------------------------------

table(data_raw$activity_2) 
data_clean$lifestyle_hunting_freq <- NA
data_raw$activity_2 <- as.numeric(data_raw$activity_2)
data_clean$lifestyle_hunting_freq[data_raw$activity_2 == 1] <- 1
data_clean$lifestyle_hunting_freq[data_raw$activity_2 == 2] <- 2
data_clean$lifestyle_hunting_freq[data_raw$activity_2 == 3] <- 3
data_clean$lifestyle_hunting_freq[data_raw$activity_2 == 4] <- 4
data_clean$lifestyle_hunting_freq[data_raw$activity_2 == 5] <- 5
table(data_clean$lifestyle_hunting_freq)

## factor

table(data_clean$lifestyle_hunting_freq)
data_clean$lifestyle_hunting_freq_factor <- NA
data_clean$lifestyle_hunting_freq_factor[data_raw$activity_2 == 1] <- "never"
data_clean$lifestyle_hunting_freq_factor[data_raw$activity_2 == 2] <- "almost_never"
data_clean$lifestyle_hunting_freq_factor[data_raw$activity_2 == 3] <- "sometimes"
data_clean$lifestyle_hunting_freq_factor[data_raw$activity_2 == 4] <- "often"
data_clean$lifestyle_hunting_freq_factor[data_raw$activity_2 == 5] <- "very_often"
data_clean$lifestyle_hunting_freq_factor <- factor(data_clean$lifestyle_hunting_freq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_hunting_freq_factor)

## numeric

data_clean$lifestyle_hunting_freq_numeric <- NA 
data_clean$lifestyle_hunting_freq_numeric <- (data_raw$activity_2 - 1) / 4
table(data_clean$lifestyle_hunting_freq_numeric)

## bin

data_clean$lifestyle_hunting_bin <- NA
data_clean$lifestyle_hunting_bin[data_raw$activity_2  == 1] <- 0
data_clean$lifestyle_hunting_bin[data_raw$activity_2  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_hunting_bin) 


## activity_3 ------------------------------------------------------------

table(data_raw$activity_3) 
data_clean$lifestyle_snow_sports_freq <- NA
data_raw$activity_3 <- as.numeric(data_raw$activity_3)
data_clean$lifestyle_snow_sports_freq[data_raw$activity_3 == 1] <- 1
data_clean$lifestyle_snow_sports_freq[data_raw$activity_3 == 2] <- 2
data_clean$lifestyle_snow_sports_freq[data_raw$activity_3 == 3] <- 3
data_clean$lifestyle_snow_sports_freq[data_raw$activity_3 == 4] <- 4
data_clean$lifestyle_snow_sports_freq[data_raw$activity_3 == 5] <- 5
table(data_clean$lifestyle_snow_sports_freq)

## factor

table(data_clean$lifestyle_snow_sports_freq)
data_clean$lifestyle_snow_sports_freq_factor <- NA
data_clean$lifestyle_snow_sports_freq_factor[data_raw$activity_3 == 1] <- "never"
data_clean$lifestyle_snow_sports_freq_factor[data_raw$activity_3 == 2] <- "almost_never"
data_clean$lifestyle_snow_sports_freq_factor[data_raw$activity_3 == 3] <- "sometimes"
data_clean$lifestyle_snow_sports_freq_factor[data_raw$activity_3 == 4] <- "often"
data_clean$lifestyle_snow_sports_freq_factor[data_raw$activity_3 == 5] <- "very_often"
data_clean$lifestyle_snow_sports_freq_factor <- factor(data_clean$lifestyle_snow_sports_freq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_snow_sports_freq_factor)

## numeric

data_clean$lifestyle_snow_sports_freq_numeric <- NA 
data_clean$lifestyle_snow_sports_freq_numeric <- (data_raw$activity_3 - 1) / 4
table(data_clean$lifestyle_snow_sports_freq_numeric)

## bin

data_clean$lifestyle_snow_sports_bin <- NA
data_clean$lifestyle_snow_sports_bin[data_raw$activity_3  == 1] <- 0
data_clean$lifestyle_snow_sports_bin[data_raw$activity_3  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_snow_sports_bin) 


## activity_4 -----------------------------------------------------------

table(data_raw$activity_4) 
data_clean$lifestyle_team_sports_freq <- NA
data_raw$activity_4 <- as.numeric(data_raw$activity_4)
data_clean$lifestyle_team_sports_freq[data_raw$activity_4 == 1] <- 1
data_clean$lifestyle_team_sports_freq[data_raw$activity_4 == 2] <- 2
data_clean$lifestyle_team_sports_freq[data_raw$activity_4 == 3] <- 3
data_clean$lifestyle_team_sports_freq[data_raw$activity_4 == 4] <- 4
data_clean$lifestyle_team_sports_freq[data_raw$activity_4 == 5] <- 5
table(data_clean$lifestyle_team_sports_freq)

## factor

table(data_clean$lifestyle_team_sports_freq)
data_clean$lifestyle_team_sports_freq_factor <- NA
data_clean$lifestyle_team_sports_freq_factor[data_raw$activity_4 == 1] <- "never"
data_clean$lifestyle_team_sports_freq_factor[data_raw$activity_4 == 2] <- "almost_never"
data_clean$lifestyle_team_sports_freq_factor[data_raw$activity_4 == 3] <- "sometimes"
data_clean$lifestyle_team_sports_freq_factor[data_raw$activity_4 == 4] <- "often"
data_clean$lifestyle_team_sports_freq_factor[data_raw$activity_4 == 5] <- "very_often"
data_clean$lifestyle_team_sports_freq_factor <- factor(data_clean$lifestyle_team_sports_freq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_team_sports_freq_factor)

## numeric

data_clean$lifestyle_team_sports_freq_numeric <- NA 
data_clean$lifestyle_team_sports_freq_numeric <- (data_raw$activity_4 - 1) / 4
table(data_clean$lifestyle_team_sports_freq_numeric)

## bin

data_clean$lifestyle_team_sports_bin <- NA
data_clean$lifestyle_team_sports_bin[data_raw$activity_4  == 1] <- 0
data_clean$lifestyle_team_sports_bin[data_raw$activity_4  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_team_sports_bin)


## activity_5 ------------------------------------------------------------

table(data_raw$activity_5) 
data_clean$lifestyle_museums_freq <- NA
data_raw$activity_5 <- as.numeric(data_raw$activity_5)
data_clean$lifestyle_museums_freq[data_raw$activity_5 == 1] <- 1
data_clean$lifestyle_museums_freq[data_raw$activity_5 == 2] <- 2
data_clean$lifestyle_museums_freq[data_raw$activity_5 == 3] <- 3
data_clean$lifestyle_museums_freq[data_raw$activity_5 == 4] <- 4
data_clean$lifestyle_museums_freq[data_raw$activity_5 == 5] <- 5
table(data_clean$lifestyle_museums_freq)

## factor

table(data_clean$lifestyle_museums_freq)
data_clean$lifestyle_museums_freq_factor <- NA
data_clean$lifestyle_museums_freq_factor[data_raw$activity_5 == 1] <- "never"
data_clean$lifestyle_museums_freq_factor[data_raw$activity_5 == 2] <- "almost_never"
data_clean$lifestyle_museums_freq_factor[data_raw$activity_5 == 3] <- "sometimes"
data_clean$lifestyle_museums_freq_factor[data_raw$activity_5 == 4] <- "often"
data_clean$lifestyle_museums_freq_factor[data_raw$activity_5 == 5] <- "very_often"
data_clean$lifestyle_museums_freq_factor <- factor(data_clean$lifestyle_museums_freq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_museums_freq_factor)

## numeric

data_clean$lifestyle_museums_freq_numeric <- NA 
data_clean$lifestyle_museums_freq_numeric <- (data_raw$activity_5 - 1) / 4
table(data_clean$lifestyle_museums_freq_numeric)

## bin

data_clean$lifestyle_museums_bin <- NA
data_clean$lifestyle_museums_bin[data_raw$activity_5  == 1] <- 0
data_clean$lifestyle_museums_bin[data_raw$activity_5  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_museums_bin)


## activity_6 ------------------------------------------------------------

table(data_raw$activity_6) 
data_clean$lifestyle_performing_arts_freq <- NA
data_raw$activity_6 <- as.numeric(data_raw$activity_6)
data_clean$lifestyle_performing_arts_freq[data_raw$activity_6 == 1] <- 1
data_clean$lifestyle_performing_arts_freq[data_raw$activity_6 == 2] <- 2
data_clean$lifestyle_performing_arts_freq[data_raw$activity_6 == 3] <- 3
data_clean$lifestyle_performing_arts_freq[data_raw$activity_6 == 4] <- 4
data_clean$lifestyle_performing_arts_freq[data_raw$activity_6 == 5] <- 5
table(data_clean$lifestyle_performing_arts_freq)

## factor

table(data_clean$lifestyle_performing_arts_freq)
data_clean$lifestyle_performing_arts_freq_factor <- NA
data_clean$lifestyle_performing_arts_freq_factor[data_raw$activity_6 == 1] <- "never"
data_clean$lifestyle_performing_arts_freq_factor[data_raw$activity_6 == 2] <- "almost_never"
data_clean$lifestyle_performing_arts_freq_factor[data_raw$activity_6 == 3] <- "sometimes"
data_clean$lifestyle_performing_arts_freq_factor[data_raw$activity_6 == 4] <- "often"
data_clean$lifestyle_performing_arts_freq_factor[data_raw$activity_6 == 5] <- "very_often"
data_clean$lifestyle_performing_arts_freq_factor <- factor(data_clean$lifestyle_performing_arts_freq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_performing_arts_freq_factor)

## numeric

data_clean$lifestyle_performing_arts_freq_numeric <- NA 
data_clean$lifestyle_performing_arts_freq_numeric <- (data_raw$activity_6 - 1) / 4
table(data_clean$lifestyle_performing_arts_freq_numeric)

## bin

data_clean$lifestyle_performing_arts_bin <- NA
data_clean$lifestyle_performing_arts_bin[data_raw$activity_6  == 1] <- 0
data_clean$lifestyle_performing_arts_bin[data_raw$activity_6  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_performing_arts_bin)


## activity_7 ------------------------------------------------------------

table(data_raw$activity_7) 
data_clean$lifestyle_manual_task_freq <- NA
data_raw$activity_7 <- as.numeric(data_raw$activity_7)
data_clean$lifestyle_manual_task_freq[data_raw$activity_7 == 1] <- 1
data_clean$lifestyle_manual_task_freq[data_raw$activity_7 == 2] <- 2
data_clean$lifestyle_manual_task_freq[data_raw$activity_7 == 3] <- 3
data_clean$lifestyle_manual_task_freq[data_raw$activity_7 == 4] <- 4
data_clean$lifestyle_manual_task_freq[data_raw$activity_7 == 5] <- 5
table(data_clean$lifestyle_manual_task_freq)

## factor

table(data_clean$lifestyle_manual_task_freq)
data_clean$lifestyle_manual_task_freq_factor <- NA
data_clean$lifestyle_manual_task_freq_factor[data_raw$activity_7 == 1] <- "never"
data_clean$lifestyle_manual_task_freq_factor[data_raw$activity_7 == 2] <- "almost_never"
data_clean$lifestyle_manual_task_freq_factor[data_raw$activity_7 == 3] <- "sometimes"
data_clean$lifestyle_manual_task_freq_factor[data_raw$activity_7 == 4] <- "often"
data_clean$lifestyle_manual_task_freq_factor[data_raw$activity_7 == 5] <- "very_often"
data_clean$lifestyle_manual_task_freq_factor <- factor(data_clean$lifestyle_manual_task_freq_factor,
                                                           levels = c("never",
                                                                      "almost_never",
                                                                      "sometimes",
                                                                      "often",
                                                                      "very_often"),
                                                           ordered = TRUE)
table(data_clean$lifestyle_manual_task_freq_factor)

## numeric

data_clean$lifestyle_manual_task_freq_numeric <- NA 
data_clean$lifestyle_manual_task_freq_numeric <- (data_raw$activity_7 - 1) / 4
table(data_clean$lifestyle_manual_task_freq_numeric)

## bin

data_clean$lifestyle_manual_task_bin <- NA
data_clean$lifestyle_manual_task_bin[data_raw$activity_7  == 1] <- 0
data_clean$lifestyle_manual_task_bin[data_raw$activity_7  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_manual_task_bin)


## activity_8 ------------------------------------------------------------

table(data_raw$activity_8) 
data_clean$lifestyle_motorized_activities_freq <- NA
data_raw$activity_8 <- as.numeric(data_raw$activity_8)
data_clean$lifestyle_motorized_activities_freq[data_raw$activity_8 == 1] <- 1
data_clean$lifestyle_motorized_activities_freq[data_raw$activity_8 == 2] <- 2
data_clean$lifestyle_motorized_activities_freq[data_raw$activity_8 == 3] <- 3
data_clean$lifestyle_motorized_activities_freq[data_raw$activity_8 == 4] <- 4
data_clean$lifestyle_motorized_activities_freq[data_raw$activity_8 == 5] <- 5
table(data_clean$lifestyle_motorized_activities_freq)

## factor

table(data_clean$lifestyle_motorized_activities_freq)
data_clean$lifestyle_motorized_activities_freq_factor <- NA
data_clean$lifestyle_motorized_activities_freq_factor[data_raw$activity_8 == 1] <- "never"
data_clean$lifestyle_motorized_activities_freq_factor[data_raw$activity_8 == 2] <- "almost_never"
data_clean$lifestyle_motorized_activities_freq_factor[data_raw$activity_8 == 3] <- "sometimes"
data_clean$lifestyle_motorized_activities_freq_factor[data_raw$activity_8 == 4] <- "often"
data_clean$lifestyle_motorized_activities_freq_factor[data_raw$activity_8 == 5] <- "very_often"
data_clean$lifestyle_motorized_activities_freq_factor <- factor(data_clean$lifestyle_motorized_activities_freq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_motorized_activities_freq_factor)

## numeric

data_clean$lifestyle_motorized_activities_freq_numeric <- NA 
data_clean$lifestyle_motorized_activities_freq_numeric <- (data_raw$activity_8 - 1) / 4
table(data_clean$lifestyle_motorized_activities_freq_numeric)

## bin

data_clean$lifestyle_motorized_activities_bin <- NA
data_clean$lifestyle_motorized_activities_bin[data_raw$activity_8  == 1] <- 0
data_clean$lifestyle_motorized_activities_bin[data_raw$activity_8  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_motorized_activities_bin)



## activity_9 ------------------------------------------------------------

table(data_raw$activity_9) 
data_clean$lifestyle_unmotorized_activities_freq <- NA
data_raw$activity_9 <- as.numeric(data_raw$activity_9)
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 1] <- 1
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 2] <- 2
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 3] <- 3
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 4] <- 4
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 5] <- 5
table(data_clean$lifestyle_unmotorized_activities_freq)

## factor

table(data_clean$lifestyle_unmotorized_activities_freq)
data_clean$lifestyle_unmotorized_activities_freq_factor <- NA
data_clean$lifestyle_unmotorized_activities_freq_factor[data_raw$activity_9 == 1] <- "never"
data_clean$lifestyle_unmotorized_activities_freq_factor[data_raw$activity_9 == 2] <- "almost_never"
data_clean$lifestyle_unmotorized_activities_freq_factor[data_raw$activity_9 == 3] <- "sometimes"
data_clean$lifestyle_unmotorized_activities_freq_factor[data_raw$activity_9 == 4] <- "often"
data_clean$lifestyle_unmotorized_activities_freq_factor[data_raw$activity_9 == 5] <- "very_often"
data_clean$lifestyle_unmotorized_activities_freq_factor <- factor(data_clean$lifestyle_unmotorized_activities_freq_factor,
                                                                levels = c("never",
                                                                           "almost_never",
                                                                           "sometimes",
                                                                           "often",
                                                                           "very_often"),
                                                                ordered = TRUE)
table(data_clean$lifestyle_unmotorized_activities_freq_factor)

## numeric

data_clean$lifestyle_unmotorized_activities_freq_numeric <- NA 
data_clean$lifestyle_unmotorized_activities_freq_numeric <- (data_raw$activity_9 - 1) / 4
table(data_clean$lifestyle_unmotorized_activities_freq_numeric)

## bin

data_clean$lifestyle_unmotorized_activities_bin <- NA
data_clean$lifestyle_unmotorized_activities_bin[data_raw$activity_9  == 1] <- 0
data_clean$lifestyle_unmotorized_activities_bin[data_raw$activity_9  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_unmotorized_activities_bin)


## activity_10 -----------------------------------------------------------

table(data_raw$activity_10) 
data_clean$lifestyle_volunteering_freq <- NA
data_raw$activity_10 <- as.numeric(data_raw$activity_10)
data_clean$lifestyle_volunteering_freq[data_raw$activity_10 == 1] <- 1
data_clean$lifestyle_volunteering_freq[data_raw$activity_10 == 2] <- 2
data_clean$lifestyle_volunteering_freq[data_raw$activity_10 == 3] <- 3
data_clean$lifestyle_volunteering_freq[data_raw$activity_10 == 4] <- 4
data_clean$lifestyle_volunteering_freq[data_raw$activity_10 == 5] <- 5
table(data_clean$lifestyle_volunteering_freq)

## factor

table(data_clean$lifestyle_volunteering_freq)
data_clean$lifestyle_volunteering_freq_factor <- NA
data_clean$lifestyle_volunteering_freq_factor[data_raw$activity_10 == 1] <- "never"
data_clean$lifestyle_volunteering_freq_factor[data_raw$activity_10 == 2] <- "almost_never"
data_clean$lifestyle_volunteering_freq_factor[data_raw$activity_10 == 3] <- "sometimes"
data_clean$lifestyle_volunteering_freq_factor[data_raw$activity_10 == 4] <- "often"
data_clean$lifestyle_volunteering_freq_factor[data_raw$activity_10 == 5] <- "very_often"
data_clean$lifestyle_volunteering_freq_factor <- factor(data_clean$lifestyle_volunteering_freq_factor,
                                                                  levels = c("never",
                                                                             "almost_never",
                                                                             "sometimes",
                                                                             "often",
                                                                             "very_often"),
                                                                  ordered = TRUE)
table(data_clean$lifestyle_volunteering_freq_factor)

## numeric

data_clean$lifestyle_volunteering_freq_numeric <- NA 
data_clean$lifestyle_volunteering_freq_numeric <- (data_raw$activity_10 - 1) / 4
table(data_clean$lifestyle_volunteering_freq_numeric)

## bin

data_clean$lifestyle_volunteering_bin <- NA
data_clean$lifestyle_volunteering_bin[data_raw$activity_10  == 1] <- 0
data_clean$lifestyle_volunteering_bin[data_raw$activity_10  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_volunteering_bin)


## activity_11 -----------------------------------------------------------

table(data_raw$activity_11) 
data_clean$lifestyle_art_activities_freq <- NA
data_raw$activity_11 <- as.numeric(data_raw$activity_11)
data_clean$lifestyle_art_activities_freq[data_raw$activity_11 == 1] <- 1
data_clean$lifestyle_art_activities_freq[data_raw$activity_11 == 2] <- 2
data_clean$lifestyle_art_activities_freq[data_raw$activity_11 == 3] <- 3
data_clean$lifestyle_art_activities_freq[data_raw$activity_11 == 4] <- 4
data_clean$lifestyle_art_activities_freq[data_raw$activity_11 == 5] <- 5
table(data_clean$lifestyle_art_activities_freq)

## factor

table(data_clean$lifestyle_art_activities_freq)
data_clean$lifestyle_art_activities_freq_factor <- NA
data_clean$lifestyle_art_activities_freq_factor[data_raw$activity_11 == 1] <- "never"
data_clean$lifestyle_art_activities_freq_factor[data_raw$activity_11 == 2] <- "almost_never"
data_clean$lifestyle_art_activities_freq_factor[data_raw$activity_11 == 3] <- "sometimes"
data_clean$lifestyle_art_activities_freq_factor[data_raw$activity_11 == 4] <- "often"
data_clean$lifestyle_art_activities_freq_factor[data_raw$activity_11 == 5] <- "very_often"
data_clean$lifestyle_art_activities_freq_factor <- factor(data_clean$lifestyle_art_activities_freq_factor,
                                                        levels = c("never",
                                                                   "almost_never",
                                                                   "sometimes",
                                                                   "often",
                                                                   "very_often"),
                                                        ordered = TRUE)
table(data_clean$lifestyle_art_activities_freq_factor)

## numeric

data_clean$lifestyle_art_activities_freq_numeric <- NA 
data_clean$lifestyle_art_activities_freq_numeric <- (data_raw$activity_11 - 1) / 4
table(data_clean$lifestyle_art_activities_freq_numeric)

## bin

data_clean$lifestyle_art_activities_bin <- NA
data_clean$lifestyle_art_activities_bin[data_raw$activity_11  == 1] <- 0
data_clean$lifestyle_art_activities_bin[data_raw$activity_11  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_art_activities_bin)


## activity_12 -----------------------------------------------------------

table(data_raw$activity_12) 
data_clean$lifestyle_traveling_freq <- NA
data_raw$activity_12 <- as.numeric(data_raw$activity_12)
data_clean$lifestyle_traveling_freq[data_raw$activity_12 == 1] <- 1
data_clean$lifestyle_traveling_freq[data_raw$activity_12 == 2] <- 2
data_clean$lifestyle_traveling_freq[data_raw$activity_12 == 3] <- 3
data_clean$lifestyle_traveling_freq[data_raw$activity_12 == 4] <- 4
data_clean$lifestyle_traveling_freq[data_raw$activity_12 == 5] <- 5
table(data_clean$lifestyle_traveling_freq)

## factor

table(data_clean$lifestyle_traveling_freq)
data_clean$lifestyle_traveling_freq_factor <- NA
data_clean$lifestyle_traveling_freq_factor[data_raw$activity_12 == 1] <- "never"
data_clean$lifestyle_traveling_freq_factor[data_raw$activity_12 == 2] <- "almost_never"
data_clean$lifestyle_traveling_freq_factor[data_raw$activity_12 == 3] <- "sometimes"
data_clean$lifestyle_traveling_freq_factor[data_raw$activity_12 == 4] <- "often"
data_clean$lifestyle_traveling_freq_factor[data_raw$activity_12 == 5] <- "very_often"
data_clean$lifestyle_traveling_freq_factor <- factor(data_clean$lifestyle_traveling_freq_factor,
                                                          levels = c("never",
                                                                     "almost_never",
                                                                     "sometimes",
                                                                     "often",
                                                                     "very_often"),
                                                          ordered = TRUE)
table(data_clean$lifestyle_traveling_freq_factor)

## numeric

data_clean$lifestyle_traveling_freq_numeric <- NA 
data_clean$lifestyle_traveling_freq_numeric <- (data_raw$activity_12 - 1) / 4
table(data_clean$lifestyle_traveling_freq_numeric)

## bin

data_clean$lifestyle_traveling_bin <- NA
data_clean$lifestyle_traveling_bin[data_raw$activity_12  == 1] <- 0
data_clean$lifestyle_traveling_bin[data_raw$activity_12  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_traveling_bin)


## activity_13 -----------------------------------------------------------

table(data_raw$activity_13) 
data_clean$lifestyle_gaming_freq <- NA
data_raw$activity_13 <- as.numeric(data_raw$activity_13)
data_clean$lifestyle_gaming_freq[data_raw$activity_13 == 1] <- 1
data_clean$lifestyle_gaming_freq[data_raw$activity_13 == 2] <- 2
data_clean$lifestyle_gaming_freq[data_raw$activity_13 == 3] <- 3
data_clean$lifestyle_gaming_freq[data_raw$activity_13 == 4] <- 4
data_clean$lifestyle_gaming_freq[data_raw$activity_13 == 5] <- 5
table(data_clean$lifestyle_gaming_freq)

## factor

table(data_clean$lifestyle_gaming_freq)
data_clean$lifestyle_gaming_freq_factor <- NA
data_clean$lifestyle_gaming_freq_factor[data_raw$activity_13 == 1] <- "never"
data_clean$lifestyle_gaming_freq_factor[data_raw$activity_13 == 2] <- "almost_never"
data_clean$lifestyle_gaming_freq_factor[data_raw$activity_13 == 3] <- "sometimes"
data_clean$lifestyle_gaming_freq_factor[data_raw$activity_13 == 4] <- "often"
data_clean$lifestyle_gaming_freq_factor[data_raw$activity_13 == 5] <- "very_often"
data_clean$lifestyle_gaming_freq_factor <- factor(data_clean$lifestyle_gaming_freq_factor,
                                                     levels = c("never",
                                                                "almost_never",
                                                                "sometimes",
                                                                "often",
                                                                "very_often"),
                                                     ordered = TRUE)
table(data_clean$lifestyle_gaming_freq_factor)

## numeric

data_clean$lifestyle_gaming_freq_numeric <- NA 
data_clean$lifestyle_gaming_freq_numeric <- (data_raw$activity_13 - 1) / 4
table(data_clean$lifestyle_gaming_freq_numeric)

## bin

data_clean$lifestyle_gaming_bin <- NA
data_clean$lifestyle_gaming_bin[data_raw$activity_13  == 1] <- 0
data_clean$lifestyle_gaming_bin[data_raw$activity_13  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_gaming_bin)


## activity_14 -----------------------------------------------------------




## type_transport --------------------------------------------------------

lifestyle_transport_occ


## choice_transport ------------------------------------------------------




## field_occupation ------------------------------------------------------




## type_occupation -------------------------------------------------------




## clothes_consumption ---------------------------------------------------




## mode_attitude ---------------------------------------------------------




## meat_consumption ------------------------------------------------------




## meal_time -------------------------------------------------------------




## fridge ----------------------------------------------------------------

### fridge_1 -------------------------------------------------------------




### fridge_2 -------------------------------------------------------------




### fridge_3 -------------------------------------------------------------




### fridge_4 -------------------------------------------------------------




### fridge_5 -------------------------------------------------------------




### fridge_6 -------------------------------------------------------------




### fridge_7 -------------------------------------------------------------




### fridge_8 -------------------------------------------------------------




### fridge_9 -------------------------------------------------------------




### fridge_10 ------------------------------------------------------------




## coffee_consumption ---------------------------------------------------




## coffee_machine --------------------------------------------------------




## cons_pet --------------------------------------------------------------




## smoking ---------------------------------------------------------------




## alcool_type -----------------------------------------------------------




## alcool_frequency ------------------------------------------------------




## marijuana_frequency ---------------------------------------------------




## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------




## social_media_time -----------------------------------------------------




## clothing_style --------------------------------------------------------




## number_tattoos --------------------------------------------------------




## chronotype ------------------------------------------------------------




## trip ------------------------------------------------------------------


