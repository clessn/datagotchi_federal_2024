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

table(data_raw$type_transport) 
data_clean$lifestyle_type_transport_clean <- NA
data_raw$type_transport <- as.numeric(data_raw$type_transport)
data_clean$lifestyle_transport_occ[data_raw$type_transport == 1] <- 1
data_clean$lifestyle_transport_occ[data_raw$type_transport == 2] <- 2
data_clean$lifestyle_transport_occ[data_raw$type_transport == 3] <- 3
data_clean$lifestyle_transport_occ[data_raw$type_transport == 4] <- 4
data_clean$lifestyle_transport_occ[data_raw$type_transport == 5] <- 5
table(data_clean$lifestyle_type_transport_clean)

## factor



## numeric



## bin

data_clean$lifestyle_car_bin <- NA
data_clean$lifestyle_car_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_car_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_car_bin)

data_clean$lifestyle_SUV_bin <- NA
data_clean$lifestyle_SUV_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_SUV_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_SUV_bin)

data_clean$lifestyle_motorcycle_bin <- NA
data_clean$lifestyle_motorcycle_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_motorcycle_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_motorcycle_bin)

data_clean$lifestyle_walking_bin <- NA
data_clean$lifestyle_walking_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_walking_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_walking_bin)

data_clean$lifestyle_bicycle_bin <- NA
data_clean$lifestyle_bicycle_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_bicycle_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_bicycle_bin)

data_clean$lifestyle_public_transport_bin <- NA
data_clean$lifestyle_public_transport_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_public_transport_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_public_transport_bin)

data_clean$lifestyle_carpooling_bin <- NA
data_clean$lifestyle_carpooling_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_carpooling_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_carpooling_bin)

data_clean$lifestyle_car_sharing_bin <- NA
data_clean$lifestyle_car_sharing_bin[data_raw$type_transport  == 1] <- 0
data_clean$lifestyle_car_sharing_bin[data_raw$type_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_car_sharing_bin)

## choice_transport ------------------------------------------------------

table(data_raw$choice_transport) 
data_clean$lifestyle_choice_transport_clean <- NA
data_raw$type_transport <- as.numeric(data_raw$type_transport)
data_clean$lifestyle_transport_occ[data_raw$choice_transport == 1] <- 1
data_clean$lifestyle_transport_occ[data_raw$choice_transport == 2] <- 2
data_clean$lifestyle_transport_occ[data_raw$choice_transport == 3] <- 3
data_clean$lifestyle_transport_occ[data_raw$choice_transport == 4] <- 4
data_clean$lifestyle_transport_occ[data_raw$choice_transport == 5] <- 5
table(data_clean$lifestyle_choice_transport_clean)

## factor



## numeric



## bin

data_clean$lifestyle_identity_bin <- NA
data_clean$lifestyle_identity_bin[data_raw$choice_transport  == 1] <- 0
data_clean$lifestyle_identity_bin[data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_identity_bin)

data_clean$lifestyle_efficiency_bin <- NA
data_clean$lifestyle_efficiency_bin[data_raw$choice_transport  == 1] <- 0
data_clean$lifestyle_efficiency_bin[data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_efficiency_bin)

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
data_clean$plifestyle_pet_ownership <- NA
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 1] <- "Chat(s)"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 2] <- "Chien(s)"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 3] <- "Chat(s) et chien(s)"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 4] <- "Diverses sortes d'animaux"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 5] <- "Autres animaux domestiques"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 6] <- "Animaux de ferme"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 7] <- "Je n'ai pas d'animal de compagnie"
data_clean$lifestyle_pet_ownership <- factor(data_clean$lifestyle_pet_ownership)
table(data_clean$lifestyle_pet_ownership)





## smoking ---------------------------------------------------------------
table(data_raw$smoking)
data_clean$lifestyle_smoke_freq <- NA
data_clean$lifestyle_smoke_freq[data_raw$smoking == 1] <- 0
data_clean$lifestyle_smoke_freq[data_raw$smoking == 2] <- 0.1667
data_clean$lifestyle_smoke_freq[data_raw$smoking == 3] <- 0.3333
data_clean$lifestyle_smoke_freq[data_raw$smoking == 4] <- 0.5
data_clean$lifestyle_smoke_freq[data_raw$smoking == 5] <- 0.6667
data_clean$lifestyle_smoke_freq[data_raw$smoking == 6] <- 0.8333
data_clean$lifestyle_smoke_freq[data_raw$smoking == 7] <- 1
table(data_clean$lifestyle_smoke_freq)


## alcool_type -----------------------------------------------------------
data_clean$lifestyle_favourite_alcool <- NA
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 1] <- "Vin rouge"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 2] <- "Vin blanc"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 3] <- "Vin rosé"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 4] <- "Vin mousseux ou champagne"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 5] <- "Bière régulière"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 6] <- "Bière artisanale et de microbrasserie"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 7] <- "Boissons spiritueuses"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 8] <- "Cocktails ou prêts-à-boire"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 9] <- "Je ne consomme pas d'alcool"
data_clean$lifestyle_favourite_alcool <- factor(data_clean$lifestyle_avourite_alcool)
table(data_clean$lifestyle_favourite_alcool)


## alcool_frequency ------------------------------------------------------
data_clean$lifestyle_alcool_freq <- NA
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 1] <- 0
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 2] <- 0.1667
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 3] <- 0.3333
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 4] <- 0.5
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 5] <- 0.6667
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 6] <- 0.8333
data_clean$lifestyle_alcool_freq[data_raw$alcool_frequency == 7] <- 1
table(data_clean$lifestyle_alcool_freq)


## marijuana_frequency ---------------------------------------------------
data_clean$lifestyle_mari_freq <- NA
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 1] <- 0
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 2] <- 0.1667
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 3] <- 0.3333
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 4] <- 0.5
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 5] <- 0.6667
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 6] <- 0.8333
data_clean$lifestyle_mari_freq[data_raw$marijuana_frequency == 7] <- 1
table(data_clean$lifestyle_mari_freq)



## musical_band ----------------------------------------------------------
table(data_raw$musical_band)
data_clean$favourite_band <- NA
data_clean$favourite_band[data_raw$musical_band == 1] <- 1
table(data_clean$favourite_band)



## musical_style ---------------------------------------------------------
table(data_raw$musical_style)
data_clean$fav_music_style <- NA
data_clean$fav_music_style[data_raw$musical_style == 1] <- 1
table(data_clean$fav_music_style)



## movie_preference ------------------------------------------------------
table(data_raw$movie_preference)
data_clean$favourite_movie <- NA
data_clean$favourite_movie[data_raw$movie_preference == 1] <- 1
table(data_clean$favourite_movie)



## social_media_use ------------------------------------------------------
data_clean$lifestyle_medsociaux_plus_frequent <- NA
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 1] <- "Facebook"
data_clean$lifestyle_fmedsociaux_plus_frequent[data_raw$social_media_use == 2] <- "Twitter / X"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 3] <- "Instagram"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 4] <- "Snapchat"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use== 5] <- "TikTok"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 6] <- "Pinterest"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 7] <- "LinkedIn"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 8] <- "Youtube"
data_clean$lifestyle_medsociaux_plus_frequent[data_raw$social_media_use == 9] <- "Autre (veuillez préciser)"
data_clean$lifestyle_favourite_alcool <- factor(data_clean$lifestyle_avourite_alcool)
table(data_clean$lifestyle_medsociaux_plus_frequent)

## social_media_time -----------------------------------------------------
data_clean$lifestyle_tempsparjour_medsociaux <- NA
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 1] <- 0
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 2] <- 0.2
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 3] <- 0.4
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 4] <- 0.6
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 5] <- 0.8
data_clean$lifestyle_tempsparjour_medsociaux[data_raw$social_media_time == 6] <- 1
table(data_clean$lifestyle_tempsparjour_medsociaux)



## clothing_style --------------------------------------------------------

attributes(data_raw$clothing_style)
table(data_raw$clothing_style)

## disaggregated
data_clean$lifestyle_clothing_style <- NA
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 1] <- "formal"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 2] <- "classic"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 3] <- "casual"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 4] <- "sport"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 5] <- "elegant"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 6] <- "hippie"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 7] <- "punk"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 8] <- "rock"
data_clean$lifestyle_clothing_style[data_raw$clothing_style == 9] <- "other"
data_clean$lifestyle_clothing_style <- factor(data_clean$lifestyle_clothing_style)
table(data_clean$lifestyle_clothing_style)

## grouped
data_clean$lifestyle_clothing_style_grouped <- NA
data_clean$lifestyle_clothing_style_grouped[data_raw$clothing_style %in% c(1, 5)] <- "formal"
data_clean$lifestyle_clothing_style_grouped[data_raw$clothing_style %in% c(2, 3, 4)] <- "easygoing"
data_clean$lifestyle_clothing_style_grouped[data_raw$clothing_style %in% c(6, 7, 8)] <- "edgy"
data_clean$lifestyle_clothing_style_grouped <- factor(data_clean$lifestyle_clothing_style_grouped)
table(data_clean$lifestyle_clothing_style_grouped)

## number_tattoos --------------------------------------------------------

attributes(data_raw$number_tattoos)
table(data_raw$number_tattoos)

data_clean$lifestyle_number_tattoos <- NA
data_clean$lifestyle_number_tattoos <- data_raw$number_tattoos
data_clean$lifestyle_number_tattoos <- as.numeric(data_clean$lifestyle_number_tattoos)
table(data_clean$lifestyle_number_tattoos)

# Créer la variable binaire 'has_tattoos'
data_clean$lifestyle_has_tattoos <- as.numeric(data_clean$lifestyle_number_tattoos > 0)
data_clean$lifestyle_has_tattoos[is.na(data_clean$lifestyle_number_tattoos)] <- NA


table(data_clean$lifestyle_has_tattoos, useNA = "ifany")




## chronotype ------------------------------------------------------------

attributes(data_raw$chronotype)
table(data_raw$chronotype)

data_clean$lifestyle_chronotype <- NA
data_clean$lifestyle_chronotype[data_raw$chronotype == 1] <- 0
data_clean$lifestyle_chronotype[data_raw$chronotype == 2] <- 0.25
data_clean$lifestyle_chronotype[data_raw$chronotype == 3] <- 0.5
data_clean$lifestyle_chronotype[data_raw$chronotype == 4] <- 0.75
data_clean$lifestyle_chronotype[data_raw$chronotype == 5] <- 1
table(data_clean$lifestyle_chronotype)
  




## trip ------------------------------------------------------------------

attributes(data_raw$trip)
table(data_raw$trip)

data_clean$lifestyle_trip <- NA
data_clean$lifestyle_trip[data_raw$trip == 1] <- "beach"
data_clean$lifestyle_trip[data_raw$trip == 2] <- "jungle"
data_clean$lifestyle_trip[data_raw$trip == 3] <- "historic"
data_clean$lifestyle_trip[data_raw$trip == 4] <- "mountains"
data_clean$lifestyle_trip <- factor(data_clean$lifestyle_trip)
table(data_clean$lifestyle_trip)




