# lifestyle

## exercise --------------------------------------------------------------

attributes(data_raw$exercise)
table(data_raw$exercise)
data_clean$lifestyle_exercise <- NA
data_clean$lifestyle_exercise[data_raw$exercise == 1] <- "gym"
data_clean$lifestyle_exercise[data_raw$exercise == 2] <- "play_a_team_sport"
data_clean$lifestyle_exercise[data_raw$exercise == 3] <- "walk"
data_clean$lifestyle_exercise[data_raw$exercise == 4] <- "run"
data_clean$lifestyle_exercise[data_raw$exercise == 5] <- "yoga"
data_clean$lifestyle_exercise[data_raw$exercise == 6] <- "swimming"
data_clean$lifestyle_exercise[data_raw$exercise == 7] <- "other"
data_clean$lifestyle_exercise[data_raw$exercise == 8] <- "i_do_not_exercise"
table(data_clean$lifestyle_exercise)


## activity_1 ------------------------------------------------------------

attributes(data_raw$activity_1)
table(data_raw$activity_1) 
data_clean$lifestyle_goFishingFreq <- NA
data_raw$activity_1 <- as.numeric(data_raw$activity_1)
data_clean$lifestyle_goFishingFreq[data_raw$activity_1 == 1] <- 1
data_clean$lifestyle_goFishingFreq[data_raw$activity_1 == 2] <- 2
data_clean$lifestyle_goFishingFreq[data_raw$activity_1 == 3] <- 3
data_clean$lifestyle_goFishingFreq[data_raw$activity_1 == 4] <- 4
data_clean$lifestyle_goFishingFreq[data_raw$activity_1 == 5] <- 5
table(data_clean$lifestyle_goFishingFreq)

## factor

table(data_clean$lifestyle_fishing_freq)
data_clean$lifestyle_goFishingFreq_factor <- NA
data_clean$lifestyle_goFishingFreq_factor[data_raw$activity_1 == 1] <- "never"
data_clean$lifestyle_goFishingFreq_factor[data_raw$activity_1 == 2] <- "almost_never"
data_clean$lifestyle_goFishingFreq_factor[data_raw$activity_1 == 3] <- "sometimes"
data_clean$lifestyle_goFishingFreq_factor[data_raw$activity_1 == 4] <- "often"
data_clean$lifestyle_goFishingFreq_factor[data_raw$activity_1 == 5] <- "very_often"
data_clean$lifestyle_goFishingFreq_factor <- factor(data_clean$lifestyle_goFishingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_goFishingFreq_factor)

## numeric

data_clean$lifestyle_goFishingFreq_numeric <- NA 
data_clean$lifestyle_goFishingFreq_numeric <- (data_raw$activity_1 - 1) / 4
table(data_clean$lifestyle_goFishingFreq_numeric)

## bin

data_clean$lifestyle_goFishingFreq_bin <- NA
data_clean$lifestyle_goFishingFreq_bin[data_raw$activity_1  == 1] <- 0
data_clean$lifestyle_goFishingFreq_bin[data_raw$activity_1  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_goFishingFreq_bin) 

## activity_2 ------------------------------------------------------------

table(data_raw$activity_2) 
data_clean$lifestyle_goHuntingFreq <- NA
data_raw$activity_2 <- as.numeric(data_raw$activity_2)
data_clean$lifestyle_goHuntingFreq[data_raw$activity_2 == 1] <- 1
data_clean$lifestyle_goHuntingFreq[data_raw$activity_2 == 2] <- 2
data_clean$lifestyle_goHuntingFreq[data_raw$activity_2 == 3] <- 3
data_clean$lifestyle_goHuntingFreq[data_raw$activity_2 == 4] <- 4
data_clean$lifestyle_goHuntingFreq[data_raw$activity_2 == 5] <- 5
table(data_clean$lifestyle_goHuntingFreq)

## factor

table(data_clean$lifestyle_goHuntingFreq)
data_clean$lifestyle_goHuntingFreq_factor <- NA
data_clean$lifestyle_goHuntingFreq_factor[data_raw$activity_2 == 1] <- "never"
data_clean$lifestyle_goHuntingFreq_factor[data_raw$activity_2 == 2] <- "almost_never"
data_clean$lifestyle_goHuntingFreq_factor[data_raw$activity_2 == 3] <- "sometimes"
data_clean$lifestyle_goHuntingFreq_factor[data_raw$activity_2 == 4] <- "often"
data_clean$lifestyle_goHuntingFreq_factor[data_raw$activity_2 == 5] <- "very_often"
data_clean$lifestyle_goHuntingFreq_factor <- factor(data_clean$lifestyle_goHuntingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_goHuntingFreq_factor)

## numeric

data_clean$lifestyle_goHuntingFreq_numeric <- NA 
data_clean$lifestyle_goHuntingFreq_numeric <- (data_raw$activity_2 - 1) / 4
table(data_clean$lifestyle_goHuntingFreq_numeric)

## bin

data_clean$lifestyle_goHuntingFreq_bin <- NA
data_clean$lifestyle_goHuntingFreq_bin[data_raw$activity_2  == 1] <- 0
data_clean$lifestyle_goHuntingFreq_bin[data_raw$activity_2  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_goHuntingFreq_bin) 


## activity_3 ------------------------------------------------------------

table(data_raw$activity_3) 
data_clean$lifestyle_snowSportsFreq <- NA
data_raw$activity_3 <- as.numeric(data_raw$activity_3)
data_clean$lifestyle_snowSportsFreq[data_raw$activity_3 == 1] <- 1
data_clean$lifestyle_snowSportsFreq[data_raw$activity_3 == 2] <- 2
data_clean$lifestyle_snowSportsFreq[data_raw$activity_3 == 3] <- 3
data_clean$lifestyle_snowSportsFreq[data_raw$activity_3 == 4] <- 4
data_clean$lifestyle_snowSportsFreq[data_raw$activity_3 == 5] <- 5
table(data_clean$lifestyle_snowSportsFreq)

## factor

table(data_clean$lifestyle_snowSportsFreq)
data_clean$lifestyle_snowSportsFreq_factor <- NA
data_clean$lifestyle_snowSportsFreq_factor[data_raw$activity_3 == 1] <- "never"
data_clean$lifestyle_snowSportsFreq_factor[data_raw$activity_3 == 2] <- "almost_never"
data_clean$lifestyle_snowSportsFreq_factor[data_raw$activity_3 == 3] <- "sometimes"
data_clean$lifestyle_snowSportsFreq_factor[data_raw$activity_3 == 4] <- "often"
data_clean$lifestyle_snowSportsFreq_factor[data_raw$activity_3 == 5] <- "very_often"
data_clean$lifestyle_snowSportsFreq_factor <- factor(data_clean$lifestyle_snowSportsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_snowSportsFreq_factor)

## numeric

data_clean$lifestyle_snowSportsFreq_numeric <- NA 
data_clean$lifestyle_snowSportsFreq_numeric <- (data_raw$activity_3 - 1) / 4
table(data_clean$ifestyle_snowSportsFreq_numeric)

## bin

data_clean$lifestyle_snowSportsFreq_bin <- NA
data_clean$lifestyle_snowSportsFreq_bin[data_raw$activity_3  == 1] <- 0
data_clean$lifestyle_snowSportsFreq_bin[data_raw$activity_3  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_snowSportsFreq_bin) 


## activity_4 -----------------------------------------------------------

table(data_raw$activity_4) 
data_clean$lifestyle_teamSportsFreq <- NA
data_raw$activity_4 <- as.numeric(data_raw$activity_4)
data_clean$lifestyle_teamSportsFreq[data_raw$activity_4 == 1] <- 1
data_clean$lifestyle_teamSportsFreq[data_raw$activity_4 == 2] <- 2
data_clean$lifestyle_teamSportsFreq[data_raw$activity_4 == 3] <- 3
data_clean$lifestyle_teamSportsFreq[data_raw$activity_4 == 4] <- 4
data_clean$lifestyle_teamSportsFreq[data_raw$activity_4 == 5] <- 5
table(data_clean$lifestyle_teamSportsFreq)

## factor

table(data_clean$lifestyle_teamSportsFreq)
data_clean$lifestyle_teamSportsFreq_factor <- NA
data_clean$lifestyle_teamSportsFreq_factor[data_raw$activity_4 == 1] <- "never"
data_clean$lifestyle_teamSportsFreq_factor[data_raw$activity_4 == 2] <- "almost_never"
data_clean$lifestyle_teamSportsFreq_factor[data_raw$activity_4 == 3] <- "sometimes"
data_clean$lifestyle_teamSportsFreq_factor[data_raw$activity_4 == 4] <- "often"
data_clean$lifestyle_teamSportsFreq_factor[data_raw$activity_4 == 5] <- "very_often"
data_clean$lifestyle_teamSportsFreq_factor <- factor(data_clean$lifestyle_teamSportsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_teamSportsFreq_factor)

## numeric

data_clean$lifestyle_teamSportsFreq_numeric <- (data_raw$activity_4 - 1) / 4
data_clean$lifestyle_teamSportsFreq_numeric <- NA 
table(data_clean$lifestyle_teamSportsFreq_numeric)

## bin

data_clean$lifestyle_teamSportsFreq_bin <- NA
data_clean$lifestyle_teamSportsFreq_bin[data_raw$activity_4  == 1] <- 0
data_clean$lifestyle_teamSportsFreq_bin[data_raw$activity_4  %in% c(2, 3, 4, 5)] <- 1
table(data_cleanlifestyle_teamSportsFreq_bin)


## activity_5 ------------------------------------------------------------

table(data_raw$activity_5) 
data_clean$lifestyle_goMuseumsFreq <- NA
data_raw$activity_5 <- as.numeric(data_raw$activity_5)
data_clean$lifestyle_goMuseumsFreq[data_raw$activity_5 == 1] <- 1
data_clean$lifestyle_goMuseumsFreq[data_raw$activity_5 == 2] <- 2
data_clean$lifestyle_goMuseumsFreq[data_raw$activity_5 == 3] <- 3
data_clean$lifestyle_goMuseumsFreq[data_raw$activity_5 == 4] <- 4
data_clean$lifestyle_goMuseumsFreq[data_raw$activity_5 == 5] <- 5
table(data_clean$lifestyle_goMuseumsFreq)

## factor

table(data_clean$lifestyle_goMuseumsFreq)
data_clean$lifestyle_goMuseumsFreq_factor <- NA
data_clean$lifestyle_goMuseumsFreq_factor[data_raw$activity_5 == 1] <- "never"
data_clean$lifestyle_goMuseumsFreq_factor[data_raw$activity_5 == 2] <- "almost_never"
data_clean$lifestyle_goMuseumsFreq_factor[data_raw$activity_5 == 3] <- "sometimes"
data_clean$lifestyle_goMuseumsFreq_factor[data_raw$activity_5 == 4] <- "often"
data_clean$lifestyle_goMuseumsFreq_factor[data_raw$activity_5 == 5] <- "very_often"
data_clean$lifestyle_goMuseumsFreq_factor <- factor(data_clean$lifestyle_goMuseumsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_goMuseumsFreq_factor)

## numeric

data_clean$lifestyle_goMuseumsFreq_numeric <- NA 
data_clean$lifestyle_goMuseumsFreq_numeric <- (data_raw$activity_5 - 1) / 4
table(data_clean$lifestyle_goMuseumsFreq_numeric)

## bin

data_clean$lifestyle_goMuseumsFreq_bin <- NA
data_clean$lifestyle_goMuseumsFreq_bin[data_raw$activity_5  == 1] <- 0
data_clean$lifestyle_goMuseumsFreq_bin[data_raw$activity_5  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_goMuseumsFreq_bin)


## activity_6 ------------------------------------------------------------

table(data_raw$activity_6) 
data_clean$lifestyle_performingArtsFreq <- NA
data_raw$activity_6 <- as.numeric(data_raw$activity_6)
data_clean$lifestyle_performingArtsFreq[data_raw$activity_6 == 1] <- 1
data_clean$lifestyle_performingArtsFreq[data_raw$activity_6 == 2] <- 2
data_clean$lifestyle_performingArtsFreq[data_raw$activity_6 == 3] <- 3
data_clean$lifestyle_performingArtsFreq[data_raw$activity_6 == 4] <- 4
data_clean$lifestyle_performingArtsFreq[data_raw$activity_6 == 5] <- 5
table(data_clean$lifestyle_performingArtsFreq)

## factor

table(data_clean$lifestyle_performingArtsFreq)
data_clean$lifestyle_performingArtsFreq_factor <- NA
data_clean$lifestyle_performingArtsFreq_factor[data_raw$activity_6 == 1] <- "never"
data_clean$lifestyle_performingArtsFreq_factor[data_raw$activity_6 == 2] <- "almost_never"
data_clean$lifestyle_performingArtsFreq_factor[data_raw$activity_6 == 3] <- "sometimes"
data_clean$lifestyle_performingArtsFreq_factor[data_raw$activity_6 == 4] <- "often"
data_clean$lifestyle_performingArtsFreq_factor[data_raw$activity_6 == 5] <- "very_often"
data_clean$lifestyle_performingArtsFreq_factor <- factor(data_clean$lifestyle_performingArtsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_performingArtsFreq_factor)

## numeric

data_clean$lifestyle_performingArtsFreq_numeric <- NA 
data_clean$lifestyle_performingArtsFreq_numeric <- (data_raw$activity_6 - 1) / 4
table(data_clean$lifestyle_performingArtsFreq_numeric)

## bin

data_clean$lifestyle_performingArtsFreq_bin <- NA
data_clean$lifestyle_performingArtsFreq_bin[data_raw$activity_6  == 1] <- 0
data_clean$lifestyle_performingArtsFreq_bin[data_raw$activity_6  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_performingArtsFreq_bin)


## activity_7 ------------------------------------------------------------

table(data_raw$activity_7) 
data_clean$lifestyle_manualTaskFreq <- NA
data_raw$activity_7 <- as.numeric(data_raw$activity_7)
data_clean$lifestyle_manualTaskFreq[data_raw$activity_7 == 1] <- 1
data_clean$lifestyle_manualTaskFreq[data_raw$activity_7 == 2] <- 2
data_clean$lifestyle_manualTaskFreq[data_raw$activity_7 == 3] <- 3
data_clean$lifestyle_manualTaskFreq[data_raw$activity_7 == 4] <- 4
data_clean$lifestyle_manualTaskFreq[data_raw$activity_7 == 5] <- 5
table(data_clean$lifestyle_manualTaskFreq)

## factor

table(data_clean$lifestyle_manualTaskFreq)
data_clean$lifestyle_manualTaskFreq_factor <- NA
data_clean$lifestyle_manualTaskFreq_factor[data_raw$activity_7 == 1] <- "never"
data_clean$lifestyle_manualTaskFreq_factor[data_raw$activity_7 == 2] <- "almost_never"
data_clean$lifestyle_manualTaskFreq_factor[data_raw$activity_7 == 3] <- "sometimes"
data_clean$lifestyle_manualTaskFreq_factor[data_raw$activity_7 == 4] <- "often"
data_clean$lifestyle_manualTaskFreq_factor[data_raw$activity_7 == 5] <- "very_often"
data_clean$lifestyle_manualTaskFreq_factor <- factor(data_clean$lifestyle_manualTaskFreq_factor,
                                                           levels = c("never",
                                                                      "almost_never",
                                                                      "sometimes",
                                                                      "often",
                                                                      "very_often"),
                                                           ordered = TRUE)
table(data_clean$lifestyle_manualTaskFreq_factor)

## numeric

data_clean$lifestyle_manualTaskFreq_numeric <- NA 
data_clean$lifestyle_manualTaskFreq_numeric <- (data_raw$activity_7 - 1) / 4
table(data_clean$lifestyle_manualTaskFreq_numeric)

## bin

data_clean$lifestyle_manualTaskFreq_bin <- NA
data_clean$lifestyle_manualTaskFreq_bin[data_raw$activity_7  == 1] <- 0
data_clean$lifestyle_manualTaskFreq_bin[data_raw$activity_7  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_manualTaskFreq_bin)


## activity_8 ------------------------------------------------------------

table(data_raw$activity_8) 
data_clean$lifestyle_motorizedActFreq <- NA
data_raw$activity_8 <- as.numeric(data_raw$activity_8)
data_clean$lifestyle_motorizedActFreq[data_raw$activity_8 == 1] <- 1
data_clean$lifestyle_motorizedActFreq[data_raw$activity_8 == 2] <- 2
data_clean$lifestyle_motorizedActFreq[data_raw$activity_8 == 3] <- 3
data_clean$lifestyle_motorizedActFreq[data_raw$activity_8 == 4] <- 4
data_clean$lifestyle_motorizedActFreq[data_raw$activity_8 == 5] <- 5
table(data_clean$lifestyle_motorizedActFreq)

## factor

table(data_clean$lifestyle_motorizedActFreq)
data_clean$lifestyle_motorizedActFreq_factor <- NA
data_clean$lifestyle_motorizedActFreq_factor[data_raw$activity_8 == 1] <- "never"
data_clean$lifestyle_motorizedActFreq_factor[data_raw$activity_8 == 2] <- "almost_never"
data_clean$lifestyle_motorizedActFreq_factor[data_raw$activity_8 == 3] <- "sometimes"
data_clean$lifestyle_motorizedActFreq_factor[data_raw$activity_8 == 4] <- "often"
data_clean$lifestyle_motorizedActFreq_factor[data_raw$activity_8 == 5] <- "very_often"
data_clean$lifestyle_motorizedActFreq_factor <- factor(data_clean$lifestyle_motorizedActFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(data_clean$lifestyle_motorizedActFreq_factor)

## numeric

data_clean$lifestyle_motorizedActFreq_numeric <- NA 
data_clean$lifestyle_motorizedActFreq_numeric <- (data_raw$activity_8 - 1) / 4
table(data_clean$lifestyle_motorizedActFreq_numeric)

## bin

data_clean$lifestyle_motorizedActFreq_bin <- NA
data_clean$lifestyle_motorizedActFreq_bin[data_raw$activity_8  == 1] <- 0
data_clean$lifestyle_motorizedActFreq_bin[data_raw$activity_8  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_motorizedActFreq_bin)


## activity_9 ------------------------------------------------------------

table(data_raw$activity_9) 
data_clean$lifestyle_unmotorizedActFreq <- NA
data_raw$activity_9 <- as.numeric(data_raw$activity_9)
data_clean$lifestyle_unmotorized_activities_freq[data_raw$activity_9 == 2] <- 2
data_clean$lifestyle_unmotorizedActFreq[data_raw$activity_9 == 1] <- 1
data_clean$lifestyle_unmotorizedActFreq[data_raw$activity_9 == 3] <- 3
data_clean$lifestyle_unmotorizedActFreq[data_raw$activity_9 == 4] <- 4
data_clean$lifestyle_unmotorizedActFreq[data_raw$activity_9 == 5] <- 5
table(data_clean$lifestyle_unmotorizedActFreq)

## factor

table(data_clean$lifestyle_unmotorizedActFreq)
data_clean$lifestyle_unmotorizedActFreq_factor <- NA
data_clean$lifestyle_unmotorizedActFreq_factor[data_raw$activity_9 == 1] <- "never"
data_clean$lifestyle_unmotorizedActFreq_factor[data_raw$activity_9 == 2] <- "almost_never"
data_clean$lifestyle_unmotorizedActFreq_factor[data_raw$activity_9 == 3] <- "sometimes"
data_clean$lifestyle_unmotorizedActFreq_factor[data_raw$activity_9 == 4] <- "often"
data_clean$lifestyle_unmotorizedActFreq_factor[data_raw$activity_9 == 5] <- "very_often"
data_clean$lifestyle_unmotorizedActFreq_factor <- factor(data_clean$lifestyle_unmotorizedActFreq_factor,
                                                                levels = c("never",
                                                                           "almost_never",
                                                                           "sometimes",
                                                                           "often",
                                                                           "very_often"),
                                                                ordered = TRUE)
table(data_clean$lifestyle_unmotorizedActFreq_factor)

## numeric

data_clean$lifestyle_unmotorizedActFreq_numeric <- NA 
data_clean$lifestyle_unmotorizedActFreq_numeric <- (data_raw$activity_9 - 1) / 4
table(data_clean$lifestyle_unmotorizedActFreq_numeric)

## bin

data_clean$lifestyle_unmotorizedActFreq_bin <- NA
data_clean$lifestyle_unmotorizedActFreq_bin[data_raw$activity_9  == 1] <- 0
data_clean$lifestyle_unmotorizedActFreq_bin[data_raw$activity_9  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_unmotorizedActFreq_bin)


## activity_10 -----------------------------------------------------------

table(data_raw$activity_10) 
data_clean$lifestyle_volunteeringFreq <- NA
data_raw$activity_10 <- as.numeric(data_raw$activity_10)
data_clean$lifestyle_volunteeringFreq[data_raw$activity_10 == 1] <- 1
data_clean$lifestyle_volunteeringFreq[data_raw$activity_10 == 2] <- 2
data_clean$lifestyle_volunteeringFreq[data_raw$activity_10 == 3] <- 3
data_clean$lifestyle_volunteeringFreq[data_raw$activity_10 == 4] <- 4
data_clean$lifestyle_volunteeringFreq[data_raw$activity_10 == 5] <- 5
table(data_clean$lifestyle_volunteeringFreq)

## factor

table(data_clean$lifestyle_volunteeringFreq)
data_clean$lifestyle_volunteeringFreq_factor <- NA
data_clean$lifestyle_volunteeringFreq_factor[data_raw$activity_10 == 1] <- "never"
data_clean$lifestyle_volunteeringFreq_factor[data_raw$activity_10 == 2] <- "almost_never"
data_clean$lifestyle_volunteeringFreq_factor[data_raw$activity_10 == 3] <- "sometimes"
data_clean$lifestyle_volunteeringFreq_factor[data_raw$activity_10 == 4] <- "often"
data_clean$lifestyle_volunteeringFreq_factor[data_raw$activity_10 == 5] <- "very_often"
data_clean$lifestyle_volunteeringFreq_factor <- factor(data_clean$lifestyle_volunteeringFreq_factor,
                                                                  levels = c("never",
                                                                             "almost_never",
                                                                             "sometimes",
                                                                             "often",
                                                                             "very_often"),
                                                                  ordered = TRUE)
table(data_clean$lifestyle_volunteeringFreq_factor)

## numeric

data_clean$lifestyle_volunteeringFreq_numeric <- NA 
data_clean$lifestyle_volunteeringFreq_numeric <- (data_raw$activity_10 - 1) / 4
table(data_clean$lifestyle_volunteeringFreq_numeric)

## bin

data_clean$lifestyle_volunteeringFreq_bin <- NA
data_clean$lifestyle_volunteeringFreq_bin[data_raw$activity_10  == 1] <- 0
data_clean$lifestyle_volunteeringFreq_bin[data_raw$activity_10  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_volunteeringFreq_bin)


## activity_11 -----------------------------------------------------------

table(data_raw$activity_11) 
data_clean$lifestyle_makeArtFreq <- NA
data_raw$activity_11 <- as.numeric(data_raw$activity_11)
data_clean$lifestyle_makeArtFreq[data_raw$activity_11 == 1] <- 1
data_clean$lifestyle_makeArtFreq[data_raw$activity_11 == 2] <- 2
data_clean$lifestyle_makeArtFreq[data_raw$activity_11 == 3] <- 3
data_clean$lifestyle_makeArtFreq[data_raw$activity_11 == 4] <- 4
data_clean$lifestyle_makeArtFreq[data_raw$activity_11 == 5] <- 5
table(data_clean$lifestyle_makeArtFreq)

## factor

table(data_clean$lifestyle_makeArtFreq)
data_clean$lifestyle_makeArtFreq_factor <- NA
data_clean$lifestyle_makeArtFreq_factor[data_raw$activity_11 == 1] <- "never"
data_clean$lifestyle_makeArtFreq_factor[data_raw$activity_11 == 2] <- "almost_never"
data_clean$lifestyle_makeArtFreq_factor[data_raw$activity_11 == 3] <- "sometimes"
data_clean$lifestyle_makeArtFreq_factor[data_raw$activity_11 == 4] <- "often"
data_clean$lifestyle_makeArtFreq_factor[data_raw$activity_11 == 5] <- "very_often"
data_clean$lifestyle_makeArtFreq_factor <- factor(data_clean$lifestyle_makeArtFreq_factor,
                                                        levels = c("never",
                                                                   "almost_never",
                                                                   "sometimes",
                                                                   "often",
                                                                   "very_often"),
                                                        ordered = TRUE)
table(data_clean$lifestyle_makeArtFreq_factor)

## numeric

data_clean$lifestyle_makeArtFreq_numeric <- NA 
data_clean$lifestyle_makeArtFreq_numeric <- (data_raw$activity_11 - 1) / 4
table(data_clean$lifestyle_makeArtFreq_numeric)

## bin

data_clean$lifestyle_makeArtFreq_bin <- NA
data_clean$lifestyle_makeArtFreq_bin[data_raw$activity_11  == 1] <- 0
data_clean$lifestyle_makeArtFreq_bin[data_raw$activity_11  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$llifestyle_makeArtFreq_bin)


## activity_12 -----------------------------------------------------------

table(data_raw$activity_12) 
data_clean$lifestyle_travelFreq <- NA
data_raw$activity_12 <- as.numeric(data_raw$activity_12)
data_clean$lifestyle_travelFreq[data_raw$activity_12 == 1] <- 1
data_clean$lifestyle_travelFreq[data_raw$activity_12 == 2] <- 2
data_clean$lifestyle_travelFreq[data_raw$activity_12 == 3] <- 3
data_clean$lifestyle_travelFreq[data_raw$activity_12 == 4] <- 4
data_clean$lifestyle_travelFreq[data_raw$activity_12 == 5] <- 5
table(data_clean$lifestyle_travelFreq)

## factor

table(data_clean$lifestyle_travelFreq)
data_clean$lifestyle_travelFreq_factor <- NA
data_clean$lifestyle_travelFreq_factor[data_raw$activity_12 == 1] <- "never"
data_clean$lifestyle_travelFreq_factor[data_raw$activity_12 == 2] <- "almost_never"
data_clean$lifestyle_travelFreq_factor[data_raw$activity_12 == 3] <- "sometimes"
data_clean$lifestyle_travelFreq_factor[data_raw$activity_12 == 4] <- "often"
data_clean$lifestyle_travelFreq_factor[data_raw$activity_12 == 5] <- "very_often"
data_clean$lifestyle_travelFreq_factor <- factor(data_clean$lifestyle_travelFreq_factor,
                                                          levels = c("never",
                                                                     "almost_never",
                                                                     "sometimes",
                                                                     "often",
                                                                     "very_often"),
                                                          ordered = TRUE)
table(data_clean$lifestyle_travelFreq_factor)

## numeric

data_clean$lifestyle_travelFreq_numeric <- NA 
data_clean$lifestyle_travelFreq_numeric <- (data_raw$activity_12 - 1) / 4
table(data_clean$lifestyle_travelFreq_numeric)

## bin

data_clean$lifestyle_travelFreq_bin <- NA
data_clean$lifestyle_travelFreq_bin[data_raw$activity_12  == 1] <- 0
data_clean$lifestyle_travelFreq_bin[data_raw$activity_12  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_travelFreq_bin)


## activity_13 -----------------------------------------------------------

table(data_raw$activity_13) 
data_clean$lifestyle_gamingFreq <- NA
data_raw$activity_13 <- as.numeric(data_raw$activity_13)
data_clean$lifestyle_gamingFreq[data_raw$activity_13 == 1] <- 1
data_clean$lifestyle_gamingFreq[data_raw$activity_13 == 2] <- 2
data_clean$lifestyle_gamingFreq[data_raw$activity_13 == 3] <- 3
data_clean$lifestyle_gamingFreq[data_raw$activity_13 == 4] <- 4
data_clean$lifestyle_gamingFreq[data_raw$activity_13 == 5] <- 5
table(data_clean$lifestyle_gamingFreq)

## factor

table(data_clean$lifestyle_gamingFreq)
data_clean$lifestyle_gamingFreq_factor <- NA
data_clean$lifestyle_gamingFreq_factor[data_raw$activity_13 == 1] <- "never"
data_clean$lifestyle_gamingFreq_factor[data_raw$activity_13 == 2] <- "almost_never"
data_clean$lifestyle_gamingFreq_factor[data_raw$activity_13 == 3] <- "sometimes"
data_clean$lifestyle_gamingFreq_factor[data_raw$activity_13 == 4] <- "often"
data_clean$lifestyle_gamingFreq_factor[data_raw$activity_13 == 5] <- "very_often"
data_clean$lifestyle_gamingFreq_factor <- factor(data_clean$lifestyle_gamingFreq_factor,
                                                     levels = c("never",
                                                                "almost_never",
                                                                "sometimes",
                                                                "often",
                                                                "very_often"),
                                                     ordered = TRUE)
table(data_clean$lifestyle_gamingFreq_factor)

## numeric

data_clean$lifestyle_gamingFreq_numeric <- NA 
data_clean$lifestyle_gamingFreq_numeric <- (data_raw$activity_13 - 1) / 4
table(data_clean$lifestyle_gamingFreq_numeric)

## bin

data_clean$lifestyle_gamingFreq_bin <- NA
data_clean$lifestyle_gamingFreq_bin[data_raw$activity_13  == 1] <- 0
data_clean$lifestyle_gamingFreq_bin[data_raw$activity_13  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$llifestyle_gamingFreq_bin)


## type_transport --------------------------------------------------------

attributes(data_raw$type_transport)
table(data_raw$type_transport) 
data_clean$lifestyle_transport_clean <- NA
data_raw$type_transport <- as.numeric(data_raw$type_transport)
data_clean$lifestyle_transport_clean[data_raw$type_transport == 1] <- "car"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 2] <- "car"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 3] <- "car"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 4] <- "active_transport"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 5] <- "active_transport"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 6] <- "shared_transport"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 7] <- "shared_transport"
data_clean$lifestyle_transport_clean[data_raw$type_transport == 8] <- "shared_transport"
table(data_clean$lifestyle_transport_clean)

## factor



## numeric



## bin

## choice_transport ------------------------------------------------------

attributes(data_raw$choice_transport)
table(data_raw$choice_transport) 
data_clean$lifestyle_choice_transport_clean <- NA
data_raw$choice_transport <- as.numeric(data_raw$choice_transport)
data_clean$lifestyle_choice_transport_clean[data_raw$choice_transport == 1] <- "part_of_who_i_am"
data_clean$lifestyle_choice_transport_clean[data_raw$choice_transport == 2] <- "point_a_to_point_b"
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

table(data_raw$field_occupation)
data_clean$lifestyle_work_field <- NA
data_clean$lifestyle_work_field[data_raw$field_occupation == 1 | data_raw$field_occupation == 2 | data_raw$field_occupation == 7 | data_raw$field_occupation == 8] <- "economics"
data_clean$lifestyle_work_field[data_raw$field_occupation == 3] <- "natural_sciences"
data_clean$lifestyle_work_field[data_raw$field_occupation == 4] <- "health"
data_clean$lifestyle_work_field[data_raw$field_occupation == 5] <- "education_law_politics"
data_clean$lifestyle_work_field[data_raw$field_occupation == 6] <- "culture_sports"
data_clean$lifestyle_work_field[data_raw$field_occupation == 9] <- "agriculture"
data_clean$lifestyle_work_field[data_raw$field_occupation == 10] <- "manufacturing"
data_clean$lifestyle_work_field <- factor(data_clean$lifestyle_work_field,
                                                   levels = c("economics",
                                                              "natural_sciences",
                                                              "health",
                                                              "education_law_politics",
                                                              "culture_sports",
                                                              "agriculture",
                                                              "manufacturing"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_work_field)

## type_occupation -------------------------------------------------------

attributes(data_raw$type_occupation)
table(data_raw$type_occupation)
data_clean$lifestyle_type_occ <- NA
data_clean$lifestyle_type_occ[data_raw$type_occupation == 1] <- "labour_job"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 2] <- "intermediate_job"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 3] <- "technical_job"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 4] <- "professional_job"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 5] <- "management_job"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 6] <- "other"
data_clean$lifestyle_type_occ[data_raw$type_occupation == 7] <- "no_job"
data_clean$lifestyle_type_occ <- factor(data_clean$lifestyle_type_occ,
                                         levels = c("labour_job",
                                                    "intermediate_job",
                                                    "technical_job",
                                                    "professional_job",
                                                    "management_job",
                                                    "other",
                                                    "no_job"),
                                         ordered = TRUE)
table(data_clean$lifestyle_type_occ)

## clothes_consumption ---------------------------------------------------

attributes(data_raw$clothes_consumption)
table(data_raw$clothes_consumption)
data_clean$lifestyle_clothes_cons <- NA
data_clean$lifestyle_clothes_cons[data_raw$clothes_consumption == 1 | data_raw$clothes_consumption == 3 | data_raw$clothes_consumption == 4] <- "large_retailers"
data_clean$lifestyle_clothes_cons[data_raw$clothes_consumption == 2 | data_raw$clothes_consumption == 6] <- "small_local_store"
data_clean$lifestyle_clothes_cons[data_raw$clothes_consumption == 5] <- "online_store"
data_clean$lifestyle_clothes_cons[data_raw$clothes_consumption == 7] <- "other"
data_clean$lifestyle_clothes_cons <- factor(data_clean$lifestyle_clothes_cons,
                                         levels = c("large_retailers",
                                                    "small_local_store",
                                                    "online_store",
                                                    "other"),
                                         ordered = TRUE)
table(data_clean$lifestyle_clothes_cons)

## mode_attitude_1 ---------------------------------------------------------

attributes(data_raw$mode_attitude_1)
table(data_raw$mode_attitude_1)
data_clean$lifestyle_mode_awareness <- NA
data_clean$lifestyle_mode_awareness <- data_raw$mode_attitude_1 / 10
table(data_clean$lifestyle_mode_awareness)


## meat_consumption ------------------------------------------------------

attributes(data_raw$meat_consumption)
table(data_raw$meat_consumption)
data_clean$lifestyle_meat_cons <- NA
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 1] <- 0
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 2] <- 0.17
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 3] <- 0.33
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 4] <- 0.5
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 5] <- 0.67
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 6] <- 0.83
data_clean$lifestyle_meat_cons[data_raw$meat_consumption == 7] <- 1
table(data_clean$lifestyle_meat_cons)

## meal_time -------------------------------------------------------------

## Sais pas trop comment cleaner ça ##
## On va devoir attendre les vrais répondants pour pouvoir le cleaner ##
attributes(data_raw$meal_time_6)
table(data_raw$meal_time_6)

## fridge_1 ----------------------------------------------------------------

attributes(data_raw$fridge_1)
table(data_raw$fridge_1)
data_clean$lifestyle_fridge_vegetal_milk <- NA
data_clean$lifestyle_fridge_vegetal_milk[data_raw$fridge_1 == 1] <- 1
data_clean$lifestyle_fridge_vegetal_milk[data_raw$fridge_1 == 2] <- 0
table(data_clean$lifestyle_fridge_vegetal_milk)

### fridge_2 -------------------------------------------------------------

attributes(data_raw$fridge_2)
table(data_raw$fridge_2)
data_clean$lifestyle_fridge_tofu_tempeh <- NA
data_clean$lifestyle_fridge_tofu_tempeh[data_raw$fridge_2 == 1] <- 1
data_clean$lifestyle_fridge_tofu_tempeh[data_raw$fridge_2 == 2] <- 0
table(data_clean$lifestyle_fridge_tofu_tempeh)

### fridge_3 -------------------------------------------------------------

table(data_raw$fridge_3)
data_clean$lifestyle_fridge_enerdrink <- NA
data_clean$lifestyle_fridge_enerdrink[data_raw$fridge_3 == 1] <- 1
data_clean$lifestyle_fridge_enerdrink[data_raw$fridge_3 == 2] <- 0
table(data_clean$lifestyle_fridge_enerdrink)

### fridge_4 -------------------------------------------------------------

table(data_raw$fridge_4)
data_clean$lifestyle_fridge_pizza <- NA
data_clean$lifestyle_fridge_pizza[data_raw$fridge_4 == 1] <- 1
data_clean$lifestyle_fridge_pizza[data_raw$fridge_4 == 2] <- 0
table(data_clean$lifestyle_fridge_pizza)

### fridge_5 -------------------------------------------------------------

attributes(data_raw$fridge_5)
table(data_raw$fridge_5)
data_clean$lifestyle_fridge_butter_tarts <- NA
data_clean$lifestyle_fridge_butter_tarts[data_raw$fridge_5 == 1] <- 1
data_clean$lifestyle_fridge_butter_tarts[data_raw$fridge_5 == 2] <- 0
table(data_clean$lifestyle_fridge_butter_tarts)

### fridge_6 -------------------------------------------------------------

table(data_raw$fridge_6)
data_clean$lifestyle_fridge_bacon <- NA
data_clean$lifestyle_fridge_bacon[data_raw$fridge_6 == 1] <- 1
data_clean$lifestyle_fridge_bacon[data_raw$fridge_6 == 2] <- 0
table(data_clean$lifestyle_fridge_bacon)

### fridge_7 -------------------------------------------------------------

table(data_raw$fridge_7)
data_clean$lifestyle_fridge_ceasar_dressing <- NA
data_clean$lifestyle_fridge_ceasar_dressing[data_raw$fridge_7 == 1] <- 1
data_clean$lifestyle_fridge_ceasar_dressing[data_raw$fridge_7 == 2] <- 0
table(data_clean$lifestyle_fridge_ceasar_dressing)

### fridge_8 -------------------------------------------------------------

table(data_raw$fridge_8)
data_clean$lifestyle_fridge_org_veggies <- NA
data_clean$lifestyle_fridge_org_veggies[data_raw$fridge_8 == 1] <- 1
data_clean$lifestyle_fridge_org_veggies[data_raw$fridge_8 == 2] <- 0
table(data_clean$lifestyle_fridge_org_veggies)

### fridge_9 -------------------------------------------------------------

table(data_raw$fridge_9)
data_clean$lifestyle_fridge_hot_sauce <- NA
data_clean$lifestyle_fridge_hot_sauce[data_raw$fridge_9 == 1] <- 1
data_clean$lifestyle_fridge_hot_sauce[data_raw$fridge_9 == 2] <- 0
table(data_clean$lifestyle_fridge_hot_sauce)

### fridge_10 -------------------------------------------------------------

table(data_raw$fridge_10)
data_clean$lifestyle_fridge_soft_drinks <- NA
data_clean$lifestyle_fridge_soft_drinks[data_raw$fridge_10 == 1] <- 1
data_clean$lifestyle_fridge_soft_drinks[data_raw$fridge_10 == 2] <- 0
table(data_clean$lifestyle_fridge_soft_drink)

### fridge_11 ------------------------------------------------------------

table(data_raw$fridge_11)
data_clean$lifestyle_fridge_ground_beef <- NA
data_clean$lifestyle_fridge_ground_beef[data_raw$fridge_11 == 1] <- 1
data_clean$lifestyle_fridge_ground_beef[data_raw$fridge_11 == 2] <- 0
table(data_clean$lifestyle_fridge_ground_beef)


## coffee_consumption ---------------------------------------------------

table(data_raw$coffee_consumption)
data_clean$lifestyle_coffee_cons <- NA
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 1] <- "tim_hortons"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 2] <- "starbucks"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 3] <- "second_cup"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 4] <- "mcdonalds"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 5] <- "other"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 6] <- "independent"
data_clean$lifestyle_coffee_cons[data_raw$coffee_consumption == 7] <- "no_coffee"
data_clean$lifestyle_coffee_cons <- factor(data_clean$lifestyle_coffee_cons,
                                         levels = c("tim_hortons",
                                                    "starbucks",
                                                    "second_cup",
                                                    "mcdonalds",
                                                    "other",
                                                    "independent",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(data_clean$lifestyle_coffee_cons)

## coffee_machine --------------------------------------------------------

table(data_raw$coffee_machine)
data_clean$lifestyle_coffee_mac <- NA
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 1] <- "filter_coffee_maker"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 2] <- "automatic_manual_machine"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 3] <- "percolator"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 4] <- "french_press"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 5] <- "single_cup_coffee"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 6] <- "instant_coffee"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 7] <- "italian_coffee_maker"
data_clean$lifestyle_coffee_mac[data_raw$coffee_machine == 8] <- "no_coffee"
data_clean$lifestyle_coffee_mac <- factor(data_clean$lifestyle_coffee_mac,
                                         levels = c("filter_coffee_maker",
                                                    "automatic_manual_machine",
                                                    "percolator",
                                                    "french_press",
                                                    "single_cup_coffee",
                                                    "instant_coffee",
                                                    "italian_coffee_maker",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(data_clean$lifestyle_coffee_mac)

## cons_pet --------------------------------------------------------------

attributes(data_raw$cons_pets)
table(data_raw$cons_pets)
data_clean$lifestyle_pet_ownership <- NA
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 1] <- "chat"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 2] <- "chien"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 3] <- "chat et chien"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 4] <- "diverses sortes d'animaux"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 5] <- "autres animaux domestiques"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 6] <- "animaux de ferme"
data_clean$lifestyle_pet_ownership[data_raw$cons_pets == 7] <- "je n'ai pas d'animal de compagnie"
data_clean$lifestyle_pet_ownership <- factor(data_clean$lifestyle_pet_ownership,
                                             levels = c("chat",
                                                        "chien",
                                                        "chat et chien",
                                                        "diverses sortes d'animaux",
                                                        "autres animaux domestiques",
                                                        "animaux de ferme",
                                                        "je n'ai pas d'animal de compagnie"),
                                             ordered = TRUE)
table(data_clean$lifestyle_pet_ownership)

data_clean$lifestyle_pet_ownership_yes <- NA
data_clean$lifestyle_pet_ownership_yes <- 0 
data_clean$lifestyle_pet_ownership_yes[data_raw$cons_pets %in% c(1:6)] <- 1
table(data_clean$lifestyle_pet_ownership_yes)

## smoking ---------------------------------------------------------------

attributes(data_raw$smoking)
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

attributes(data_raw$alcool_type)
table(data_raw$alcool_type) 
data_clean$lifestyle_favourite_alcool <- NA
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type %in% c(1:4)] <- "wine"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type %in% c(5:6)] <- "beer"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 7] <- "spirits"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 8] <- "cocktail"
data_clean$lifestyle_favourite_alcool[data_raw$alcool_type == 9] <- "dont_drink"
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




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------

attributes(data_raw$social_media_use)
table(data_raw$social_media_use)

data_clean$lifestyle_most_freq_social_media <- NA
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 1] <- "Facebook"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 2] <- "Twitter / X"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 3] <- "Instagram"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 4] <- "Snapchat"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 5] <- "TikTok"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 6] <- "Pinterest"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 7] <- "LinkedIn"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 8] <- "Youtube"
data_clean$lifestyle_most_freq_social_media[data_raw$social_media_use == 9] <- "Autre"
data_clean$lifestyle_most_freq_social_media <- factor(data_clean$lifestyle_most_freq_social_media)
table(data_clean$lifestyle_most_freq_social_media)

## social_media_time -----------------------------------------------------

attributes(data_raw$social_media_time)
data_clean$lifestyle_social_media_time_day <- NA
data_clean$lifestyle_social_media_time_day[data_raw$social_media_time == 1] <- 0
data_clean$ifestyle_social_media_time_day[data_raw$social_media_time == 2] <- 0.2
data_clean$ifestyle_social_media_time_day[data_raw$social_media_time == 3] <- 0.4
data_clean$ifestyle_social_media_time_day[data_raw$social_media_time == 4] <- 0.6
data_clean$ifestyle_social_media_time_day[data_raw$social_media_time == 5] <- 0.8
data_clean$ifestyle_social_media_time_day[data_raw$social_media_time == 6] <- 1
table(data_clean$ifestyle_social_media_time_day)

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
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 9] <- 0  # "0 tatouage"
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 10] <- 1 # "1 tatouage"
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 11] <- 2 # "2 tatouages"
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 4] <- 3  # "3 tatouages"
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 5] <- 4  # "4 tatouages"
data_clean$lifestyle_number_tattoos[data_raw$number_tattoos == 6] <- 5  # "5+ tatouages" traité comme 5

table(data_clean$lifestyle_number_tattoos, useNA = "ifany")

# Créer la variable binaire 'has_tattoos'
data_clean$lifestyle_has_tattoos <- as.numeric(data_clean$lifestyle_number_tattoos > 0)
data_clean$lifestyle_has_tattoos[is.na(data_clean$lifestyle_number_tattoos)] <- NA


table(data_clean$lifestyle_has_tattoos, useNA = "ifany")

## chronotype ------------------------------------------------------------

attributes(data_raw$chronotype)
table(data_raw$chronotype)

data_clean$lifestyle_morning_to_evening <- NA
data_clean$lifestyle_morning_to_evening[data_raw$chronotype == 1] <- 0
data_clean$lifestyle_morning_to_evening[data_raw$chronotype == 2] <- 0.25
data_clean$lifestyle_morning_to_evening[data_raw$chronotype == 3] <- 0.5
data_clean$lifestyle_morning_to_evening[data_raw$chronotype == 4] <- 0.75
data_clean$lifestyle_morning_to_evening[data_raw$chronotype == 5] <- 1
table(data_clean$lifestyle_morning_to_evening)

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

