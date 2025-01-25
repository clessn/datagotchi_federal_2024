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
data_clean$lifestyle_typeTransport <- NA
data_raw$type_transport <- as.numeric(data_raw$type_transport)
data_clean$lifestyle_typeTransport[data_raw$type_transport == 1] <- "car"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 2] <- "car"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 3] <- "car"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 4] <- "active_transport"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 5] <- "active_transport"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 6] <- "shared_transport"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 7] <- "shared_transport"
data_clean$lifestyle_typeTransport[data_raw$type_transport == 8] <- "shared_transport"
table(data_clean$lifestyle_typeTransport)

## factor



## numeric



## bin

## choice_transport ------------------------------------------------------

attributes(data_raw$choice_transport)
table(data_raw$choice_transport) 
data_clean$lifestyle_choiceTransport <- NA
data_raw$choice_transport <- as.numeric(data_raw$choice_transport)
data_clean$llifestyle_choiceTransport[data_raw$choice_transport == 1] <- "part_of_who_i_am"
data_clean$llifestyle_choiceTransport[data_raw$choice_transport == 2] <- "point_a_to_point_b"
table(data_clean$lifestyle_choiceTransport)

## factor



## numeric



## bin

data_clean$lifestyle_transportIdentity_bin <- NA
data_clean$lifestyle_transportIdentity_bin[data_raw$choice_transport  == 1] <- 0
data_clean$lifestyle_transportIdentity_bin[data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_transportIdentity_bin)

data_clean$lifestyle_transportEfficiency_bin <- NA
data_clean$lifestyle_transportEfficiency_bin[data_raw$choice_transport  == 1] <- 0
data_clean$lifestyle_transportEfficiency_bin[data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_transportEfficiency_bin)

## field_occupation ------------------------------------------------------

attributes(data_raw$field_occupation)
table(data_raw$field_occupation)
data_clean$lifestyle_workField <- NA
data_clean$lifestyle_workField[data_raw$field_occupation == 1 | data_raw$field_occupation == 2 | data_raw$field_occupation == 7 | data_raw$field_occupation == 8] <- "economics"
data_clean$lifestyle_workField[data_raw$field_occupation == 3] <- "natural_sciences"
data_clean$lifestyle_workField[data_raw$field_occupation == 4] <- "health"
data_clean$lifestyle_workField[data_raw$field_occupation == 5] <- "education_law_politics"
data_clean$lifestyle_workField[data_raw$field_occupation == 6] <- "culture_sports"
data_clean$lifestyle_workField[data_raw$field_occupation == 9] <- "agriculture"
data_clean$lifestyle_workField[data_raw$field_occupation == 10] <- "manufacturing"
data_clean$lifestyle_workField <- factor(data_clean$lifestyle_workField,
                                                   levels = c("economics",
                                                              "natural_sciences",
                                                              "health",
                                                              "education_law_politics",
                                                              "culture_sports",
                                                              "agriculture",
                                                              "manufacturing"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_workField)

## type_occupation -------------------------------------------------------

attributes(data_raw$type_occupation)
table(data_raw$type_occupation)
data_clean$lifestyle_workType <- NA
data_clean$lifestyle_workType[data_raw$type_occupation == 1] <- "labour_job"
data_clean$lifestyle_workType[data_raw$type_occupation == 2] <- "intermediate_job"
data_clean$lifestyle_workType[data_raw$type_occupation == 3] <- "technical_job"
data_clean$lifestyle_workType[data_raw$type_occupation == 4] <- "professional_job"
data_clean$lifestyle_workType[data_raw$type_occupation == 5] <- "management_job"
data_clean$lifestyle_workType[data_raw$type_occupation == 6] <- "other"
data_clean$lifestyle_workType[data_raw$type_occupation == 7] <- "no_job"
data_clean$lifestyle_workType <- factor(data_clean$lifestyle_workType,
                                         levels = c("labour_job",
                                                    "intermediate_job",
                                                    "technical_job",
                                                    "professional_job",
                                                    "management_job",
                                                    "other",
                                                    "no_job"),
                                         ordered = TRUE)
table(data_clean$lifestyle_workType)

## clothes_consumption ---------------------------------------------------

attributes(data_raw$clothes_consumption)
table(data_raw$clothes_consumption)
data_clean$lifestyle_consClothes <- NA
data_clean$lifestyle_consClothes[data_raw$clothes_consumption == 1 | data_raw$clothes_consumption == 3 | data_raw$clothes_consumption == 4] <- "large_retailers"
data_clean$lifestyle_consClothes[data_raw$clothes_consumption == 2 | data_raw$clothes_consumption == 6] <- "small_local_store"
data_clean$lifestyle_consClothes[data_raw$clothes_consumption == 5] <- "online_store"
data_clean$lifestyle_consClothes[data_raw$clothes_consumption == 7] <- "other"
data_clean$lifestyle_consClothes <- factor(data_clean$lifestyle_consClothes,
                                         levels = c("large_retailers",
                                                    "small_local_store",
                                                    "online_store",
                                                    "other"),
                                         ordered = TRUE)
table(data_clean$lifestyle_consClothes)

## mode_attitude_1 ---------------------------------------------------------

attributes(data_raw$mode_attitude_1)
table(data_raw$mode_attitude_1)
data_clean$lifestyle_followFashion <- NA
data_clean$lifestyle_followFashion <- data_raw$mode_attitude_1 / 10
table(data_clean$lifestyle_followFashion)


## meat_consumption ------------------------------------------------------

attributes(data_raw$meat_consumption)
table(data_raw$meat_consumption)
data_clean$lifestyle_eatMeatFreq <- NA
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 1] <- 0
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 2] <- 0.17
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 3] <- 0.33
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 4] <- 0.5
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 5] <- 0.67
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 6] <- 0.83
data_clean$lifestyle_eatMeatFreq[data_raw$meat_consumption == 7] <- 1
table(data_clean$lifestyle_eatMeatFreq)

## meal_time -------------------------------------------------------------

## Sais pas trop comment cleaner ça ##
## On va devoir attendre les vrais répondants pour pouvoir le cleaner ##
attributes(data_raw$meal_time_6)
table(data_raw$meal_time_6)

## fridge_1 ----------------------------------------------------------------

attributes(data_raw$fridge_1)
table(data_raw$fridge_1)
data_clean$lifestyle_fridgeVegetalMilk <- NA
data_clean$lifestyle_fridgeVegetalMilk[data_raw$fridge_1 == 1] <- 1
data_clean$lifestyle_fridgeVegetalMilk[data_raw$fridge_1 == 2] <- 0
table(data_clean$lifestyle_fridgeVegetalMilk)

### fridge_2 -------------------------------------------------------------

attributes(data_raw$fridge_2)
table(data_raw$fridge_2)
data_clean$lifestyle_fridgeTofuTempeh <- NA
data_clean$lifestyle_fridgeTofuTempeh[data_raw$fridge_2 == 1] <- 1
data_clean$lifestyle_fridgeTofuTempeh[data_raw$fridge_2 == 2] <- 0
table(data_clean$lifestyle_fridgeTofuTempeh)

### fridge_3 -------------------------------------------------------------

table(data_raw$fridge_3)
data_clean$lifestyle_fridgeEnergyDrink <- NA
data_clean$lifestyle_fridgeEnergyDrink[data_raw$fridge_3 == 1] <- 1
data_clean$lifestyle_fridgeEnergyDrink[data_raw$fridge_3 == 2] <- 0
table(data_clean$lifestyle_fridgeEnergyDrink)

### fridge_4 -------------------------------------------------------------

table(data_raw$fridge_4)
data_clean$lifestyle_fridgePizza <- NA
data_clean$lifestyle_fridgePizza[data_raw$fridge_4 == 1] <- 1
data_clean$lifestyle_fridgePizza[data_raw$fridge_4 == 2] <- 0
table(data_clean$lifestyle_fridgePizza)

### fridge_5 -------------------------------------------------------------

attributes(data_raw$fridge_5)
table(data_raw$fridge_5)
data_clean$lifestyle_fridgeButterTarts <- NA
data_clean$lifestyle_fridgeButterTarts[data_raw$fridge_5 == 1] <- 1
data_clean$lifestyle_fridgeButterTarts[data_raw$fridge_5 == 2] <- 0
table(data_clean$lifestyle_fridgeButterTarts)

### fridge_6 -------------------------------------------------------------

table(data_raw$fridge_6)
data_clean$lifestyle_fridgeBacon <- NA
data_clean$lifestyle_fridgeBacon[data_raw$fridge_6 == 1] <- 1
data_clean$lifestyle_fridgeBacon[data_raw$fridge_6 == 2] <- 0
table(data_clean$lifestyle_fridgeBacon)

### fridge_7 -------------------------------------------------------------

table(data_raw$fridge_7)
data_clean$lifestyle_fridgeCeasarDressing <- NA
data_clean$lifestyle_fridgeCeasarDressing[data_raw$fridge_7 == 1] <- 1
data_clean$lifestyle_fridgeCeasarDressing[data_raw$fridge_7 == 2] <- 0
table(data_clean$lifestyle_fridgeCeasarDressing)

### fridge_8 -------------------------------------------------------------

table(data_raw$fridge_8)
data_clean$lifestyle_fridgeOrganicVeggies <- NA
data_clean$lifestyle_fridgeOrganicVeggies[data_raw$fridge_8 == 1] <- 1
data_clean$lifestyle_fridgeOrganicVeggies[data_raw$fridge_8 == 2] <- 0
table(data_clean$lifestyle_fridgeOrganicVeggies)

### fridge_9 -------------------------------------------------------------

table(data_raw$fridge_9)
data_clean$lifestyle_fridgeHotSauce <- NA
data_clean$lifestyle_fridgeHotSauce[data_raw$fridge_9 == 1] <- 1
data_clean$lifestyle_fridgeHotSauce[data_raw$fridge_9 == 2] <- 0
table(data_clean$lifestyle_fridgeHotSauce)

### fridge_10 -------------------------------------------------------------

table(data_raw$fridge_10)
data_clean$lifestyle_fridgeSoftDrinks <- NA
data_clean$lifestyle_fridgeSoftDrinks[data_raw$fridge_10 == 1] <- 1
data_clean$lifestyle_fridgeSoftDrinks[data_raw$fridge_10 == 2] <- 0
table(data_clean$lifestyle_fridgeSoftDrinks)

### fridge_11 ------------------------------------------------------------

table(data_raw$fridge_11)
data_clean$lifestyle_fridgeGroundBeef <- NA
data_clean$lifestyle_fridgeGroundBeef[data_raw$fridge_11 == 1] <- 1
data_clean$lifestyle_fridgeGroundBeef[data_raw$fridge_11 == 2] <- 0
table(data_clean$lifestyle_fridgeGroundBeef)


## coffee_consumption ---------------------------------------------------

table(data_raw$coffee_consumption)
data_clean$lifestyle_consCoffee <- NA
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 1] <- "tim_hortons"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 2] <- "starbucks"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 3] <- "second_cup"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 4] <- "mcdonalds"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 5] <- "other"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 6] <- "independent"
data_clean$lifestyle_consCoffee[data_raw$coffee_consumption == 7] <- "no_coffee"
data_clean$lifestyle_consCoffee <- factor(data_clean$lifestyle_consCoffee,
                                         levels = c("tim_hortons",
                                                    "starbucks",
                                                    "second_cup",
                                                    "mcdonalds",
                                                    "other",
                                                    "independent",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(data_clean$lifestyle_consCoffee)

## coffee_machine --------------------------------------------------------

table(data_raw$coffee_machine)
data_clean$lifestyle_makeCoffee <- NA
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 1] <- "filter_coffee_maker"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 2] <- "automatic_manual_machine"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 3] <- "percolator"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 4] <- "french_press"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 5] <- "single_cup_coffee"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 6] <- "instant_coffee"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 7] <- "italian_coffee_maker"
data_clean$lifestyle_makeCoffee[data_raw$coffee_machine == 8] <- "no_coffee"
data_clean$lifestyle_makeCoffee <- factor(data_clean$lifestyle_makeCoffee,
                                         levels = c("filter_coffee_maker",
                                                    "automatic_manual_machine",
                                                    "percolator",
                                                    "french_press",
                                                    "single_cup_coffee",
                                                    "instant_coffee",
                                                    "italian_coffee_maker",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(data_clean$lifestyle_makeCoffee)

## cons_pet --------------------------------------------------------------

attributes(data_raw$cons_pets)
table(data_raw$cons_pets)
data_clean$lifestyle_ownPet <- NA
data_clean$lifestyle_ownPet[data_raw$cons_pets == 1] <- "chat"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 2] <- "chien"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 3] <- "chat et chien"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 4] <- "diverses sortes d'animaux"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 5] <- "autres animaux domestiques"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 6] <- "animaux de ferme"
data_clean$lifestyle_ownPet[data_raw$cons_pets == 7] <- "je n'ai pas d'animal de compagnie"
data_clean$lifestyle_ownPet <- factor(data_clean$lifestyle_ownPet,
                                             levels = c("chat",
                                                        "chien",
                                                        "chat et chien",
                                                        "diverses sortes d'animaux",
                                                        "autres animaux domestiques",
                                                        "animaux de ferme",
                                                        "je n'ai pas d'animal de compagnie"),
                                             ordered = TRUE)
table(data_clean$lifestyle_ownPet)

data_clean$lifestyle_ownPet_bin <- NA
data_clean$lifestyle_ownPet_bin <- 0 
data_clean$lifestyle_ownPet_bin[data_raw$cons_pets %in% c(1:6)] <- 1
table(data_clean$lifestyle_ownPet_bin)

## smoking ---------------------------------------------------------------

attributes(data_raw$smoking)
table(data_raw$smoking)
data_clean$lifestyle_smokeFreq <- NA
data_clean$lifestyle_smokeFreq[data_raw$smoking == 1] <- 0
data_clean$lifestyle_smokeFreq[data_raw$smoking == 2] <- 0.1667
data_clean$lifestyle_smokeFreq[data_raw$smoking == 3] <- 0.3333
data_clean$lifestyle_smokeFreq[data_raw$smoking == 4] <- 0.5
data_clean$lifestyle_smokeFreq[data_raw$smoking == 5] <- 0.6667
data_clean$lifestyle_smokeFreq[data_raw$smoking == 6] <- 0.8333
data_clean$lifestyle_smokeFreq[data_raw$smoking == 7] <- 1
table(data_clean$lifestyle_smokeFreq)

## alcool_type -----------------------------------------------------------

attributes(data_raw$alcool_type)
table(data_raw$alcool_type) 
data_clean$lifestyle_favAlcool <- NA
data_clean$lifestyle_favAlcool[data_raw$alcool_type %in% c(1:4)] <- "wine"
data_clean$lifestyle_favAlcool[data_raw$alcool_type %in% c(5:6)] <- "beer"
data_clean$lifestyle_favAlcool[data_raw$alcool_type == 7] <- "spirits"
data_clean$lifestyle_favAlcool[data_raw$alcool_type == 8] <- "cocktail"
data_clean$lifestyle_favAlcool[data_raw$alcool_type == 9] <- "dont_drink"
table(data_clean$lifestyle_favAlcool)

## alcool_frequency ------------------------------------------------------
data_clean$lifestyle_alcoolFreq <- NA
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 1] <- 0
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 2] <- 0.1667
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 3] <- 0.3333
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 4] <- 0.5
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 5] <- 0.6667
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 6] <- 0.8333
data_clean$lifestyle_alcoolFreq[data_raw$alcool_frequency == 7] <- 1
table(data_clean$lifestyle_alcoolFreq)


## marijuana_frequency ---------------------------------------------------
data_clean$lifestyle_weedFreq <- NA
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 1] <- 0
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 2] <- 0.1667
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 3] <- 0.3333
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 4] <- 0.5
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 5] <- 0.6667
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 6] <- 0.8333
data_clean$lifestyle_weedFreq[data_raw$marijuana_frequency == 7] <- 1
table(data_clean$lifestyle_weedFreq)



## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------

attributes(data_raw$social_media_use)
table(data_raw$social_media_use)

data_clean$lifestyle_mostFreqSocialMedia <- NA
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 1] <- "Facebook"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 2] <- "Twitter / X"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 3] <- "Instagram"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 4] <- "Snapchat"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 5] <- "TikTok"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 6] <- "Pinterest"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 7] <- "LinkedIn"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 8] <- "Youtube"
data_clean$lifestyle_mostFreqSocialMedia[data_raw$social_media_use == 9] <- "Autre"
data_clean$lifestyle_mostFreqSocialMedia <- factor(data_clean$lifestyle_mostFreqSocialMedia)
table(data_clean$lifestyle_mostFreqSocialMedia)

## social_media_time -----------------------------------------------------

attributes(data_raw$social_media_time)
data_clean$lifestyle_socialMediaTime <- NA
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 1] <- 0
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 2] <- 0.2
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 3] <- 0.4
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 4] <- 0.6
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 5] <- 0.8
data_clean$lifestyle_socialMediaTime[data_raw$social_media_time == 6] <- 1
table(data_clean$lifestyle_socialMediaTime)

## clothing_style --------------------------------------------------------

attributes(data_raw$clothing_style)
table(data_raw$clothing_style)

## disaggregated
data_clean$lifestyle_clothingStyle <- NA
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 1] <- "formal"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 2] <- "classic"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 3] <- "casual"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 4] <- "sport"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 5] <- "elegant"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 6] <- "hippie"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 7] <- "punk"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 8] <- "rock"
data_clean$lifestyle_clothingStyle[data_raw$clothing_style == 9] <- "other"
data_clean$lifestyle_clothingStyle <- factor(data_clean$lifestyle_clothingStyle)
table(data_clean$lifestyle_clothingStyle)

## grouped
data_clean$lifestyle_clothingStyleGroups <- NA
data_clean$lifestyle_clothingStyleGroups[data_raw$clothing_style %in% c(1, 5)] <- "formal"
data_clean$lifestyle_clothingStyleGroups[data_raw$clothing_style %in% c(2, 3, 4)] <- "easygoing"
data_clean$lifestyle_clothingStyleGroups[data_raw$clothing_style %in% c(6, 7, 8)] <- "edgy"
data_clean$lifestyle_clothingStyleGroups <- factor(data_clean$lifestyle_clothingStyleGroups)
table(data_clean$lifestyle_clothingStyleGroups)

## number_tattoos --------------------------------------------------------

attributes(data_raw$number_tattoos)
table(data_raw$number_tattoos)

data_clean$lifestyle_numberTattoos <- NA
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 9] <- 0  # "0 tatouage"
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 10] <- 1 # "1 tatouage"
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 11] <- 2 # "2 tatouages"
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 4] <- 3  # "3 tatouages"
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 5] <- 4  # "4 tatouages"
data_clean$lifestyle_numberTattoos[data_raw$number_tattoos == 6] <- 5  # "5+ tatouages" traité comme 5

table(data_clean$lifestyle_numberTattoos, useNA = "ifany")

# Créer la variable binaire 'has_tattoos'
data_clean$lifestyle_hasTattoos <- as.numeric(data_clean$lifestyle_numberTattoos > 0)
data_clean$lifestyle_hasTattoos[is.na(data_clean$lifestyle_numberTattoos)] <- NA


table(data_clean$lifestyle_hasTattoos, useNA = "ifany")

## chronotype ------------------------------------------------------------

attributes(data_raw$chronotype)
table(data_raw$chronotype)

data_clean$lifestyle_typeMorningToEvening <- NA
data_clean$lifestyle_typeMorningToEvening[data_raw$chronotype == 1] <- 0
data_clean$lifestyle_typeMorningToEvening[data_raw$chronotype == 2] <- 0.25
data_clean$lifestyle_typeMorningToEvening[data_raw$chronotype == 3] <- 0.5
data_clean$lifestyle_typeMorningToEvening[data_raw$chronotype == 4] <- 0.75
data_clean$lifestyle_typeMorningToEvening[data_raw$chronotype == 5] <- 1
table(data_clean$lifestyle_typeMorningToEvening)

## trip ------------------------------------------------------------------

attributes(data_raw$trip)
table(data_raw$trip)

data_clean$lifestyle_typeTravel <- NA
data_clean$lifestyle_typeTravel[data_raw$trip == 1] <- "beach"
data_clean$lifestyle_typeTravel[data_raw$trip == 2] <- "jungle"
data_clean$lifestyle_typeTravel[data_raw$trip == 3] <- "historic"
data_clean$lifestyle_typeTravel[data_raw$trip == 4] <- "mountains"
data_clean$lifestyle_typeTravel <- factor(data_clean$lifestyle_typeTravel)
table(data_clean$lifestyle_typeTravel)

