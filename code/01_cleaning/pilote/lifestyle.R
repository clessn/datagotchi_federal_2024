# lifestyle

## exercise --------------------------------------------------------------

attributes(Data_raw$exercise)
table(Data_raw$exercise)
Data_clean$lifestyle_exercise <- NA
Data_clean$lifestyle_exercise[Data_raw$exercise == 1] <- "gym"
Data_clean$lifestyle_exercise[Data_raw$exercise == 2] <- "play_a_team_sport"
Data_clean$lifestyle_exercise[Data_raw$exercise == 3] <- "walk"
Data_clean$lifestyle_exercise[Data_raw$exercise == 4] <- "run"
Data_clean$lifestyle_exercise[Data_raw$exercise == 5] <- "yoga"
Data_clean$lifestyle_exercise[Data_raw$exercise == 6] <- "swimming"
Data_clean$lifestyle_exercise[Data_raw$exercise == 7] <- "other"
Data_clean$lifestyle_exercise[Data_raw$exercise == 8] <- "i_do_not_exercise"
table(Data_clean$lifestyle_exercise)


## activity_1 ------------------------------------------------------------

attributes(Data_raw$activity_1)
table(Data_raw$activity_1) 
Data_clean$lifestyle_goFishingFreq <- NA
Data_raw$activity_1 <- as.numeric(Data_raw$activity_1)
Data_clean$lifestyle_goFishingFreq[Data_raw$activity_1 == 1] <- 1
Data_clean$lifestyle_goFishingFreq[Data_raw$activity_1 == 2] <- 2
Data_clean$lifestyle_goFishingFreq[Data_raw$activity_1 == 3] <- 3
Data_clean$lifestyle_goFishingFreq[Data_raw$activity_1 == 4] <- 4
Data_clean$lifestyle_goFishingFreq[Data_raw$activity_1 == 5] <- 5
table(Data_clean$lifestyle_goFishingFreq)

## factor

table(Data_clean$lifestyle_fishing_freq)
Data_clean$lifestyle_goFishingFreq_factor <- NA
Data_clean$lifestyle_goFishingFreq_factor[Data_raw$activity_1 == 1] <- "never"
Data_clean$lifestyle_goFishingFreq_factor[Data_raw$activity_1 == 2] <- "almost_never"
Data_clean$lifestyle_goFishingFreq_factor[Data_raw$activity_1 == 3] <- "sometimes"
Data_clean$lifestyle_goFishingFreq_factor[Data_raw$activity_1 == 4] <- "often"
Data_clean$lifestyle_goFishingFreq_factor[Data_raw$activity_1 == 5] <- "very_often"
Data_clean$lifestyle_goFishingFreq_factor <- factor(Data_clean$lifestyle_goFishingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(Data_clean$lifestyle_goFishingFreq_factor)

## numeric

Data_clean$lifestyle_goFishingFreq_numeric <- NA 
Data_clean$lifestyle_goFishingFreq_numeric <- (Data_raw$activity_1 - 1) / 4
table(Data_clean$lifestyle_goFishingFreq_numeric)

## bin

Data_clean$lifestyle_goFishingFreq_bin <- NA
Data_clean$lifestyle_goFishingFreq_bin[Data_raw$activity_1  == 1] <- 0
Data_clean$lifestyle_goFishingFreq_bin[Data_raw$activity_1  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_goFishingFreq_bin) 

## activity_2 ------------------------------------------------------------

table(Data_raw$activity_2) 
Data_clean$lifestyle_goHuntingFreq <- NA
Data_raw$activity_2 <- as.numeric(Data_raw$activity_2)
Data_clean$lifestyle_goHuntingFreq[Data_raw$activity_2 == 1] <- 1
Data_clean$lifestyle_goHuntingFreq[Data_raw$activity_2 == 2] <- 2
Data_clean$lifestyle_goHuntingFreq[Data_raw$activity_2 == 3] <- 3
Data_clean$lifestyle_goHuntingFreq[Data_raw$activity_2 == 4] <- 4
Data_clean$lifestyle_goHuntingFreq[Data_raw$activity_2 == 5] <- 5
table(Data_clean$lifestyle_goHuntingFreq)

## factor

table(Data_clean$lifestyle_goHuntingFreq)
Data_clean$lifestyle_goHuntingFreq_factor <- NA
Data_clean$lifestyle_goHuntingFreq_factor[Data_raw$activity_2 == 1] <- "never"
Data_clean$lifestyle_goHuntingFreq_factor[Data_raw$activity_2 == 2] <- "almost_never"
Data_clean$lifestyle_goHuntingFreq_factor[Data_raw$activity_2 == 3] <- "sometimes"
Data_clean$lifestyle_goHuntingFreq_factor[Data_raw$activity_2 == 4] <- "often"
Data_clean$lifestyle_goHuntingFreq_factor[Data_raw$activity_2 == 5] <- "very_often"
Data_clean$lifestyle_goHuntingFreq_factor <- factor(Data_clean$lifestyle_goHuntingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(Data_clean$lifestyle_goHuntingFreq_factor)

## numeric

Data_clean$lifestyle_goHuntingFreq_numeric <- NA 
Data_clean$lifestyle_goHuntingFreq_numeric <- (Data_raw$activity_2 - 1) / 4
table(Data_clean$lifestyle_goHuntingFreq_numeric)

## bin

Data_clean$lifestyle_goHuntingFreq_bin <- NA
Data_clean$lifestyle_goHuntingFreq_bin[Data_raw$activity_2  == 1] <- 0
Data_clean$lifestyle_goHuntingFreq_bin[Data_raw$activity_2  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_goHuntingFreq_bin) 


## activity_3 ------------------------------------------------------------

table(Data_raw$activity_3) 
Data_clean$lifestyle_snowSportsFreq <- NA
Data_raw$activity_3 <- as.numeric(Data_raw$activity_3)
Data_clean$lifestyle_snowSportsFreq[Data_raw$activity_3 == 1] <- 1
Data_clean$lifestyle_snowSportsFreq[Data_raw$activity_3 == 2] <- 2
Data_clean$lifestyle_snowSportsFreq[Data_raw$activity_3 == 3] <- 3
Data_clean$lifestyle_snowSportsFreq[Data_raw$activity_3 == 4] <- 4
Data_clean$lifestyle_snowSportsFreq[Data_raw$activity_3 == 5] <- 5
table(Data_clean$lifestyle_snowSportsFreq)

## factor

table(Data_clean$lifestyle_snowSportsFreq)
Data_clean$lifestyle_snowSportsFreq_factor <- NA
Data_clean$lifestyle_snowSportsFreq_factor[Data_raw$activity_3 == 1] <- "never"
Data_clean$lifestyle_snowSportsFreq_factor[Data_raw$activity_3 == 2] <- "almost_never"
Data_clean$lifestyle_snowSportsFreq_factor[Data_raw$activity_3 == 3] <- "sometimes"
Data_clean$lifestyle_snowSportsFreq_factor[Data_raw$activity_3 == 4] <- "often"
Data_clean$lifestyle_snowSportsFreq_factor[Data_raw$activity_3 == 5] <- "very_often"
Data_clean$lifestyle_snowSportsFreq_factor <- factor(Data_clean$lifestyle_snowSportsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(Data_clean$lifestyle_snowSportsFreq_factor)

## numeric

Data_clean$lifestyle_snowSportsFreq_numeric <- NA 
Data_clean$lifestyle_snowSportsFreq_numeric <- (Data_raw$activity_3 - 1) / 4
table(Data_clean$ifestyle_snowSportsFreq_numeric)

## bin

Data_clean$lifestyle_snowSportsFreq_bin <- NA
Data_clean$lifestyle_snowSportsFreq_bin[Data_raw$activity_3  == 1] <- 0
Data_clean$lifestyle_snowSportsFreq_bin[Data_raw$activity_3  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_snowSportsFreq_bin) 


## activity_4 -----------------------------------------------------------

table(Data_raw$activity_4) 
Data_clean$lifestyle_teamSportsFreq <- NA
Data_raw$activity_4 <- as.numeric(Data_raw$activity_4)
Data_clean$lifestyle_teamSportsFreq[Data_raw$activity_4 == 1] <- 1
Data_clean$lifestyle_teamSportsFreq[Data_raw$activity_4 == 2] <- 2
Data_clean$lifestyle_teamSportsFreq[Data_raw$activity_4 == 3] <- 3
Data_clean$lifestyle_teamSportsFreq[Data_raw$activity_4 == 4] <- 4
Data_clean$lifestyle_teamSportsFreq[Data_raw$activity_4 == 5] <- 5
table(Data_clean$lifestyle_teamSportsFreq)

## factor

table(Data_clean$lifestyle_teamSportsFreq)
Data_clean$lifestyle_teamSportsFreq_factor <- NA
Data_clean$lifestyle_teamSportsFreq_factor[Data_raw$activity_4 == 1] <- "never"
Data_clean$lifestyle_teamSportsFreq_factor[Data_raw$activity_4 == 2] <- "almost_never"
Data_clean$lifestyle_teamSportsFreq_factor[Data_raw$activity_4 == 3] <- "sometimes"
Data_clean$lifestyle_teamSportsFreq_factor[Data_raw$activity_4 == 4] <- "often"
Data_clean$lifestyle_teamSportsFreq_factor[Data_raw$activity_4 == 5] <- "very_often"
Data_clean$lifestyle_teamSportsFreq_factor <- factor(Data_clean$lifestyle_teamSportsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(Data_clean$lifestyle_teamSportsFreq_factor)

## numeric

Data_clean$lifestyle_teamSportsFreq_numeric <- (Data_raw$activity_4 - 1) / 4
Data_clean$lifestyle_teamSportsFreq_numeric <- NA 
table(Data_clean$lifestyle_teamSportsFreq_numeric)

## bin

Data_clean$lifestyle_teamSportsFreq_bin <- NA
Data_clean$lifestyle_teamSportsFreq_bin[Data_raw$activity_4  == 1] <- 0
Data_clean$lifestyle_teamSportsFreq_bin[Data_raw$activity_4  %in% c(2, 3, 4, 5)] <- 1
table(Data_cleanlifestyle_teamSportsFreq_bin)


## activity_5 ------------------------------------------------------------

table(Data_raw$activity_5) 
Data_clean$lifestyle_goMuseumsFreq <- NA
Data_raw$activity_5 <- as.numeric(Data_raw$activity_5)
Data_clean$lifestyle_goMuseumsFreq[Data_raw$activity_5 == 1] <- 1
Data_clean$lifestyle_goMuseumsFreq[Data_raw$activity_5 == 2] <- 2
Data_clean$lifestyle_goMuseumsFreq[Data_raw$activity_5 == 3] <- 3
Data_clean$lifestyle_goMuseumsFreq[Data_raw$activity_5 == 4] <- 4
Data_clean$lifestyle_goMuseumsFreq[Data_raw$activity_5 == 5] <- 5
table(Data_clean$lifestyle_goMuseumsFreq)

## factor

table(Data_clean$lifestyle_goMuseumsFreq)
Data_clean$lifestyle_goMuseumsFreq_factor <- NA
Data_clean$lifestyle_goMuseumsFreq_factor[Data_raw$activity_5 == 1] <- "never"
Data_clean$lifestyle_goMuseumsFreq_factor[Data_raw$activity_5 == 2] <- "almost_never"
Data_clean$lifestyle_goMuseumsFreq_factor[Data_raw$activity_5 == 3] <- "sometimes"
Data_clean$lifestyle_goMuseumsFreq_factor[Data_raw$activity_5 == 4] <- "often"
Data_clean$lifestyle_goMuseumsFreq_factor[Data_raw$activity_5 == 5] <- "very_often"
Data_clean$lifestyle_goMuseumsFreq_factor <- factor(Data_clean$lifestyle_goMuseumsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(Data_clean$lifestyle_goMuseumsFreq_factor)

## numeric

Data_clean$lifestyle_goMuseumsFreq_numeric <- NA 
Data_clean$lifestyle_goMuseumsFreq_numeric <- (Data_raw$activity_5 - 1) / 4
table(Data_clean$lifestyle_goMuseumsFreq_numeric)

## bin

Data_clean$lifestyle_goMuseumsFreq_bin <- NA
Data_clean$lifestyle_goMuseumsFreq_bin[Data_raw$activity_5  == 1] <- 0
Data_clean$lifestyle_goMuseumsFreq_bin[Data_raw$activity_5  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_goMuseumsFreq_bin)


## activity_6 ------------------------------------------------------------

table(Data_raw$activity_6) 
Data_clean$lifestyle_performingArtsFreq <- NA
Data_raw$activity_6 <- as.numeric(Data_raw$activity_6)
Data_clean$lifestyle_performingArtsFreq[Data_raw$activity_6 == 1] <- 1
Data_clean$lifestyle_performingArtsFreq[Data_raw$activity_6 == 2] <- 2
Data_clean$lifestyle_performingArtsFreq[Data_raw$activity_6 == 3] <- 3
Data_clean$lifestyle_performingArtsFreq[Data_raw$activity_6 == 4] <- 4
Data_clean$lifestyle_performingArtsFreq[Data_raw$activity_6 == 5] <- 5
table(Data_clean$lifestyle_performingArtsFreq)

## factor

table(Data_clean$lifestyle_performingArtsFreq)
Data_clean$lifestyle_performingArtsFreq_factor <- NA
Data_clean$lifestyle_performingArtsFreq_factor[Data_raw$activity_6 == 1] <- "never"
Data_clean$lifestyle_performingArtsFreq_factor[Data_raw$activity_6 == 2] <- "almost_never"
Data_clean$lifestyle_performingArtsFreq_factor[Data_raw$activity_6 == 3] <- "sometimes"
Data_clean$lifestyle_performingArtsFreq_factor[Data_raw$activity_6 == 4] <- "often"
Data_clean$lifestyle_performingArtsFreq_factor[Data_raw$activity_6 == 5] <- "very_often"
Data_clean$lifestyle_performingArtsFreq_factor <- factor(Data_clean$lifestyle_performingArtsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(Data_clean$lifestyle_performingArtsFreq_factor)

## numeric

Data_clean$lifestyle_performingArtsFreq_numeric <- NA 
Data_clean$lifestyle_performingArtsFreq_numeric <- (Data_raw$activity_6 - 1) / 4
table(Data_clean$lifestyle_performingArtsFreq_numeric)

## bin

Data_clean$lifestyle_performingArtsFreq_bin <- NA
Data_clean$lifestyle_performingArtsFreq_bin[Data_raw$activity_6  == 1] <- 0
Data_clean$lifestyle_performingArtsFreq_bin[Data_raw$activity_6  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_performingArtsFreq_bin)


## activity_7 ------------------------------------------------------------

table(Data_raw$activity_7) 
Data_clean$lifestyle_manualTaskFreq <- NA
Data_raw$activity_7 <- as.numeric(Data_raw$activity_7)
Data_clean$lifestyle_manualTaskFreq[Data_raw$activity_7 == 1] <- 1
Data_clean$lifestyle_manualTaskFreq[Data_raw$activity_7 == 2] <- 2
Data_clean$lifestyle_manualTaskFreq[Data_raw$activity_7 == 3] <- 3
Data_clean$lifestyle_manualTaskFreq[Data_raw$activity_7 == 4] <- 4
Data_clean$lifestyle_manualTaskFreq[Data_raw$activity_7 == 5] <- 5
table(Data_clean$lifestyle_manualTaskFreq)

## factor

table(Data_clean$lifestyle_manualTaskFreq)
Data_clean$lifestyle_manualTaskFreq_factor <- NA
Data_clean$lifestyle_manualTaskFreq_factor[Data_raw$activity_7 == 1] <- "never"
Data_clean$lifestyle_manualTaskFreq_factor[Data_raw$activity_7 == 2] <- "almost_never"
Data_clean$lifestyle_manualTaskFreq_factor[Data_raw$activity_7 == 3] <- "sometimes"
Data_clean$lifestyle_manualTaskFreq_factor[Data_raw$activity_7 == 4] <- "often"
Data_clean$lifestyle_manualTaskFreq_factor[Data_raw$activity_7 == 5] <- "very_often"
Data_clean$lifestyle_manualTaskFreq_factor <- factor(Data_clean$lifestyle_manualTaskFreq_factor,
                                                           levels = c("never",
                                                                      "almost_never",
                                                                      "sometimes",
                                                                      "often",
                                                                      "very_often"),
                                                           ordered = TRUE)
table(Data_clean$lifestyle_manualTaskFreq_factor)

## numeric

Data_clean$lifestyle_manualTaskFreq_numeric <- NA 
Data_clean$lifestyle_manualTaskFreq_numeric <- (Data_raw$activity_7 - 1) / 4
table(Data_clean$lifestyle_manualTaskFreq_numeric)

## bin

Data_clean$lifestyle_manualTaskFreq_bin <- NA
Data_clean$lifestyle_manualTaskFreq_bin[Data_raw$activity_7  == 1] <- 0
Data_clean$lifestyle_manualTaskFreq_bin[Data_raw$activity_7  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_manualTaskFreq_bin)


## activity_8 ------------------------------------------------------------

table(Data_raw$activity_8) 
Data_clean$lifestyle_motorizedActFreq <- NA
Data_raw$activity_8 <- as.numeric(Data_raw$activity_8)
Data_clean$lifestyle_motorizedActFreq[Data_raw$activity_8 == 1] <- 1
Data_clean$lifestyle_motorizedActFreq[Data_raw$activity_8 == 2] <- 2
Data_clean$lifestyle_motorizedActFreq[Data_raw$activity_8 == 3] <- 3
Data_clean$lifestyle_motorizedActFreq[Data_raw$activity_8 == 4] <- 4
Data_clean$lifestyle_motorizedActFreq[Data_raw$activity_8 == 5] <- 5
table(Data_clean$lifestyle_motorizedActFreq)

## factor

table(Data_clean$lifestyle_motorizedActFreq)
Data_clean$lifestyle_motorizedActFreq_factor <- NA
Data_clean$lifestyle_motorizedActFreq_factor[Data_raw$activity_8 == 1] <- "never"
Data_clean$lifestyle_motorizedActFreq_factor[Data_raw$activity_8 == 2] <- "almost_never"
Data_clean$lifestyle_motorizedActFreq_factor[Data_raw$activity_8 == 3] <- "sometimes"
Data_clean$lifestyle_motorizedActFreq_factor[Data_raw$activity_8 == 4] <- "often"
Data_clean$lifestyle_motorizedActFreq_factor[Data_raw$activity_8 == 5] <- "very_often"
Data_clean$lifestyle_motorizedActFreq_factor <- factor(Data_clean$lifestyle_motorizedActFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(Data_clean$lifestyle_motorizedActFreq_factor)

## numeric

Data_clean$lifestyle_motorizedActFreq_numeric <- NA 
Data_clean$lifestyle_motorizedActFreq_numeric <- (Data_raw$activity_8 - 1) / 4
table(Data_clean$lifestyle_motorizedActFreq_numeric)

## bin

Data_clean$lifestyle_motorizedActFreq_bin <- NA
Data_clean$lifestyle_motorizedActFreq_bin[Data_raw$activity_8  == 1] <- 0
Data_clean$lifestyle_motorizedActFreq_bin[Data_raw$activity_8  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_motorizedActFreq_bin)


## activity_9 ------------------------------------------------------------

table(Data_raw$activity_9) 
Data_clean$lifestyle_unmotorizedActFreq <- NA
Data_raw$activity_9 <- as.numeric(Data_raw$activity_9)
Data_clean$lifestyle_unmotorized_activities_freq[Data_raw$activity_9 == 2] <- 2
Data_clean$lifestyle_unmotorizedActFreq[Data_raw$activity_9 == 1] <- 1
Data_clean$lifestyle_unmotorizedActFreq[Data_raw$activity_9 == 3] <- 3
Data_clean$lifestyle_unmotorizedActFreq[Data_raw$activity_9 == 4] <- 4
Data_clean$lifestyle_unmotorizedActFreq[Data_raw$activity_9 == 5] <- 5
table(Data_clean$lifestyle_unmotorizedActFreq)

## factor

table(Data_clean$lifestyle_unmotorizedActFreq)
Data_clean$lifestyle_unmotorizedActFreq_factor <- NA
Data_clean$lifestyle_unmotorizedActFreq_factor[Data_raw$activity_9 == 1] <- "never"
Data_clean$lifestyle_unmotorizedActFreq_factor[Data_raw$activity_9 == 2] <- "almost_never"
Data_clean$lifestyle_unmotorizedActFreq_factor[Data_raw$activity_9 == 3] <- "sometimes"
Data_clean$lifestyle_unmotorizedActFreq_factor[Data_raw$activity_9 == 4] <- "often"
Data_clean$lifestyle_unmotorizedActFreq_factor[Data_raw$activity_9 == 5] <- "very_often"
Data_clean$lifestyle_unmotorizedActFreq_factor <- factor(Data_clean$lifestyle_unmotorizedActFreq_factor,
                                                                levels = c("never",
                                                                           "almost_never",
                                                                           "sometimes",
                                                                           "often",
                                                                           "very_often"),
                                                                ordered = TRUE)
table(Data_clean$lifestyle_unmotorizedActFreq_factor)

## numeric

Data_clean$lifestyle_unmotorizedActFreq_numeric <- NA 
Data_clean$lifestyle_unmotorizedActFreq_numeric <- (Data_raw$activity_9 - 1) / 4
table(Data_clean$lifestyle_unmotorizedActFreq_numeric)

## bin

Data_clean$lifestyle_unmotorizedActFreq_bin <- NA
Data_clean$lifestyle_unmotorizedActFreq_bin[Data_raw$activity_9  == 1] <- 0
Data_clean$lifestyle_unmotorizedActFreq_bin[Data_raw$activity_9  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_unmotorizedActFreq_bin)


## activity_10 -----------------------------------------------------------

table(Data_raw$activity_10) 
Data_clean$lifestyle_volunteeringFreq <- NA
Data_raw$activity_10 <- as.numeric(Data_raw$activity_10)
Data_clean$lifestyle_volunteeringFreq[Data_raw$activity_10 == 1] <- 1
Data_clean$lifestyle_volunteeringFreq[Data_raw$activity_10 == 2] <- 2
Data_clean$lifestyle_volunteeringFreq[Data_raw$activity_10 == 3] <- 3
Data_clean$lifestyle_volunteeringFreq[Data_raw$activity_10 == 4] <- 4
Data_clean$lifestyle_volunteeringFreq[Data_raw$activity_10 == 5] <- 5
table(Data_clean$lifestyle_volunteeringFreq)

## factor

table(Data_clean$lifestyle_volunteeringFreq)
Data_clean$lifestyle_volunteeringFreq_factor <- NA
Data_clean$lifestyle_volunteeringFreq_factor[Data_raw$activity_10 == 1] <- "never"
Data_clean$lifestyle_volunteeringFreq_factor[Data_raw$activity_10 == 2] <- "almost_never"
Data_clean$lifestyle_volunteeringFreq_factor[Data_raw$activity_10 == 3] <- "sometimes"
Data_clean$lifestyle_volunteeringFreq_factor[Data_raw$activity_10 == 4] <- "often"
Data_clean$lifestyle_volunteeringFreq_factor[Data_raw$activity_10 == 5] <- "very_often"
Data_clean$lifestyle_volunteeringFreq_factor <- factor(Data_clean$lifestyle_volunteeringFreq_factor,
                                                                  levels = c("never",
                                                                             "almost_never",
                                                                             "sometimes",
                                                                             "often",
                                                                             "very_often"),
                                                                  ordered = TRUE)
table(Data_clean$lifestyle_volunteeringFreq_factor)

## numeric

Data_clean$lifestyle_volunteeringFreq_numeric <- NA 
Data_clean$lifestyle_volunteeringFreq_numeric <- (Data_raw$activity_10 - 1) / 4
table(Data_clean$lifestyle_volunteeringFreq_numeric)

## bin

Data_clean$lifestyle_volunteeringFreq_bin <- NA
Data_clean$lifestyle_volunteeringFreq_bin[Data_raw$activity_10  == 1] <- 0
Data_clean$lifestyle_volunteeringFreq_bin[Data_raw$activity_10  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_volunteeringFreq_bin)


## activity_11 -----------------------------------------------------------

table(Data_raw$activity_11) 
Data_clean$lifestyle_makeArtFreq <- NA
Data_raw$activity_11 <- as.numeric(Data_raw$activity_11)
Data_clean$lifestyle_makeArtFreq[Data_raw$activity_11 == 1] <- 1
Data_clean$lifestyle_makeArtFreq[Data_raw$activity_11 == 2] <- 2
Data_clean$lifestyle_makeArtFreq[Data_raw$activity_11 == 3] <- 3
Data_clean$lifestyle_makeArtFreq[Data_raw$activity_11 == 4] <- 4
Data_clean$lifestyle_makeArtFreq[Data_raw$activity_11 == 5] <- 5
table(Data_clean$lifestyle_makeArtFreq)

## factor

table(Data_clean$lifestyle_makeArtFreq)
Data_clean$lifestyle_makeArtFreq_factor <- NA
Data_clean$lifestyle_makeArtFreq_factor[Data_raw$activity_11 == 1] <- "never"
Data_clean$lifestyle_makeArtFreq_factor[Data_raw$activity_11 == 2] <- "almost_never"
Data_clean$lifestyle_makeArtFreq_factor[Data_raw$activity_11 == 3] <- "sometimes"
Data_clean$lifestyle_makeArtFreq_factor[Data_raw$activity_11 == 4] <- "often"
Data_clean$lifestyle_makeArtFreq_factor[Data_raw$activity_11 == 5] <- "very_often"
Data_clean$lifestyle_makeArtFreq_factor <- factor(Data_clean$lifestyle_makeArtFreq_factor,
                                                        levels = c("never",
                                                                   "almost_never",
                                                                   "sometimes",
                                                                   "often",
                                                                   "very_often"),
                                                        ordered = TRUE)
table(Data_clean$lifestyle_makeArtFreq_factor)

## numeric

Data_clean$lifestyle_makeArtFreq_numeric <- NA 
Data_clean$lifestyle_makeArtFreq_numeric <- (Data_raw$activity_11 - 1) / 4
table(Data_clean$lifestyle_makeArtFreq_numeric)

## bin

Data_clean$lifestyle_makeArtFreq_bin <- NA
Data_clean$lifestyle_makeArtFreq_bin[Data_raw$activity_11  == 1] <- 0
Data_clean$lifestyle_makeArtFreq_bin[Data_raw$activity_11  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$llifestyle_makeArtFreq_bin)


## activity_12 -----------------------------------------------------------

table(Data_raw$activity_12) 
Data_clean$lifestyle_travelFreq <- NA
Data_raw$activity_12 <- as.numeric(Data_raw$activity_12)
Data_clean$lifestyle_travelFreq[Data_raw$activity_12 == 1] <- 1
Data_clean$lifestyle_travelFreq[Data_raw$activity_12 == 2] <- 2
Data_clean$lifestyle_travelFreq[Data_raw$activity_12 == 3] <- 3
Data_clean$lifestyle_travelFreq[Data_raw$activity_12 == 4] <- 4
Data_clean$lifestyle_travelFreq[Data_raw$activity_12 == 5] <- 5
table(Data_clean$lifestyle_travelFreq)

## factor

table(Data_clean$lifestyle_travelFreq)
Data_clean$lifestyle_travelFreq_factor <- NA
Data_clean$lifestyle_travelFreq_factor[Data_raw$activity_12 == 1] <- "never"
Data_clean$lifestyle_travelFreq_factor[Data_raw$activity_12 == 2] <- "almost_never"
Data_clean$lifestyle_travelFreq_factor[Data_raw$activity_12 == 3] <- "sometimes"
Data_clean$lifestyle_travelFreq_factor[Data_raw$activity_12 == 4] <- "often"
Data_clean$lifestyle_travelFreq_factor[Data_raw$activity_12 == 5] <- "very_often"
Data_clean$lifestyle_travelFreq_factor <- factor(Data_clean$lifestyle_travelFreq_factor,
                                                          levels = c("never",
                                                                     "almost_never",
                                                                     "sometimes",
                                                                     "often",
                                                                     "very_often"),
                                                          ordered = TRUE)
table(Data_clean$lifestyle_travelFreq_factor)

## numeric

Data_clean$lifestyle_travelFreq_numeric <- NA 
Data_clean$lifestyle_travelFreq_numeric <- (Data_raw$activity_12 - 1) / 4
table(Data_clean$lifestyle_travelFreq_numeric)

## bin

Data_clean$lifestyle_travelFreq_bin <- NA
Data_clean$lifestyle_travelFreq_bin[Data_raw$activity_12  == 1] <- 0
Data_clean$lifestyle_travelFreq_bin[Data_raw$activity_12  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_travelFreq_bin)


## activity_13 -----------------------------------------------------------

table(Data_raw$activity_13) 
Data_clean$lifestyle_gamingFreq <- NA
Data_raw$activity_13 <- as.numeric(Data_raw$activity_13)
Data_clean$lifestyle_gamingFreq[Data_raw$activity_13 == 1] <- 1
Data_clean$lifestyle_gamingFreq[Data_raw$activity_13 == 2] <- 2
Data_clean$lifestyle_gamingFreq[Data_raw$activity_13 == 3] <- 3
Data_clean$lifestyle_gamingFreq[Data_raw$activity_13 == 4] <- 4
Data_clean$lifestyle_gamingFreq[Data_raw$activity_13 == 5] <- 5
table(Data_clean$lifestyle_gamingFreq)

## factor

table(Data_clean$lifestyle_gamingFreq)
Data_clean$lifestyle_gamingFreq_factor <- NA
Data_clean$lifestyle_gamingFreq_factor[Data_raw$activity_13 == 1] <- "never"
Data_clean$lifestyle_gamingFreq_factor[Data_raw$activity_13 == 2] <- "almost_never"
Data_clean$lifestyle_gamingFreq_factor[Data_raw$activity_13 == 3] <- "sometimes"
Data_clean$lifestyle_gamingFreq_factor[Data_raw$activity_13 == 4] <- "often"
Data_clean$lifestyle_gamingFreq_factor[Data_raw$activity_13 == 5] <- "very_often"
Data_clean$lifestyle_gamingFreq_factor <- factor(Data_clean$lifestyle_gamingFreq_factor,
                                                     levels = c("never",
                                                                "almost_never",
                                                                "sometimes",
                                                                "often",
                                                                "very_often"),
                                                     ordered = TRUE)
table(Data_clean$lifestyle_gamingFreq_factor)

## numeric

Data_clean$lifestyle_gamingFreq_numeric <- NA 
Data_clean$lifestyle_gamingFreq_numeric <- (Data_raw$activity_13 - 1) / 4
table(Data_clean$lifestyle_gamingFreq_numeric)

## bin

Data_clean$lifestyle_gamingFreq_bin <- NA
Data_clean$lifestyle_gamingFreq_bin[Data_raw$activity_13  == 1] <- 0
Data_clean$lifestyle_gamingFreq_bin[Data_raw$activity_13  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$llifestyle_gamingFreq_bin)


## type_transport --------------------------------------------------------

attributes(Data_raw$type_transport)
table(Data_raw$type_transport) 
Data_clean$lifestyle_typeTransport <- NA
Data_raw$type_transport <- as.numeric(Data_raw$type_transport)
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 1] <- "car"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 2] <- "car"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 3] <- "car"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 4] <- "active_transport"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 5] <- "active_transport"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 6] <- "shared_transport"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 7] <- "shared_transport"
Data_clean$lifestyle_typeTransport[Data_raw$type_transport == 8] <- "shared_transport"
table(Data_clean$lifestyle_typeTransport)

## factor



## numeric



## bin

## choice_transport ------------------------------------------------------

attributes(Data_raw$choice_transport)
table(Data_raw$choice_transport) 
Data_clean$lifestyle_choiceTransport <- NA
Data_raw$choice_transport <- as.numeric(Data_raw$choice_transport)
Data_clean$llifestyle_choiceTransport[Data_raw$choice_transport == 1] <- "part_of_who_i_am"
Data_clean$llifestyle_choiceTransport[Data_raw$choice_transport == 2] <- "point_a_to_point_b"
table(Data_clean$lifestyle_choiceTransport)

## factor



## numeric



## bin

Data_clean$lifestyle_transportIdentity_bin <- NA
Data_clean$lifestyle_transportIdentity_bin[Data_raw$choice_transport  == 1] <- 0
Data_clean$lifestyle_transportIdentity_bin[Data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_transportIdentity_bin)

Data_clean$lifestyle_transportEfficiency_bin <- NA
Data_clean$lifestyle_transportEfficiency_bin[Data_raw$choice_transport  == 1] <- 0
Data_clean$lifestyle_transportEfficiency_bin[Data_raw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(Data_clean$lifestyle_transportEfficiency_bin)

## field_occupation ------------------------------------------------------

attributes(Data_raw$field_occupation)
table(Data_raw$field_occupation)
Data_clean$lifestyle_workField <- NA
Data_clean$lifestyle_workField[Data_raw$field_occupation == 1 | Data_raw$field_occupation == 2 | Data_raw$field_occupation == 7 | Data_raw$field_occupation == 8] <- "economics"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 3] <- "natural_sciences"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 4] <- "health"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 5] <- "education_law_politics"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 6] <- "culture_sports"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 9] <- "agriculture"
Data_clean$lifestyle_workField[Data_raw$field_occupation == 10] <- "manufacturing"
Data_clean$lifestyle_workField <- factor(Data_clean$lifestyle_workField,
                                                   levels = c("economics",
                                                              "natural_sciences",
                                                              "health",
                                                              "education_law_politics",
                                                              "culture_sports",
                                                              "agriculture",
                                                              "manufacturing"),
                                                   ordered = TRUE)
table(Data_clean$lifestyle_workField)

## type_occupation -------------------------------------------------------

attributes(Data_raw$type_occupation)
table(Data_raw$type_occupation)
Data_clean$lifestyle_workType <- NA
Data_clean$lifestyle_workType[Data_raw$type_occupation == 1] <- "labour_job"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 2] <- "intermediate_job"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 3] <- "technical_job"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 4] <- "professional_job"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 5] <- "management_job"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 6] <- "other"
Data_clean$lifestyle_workType[Data_raw$type_occupation == 7] <- "no_job"
Data_clean$lifestyle_workType <- factor(Data_clean$lifestyle_workType,
                                         levels = c("labour_job",
                                                    "intermediate_job",
                                                    "technical_job",
                                                    "professional_job",
                                                    "management_job",
                                                    "other",
                                                    "no_job"),
                                         ordered = TRUE)
table(Data_clean$lifestyle_workType)

## clothes_consumption ---------------------------------------------------

attributes(Data_raw$clothes_consumption)
table(Data_raw$clothes_consumption)
Data_clean$lifestyle_consClothes <- NA
Data_clean$lifestyle_consClothes[Data_raw$clothes_consumption == 1 | Data_raw$clothes_consumption == 3 | Data_raw$clothes_consumption == 4] <- "large_retailers"
Data_clean$lifestyle_consClothes[Data_raw$clothes_consumption == 2 | Data_raw$clothes_consumption == 6] <- "small_local_store"
Data_clean$lifestyle_consClothes[Data_raw$clothes_consumption == 5] <- "online_store"
Data_clean$lifestyle_consClothes[Data_raw$clothes_consumption == 7] <- "other"
Data_clean$lifestyle_consClothes <- factor(Data_clean$lifestyle_consClothes,
                                         levels = c("large_retailers",
                                                    "small_local_store",
                                                    "online_store",
                                                    "other"),
                                         ordered = TRUE)
table(Data_clean$lifestyle_consClothes)

## mode_attitude_1 ---------------------------------------------------------

attributes(Data_raw$mode_attitude_1)
table(Data_raw$mode_attitude_1)
Data_clean$lifestyle_followFashion <- NA
Data_clean$lifestyle_followFashion <- Data_raw$mode_attitude_1 / 10
table(Data_clean$lifestyle_followFashion)


## meat_consumption ------------------------------------------------------

attributes(Data_raw$meat_consumption)
table(Data_raw$meat_consumption)
Data_clean$lifestyle_eatMeatFreq <- NA
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 1] <- 0
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 2] <- 0.17
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 3] <- 0.33
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 4] <- 0.5
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 5] <- 0.67
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 6] <- 0.83
Data_clean$lifestyle_eatMeatFreq[Data_raw$meat_consumption == 7] <- 1
table(Data_clean$lifestyle_eatMeatFreq)

## meal_time -------------------------------------------------------------

## Sais pas trop comment cleaner ça ##
## On va devoir attendre les vrais répondants pour pouvoir le cleaner ##
attributes(Data_raw$meal_time_6)
table(Data_raw$meal_time_6)

## fridge_1 ----------------------------------------------------------------

attributes(Data_raw$fridge_1)
table(Data_raw$fridge_1)
Data_clean$lifestyle_fridgeVegetalMilk <- NA
Data_clean$lifestyle_fridgeVegetalMilk[Data_raw$fridge_1 == 1] <- 1
Data_clean$lifestyle_fridgeVegetalMilk[Data_raw$fridge_1 == 2] <- 0
table(Data_clean$lifestyle_fridgeVegetalMilk)

### fridge_2 -------------------------------------------------------------

attributes(Data_raw$fridge_2)
table(Data_raw$fridge_2)
Data_clean$lifestyle_fridgeTofuTempeh <- NA
Data_clean$lifestyle_fridgeTofuTempeh[Data_raw$fridge_2 == 1] <- 1
Data_clean$lifestyle_fridgeTofuTempeh[Data_raw$fridge_2 == 2] <- 0
table(Data_clean$lifestyle_fridgeTofuTempeh)

### fridge_3 -------------------------------------------------------------

table(Data_raw$fridge_3)
Data_clean$lifestyle_fridgeEnergyDrink <- NA
Data_clean$lifestyle_fridgeEnergyDrink[Data_raw$fridge_3 == 1] <- 1
Data_clean$lifestyle_fridgeEnergyDrink[Data_raw$fridge_3 == 2] <- 0
table(Data_clean$lifestyle_fridgeEnergyDrink)

### fridge_4 -------------------------------------------------------------

table(Data_raw$fridge_4)
Data_clean$lifestyle_fridgePizza <- NA
Data_clean$lifestyle_fridgePizza[Data_raw$fridge_4 == 1] <- 1
Data_clean$lifestyle_fridgePizza[Data_raw$fridge_4 == 2] <- 0
table(Data_clean$lifestyle_fridgePizza)

### fridge_5 -------------------------------------------------------------

attributes(Data_raw$fridge_5)
table(Data_raw$fridge_5)
Data_clean$lifestyle_fridgeButterTarts <- NA
Data_clean$lifestyle_fridgeButterTarts[Data_raw$fridge_5 == 1] <- 1
Data_clean$lifestyle_fridgeButterTarts[Data_raw$fridge_5 == 2] <- 0
table(Data_clean$lifestyle_fridgeButterTarts)

### fridge_6 -------------------------------------------------------------

table(Data_raw$fridge_6)
Data_clean$lifestyle_fridgeBacon <- NA
Data_clean$lifestyle_fridgeBacon[Data_raw$fridge_6 == 1] <- 1
Data_clean$lifestyle_fridgeBacon[Data_raw$fridge_6 == 2] <- 0
table(Data_clean$lifestyle_fridgeBacon)

### fridge_7 -------------------------------------------------------------

table(Data_raw$fridge_7)
Data_clean$lifestyle_fridgeCeasarDressing <- NA
Data_clean$lifestyle_fridgeCeasarDressing[Data_raw$fridge_7 == 1] <- 1
Data_clean$lifestyle_fridgeCeasarDressing[Data_raw$fridge_7 == 2] <- 0
table(Data_clean$lifestyle_fridgeCeasarDressing)

### fridge_8 -------------------------------------------------------------

table(Data_raw$fridge_8)
Data_clean$lifestyle_fridgeOrganicVeggies <- NA
Data_clean$lifestyle_fridgeOrganicVeggies[Data_raw$fridge_8 == 1] <- 1
Data_clean$lifestyle_fridgeOrganicVeggies[Data_raw$fridge_8 == 2] <- 0
table(Data_clean$lifestyle_fridgeOrganicVeggies)

### fridge_9 -------------------------------------------------------------

table(Data_raw$fridge_9)
Data_clean$lifestyle_fridgeHotSauce <- NA
Data_clean$lifestyle_fridgeHotSauce[Data_raw$fridge_9 == 1] <- 1
Data_clean$lifestyle_fridgeHotSauce[Data_raw$fridge_9 == 2] <- 0
table(Data_clean$lifestyle_fridgeHotSauce)

### fridge_10 -------------------------------------------------------------

table(Data_raw$fridge_10)
Data_clean$lifestyle_fridgeSoftDrinks <- NA
Data_clean$lifestyle_fridgeSoftDrinks[Data_raw$fridge_10 == 1] <- 1
Data_clean$lifestyle_fridgeSoftDrinks[Data_raw$fridge_10 == 2] <- 0
table(Data_clean$lifestyle_fridgeSoftDrinks)

### fridge_11 ------------------------------------------------------------

table(Data_raw$fridge_11)
Data_clean$lifestyle_fridgeGroundBeef <- NA
Data_clean$lifestyle_fridgeGroundBeef[Data_raw$fridge_11 == 1] <- 1
Data_clean$lifestyle_fridgeGroundBeef[Data_raw$fridge_11 == 2] <- 0
table(Data_clean$lifestyle_fridgeGroundBeef)


## coffee_consumption ---------------------------------------------------

table(Data_raw$coffee_consumption)
Data_clean$lifestyle_consCoffee <- NA
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 1] <- "tim_hortons"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 2] <- "starbucks"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 3] <- "second_cup"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 4] <- "mcdonalds"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 5] <- "other"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 6] <- "independent"
Data_clean$lifestyle_consCoffee[Data_raw$coffee_consumption == 7] <- "no_coffee"
Data_clean$lifestyle_consCoffee <- factor(Data_clean$lifestyle_consCoffee,
                                         levels = c("tim_hortons",
                                                    "starbucks",
                                                    "second_cup",
                                                    "mcdonalds",
                                                    "other",
                                                    "independent",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(Data_clean$lifestyle_consCoffee)

## coffee_machine --------------------------------------------------------

table(Data_raw$coffee_machine)
Data_clean$lifestyle_makeCoffee <- NA
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 1] <- "filter_coffee_maker"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 2] <- "automatic_manual_machine"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 3] <- "percolator"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 4] <- "french_press"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 5] <- "single_cup_coffee"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 6] <- "instant_coffee"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 7] <- "italian_coffee_maker"
Data_clean$lifestyle_makeCoffee[Data_raw$coffee_machine == 8] <- "no_coffee"
Data_clean$lifestyle_makeCoffee <- factor(Data_clean$lifestyle_makeCoffee,
                                         levels = c("filter_coffee_maker",
                                                    "automatic_manual_machine",
                                                    "percolator",
                                                    "french_press",
                                                    "single_cup_coffee",
                                                    "instant_coffee",
                                                    "italian_coffee_maker",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(Data_clean$lifestyle_makeCoffee)

## cons_pet --------------------------------------------------------------

attributes(Data_raw$cons_pets)
table(Data_raw$cons_pets)
Data_clean$lifestyle_ownPet <- NA
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 1] <- "chat"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 2] <- "chien"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 3] <- "chat et chien"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 4] <- "diverses sortes d'animaux"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 5] <- "autres animaux domestiques"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 6] <- "animaux de ferme"
Data_clean$lifestyle_ownPet[Data_raw$cons_pets == 7] <- "je n'ai pas d'animal de compagnie"
Data_clean$lifestyle_ownPet <- factor(Data_clean$lifestyle_ownPet,
                                             levels = c("chat",
                                                        "chien",
                                                        "chat et chien",
                                                        "diverses sortes d'animaux",
                                                        "autres animaux domestiques",
                                                        "animaux de ferme",
                                                        "je n'ai pas d'animal de compagnie"),
                                             ordered = TRUE)
table(Data_clean$lifestyle_ownPet)

Data_clean$lifestyle_ownPet_bin <- NA
Data_clean$lifestyle_ownPet_bin <- 0 
Data_clean$lifestyle_ownPet_bin[Data_raw$cons_pets %in% c(1:6)] <- 1
table(Data_clean$lifestyle_ownPet_bin)

## smoking ---------------------------------------------------------------

attributes(Data_raw$smoking)
table(Data_raw$smoking)
Data_clean$lifestyle_smokeFreq <- NA
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 1] <- 0
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 2] <- 0.1667
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 3] <- 0.3333
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 4] <- 0.5
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 5] <- 0.6667
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 6] <- 0.8333
Data_clean$lifestyle_smokeFreq[Data_raw$smoking == 7] <- 1
table(Data_clean$lifestyle_smokeFreq)

## alcool_type -----------------------------------------------------------

attributes(Data_raw$alcool_type)
table(Data_raw$alcool_type) 
Data_clean$lifestyle_favAlcool <- NA
Data_clean$lifestyle_favAlcool[Data_raw$alcool_type %in% c(1:4)] <- "wine"
Data_clean$lifestyle_favAlcool[Data_raw$alcool_type %in% c(5:6)] <- "beer"
Data_clean$lifestyle_favAlcool[Data_raw$alcool_type == 7] <- "spirits"
Data_clean$lifestyle_favAlcool[Data_raw$alcool_type == 8] <- "cocktail"
Data_clean$lifestyle_favAlcool[Data_raw$alcool_type == 9] <- "dont_drink"
table(Data_clean$lifestyle_favAlcool)

## alcool_frequency ------------------------------------------------------
Data_clean$lifestyle_alcoolFreq <- NA
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 1] <- 0
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 2] <- 0.1667
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 3] <- 0.3333
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 4] <- 0.5
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 5] <- 0.6667
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 6] <- 0.8333
Data_clean$lifestyle_alcoolFreq[Data_raw$alcool_frequency == 7] <- 1
table(Data_clean$lifestyle_alcoolFreq)


## marijuana_frequency ---------------------------------------------------
Data_clean$lifestyle_weedFreq <- NA
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 1] <- 0
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 2] <- 0.1667
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 3] <- 0.3333
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 4] <- 0.5
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 5] <- 0.6667
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 6] <- 0.8333
Data_clean$lifestyle_weedFreq[Data_raw$marijuana_frequency == 7] <- 1
table(Data_clean$lifestyle_weedFreq)



## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------

attributes(Data_raw$social_media_use)
table(Data_raw$social_media_use)

Data_clean$lifestyle_mostFreqSocialMedia <- NA
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 1] <- "Facebook"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 2] <- "Twitter / X"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 3] <- "Instagram"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 4] <- "Snapchat"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 5] <- "TikTok"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 6] <- "Pinterest"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 7] <- "LinkedIn"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 8] <- "Youtube"
Data_clean$lifestyle_mostFreqSocialMedia[Data_raw$social_media_use == 9] <- "Autre"
Data_clean$lifestyle_mostFreqSocialMedia <- factor(Data_clean$lifestyle_mostFreqSocialMedia)
table(Data_clean$lifestyle_mostFreqSocialMedia)

## social_media_time -----------------------------------------------------

attributes(Data_raw$social_media_time)
Data_clean$lifestyle_socialMediaTime <- NA
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 1] <- 0
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 2] <- 0.2
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 3] <- 0.4
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 4] <- 0.6
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 5] <- 0.8
Data_clean$lifestyle_socialMediaTime[Data_raw$social_media_time == 6] <- 1
table(Data_clean$lifestyle_socialMediaTime)

## clothing_style --------------------------------------------------------

attributes(Data_raw$clothing_style)
table(Data_raw$clothing_style)

## disaggregated
Data_clean$lifestyle_clothingStyle <- NA
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 1] <- "formal"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 2] <- "classic"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 3] <- "casual"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 4] <- "sport"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 5] <- "elegant"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 6] <- "hippie"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 7] <- "punk"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 8] <- "rock"
Data_clean$lifestyle_clothingStyle[Data_raw$clothing_style == 9] <- "other"
Data_clean$lifestyle_clothingStyle <- factor(Data_clean$lifestyle_clothingStyle)
table(Data_clean$lifestyle_clothingStyle)

## grouped
Data_clean$lifestyle_clothingStyleGroups <- NA
Data_clean$lifestyle_clothingStyleGroups[Data_raw$clothing_style %in% c(1, 5)] <- "formal"
Data_clean$lifestyle_clothingStyleGroups[Data_raw$clothing_style %in% c(2, 3, 4)] <- "easygoing"
Data_clean$lifestyle_clothingStyleGroups[Data_raw$clothing_style %in% c(6, 7, 8)] <- "edgy"
Data_clean$lifestyle_clothingStyleGroups <- factor(Data_clean$lifestyle_clothingStyleGroups)
table(Data_clean$lifestyle_clothingStyleGroups)

## number_tattoos --------------------------------------------------------

attributes(Data_raw$number_tattoos)
table(Data_raw$number_tattoos)

Data_clean$lifestyle_numberTattoos <- NA
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 9] <- 0  # "0 tatouage"
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 10] <- 1 # "1 tatouage"
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 11] <- 2 # "2 tatouages"
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 4] <- 3  # "3 tatouages"
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 5] <- 4  # "4 tatouages"
Data_clean$lifestyle_numberTattoos[Data_raw$number_tattoos == 6] <- 5  # "5+ tatouages" traité comme 5

table(Data_clean$lifestyle_numberTattoos, useNA = "ifany")

# Créer la variable binaire 'has_tattoos'
Data_clean$lifestyle_hasTattoos <- as.numeric(Data_clean$lifestyle_numberTattoos > 0)
Data_clean$lifestyle_hasTattoos[is.na(Data_clean$lifestyle_numberTattoos)] <- NA


table(Data_clean$lifestyle_hasTattoos, useNA = "ifany")

## chronotype ------------------------------------------------------------

attributes(Data_raw$chronotype)
table(Data_raw$chronotype)

Data_clean$lifestyle_typeMorningToEvening <- NA
Data_clean$lifestyle_typeMorningToEvening[Data_raw$chronotype == 1] <- 0
Data_clean$lifestyle_typeMorningToEvening[Data_raw$chronotype == 2] <- 0.25
Data_clean$lifestyle_typeMorningToEvening[Data_raw$chronotype == 3] <- 0.5
Data_clean$lifestyle_typeMorningToEvening[Data_raw$chronotype == 4] <- 0.75
Data_clean$lifestyle_typeMorningToEvening[Data_raw$chronotype == 5] <- 1
table(Data_clean$lifestyle_typeMorningToEvening)

## trip ------------------------------------------------------------------

attributes(Data_raw$trip)
table(Data_raw$trip)

Data_clean$lifestyle_typeTravel <- NA
Data_clean$lifestyle_typeTravel[Data_raw$trip == 1] <- "beach"
Data_clean$lifestyle_typeTravel[Data_raw$trip == 2] <- "jungle"
Data_clean$lifestyle_typeTravel[Data_raw$trip == 3] <- "historic"
Data_clean$lifestyle_typeTravel[Data_raw$trip == 4] <- "mountains"
Data_clean$lifestyle_typeTravel <- factor(Data_clean$lifestyle_typeTravel)
table(Data_clean$lifestyle_typeTravel)

