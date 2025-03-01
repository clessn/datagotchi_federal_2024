# lifestyle

## exercise --------------------------------------------------------------

attributes(DataRaw$exercise)
table(DataRaw$exercise)
DataClean$lifestyle_exercise <- NA
DataClean$lifestyle_exercise[DataRaw$exercise == 1] <- "gym"
DataClean$lifestyle_exercise[DataRaw$exercise == 2] <- "play_a_team_sport"
DataClean$lifestyle_exercise[DataRaw$exercise == 3] <- "walk"
DataClean$lifestyle_exercise[DataRaw$exercise == 4] <- "run"
DataClean$lifestyle_exercise[DataRaw$exercise == 5] <- "yoga"
DataClean$lifestyle_exercise[DataRaw$exercise == 6] <- "swimming"
DataClean$lifestyle_exercise[DataRaw$exercise == 7] <- "other"
DataClean$lifestyle_exercise[DataRaw$exercise == 8] <- "i_do_not_exercise"
DataClean$lifestyle_exercise <- factor(DataClean$lifestyle_exercise)
table(DataClean$lifestyle_exercise)

# Each sport bin

DataClean$lifestyle_exerciseGym <- NA
DataClean$lifestyle_exerciseGym[DataRaw$exercise  == 1] <- 1
DataClean$lifestyle_exerciseGym[DataRaw$exercise  != 1] <- 0
table(DataClean$lifestyle_exerciseGym)

DataClean$lifestyle_exerciseTeamSport <- NA
DataClean$lifestyle_exerciseTeamSport[DataRaw$exercise  == 2] <- 1
DataClean$lifestyle_exerciseTeamSport[DataRaw$exercise  != 2] <- 0
table(DataClean$lifestyle_exerciseTeamSport)

DataClean$lifestyle_exerciseWalk <- NA
DataClean$lifestyle_exerciseWalk[DataRaw$exercise == 3] <- 1
DataClean$lifestyle_exerciseWalk[DataRaw$exercise != 3] <- 0
table(DataClean$lifestyle_exerciseWalk)

DataClean$lifestyle_exerciseRun <- NA
DataClean$lifestyle_exerciseRun[DataRaw$exercise == 4] <- 1
DataClean$lifestyle_exerciseRun[DataRaw$exercise != 4] <- 0
table(DataClean$lifestyle_exerciseRun)

DataClean$lifestyle_exerciseYoga <- NA
DataClean$lifestyle_exerciseYoga[DataRaw$exercise == 5] <- 1
DataClean$lifestyle_exerciseYoga[DataRaw$exercise != 5] <- 0
table(DataClean$lifestyle_exerciseYoga)

DataClean$lifestyle_exerciseSwim <- NA
DataClean$lifestyle_exerciseSwim[DataRaw$exercise == 6] <- 1
DataClean$lifestyle_exerciseSwim[DataRaw$exercise != 6] <- 0
table(DataClean$lifestyle_exerciseSwim)

DataClean$lifestyle_exerciseOther <- NA
DataClean$lifestyle_exerciseOther[DataRaw$exercise == 7] <- 1
DataClean$lifestyle_exerciseOther[DataRaw$exercise != 7] <- 0
table(DataClean$lifestyle_exerciseOther)

DataClean$lifestyle_exerciseNone <- NA
DataClean$lifestyle_exerciseNone[DataRaw$exercise == 8] <- 1
DataClean$lifestyle_exerciseNone[DataRaw$exercise != 8] <- 0
table(DataClean$lifestyle_exerciseNone)

# 祖国万岁！

## activity_1 ------------------------------------------------------------

attributes(DataRaw$activity_1)
table(DataRaw$activity_1) 
DataClean$lifestyle_goFishingFreq <- NA
DataRaw$activity_1 <- as.numeric(DataRaw$activity_1)
DataClean$lifestyle_goFishingFreq[DataRaw$activity_1 == 1] <- 1
DataClean$lifestyle_goFishingFreq[DataRaw$activity_1 == 2] <- 2
DataClean$lifestyle_goFishingFreq[DataRaw$activity_1 == 3] <- 3
DataClean$lifestyle_goFishingFreq[DataRaw$activity_1 == 4] <- 4
DataClean$lifestyle_goFishingFreq[DataRaw$activity_1 == 5] <- 5
table(DataClean$lifestyle_goFishingFreq)

## factor

table(DataClean$lifestyle_fishing_freq)
DataClean$lifestyle_goFishingFreq_factor <- NA
DataClean$lifestyle_goFishingFreq_factor[DataRaw$activity_1 == 1] <- "never"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$activity_1 == 2] <- "almost_never"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$activity_1 == 3] <- "sometimes"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$activity_1 == 4] <- "often"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$activity_1 == 5] <- "very_often"
DataClean$lifestyle_goFishingFreq_factor <- factor(DataClean$lifestyle_goFishingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_goFishingFreq_factor)

## numeric

DataClean$lifestyle_goFishingFreq_numeric <- NA 
DataClean$lifestyle_goFishingFreq_numeric <- (DataRaw$activity_1 - 1) / 4
table(DataClean$lifestyle_goFishingFreq_numeric)

## bin

DataClean$lifestyle_goFishingFreq_bin <- NA
DataClean$lifestyle_goFishingFreq_bin[DataRaw$activity_1  == 1] <- 0
DataClean$lifestyle_goFishingFreq_bin[DataRaw$activity_1  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_goFishingFreq_bin) 

## activity_2 ------------------------------------------------------------

table(DataRaw$activity_2) 
DataClean$lifestyle_goHuntingFreq <- NA
DataRaw$activity_2 <- as.numeric(DataRaw$activity_2)
DataClean$lifestyle_goHuntingFreq[DataRaw$activity_2 == 1] <- 1
DataClean$lifestyle_goHuntingFreq[DataRaw$activity_2 == 2] <- 2
DataClean$lifestyle_goHuntingFreq[DataRaw$activity_2 == 3] <- 3
DataClean$lifestyle_goHuntingFreq[DataRaw$activity_2 == 4] <- 4
DataClean$lifestyle_goHuntingFreq[DataRaw$activity_2 == 5] <- 5
table(DataClean$lifestyle_goHuntingFreq)

## factor

table(DataClean$lifestyle_goHuntingFreq)
DataClean$lifestyle_goHuntingFreq_factor <- NA
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$activity_2 == 1] <- "never"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$activity_2 == 2] <- "almost_never"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$activity_2 == 3] <- "sometimes"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$activity_2 == 4] <- "often"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$activity_2 == 5] <- "very_often"
DataClean$lifestyle_goHuntingFreq_factor <- factor(DataClean$lifestyle_goHuntingFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_goHuntingFreq_factor)

## numeric

DataClean$lifestyle_goHuntingFreq_numeric <- NA 
DataClean$lifestyle_goHuntingFreq_numeric <- (DataRaw$activity_2 - 1) / 4
table(DataClean$lifestyle_goHuntingFreq_numeric)

## bin

DataClean$lifestyle_goHuntingFreq_bin <- NA
DataClean$lifestyle_goHuntingFreq_bin[DataRaw$activity_2  == 1] <- 0
DataClean$lifestyle_goHuntingFreq_bin[DataRaw$activity_2  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_goHuntingFreq_bin) 


## activity_3 ------------------------------------------------------------

table(DataRaw$activity_3) 
DataClean$lifestyle_snowSportsFreq <- NA
DataRaw$activity_3 <- as.numeric(DataRaw$activity_3)
DataClean$lifestyle_snowSportsFreq[DataRaw$activity_3 == 1] <- 1
DataClean$lifestyle_snowSportsFreq[DataRaw$activity_3 == 2] <- 2
DataClean$lifestyle_snowSportsFreq[DataRaw$activity_3 == 3] <- 3
DataClean$lifestyle_snowSportsFreq[DataRaw$activity_3 == 4] <- 4
DataClean$lifestyle_snowSportsFreq[DataRaw$activity_3 == 5] <- 5
table(DataClean$lifestyle_snowSportsFreq)

## factor

table(DataClean$lifestyle_snowSportsFreq)
DataClean$lifestyle_snowSportsFreq_factor <- NA
DataClean$lifestyle_snowSportsFreq_factor[DataRaw$activity_3 == 1] <- "never"
DataClean$lifestyle_snowSportsFreq_factor[DataRaw$activity_3 == 2] <- "almost_never"
DataClean$lifestyle_snowSportsFreq_factor[DataRaw$activity_3 == 3] <- "sometimes"
DataClean$lifestyle_snowSportsFreq_factor[DataRaw$activity_3 == 4] <- "often"
DataClean$lifestyle_snowSportsFreq_factor[DataRaw$activity_3 == 5] <- "very_often"
DataClean$lifestyle_snowSportsFreq_factor <- factor(DataClean$lifestyle_snowSportsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_snowSportsFreq_factor)

## numeric

DataClean$lifestyle_snowSportsFreq_numeric <- NA 
DataClean$lifestyle_snowSportsFreq_numeric <- (DataRaw$activity_3 - 1) / 4
table(DataClean$ifestyle_snowSportsFreq_numeric)

## bin

DataClean$lifestyle_snowSportsFreq_bin <- NA
DataClean$lifestyle_snowSportsFreq_bin[DataRaw$activity_3  == 1] <- 0
DataClean$lifestyle_snowSportsFreq_bin[DataRaw$activity_3  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_snowSportsFreq_bin) 


## activity_4 -----------------------------------------------------------

table(DataRaw$activity_4) 
DataClean$lifestyle_teamSportsFreq <- NA
DataRaw$activity_4 <- as.numeric(DataRaw$activity_4)
DataClean$lifestyle_teamSportsFreq[DataRaw$activity_4 == 1] <- 1
DataClean$lifestyle_teamSportsFreq[DataRaw$activity_4 == 2] <- 2
DataClean$lifestyle_teamSportsFreq[DataRaw$activity_4 == 3] <- 3
DataClean$lifestyle_teamSportsFreq[DataRaw$activity_4 == 4] <- 4
DataClean$lifestyle_teamSportsFreq[DataRaw$activity_4 == 5] <- 5
table(DataClean$lifestyle_teamSportsFreq)

## factor

table(DataClean$lifestyle_teamSportsFreq)
DataClean$lifestyle_teamSportsFreq_factor <- NA
DataClean$lifestyle_teamSportsFreq_factor[DataRaw$activity_4 == 1] <- "never"
DataClean$lifestyle_teamSportsFreq_factor[DataRaw$activity_4 == 2] <- "almost_never"
DataClean$lifestyle_teamSportsFreq_factor[DataRaw$activity_4 == 3] <- "sometimes"
DataClean$lifestyle_teamSportsFreq_factor[DataRaw$activity_4 == 4] <- "often"
DataClean$lifestyle_teamSportsFreq_factor[DataRaw$activity_4 == 5] <- "very_often"
DataClean$lifestyle_teamSportsFreq_factor <- factor(DataClean$lifestyle_teamSportsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(DataClean$lifestyle_teamSportsFreq_factor)

## numeric

DataClean$lifestyle_teamSportsFreq_numeric <- NA 
DataClean$lifestyle_teamSportsFreq_numeric <- (DataRaw$activity_4 - 1) / 4
table(DataClean$lifestyle_teamSportsFreq_numeric)

## bin

DataClean$lifestyle_teamSportsFreq_bin <- NA
DataClean$lifestyle_teamSportsFreq_bin[DataRaw$activity_4  == 1] <- 0
DataClean$lifestyle_teamSportsFreq_bin[DataRaw$activity_4  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_teamSportsFreq_bin)


## activity_5 ------------------------------------------------------------

table(DataRaw$activity_5) 
DataClean$lifestyle_goMuseumsFreq <- NA
DataRaw$activity_5 <- as.numeric(DataRaw$activity_5)
DataClean$lifestyle_goMuseumsFreq[DataRaw$activity_5 == 1] <- 1
DataClean$lifestyle_goMuseumsFreq[DataRaw$activity_5 == 2] <- 2
DataClean$lifestyle_goMuseumsFreq[DataRaw$activity_5 == 3] <- 3
DataClean$lifestyle_goMuseumsFreq[DataRaw$activity_5 == 4] <- 4
DataClean$lifestyle_goMuseumsFreq[DataRaw$activity_5 == 5] <- 5
table(DataClean$lifestyle_goMuseumsFreq)

## factor

table(DataClean$lifestyle_goMuseumsFreq)
DataClean$lifestyle_goMuseumsFreq_factor <- NA
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$activity_5 == 1] <- "never"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$activity_5 == 2] <- "almost_never"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$activity_5 == 3] <- "sometimes"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$activity_5 == 4] <- "often"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$activity_5 == 5] <- "very_often"
DataClean$lifestyle_goMuseumsFreq_factor <- factor(DataClean$lifestyle_goMuseumsFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(DataClean$lifestyle_goMuseumsFreq_factor)

## numeric

DataClean$lifestyle_goMuseumsFreq_numeric <- NA 
DataClean$lifestyle_goMuseumsFreq_numeric <- (DataRaw$activity_5 - 1) / 4
table(DataClean$lifestyle_goMuseumsFreq_numeric)

## bin

DataClean$lifestyle_goMuseumsFreq_bin <- NA
DataClean$lifestyle_goMuseumsFreq_bin[DataRaw$activity_5  == 1] <- 0
DataClean$lifestyle_goMuseumsFreq_bin[DataRaw$activity_5  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_goMuseumsFreq_bin)


## activity_6 ------------------------------------------------------------

table(DataRaw$activity_6) 
DataClean$lifestyle_performingArtsFreq <- NA
DataRaw$activity_6 <- as.numeric(DataRaw$activity_6)
DataClean$lifestyle_performingArtsFreq[DataRaw$activity_6 == 1] <- 1
DataClean$lifestyle_performingArtsFreq[DataRaw$activity_6 == 2] <- 2
DataClean$lifestyle_performingArtsFreq[DataRaw$activity_6 == 3] <- 3
DataClean$lifestyle_performingArtsFreq[DataRaw$activity_6 == 4] <- 4
DataClean$lifestyle_performingArtsFreq[DataRaw$activity_6 == 5] <- 5
table(DataClean$lifestyle_performingArtsFreq)

## factor

table(DataClean$lifestyle_performingArtsFreq)
DataClean$lifestyle_performingArtsFreq_factor <- NA
DataClean$lifestyle_performingArtsFreq_factor[DataRaw$activity_6 == 1] <- "never"
DataClean$lifestyle_performingArtsFreq_factor[DataRaw$activity_6 == 2] <- "almost_never"
DataClean$lifestyle_performingArtsFreq_factor[DataRaw$activity_6 == 3] <- "sometimes"
DataClean$lifestyle_performingArtsFreq_factor[DataRaw$activity_6 == 4] <- "often"
DataClean$lifestyle_performingArtsFreq_factor[DataRaw$activity_6 == 5] <- "very_often"
DataClean$lifestyle_performingArtsFreq_factor <- factor(DataClean$lifestyle_performingArtsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_performingArtsFreq_factor)

## numeric

DataClean$lifestyle_performingArtsFreq_numeric <- NA 
DataClean$lifestyle_performingArtsFreq_numeric <- (DataRaw$activity_6 - 1) / 4
table(DataClean$lifestyle_performingArtsFreq_numeric)

## bin

DataClean$lifestyle_performingArtsFreq_bin <- NA
DataClean$lifestyle_performingArtsFreq_bin[DataRaw$activity_6  == 1] <- 0
DataClean$lifestyle_performingArtsFreq_bin[DataRaw$activity_6  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_performingArtsFreq_bin)


## activity_7 ------------------------------------------------------------

table(DataRaw$activity_7) 
DataClean$lifestyle_manualTaskFreq <- NA
DataRaw$activity_7 <- as.numeric(DataRaw$activity_7)
DataClean$lifestyle_manualTaskFreq[DataRaw$activity_7 == 1] <- 1
DataClean$lifestyle_manualTaskFreq[DataRaw$activity_7 == 2] <- 2
DataClean$lifestyle_manualTaskFreq[DataRaw$activity_7 == 3] <- 3
DataClean$lifestyle_manualTaskFreq[DataRaw$activity_7 == 4] <- 4
DataClean$lifestyle_manualTaskFreq[DataRaw$activity_7 == 5] <- 5
table(DataClean$lifestyle_manualTaskFreq)

## factor

table(DataClean$lifestyle_manualTaskFreq)
DataClean$lifestyle_manualTaskFreq_factor <- NA
DataClean$lifestyle_manualTaskFreq_factor[DataRaw$activity_7 == 1] <- "never"
DataClean$lifestyle_manualTaskFreq_factor[DataRaw$activity_7 == 2] <- "almost_never"
DataClean$lifestyle_manualTaskFreq_factor[DataRaw$activity_7 == 3] <- "sometimes"
DataClean$lifestyle_manualTaskFreq_factor[DataRaw$activity_7 == 4] <- "often"
DataClean$lifestyle_manualTaskFreq_factor[DataRaw$activity_7 == 5] <- "very_often"
DataClean$lifestyle_manualTaskFreq_factor <- factor(DataClean$lifestyle_manualTaskFreq_factor,
                                                           levels = c("never",
                                                                      "almost_never",
                                                                      "sometimes",
                                                                      "often",
                                                                      "very_often"),
                                                           ordered = TRUE)
table(DataClean$lifestyle_manualTaskFreq_factor)

## numeric

DataClean$lifestyle_manualTaskFreq_numeric <- NA 
DataClean$lifestyle_manualTaskFreq_numeric <- (DataRaw$activity_7 - 1) / 4
table(DataClean$lifestyle_manualTaskFreq_numeric)

## bin

DataClean$lifestyle_manualTaskFreq_bin <- NA
DataClean$lifestyle_manualTaskFreq_bin[DataRaw$activity_7  == 1] <- 0
DataClean$lifestyle_manualTaskFreq_bin[DataRaw$activity_7  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_manualTaskFreq_bin)


## activity_8 ------------------------------------------------------------

table(DataRaw$activity_8) 
DataClean$lifestyle_motorizedActFreq <- NA
DataRaw$activity_8 <- as.numeric(DataRaw$activity_8)
DataClean$lifestyle_motorizedActFreq[DataRaw$activity_8 == 1] <- 1
DataClean$lifestyle_motorizedActFreq[DataRaw$activity_8 == 2] <- 2
DataClean$lifestyle_motorizedActFreq[DataRaw$activity_8 == 3] <- 3
DataClean$lifestyle_motorizedActFreq[DataRaw$activity_8 == 4] <- 4
DataClean$lifestyle_motorizedActFreq[DataRaw$activity_8 == 5] <- 5
table(DataClean$lifestyle_motorizedActFreq)

## factor

table(DataClean$lifestyle_motorizedActFreq)
DataClean$lifestyle_motorizedActFreq_factor <- NA
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$activity_8 == 1] <- "never"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$activity_8 == 2] <- "almost_never"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$activity_8 == 3] <- "sometimes"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$activity_8 == 4] <- "often"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$activity_8 == 5] <- "very_often"
DataClean$lifestyle_motorizedActFreq_factor <- factor(DataClean$lifestyle_motorizedActFreq_factor,
                                                       levels = c("never",
                                                                  "almost_never",
                                                                  "sometimes",
                                                                  "often",
                                                                  "very_often"),
                                                       ordered = TRUE)
table(DataClean$lifestyle_motorizedActFreq_factor)

## numeric

DataClean$lifestyle_motorizedActFreq_numeric <- NA 
DataClean$lifestyle_motorizedActFreq_numeric <- (DataRaw$activity_8 - 1) / 4
table(DataClean$lifestyle_motorizedActFreq_numeric)

## bin

DataClean$lifestyle_motorizedActFreq_bin <- NA
DataClean$lifestyle_motorizedActFreq_bin[DataRaw$activity_8  == 1] <- 0
DataClean$lifestyle_motorizedActFreq_bin[DataRaw$activity_8  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_motorizedActFreq_bin)


## activity_9 ------------------------------------------------------------

table(DataRaw$activity_9) 
DataClean$lifestyle_unmotorizedActFreq <- NA
DataRaw$activity_9 <- as.numeric(DataRaw$activity_9)
DataClean$lifestyle_unmotorizedActFreq[DataRaw$activity_9 == 2] <- 2
DataClean$lifestyle_unmotorizedActFreq[DataRaw$activity_9 == 1] <- 1
DataClean$lifestyle_unmotorizedActFreq[DataRaw$activity_9 == 3] <- 3
DataClean$lifestyle_unmotorizedActFreq[DataRaw$activity_9 == 4] <- 4
DataClean$lifestyle_unmotorizedActFreq[DataRaw$activity_9 == 5] <- 5
table(DataClean$lifestyle_unmotorizedActFreq)

## factor

table(DataClean$lifestyle_unmotorizedActFreq)
DataClean$lifestyle_unmotorizedActFreq_factor <- NA
DataClean$lifestyle_unmotorizedActFreq_factor[DataRaw$activity_9 == 1] <- "never"
DataClean$lifestyle_unmotorizedActFreq_factor[DataRaw$activity_9 == 2] <- "almost_never"
DataClean$lifestyle_unmotorizedActFreq_factor[DataRaw$activity_9 == 3] <- "sometimes"
DataClean$lifestyle_unmotorizedActFreq_factor[DataRaw$activity_9 == 4] <- "often"
DataClean$lifestyle_unmotorizedActFreq_factor[DataRaw$activity_9 == 5] <- "very_often"
DataClean$lifestyle_unmotorizedActFreq_factor <- factor(DataClean$lifestyle_unmotorizedActFreq_factor,
                                                                levels = c("never",
                                                                           "almost_never",
                                                                           "sometimes",
                                                                           "often",
                                                                           "very_often"),
                                                                ordered = TRUE)
table(DataClean$lifestyle_unmotorizedActFreq_factor)

## numeric

DataClean$lifestyle_unmotorizedActFreq_numeric <- NA 
DataClean$lifestyle_unmotorizedActFreq_numeric <- (DataRaw$activity_9 - 1) / 4
table(DataClean$lifestyle_unmotorizedActFreq_numeric)

## bin

DataClean$lifestyle_unmotorizedActFreq_bin <- NA
DataClean$lifestyle_unmotorizedActFreq_bin[DataRaw$activity_9  == 1] <- 0
DataClean$lifestyle_unmotorizedActFreq_bin[DataRaw$activity_9  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_unmotorizedActFreq_bin)


## activity_10 -----------------------------------------------------------

table(DataRaw$activity_10) 
DataClean$lifestyle_volunteeringFreq <- NA
DataRaw$activity_10 <- as.numeric(DataRaw$activity_10)
DataClean$lifestyle_volunteeringFreq[DataRaw$activity_10 == 1] <- 1
DataClean$lifestyle_volunteeringFreq[DataRaw$activity_10 == 2] <- 2
DataClean$lifestyle_volunteeringFreq[DataRaw$activity_10 == 3] <- 3
DataClean$lifestyle_volunteeringFreq[DataRaw$activity_10 == 4] <- 4
DataClean$lifestyle_volunteeringFreq[DataRaw$activity_10 == 5] <- 5
table(DataClean$lifestyle_volunteeringFreq)

## factor

table(DataClean$lifestyle_volunteeringFreq)
DataClean$lifestyle_volunteeringFreq_factor <- NA
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$activity_10 == 1] <- "never"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$activity_10 == 2] <- "almost_never"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$activity_10 == 3] <- "sometimes"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$activity_10 == 4] <- "often"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$activity_10 == 5] <- "very_often"
DataClean$lifestyle_volunteeringFreq_factor <- factor(DataClean$lifestyle_volunteeringFreq_factor,
                                                                  levels = c("never",
                                                                             "almost_never",
                                                                             "sometimes",
                                                                             "often",
                                                                             "very_often"),
                                                                  ordered = TRUE)
table(DataClean$lifestyle_volunteeringFreq_factor)

## numeric

DataClean$lifestyle_volunteeringFreq_numeric <- NA 
DataClean$lifestyle_volunteeringFreq_numeric <- (DataRaw$activity_10 - 1) / 4
table(DataClean$lifestyle_volunteeringFreq_numeric)

## bin

DataClean$lifestyle_volunteeringFreq_bin <- NA
DataClean$lifestyle_volunteeringFreq_bin[DataRaw$activity_10  == 1] <- 0
DataClean$lifestyle_volunteeringFreq_bin[DataRaw$activity_10  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_volunteeringFreq_bin)


## activity_11 -----------------------------------------------------------

table(DataRaw$activity_11) 
DataClean$lifestyle_makeArtFreq <- NA
DataRaw$activity_11 <- as.numeric(DataRaw$activity_11)
DataClean$lifestyle_makeArtFreq[DataRaw$activity_11 == 1] <- 1
DataClean$lifestyle_makeArtFreq[DataRaw$activity_11 == 2] <- 2
DataClean$lifestyle_makeArtFreq[DataRaw$activity_11 == 3] <- 3
DataClean$lifestyle_makeArtFreq[DataRaw$activity_11 == 4] <- 4
DataClean$lifestyle_makeArtFreq[DataRaw$activity_11 == 5] <- 5
table(DataClean$lifestyle_makeArtFreq)

## factor

table(DataClean$lifestyle_makeArtFreq)
DataClean$lifestyle_makeArtFreq_factor <- NA
DataClean$lifestyle_makeArtFreq_factor[DataRaw$activity_11 == 1] <- "never"
DataClean$lifestyle_makeArtFreq_factor[DataRaw$activity_11 == 2] <- "almost_never"
DataClean$lifestyle_makeArtFreq_factor[DataRaw$activity_11 == 3] <- "sometimes"
DataClean$lifestyle_makeArtFreq_factor[DataRaw$activity_11 == 4] <- "often"
DataClean$lifestyle_makeArtFreq_factor[DataRaw$activity_11 == 5] <- "very_often"
DataClean$lifestyle_makeArtFreq_factor <- factor(DataClean$lifestyle_makeArtFreq_factor,
                                                        levels = c("never",
                                                                   "almost_never",
                                                                   "sometimes",
                                                                   "often",
                                                                   "very_often"),
                                                        ordered = TRUE)
table(DataClean$lifestyle_makeArtFreq_factor)

## numeric

DataClean$lifestyle_makeArtFreq_numeric <- NA 
DataClean$lifestyle_makeArtFreq_numeric <- (DataRaw$activity_11 - 1) / 4
table(DataClean$lifestyle_makeArtFreq_numeric)

## bin

DataClean$lifestyle_makeArtFreq_bin <- NA
DataClean$lifestyle_makeArtFreq_bin[DataRaw$activity_11  == 1] <- 0
DataClean$lifestyle_makeArtFreq_bin[DataRaw$activity_11  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_makeArtFreq_bin)


## activity_12 -----------------------------------------------------------

table(DataRaw$activity_12) 
DataClean$lifestyle_travelFreq <- NA
DataRaw$activity_12 <- as.numeric(DataRaw$activity_12)
DataClean$lifestyle_travelFreq[DataRaw$activity_12 == 1] <- 1
DataClean$lifestyle_travelFreq[DataRaw$activity_12 == 2] <- 2
DataClean$lifestyle_travelFreq[DataRaw$activity_12 == 3] <- 3
DataClean$lifestyle_travelFreq[DataRaw$activity_12 == 4] <- 4
DataClean$lifestyle_travelFreq[DataRaw$activity_12 == 5] <- 5
table(DataClean$lifestyle_travelFreq)

## factor

table(DataClean$lifestyle_travelFreq)
DataClean$lifestyle_travelFreq_factor <- NA
DataClean$lifestyle_travelFreq_factor[DataRaw$activity_12 == 1] <- "never"
DataClean$lifestyle_travelFreq_factor[DataRaw$activity_12 == 2] <- "almost_never"
DataClean$lifestyle_travelFreq_factor[DataRaw$activity_12 == 3] <- "sometimes"
DataClean$lifestyle_travelFreq_factor[DataRaw$activity_12 == 4] <- "often"
DataClean$lifestyle_travelFreq_factor[DataRaw$activity_12 == 5] <- "very_often"
DataClean$lifestyle_travelFreq_factor <- factor(DataClean$lifestyle_travelFreq_factor,
                                                          levels = c("never",
                                                                     "almost_never",
                                                                     "sometimes",
                                                                     "often",
                                                                     "very_often"),
                                                          ordered = TRUE)
table(DataClean$lifestyle_travelFreq_factor)

## numeric

DataClean$lifestyle_travelFreq_numeric <- NA 
DataClean$lifestyle_travelFreq_numeric <- (DataRaw$activity_12 - 1) / 4
table(DataClean$lifestyle_travelFreq_numeric)

## bin

DataClean$lifestyle_travelFreq_bin <- NA
DataClean$lifestyle_travelFreq_bin[DataRaw$activity_12  == 1] <- 0
DataClean$lifestyle_travelFreq_bin[DataRaw$activity_12  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_travelFreq_bin)


## activity_13 -----------------------------------------------------------

table(DataRaw$activity_13) 
DataClean$lifestyle_gamingFreq <- NA
DataRaw$activity_13 <- as.numeric(DataRaw$activity_13)
DataClean$lifestyle_gamingFreq[DataRaw$activity_13 == 1] <- 1
DataClean$lifestyle_gamingFreq[DataRaw$activity_13 == 2] <- 2
DataClean$lifestyle_gamingFreq[DataRaw$activity_13 == 3] <- 3
DataClean$lifestyle_gamingFreq[DataRaw$activity_13 == 4] <- 4
DataClean$lifestyle_gamingFreq[DataRaw$activity_13 == 5] <- 5
table(DataClean$lifestyle_gamingFreq)

## factor

table(DataClean$lifestyle_gamingFreq)
DataClean$lifestyle_gamingFreq_factor <- NA
DataClean$lifestyle_gamingFreq_factor[DataRaw$activity_13 == 1] <- "never"
DataClean$lifestyle_gamingFreq_factor[DataRaw$activity_13 == 2] <- "almost_never"
DataClean$lifestyle_gamingFreq_factor[DataRaw$activity_13 == 3] <- "sometimes"
DataClean$lifestyle_gamingFreq_factor[DataRaw$activity_13 == 4] <- "often"
DataClean$lifestyle_gamingFreq_factor[DataRaw$activity_13 == 5] <- "very_often"
DataClean$lifestyle_gamingFreq_factor <- factor(DataClean$lifestyle_gamingFreq_factor,
                                                     levels = c("never",
                                                                "almost_never",
                                                                "sometimes",
                                                                "often",
                                                                "very_often"),
                                                     ordered = TRUE)
table(DataClean$lifestyle_gamingFreq_factor)

## numeric

DataClean$lifestyle_gamingFreq_numeric <- NA 
DataClean$lifestyle_gamingFreq_numeric <- (DataRaw$activity_13 - 1) / 4
table(DataClean$lifestyle_gamingFreq_numeric)

## bin

DataClean$lifestyle_gamingFreq_bin <- NA
DataClean$lifestyle_gamingFreq_bin[DataRaw$activity_13  == 1] <- 0
DataClean$lifestyle_gamingFreq_bin[DataRaw$activity_13  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_gamingFreq_bin)


## type_transport --------------------------------------------------------

attributes(DataRaw$type_transport)
table(DataRaw$type_transport) 
DataClean$lifestyle_typeTransport <- NA
DataRaw$type_transport <- as.numeric(DataRaw$type_transport)
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 1] <- "car"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 2] <- "car"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 3] <- "car"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 4] <- "active_transport"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 5] <- "active_transport"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 6] <- "shared_transport"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 7] <- "shared_transport"
DataClean$lifestyle_typeTransport[DataRaw$type_transport == 8] <- "shared_transport"
DataClean$lifestyle_typeTransport <- as.factor(DataClean$lifestyle_typeTransport)
table(DataClean$lifestyle_typeTransport)

## bin

DataClean$lifestyle_typeTransportCar <- NA
DataClean$lifestyle_typeTransportCar[DataRaw$type_transport == 1] <- 1
DataClean$lifestyle_typeTransportCar[DataRaw$type_transport != 1] <- 0
table(DataClean$lifestyle_typeTransportCar)

DataClean$lifestyle_typeTransportSUV <- NA
DataClean$lifestyle_typeTransportSUV[DataRaw$type_transport == 2] <- 1
DataClean$lifestyle_typeTransportSUV[DataRaw$type_transport != 2] <- 0
table(DataClean$lifestyle_typeTransportSUV)

DataClean$lifestyle_typeTransportMoto <- NA
DataClean$lifestyle_typeTransportMoto[DataRaw$type_transport == 3] <- 1
DataClean$lifestyle_typeTransportMoto[DataRaw$type_transport != 3] <- 0
table(DataClean$lifestyle_typeTransportMoto)

DataClean$lifestyle_typeTransportWalk <- NA
DataClean$lifestyle_typeTransportWalk[DataRaw$type_transport == 4] <- 1
DataClean$lifestyle_typeTransportWalk[DataRaw$type_transport != 4] <- 0
table(DataClean$lifestyle_typeTransportWalk)

DataClean$lifestyle_typeTransportBicycle <- NA
DataClean$lifestyle_typeTransportBicycle[DataRaw$type_transport == 5] <- 1
DataClean$lifestyle_typeTransportBicycle[DataRaw$type_transport != 5] <- 0
table(DataClean$lifestyle_typeTransportBicycle)

DataClean$lifestyle_typeTransportPublicTransit <- NA
DataClean$lifestyle_typeTransportPublicTransit[DataRaw$type_transport == 6] <- 1
DataClean$lifestyle_typeTransportPublicTransit[DataRaw$type_transport != 6] <- 0
table(DataClean$lifestyle_typeTransportPublicTransit)

DataClean$lifestyle_typeTransportCarpooling <- NA
DataClean$lifestyle_typeTransportCarpooling[DataRaw$type_transport == 7] <- 1
DataClean$lifestyle_typeTransportCarpooling[DataRaw$type_transport != 7] <- 0
table(DataClean$lifestyle_typeTransportCarpooling)

DataClean$lifestyle_typeTransportCarsharing <- NA
DataClean$lifestyle_typeTransportCarsharing[DataRaw$type_transport == 8] <- 1
DataClean$lifestyle_typeTransportCarsharing[DataRaw$type_transport != 8] <- 0
table(DataClean$lifestyle_typeTransportCarsharing)


## choice_transport ------------------------------------------------------

attributes(DataRaw$choice_transport)
table(DataRaw$choice_transport) 
DataClean$lifestyle_choiceTransport <- NA
DataRaw$choice_transport <- as.numeric(DataRaw$choice_transport)
DataClean$lifestyle_choiceTransport[DataRaw$choice_transport == 1] <- "part_of_who_i_am"
DataClean$lifestyle_choiceTransport[DataRaw$choice_transport == 2] <- "point_a_to_point_b"
table(DataClean$lifestyle_choiceTransport)

## factor



## numeric



## bin

DataClean$lifestyle_transportIdentity_bin <- NA
DataClean$lifestyle_transportIdentity_bin[DataRaw$choice_transport  == 1] <- 0
DataClean$lifestyle_transportIdentity_bin[DataRaw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_transportIdentity_bin)

DataClean$lifestyle_transportEfficiency_bin <- NA
DataClean$lifestyle_transportEfficiency_bin[DataRaw$choice_transport  == 1] <- 0
DataClean$lifestyle_transportEfficiency_bin[DataRaw$choice_transport  %in% c(2, 3, 4, 5)] <- 1
table(DataClean$lifestyle_transportEfficiency_bin)

## field_occupation ------------------------------------------------------

attributes(DataRaw$field_occupation)
table(DataRaw$field_occupation)
DataClean$lifestyle_workField <- NA
DataClean$lifestyle_workField[DataRaw$field_occupation == 1 | DataRaw$field_occupation == 2 | DataRaw$field_occupation == 7 | DataRaw$field_occupation == 8] <- "economics"
DataClean$lifestyle_workField[DataRaw$field_occupation == 3] <- "natural_sciences"
DataClean$lifestyle_workField[DataRaw$field_occupation == 4] <- "health"
DataClean$lifestyle_workField[DataRaw$field_occupation == 5] <- "education_law_politics"
DataClean$lifestyle_workField[DataRaw$field_occupation == 6] <- "culture_sports"
DataClean$lifestyle_workField[DataRaw$field_occupation == 9] <- "agriculture"
DataClean$lifestyle_workField[DataRaw$field_occupation == 10] <- "manufacturing"
DataClean$lifestyle_workField <- factor(DataClean$lifestyle_workField,
                                                   levels = c("economics",
                                                              "natural_sciences",
                                                              "health",
                                                              "education_law_politics",
                                                              "culture_sports",
                                                              "agriculture",
                                                              "manufacturing"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_workField)

## type_occupation -------------------------------------------------------

attributes(DataRaw$type_occupation)
table(DataRaw$type_occupation)
DataClean$lifestyle_workType <- NA
DataClean$lifestyle_workType[DataRaw$type_occupation == 1] <- "labour_job"
DataClean$lifestyle_workType[DataRaw$type_occupation == 2] <- "intermediate_job"
DataClean$lifestyle_workType[DataRaw$type_occupation == 3] <- "technical_job"
DataClean$lifestyle_workType[DataRaw$type_occupation == 4] <- "professional_job"
DataClean$lifestyle_workType[DataRaw$type_occupation == 5] <- "management_job"
DataClean$lifestyle_workType[DataRaw$type_occupation == 6] <- "other"
DataClean$lifestyle_workType[DataRaw$type_occupation == 7] <- "no_job"
DataClean$lifestyle_workType <- factor(DataClean$lifestyle_workType,
                                         levels = c("labour_job",
                                                    "intermediate_job",
                                                    "technical_job",
                                                    "professional_job",
                                                    "management_job",
                                                    "other",
                                                    "no_job"),
                                         ordered = TRUE)
table(DataClean$lifestyle_workType)

## clothes_consumption ---------------------------------------------------

attributes(DataRaw$clothes_consumption)
table(DataRaw$clothes_consumption)
DataClean$lifestyle_consClothes <- NA
DataClean$lifestyle_consClothes[DataRaw$clothes_consumption == 1 | DataRaw$clothes_consumption == 3 | DataRaw$clothes_consumption == 4] <- "large_retailers"
DataClean$lifestyle_consClothes[DataRaw$clothes_consumption == 2 | DataRaw$clothes_consumption == 6] <- "small_local_store"
DataClean$lifestyle_consClothes[DataRaw$clothes_consumption == 5] <- "online_store"
DataClean$lifestyle_consClothes[DataRaw$clothes_consumption == 7] <- "other"
DataClean$lifestyle_consClothes <- factor(DataClean$lifestyle_consClothes,
                                         levels = c("large_retailers",
                                                    "small_local_store",
                                                    "online_store",
                                                    "other"),
                                         ordered = TRUE)
table(DataClean$lifestyle_consClothes)

## bin

DataClean$lifestyle_consClothesDepartment <- NA
DataClean$lifestyle_consClothesDepartment[DataRaw$clothes_consumption == 1] <- 1
DataClean$lifestyle_consClothesDepartment[DataRaw$clothes_consumption != 1] <- 0
table(DataClean$lifestyle_consClothesDepartment)

DataClean$lifestyle_consClothesIndependent <- NA
DataClean$lifestyle_consClothesIndependent[DataRaw$clothes_consumption == 2] <- 1
DataClean$lifestyle_consClothesIndependent[DataRaw$clothes_consumption != 2] <- 0
table(DataClean$lifestyle_consClothesIndependent)

DataClean$lifestyle_consClothesChain <- NA
DataClean$lifestyle_consClothesChain[DataRaw$clothes_consumption == 3] <- 1
DataClean$lifestyle_consClothesChain[DataRaw$clothes_consumption != 3] <- 0
table(DataClean$lifestyle_consClothesChain)

DataClean$lifestyle_consClothesSuperstores <- NA
DataClean$lifestyle_consClothesSuperstores[DataRaw$clothes_consumption == 4] <- 1
DataClean$lifestyle_consClothesSuperstores[DataRaw$clothes_consumption != 4] <- 0
table(DataClean$lifestyle_consClothesSuperstores)

DataClean$lifestyle_consClothesOnline <- NA
DataClean$lifestyle_consClothesOnline[DataRaw$clothes_consumption == 5] <- 1
DataClean$lifestyle_consClothesOnline[DataRaw$clothes_consumption != 5] <- 0
table(DataClean$lifestyle_consClothesOnline)

DataClean$lifestyle_consClothesFrip <- NA
DataClean$lifestyle_consClothesFrip[DataRaw$clothes_consumption == 6] <- 1
DataClean$lifestyle_consClothesFrip[DataRaw$clothes_consumption != 6] <- 0
table(DataClean$lifestyle_consClothesFrip)

DataClean$lifestyle_consClothesOther <- NA
DataClean$lifestyle_consClothesOther[DataRaw$clothes_consumption == 7] <- 1
DataClean$lifestyle_consClothesOther[DataRaw$clothes_consumption != 7] <- 0
table(DataClean$lifestyle_consClothesOther)

## mode_attitude_1 ---------------------------------------------------------

attributes(DataRaw$mode_attitude_1)
table(DataRaw$mode_attitude_1)
DataClean$lifestyle_followFashion <- NA
DataClean$lifestyle_followFashion <- DataRaw$mode_attitude_1 / 10
table(DataClean$lifestyle_followFashion)


## meat_consumption ------------------------------------------------------

attributes(DataRaw$meat_consumption)
table(DataRaw$meat_consumption)
DataClean$lifestyle_eatMeatFreq <- NA
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 1] <- 0
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 2] <- 0.17
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 3] <- 0.33
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 4] <- 0.5
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 5] <- 0.67
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 6] <- 0.83
DataClean$lifestyle_eatMeatFreq[DataRaw$meat_consumption == 7] <- 1
table(DataClean$lifestyle_eatMeatFreq)

## meal_time -------------------------------------------------------------

## Sais pas trop comment cleaner ça ##
## On va devoir attendre les vrais répondants pour pouvoir le cleaner ##
attributes(DataRaw$meal_time_6)
table(DataRaw$meal_time_6)

## fridge_1 ----------------------------------------------------------------

attributes(DataRaw$fridge_1)
table(DataRaw$fridge_1)
DataClean$lifestyle_fridgeVegetalMilk <- NA
DataClean$lifestyle_fridgeVegetalMilk[DataRaw$fridge_1 == 1] <- 1
DataClean$lifestyle_fridgeVegetalMilk[DataRaw$fridge_1 == 2] <- 0
table(DataClean$lifestyle_fridgeVegetalMilk)

### fridge_2 -------------------------------------------------------------

attributes(DataRaw$fridge_2)
table(DataRaw$fridge_2)
DataClean$lifestyle_fridgeTofuTempeh <- NA
DataClean$lifestyle_fridgeTofuTempeh[DataRaw$fridge_2 == 1] <- 1
DataClean$lifestyle_fridgeTofuTempeh[DataRaw$fridge_2 == 2] <- 0
table(DataClean$lifestyle_fridgeTofuTempeh)

### fridge_3 -------------------------------------------------------------

table(DataRaw$fridge_3)
DataClean$lifestyle_fridgeEnergyDrink <- NA
DataClean$lifestyle_fridgeEnergyDrink[DataRaw$fridge_3 == 1] <- 1
DataClean$lifestyle_fridgeEnergyDrink[DataRaw$fridge_3 == 2] <- 0
table(DataClean$lifestyle_fridgeEnergyDrink)

### fridge_4 -------------------------------------------------------------

table(DataRaw$fridge_4)
DataClean$lifestyle_fridgePizza <- NA
DataClean$lifestyle_fridgePizza[DataRaw$fridge_4 == 1] <- 1
DataClean$lifestyle_fridgePizza[DataRaw$fridge_4 == 2] <- 0
table(DataClean$lifestyle_fridgePizza)

### fridge_5 -------------------------------------------------------------

attributes(DataRaw$fridge_5)
table(DataRaw$fridge_5)
DataClean$lifestyle_fridgeButterTarts <- NA
DataClean$lifestyle_fridgeButterTarts[DataRaw$fridge_5 == 1] <- 1
DataClean$lifestyle_fridgeButterTarts[DataRaw$fridge_5 == 2] <- 0
table(DataClean$lifestyle_fridgeButterTarts)

### fridge_6 -------------------------------------------------------------

table(DataRaw$fridge_6)
DataClean$lifestyle_fridgeBacon <- NA
DataClean$lifestyle_fridgeBacon[DataRaw$fridge_6 == 1] <- 1
DataClean$lifestyle_fridgeBacon[DataRaw$fridge_6 == 2] <- 0
table(DataClean$lifestyle_fridgeBacon)

### fridge_7 -------------------------------------------------------------

table(DataRaw$fridge_7)
DataClean$lifestyle_fridgeCeasarDressing <- NA
DataClean$lifestyle_fridgeCeasarDressing[DataRaw$fridge_7 == 1] <- 1
DataClean$lifestyle_fridgeCeasarDressing[DataRaw$fridge_7 == 2] <- 0
table(DataClean$lifestyle_fridgeCeasarDressing)

### fridge_8 -------------------------------------------------------------

table(DataRaw$fridge_8)
DataClean$lifestyle_fridgeOrganicVeggies <- NA
DataClean$lifestyle_fridgeOrganicVeggies[DataRaw$fridge_8 == 1] <- 1
DataClean$lifestyle_fridgeOrganicVeggies[DataRaw$fridge_8 == 2] <- 0
table(DataClean$lifestyle_fridgeOrganicVeggies)

### fridge_9 -------------------------------------------------------------

table(DataRaw$fridge_9)
DataClean$lifestyle_fridgeHotSauce <- NA
DataClean$lifestyle_fridgeHotSauce[DataRaw$fridge_9 == 1] <- 1
DataClean$lifestyle_fridgeHotSauce[DataRaw$fridge_9 == 2] <- 0
table(DataClean$lifestyle_fridgeHotSauce)

### fridge_10 -------------------------------------------------------------

table(DataRaw$fridge_10)
DataClean$lifestyle_fridgeSoftDrinks <- NA
DataClean$lifestyle_fridgeSoftDrinks[DataRaw$fridge_10 == 1] <- 1
DataClean$lifestyle_fridgeSoftDrinks[DataRaw$fridge_10 == 2] <- 0
table(DataClean$lifestyle_fridgeSoftDrinks)

### fridge_11 ------------------------------------------------------------

table(DataRaw$fridge_11)
DataClean$lifestyle_fridgeGroundBeef <- NA
DataClean$lifestyle_fridgeGroundBeef[DataRaw$fridge_11 == 1] <- 1
DataClean$lifestyle_fridgeGroundBeef[DataRaw$fridge_11 == 2] <- 0
table(DataClean$lifestyle_fridgeGroundBeef)


## coffee_consumption ---------------------------------------------------

attributes(DataRaw$coffee_consumption)
table(DataRaw$coffee_consumption)
DataClean$lifestyle_consCoffee <- NA
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 1] <- "tim_hortons"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 2] <- "starbucks"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 3] <- "second_cup"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 4] <- "mcdonalds"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 5] <- "other"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 6] <- "independent"
DataClean$lifestyle_consCoffee[DataRaw$coffee_consumption == 7] <- "no_coffee"
DataClean$lifestyle_consCoffee <- factor(DataClean$lifestyle_consCoffee,
                                         levels = c("tim_hortons",
                                                    "starbucks",
                                                    "second_cup",
                                                    "mcdonalds",
                                                    "other",
                                                    "independent",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(DataClean$lifestyle_consCoffee)

## bin

DataClean$lifestyle_consCoffeeTimHortons <- NA
DataClean$lifestyle_consCoffeeTimHortons[DataRaw$coffee_consumption == 1] <- 1
DataClean$lifestyle_consCoffeeTimHortons[DataRaw$coffee_consumption != 1] <- 0
table(DataClean$lifestyle_consCoffeeTimHortons)

DataClean$lifestyle_consCoffeeStarbucks <- NA
DataClean$lifestyle_consCoffeeStarbucks[DataRaw$coffee_consumption == 2] <- 1
DataClean$lifestyle_consCoffeeStarbucks[DataRaw$coffee_consumption != 2] <- 0
table(DataClean$lifestyle_consCoffeeStarbucks)

DataClean$lifestyle_consCoffeeSecondCup <- NA
DataClean$lifestyle_consCoffeeSecondCup[DataRaw$coffee_consumption == 3] <- 1
DataClean$lifestyle_consCoffeeSecondCup[DataRaw$coffee_consumption != 3] <- 0
table(DataClean$lifestyle_consCoffeeSecondCup)

DataClean$lifestyle_consCoffeeMcDo <- NA
DataClean$lifestyle_consCoffeeMcDo[DataRaw$coffee_consumption == 4] <- 1
DataClean$lifestyle_consCoffeeMcDo[DataRaw$coffee_consumption != 4] <- 0
table(DataClean$lifestyle_consCoffeeMcDo)

DataClean$lifestyle_consCoffeeOther <- NA
DataClean$lifestyle_consCoffeeOther[DataRaw$coffee_consumption == 5] <- 1
DataClean$lifestyle_consCoffeeOther[DataRaw$coffee_consumption != 5] <- 0
table(DataClean$lifestyle_consCoffeeOther)

DataClean$lifestyle_consCoffeeIndependent <- NA
DataClean$lifestyle_consCoffeeIndependent[DataRaw$coffee_consumption == 6] <- 1
DataClean$lifestyle_consCoffeeIndependent[DataRaw$coffee_consumption != 6] <- 0
table(DataClean$lifestyle_consCoffeeIndependent)

DataClean$lifestyle_consCoffeeNone <- NA
DataClean$lifestyle_consCoffeeNone[DataRaw$coffee_consumption == 7] <- 1
DataClean$lifestyle_consCoffeeNone[DataRaw$coffee_consumption != 7] <- 0
table(DataClean$lifestyle_consCoffeeNoCoffee)


## coffee_machine --------------------------------------------------------

table(DataRaw$coffee_machine)
DataClean$lifestyle_makeCoffee <- NA
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 1] <- "filter_coffee_maker"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 2] <- "automatic_manual_machine"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 3] <- "percolator"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 4] <- "french_press"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 5] <- "single_cup_coffee"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 6] <- "instant_coffee"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 7] <- "italian_coffee_maker"
DataClean$lifestyle_makeCoffee[DataRaw$coffee_machine == 8] <- "no_coffee"
DataClean$lifestyle_makeCoffee <- factor(DataClean$lifestyle_makeCoffee,
                                         levels = c("filter_coffee_maker",
                                                    "automatic_manual_machine",
                                                    "percolator",
                                                    "french_press",
                                                    "single_cup_coffee",
                                                    "instant_coffee",
                                                    "italian_coffee_maker",
                                                    "no_coffee"),
                                         ordered = TRUE)
table(DataClean$lifestyle_makeCoffee)

## cons_pet --------------------------------------------------------------

attributes(DataRaw$cons_pets)
table(DataRaw$cons_pets)
DataClean$lifestyle_ownPet <- NA
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 1] <- "cat"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 2] <- "dog"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 3] <- "cat_and_dog"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 4] <- "diverse_animals"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 5] <- "other"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 6] <- "farm_animals"
DataClean$lifestyle_ownPet[DataRaw$cons_pets == 7] <- "none"
DataClean$lifestyle_ownPet <- factor(DataClean$lifestyle_ownPet,
                                             levels = c("cat",
                                                        "dog",
                                                        "cat_and_dog",
                                                        "diverse_animals",
                                                        "other",
                                                        "farm_animals",
                                                        "none"),
                                             ordered = TRUE)
table(DataClean$lifestyle_ownPet)

DataClean$lifestyle_ownPet_bin <- NA
DataClean$lifestyle_ownPet_bin[DataRaw$cons_pets == 7] <- 0 
DataClean$lifestyle_ownPet_bin[DataRaw$cons_pets %in% c(1:6)] <- 1
table(DataClean$lifestyle_ownPet_bin)

## bin

DataClean$lifestyle_ownPetCat <- NA
DataClean$lifestyle_ownPetCat[DataRaw$cons_pets == 1] <- 1
DataClean$lifestyle_ownPetCat[DataRaw$cons_pets != 1] <- 0
table(DataClean$lifestyle_ownPetCat)

DataClean$lifestyle_ownPetDog <- NA
DataClean$lifestyle_ownPetDog[DataRaw$cons_pets == 2] <- 1
DataClean$lifestyle_ownPetDog[DataRaw$cons_pets != 2] <- 0
table(DataClean$lifestyle_ownPetDog)

DataClean$lifestyle_ownPetCatAndDog <- NA
DataClean$lifestyle_ownPetCatAndDog[DataRaw$cons_pets == 3] <- 1
DataClean$lifestyle_ownPetCatAndDog[DataRaw$cons_pets != 3] <- 0
table(DataClean$lifestyle_ownPetCatAndDog)

DataClean$lifestyle_ownPetDiverseAnimals <- NA
DataClean$lifestyle_ownPetDiverseAnimals[DataRaw$cons_pets == 4] <- 1
DataClean$lifestyle_ownPetDiverseAnimals[DataRaw$cons_pets != 4] <- 0
table(DataClean$lifestyle_ownPetDiverseAnimals)

DataClean$lifestyle_ownPetOther <- NA
DataClean$lifestyle_ownPetOther[DataRaw$cons_pets == 5] <- 1
DataClean$lifestyle_ownPetOther[DataRaw$cons_pets != 5] <- 0
table(DataClean$lifestyle_ownPetOther)

DataClean$lifestyle_ownPetFarmAnimals <- NA
DataClean$lifestyle_ownPetFarmAnimals[DataRaw$cons_pets == 6] <- 1
DataClean$lifestyle_ownPetFarmAnimals[DataRaw$cons_pets != 6] <- 0
table(DataClean$lifestyle_ownPetFarmAnimals)

DataClean$lifestyle_ownPetNone <- NA
DataClean$lifestyle_ownPetNone[DataRaw$cons_pets == 7] <- 1
DataClean$lifestyle_ownPetNone[DataRaw$cons_pets != 7] <- 0
table(DataClean$lifestyle_ownPetNone)


## smoking ---------------------------------------------------------------

attributes(DataRaw$smoking)
table(DataRaw$smoking)
DataClean$lifestyle_smokeFreq <- NA
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 1] <- 0
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 2] <- 0.1667
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 3] <- 0.3333
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 4] <- 0.5
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 5] <- 0.6667
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 6] <- 0.8333
DataClean$lifestyle_smokeFreq[DataRaw$smoking == 7] <- 1
table(DataClean$lifestyle_smokeFreq)

## alcool_type -----------------------------------------------------------

attributes(DataRaw$alcool_type)
table(DataRaw$alcool_type) 
DataClean$lifestyle_favAlcool <- NA
DataClean$lifestyle_favAlcool[DataRaw$alcool_type %in% c(1:4)] <- "wine"
DataClean$lifestyle_favAlcool[DataRaw$alcool_type %in% c(5:6)] <- "beer"
DataClean$lifestyle_favAlcool[DataRaw$alcool_type == 7] <- "spirits"
DataClean$lifestyle_favAlcool[DataRaw$alcool_type == 8] <- "cocktail"
DataClean$lifestyle_favAlcool[DataRaw$alcool_type == 9] <- "dont_drink"
DataClean$lifestyle_favAlcool <- factor(DataClean$lifestyle_favAlcool)
table(DataClean$lifestyle_favAlcool)

## bin

DataClean$lifestyle_favAlcoolRedWine <- NA
DataClean$lifestyle_favAlcoolRedWine[DataRaw$alcool_type == 1] <- 1
DataClean$lifestyle_favAlcoolRedWine[DataRaw$alcool_type != 1] <- 0
table(DataClean$lifestyle_favAlcoolRedWine)

DataClean$lifestyle_favAlcoolWhiteWine <- NA
DataClean$lifestyle_favAlcoolWhiteWine[DataRaw$alcool_type == 2] <- 1
DataClean$lifestyle_favAlcoolWhiteWine[DataRaw$alcool_type != 2] <- 0
table(DataClean$lifestyle_favAlcoolWhiteWine)

DataClean$lifestyle_favAlcoolRoseWine <- NA
DataClean$lifestyle_favAlcoolRoseWine[DataRaw$alcool_type == 3] <- 1
DataClean$lifestyle_favAlcoolRoseWine[DataRaw$alcool_type != 3] <- 0
table(DataClean$lifestyle_favAlcoolRoseWine)

DataClean$lifestyle_favAlcoolBubbleDrink <- NA
DataClean$lifestyle_favAlcoolBubbleDrink[DataRaw$alcool_type == 4] <- 1
DataClean$lifestyle_favAlcoolBubbleDrink[DataRaw$alcool_type != 4] <- 0
table(DataClean$lifestyle_favAlcoolBubbleDrink)

DataClean$lifestyle_favAlcoolBeer <- NA
DataClean$lifestyle_favAlcoolBeer[DataRaw$alcool_type == 5] <- 1
DataClean$lifestyle_favAlcoolBeer[DataRaw$alcool_type != 5] <- 0
table(DataClean$lifestyle_favAlcoolBeer)

DataClean$lifestyle_favAlcoolMicroBeer <- NA
DataClean$lifestyle_favAlcoolMicroBeer[DataRaw$alcool_type == 6] <- 1
DataClean$lifestyle_favAlcoolMicroBeer[DataRaw$alcool_type != 6] <- 0
table(DataClean$lifestyle_favAlcoolMicroBeer)

DataClean$lifestyle_favAlcoolSpirits <- NA
DataClean$lifestyle_favAlcoolSpirits[DataRaw$alcool_type == 7] <- 1
DataClean$lifestyle_favAlcoolSpirits[DataRaw$alcool_type != 7] <- 0
table(DataClean$lifestyle_favAlcoolSpirits)

DataClean$lifestyle_favAlcoolCocktail <- NA
DataClean$lifestyle_favAlcoolCocktail[DataRaw$alcool_type == 8] <- 1
DataClean$lifestyle_favAlcoolCocktail[DataRaw$alcool_type != 8] <- 0
table(DataClean$lifestyle_favAlcoolCocktail)

DataClean$lifestyle_favAlcoolDontDrink <- NA
DataClean$lifestyle_favAlcoolDontDrink[DataRaw$alcool_type == 9] <- 1
DataClean$lifestyle_favAlcoolDontDrink[DataRaw$alcool_type != 9] <- 0
table(DataClean$lifestyle_favAlcoolDontDrink)


## alcool_frequency ------------------------------------------------------
DataClean$lifestyle_alcoolFreq <- NA
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 1] <- 0
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 2] <- 0.1667
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 3] <- 0.3333
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 4] <- 0.5
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 5] <- 0.6667
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 6] <- 0.8333
DataClean$lifestyle_alcoolFreq[DataRaw$alcool_frequency == 7] <- 1
table(DataClean$lifestyle_alcoolFreq)


## marijuana_frequency ---------------------------------------------------
DataClean$lifestyle_weedFreq <- NA
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 1] <- 0
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 2] <- 0.1667
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 3] <- 0.3333
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 4] <- 0.5
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 5] <- 0.6667
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 6] <- 0.8333
DataClean$lifestyle_weedFreq[DataRaw$marijuana_frequency == 7] <- 1
table(DataClean$lifestyle_weedFreq)



## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------

attributes(DataRaw$social_media_use)
table(DataRaw$social_media_use)

DataClean$lifestyle_mostFreqSocialMedia <- NA
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 1] <- "Facebook"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 2] <- "Twitter / X"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 3] <- "Instagram"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 4] <- "Snapchat"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 5] <- "TikTok"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 6] <- "Pinterest"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 7] <- "LinkedIn"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 8] <- "Youtube"
DataClean$lifestyle_mostFreqSocialMedia[DataRaw$social_media_use == 9] <- "Autre"
DataClean$lifestyle_mostFreqSocialMedia <- factor(DataClean$lifestyle_mostFreqSocialMedia)
table(DataClean$lifestyle_mostFreqSocialMedia)

## social_media_time -----------------------------------------------------

attributes(DataRaw$social_media_time)
DataClean$lifestyle_socialMediaTime <- NA
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 1] <- 0
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 2] <- 0.2
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 3] <- 0.4
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 4] <- 0.6
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 5] <- 0.8
DataClean$lifestyle_socialMediaTime[DataRaw$social_media_time == 6] <- 1
table(DataClean$lifestyle_socialMediaTime)

## clothing_style --------------------------------------------------------

attributes(DataRaw$clothing_style)
table(DataRaw$clothing_style)

## disaggregated
DataClean$lifestyle_clothingStyle <- NA
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 1] <- "formal"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 2] <- "classic"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 3] <- "casual"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 4] <- "sport"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 5] <- "elegant"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 6] <- "hippie"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 7] <- "punk"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 8] <- "rock"
DataClean$lifestyle_clothingStyle[DataRaw$clothing_style == 9] <- "other"
DataClean$lifestyle_clothingStyle <- factor(DataClean$lifestyle_clothingStyle)
table(DataClean$lifestyle_clothingStyle)

## grouped
DataClean$lifestyle_clothingStyleGroups <- NA
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing_style %in% c(1, 5)] <- "formal"
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing_style %in% c(2, 3, 4)] <- "easygoing"
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing_style %in% c(6, 7, 8)] <- "edgy"
DataClean$lifestyle_clothingStyleGroups <- factor(DataClean$lifestyle_clothingStyleGroups)
table(DataClean$lifestyle_clothingStyleGroups)

## bin
DataClean$lifestyle_clothingStyleFormal <- NA
DataClean$lifestyle_clothingStyleFormal[DataRaw$clothing_style == 1] <- 1
DataClean$lifestyle_clothingStyleFormal[DataRaw$clothing_style != 1] <- 0
table(DataClean$lifestyle_clothingStyleFormal)

DataClean$lifestyle_clothingStyleClassic <- NA
DataClean$lifestyle_clothingStyleClassic[DataRaw$clothing_style == 2] <- 1
DataClean$lifestyle_clothingStyleClassic[DataRaw$clothing_style != 2] <- 0
table(DataClean$lifestyle_clothingStyleClassic)

DataClean$lifestyle_clothingStyleCasual <- NA
DataClean$lifestyle_clothingStyleCasual[DataRaw$clothing_style == 3] <- 1
DataClean$lifestyle_clothingStyleCasual[DataRaw$clothing_style != 3] <- 0
table(DataClean$lifestyle_clothingStyleCasual)

DataClean$lifestyle_clothingStyleSport <- NA
DataClean$lifestyle_clothingStyleSport[DataRaw$clothing_style == 4] <- 1
DataClean$lifestyle_clothingStyleSport[DataRaw$clothing_style != 4] <- 0
table(DataClean$lifestyle_clothingStyleSport)

DataClean$lifestyle_clothingStyleElegant <- NA
DataClean$lifestyle_clothingStyleElegant[DataRaw$clothing_style == 5] <- 1
DataClean$lifestyle_clothingStyleElegant[DataRaw$clothing_style != 5] <- 0
table(DataClean$lifestyle_clothingStyleElegant)

DataClean$lifestyle_clothingStyleHippie <- NA
DataClean$lifestyle_clothingStyleHippie[DataRaw$clothing_style == 6] <- 1
DataClean$lifestyle_clothingStyleHippie[DataRaw$clothing_style != 6] <- 0
table(DataClean$lifestyle_clothingStyleHippie)

DataClean$lifestyle_clothingStylePunk <- NA
DataClean$lifestyle_clothingStylePunk[DataRaw$clothing_style == 7] <- 1
DataClean$lifestyle_clothingStylePunk[DataRaw$clothing_style != 7] <- 0
table(DataClean$lifestyle_clothingStylePunk)

DataClean$lifestyle_clothingStyleRock <- NA
DataClean$lifestyle_clothingStyleRock[DataRaw$clothing_style == 8] <- 1
DataClean$lifestyle_clothingStyleRock[DataRaw$clothing_style != 8] <- 0
table(DataClean$lifestyle_clothingStyleRock)

DataClean$lifestyle_clothingStyleOther <- NA
DataClean$lifestyle_clothingStyleOther[DataRaw$clothing_style == 9] <- 1
DataClean$lifestyle_clothingStyleOther[DataRaw$clothing_style != 9] <- 0
table(DataClean$lifestyle_clothingStyleOther)

## number_tattoos --------------------------------------------------------

attributes(DataRaw$number_tattoos)
table(DataRaw$number_tattoos)

DataClean$lifestyle_numberTattoos <- NA
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 9] <- 0  # "0 tatouage"
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 10] <- 1 # "1 tatouage"
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 11] <- 2 # "2 tatouages"
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 4] <- 3  # "3 tatouages"
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 5] <- 4  # "4 tatouages"
DataClean$lifestyle_numberTattoos[DataRaw$number_tattoos == 6] <- 5  # "5+ tatouages" traité comme 5

table(DataClean$lifestyle_numberTattoos, useNA = "ifany")

# Créer la variable binaire 'has_tattoos'
DataClean$lifestyle_hasTattoos <- as.numeric(DataClean$lifestyle_numberTattoos > 0)
DataClean$lifestyle_hasTattoos[is.na(DataClean$lifestyle_numberTattoos)] <- NA


table(DataClean$lifestyle_hasTattoos, useNA = "ifany")

## chronotype ------------------------------------------------------------

attributes(DataRaw$chronotype)
table(DataRaw$chronotype)

DataClean$lifestyle_typeMorningToEvening <- NA
DataClean$lifestyle_typeMorningToEvening[DataRaw$chronotype == 1] <- 0
DataClean$lifestyle_typeMorningToEvening[DataRaw$chronotype == 2] <- 0.25
DataClean$lifestyle_typeMorningToEvening[DataRaw$chronotype == 3] <- 0.5
DataClean$lifestyle_typeMorningToEvening[DataRaw$chronotype == 4] <- 0.75
DataClean$lifestyle_typeMorningToEvening[DataRaw$chronotype == 5] <- 1
table(DataClean$lifestyle_typeMorningToEvening)

## trip ------------------------------------------------------------------

attributes(DataRaw$trip)
table(DataRaw$trip)

DataClean$lifestyle_typeTravel <- NA
DataClean$lifestyle_typeTravel[DataRaw$trip == 1] <- "beach"
DataClean$lifestyle_typeTravel[DataRaw$trip == 2] <- "jungle"
DataClean$lifestyle_typeTravel[DataRaw$trip == 3] <- "historic"
DataClean$lifestyle_typeTravel[DataRaw$trip == 4] <- "mountains"
DataClean$lifestyle_typeTravel <- factor(DataClean$lifestyle_typeTravel)
table(DataClean$lifestyle_typeTravel)

