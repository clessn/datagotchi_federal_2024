# DV

## turnout ---------------------------------------------------------------

table(DataRaw$turnout_1)
DataClean$dv_turnout <- NA
DataClean$dv_turnout <- DataRaw$turnout_1 / 10
table(DataClean$dv_turnout)

## vote_choice -----------------------------------------------------------

attributes(DataRaw$vote_choice)
table(DataRaw$vote_choice)
DataClean$dv_voteChoice <- NA
DataClean$dv_voteChoice[DataRaw$vote_choice == 1] <- "lpc"
DataClean$dv_voteChoice[DataRaw$vote_choice == 2] <- "cpc"
DataClean$dv_voteChoice[DataRaw$vote_choice == 3] <- "ndp"
DataClean$dv_voteChoice[DataRaw$vote_choice == 4] <- "bq"
DataClean$dv_voteChoice[DataRaw$vote_choice == 5] <- "gpc"
DataClean$dv_voteChoice[DataRaw$vote_choice == 6] <- "other"
DataClean$dv_voteChoice[DataRaw$vote_choice == 7] <- "other"
DataClean$dv_voteChoice[DataRaw$vote_choice == 8] <- "other"
DataClean$dv_voteChoice[DataRaw$vote_choice == 9] <- "other"
DataClean$dv_voteChoice[DataRaw$vote_choice == 10] <- "other"
table(DataClean$dv_voteChoice)

## vote_choice_raw -------------------------------------------------------

DataClean$dv_voteChoiceAllOptions <- NA
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 1] <- "lpc"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 2] <- "cpc"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 3] <- "ndp"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 4] <- "bq"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 5] <- "gpc"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 6] <- "ppc"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 7] <- "other"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 8] <- "would_not_vote"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 9] <- "would_spoil_ballot"
DataClean$dv_voteChoiceAllOptions[DataRaw$vote_choice == 10] <- "dk"
table(DataClean$dv_voteChoiceAllOptions)

## bin

DataClean$dv_voteChoiceLPC <- NA
DataClean$dv_voteChoiceLPC[DataRaw$vote_choice == 1] <- 1
DataClean$dv_voteChoiceLPC[DataRaw$vote_choice != 1] <- 0
table(DataClean$dv_voteChoiceLPC)

DataClean$dv_voteChoiceCPC <- NA
DataClean$dv_voteChoiceCPC[DataRaw$vote_choice == 2] <- 1
DataClean$dv_voteChoiceCPC[DataRaw$vote_choice != 2] <- 0
table(DataClean$dv_voteChoiceCPC)

DataClean$dv_voteChoiceNDP <- NA
DataClean$dv_voteChoiceNDP[DataRaw$vote_choice == 3] <- 1
DataClean$dv_voteChoiceNDP[DataRaw$vote_choice != 3] <- 0
table(DataClean$dv_voteChoiceNDP)

DataClean$dv_voteChoiceBQ <- NA
DataClean$dv_voteChoiceBQ[DataRaw$vote_choice == 4] <- 1
DataClean$dv_voteChoiceBQ[DataRaw$vote_choice != 4] <- 0
table(DataClean$dv_voteChoiceBQ)

DataClean$dv_voteChoiceGPC <- NA
DataClean$dv_voteChoiceGPC[DataRaw$vote_choice == 5] <- 1
DataClean$dv_voteChoiceGPC[DataRaw$vote_choice != 5] <- 0
table(DataClean$dv_voteChoiceGPC)

DataClean$dv_voteChoiceOther <- NA
DataClean$dv_voteChoiceOther[DataRaw$vote_choice %in% c(6,7,8,9,10)] <- 1
DataClean$dv_voteChoiceOther[DataRaw$vote_choice %in% c(1,2,4,4,5)] <- 0
table(DataClean$dv_voteChoiceOther)


## vote_certainty --------------------------------------------------------

table(DataRaw$vote__certainty)
DataClean$dv_voteCertainty <- NA
DataClean$dv_voteCertainty[DataRaw$vote__certainty == 1] <- 0
DataClean$dv_voteCertainty[DataRaw$vote__certainty == 2] <- 0.33
DataClean$dv_voteCertainty[DataRaw$vote__certainty == 3] <- 0.67
DataClean$dv_voteCertainty[DataRaw$vote__certainty == 4] <- 1
table(DataClean$dv_voteCertainty)

## potgrowth_fed ---------------------------------------------------------

table(DataRaw$potgrowth_fed_1)
DataClean$dv_potgrowthLPC <- NA
DataClean$dv_potgrowthLPC <- DataRaw$potgrowth_fed_1 / 10
table(DataClean$dv_potgrowthLPC)

## potgrowth_fed_1 -------------------------------------------------------

table(DataRaw$potgrowth_fed_2)
DataClean$dv_potgrowthCPC <- NA
DataClean$dv_potgrowthCPC <- DataRaw$potgrowth_fed_2 / 10
table(DataClean$dv_potgrowthCPC)

## potgrowth_fed_2 -------------------------------------------------------

table(DataRaw$potgrowth_fed_3)
DataClean$dv_potgrowthNDP <- NA
DataClean$dv_potgrowthNDP <- DataRaw$potgrowth_fed_3 / 10
table(DataClean$dv_potgrowthNDP)

## potgrowth_fed_3 -------------------------------------------------------

table(DataRaw$potgrowth_fed_4)
DataClean$dv_potgrowthBQ <- NA
DataClean$dv_potgrowthBQ <- DataRaw$potgrowth_fed_4 / 10
table(DataClean$dv_potgrowthBQ)

## potgrowth_fed_4 -------------------------------------------------------

table(DataRaw$potgrowth_fed_5)
DataClean$dv_potgrowthGPC <- NA
DataClean$dv_potgrowthGPC <- DataRaw$potgrowth_fed_5 / 10
table(DataClean$dv_potgrowthGPC)

## potgrowth_fed_5 -------------------------------------------------------

table(DataRaw$potgrowth_fed_6)
DataClean$dv_potgrowthPPC <- NA
DataClean$dv_potgrowthPPC <- DataRaw$potgrowth_fed_6 / 10
table(DataClean$dv_potgrowthPPC)

## turnout_2021 ---------------------------------------------------------------

table(DataRaw$turnout_2021)
DataClean$dv_turnout2021_bin <- NA
DataClean$dv_turnout2021_bin[DataRaw$turnout_2021 == 1] <- 1
DataClean$dv_turnout2021_bin[DataRaw$turnout_2021 == 2] <- 0
table(DataClean$dv_turnout2021_bin)

## potgrowth_qc ----------------------------------------------------------

### potgrowth_qc_1 -------------------------------------------------------

table(DataRaw$potgrowth_qc_1)
DataClean$dv_potgrowthQcCAQ <- NA
DataClean$dv_potgrowthQcCAQ <- DataRaw$potgrowth_qc_1 / 10
table(DataClean$dv_potgrowthQcCAQ)

### potgrowth_qc_2 -------------------------------------------------------

table(DataRaw$potgrowth_qc_2)
DataClean$dv_potgrowthQcPLQ <- NA
DataClean$dv_potgrowthQcPLQ <- DataRaw$potgrowth_qc_2 / 10
table(DataClean$dv_potgrowthQcPLQ)

### potgrowth_qc_3 --------------------------------------------------------

table(DataRaw$potgrowth_qc_3)
DataClean$dv_potgrowthQcQS <- NA
DataClean$dv_potgrowthQcQS <- DataRaw$potgrowth_qc_3 / 10
table(DataClean$dv_potgrowthQcQS)

### potgrowth_qc_4 -------------------------------------------------------

table(DataRaw$potgrowth_qc_4)
DataClean$dv_potgrowthQcPQ <- NA
DataClean$dv_potgrowthQcPQ <- DataRaw$potgrowth_qc_4 / 10
table(DataClean$dv_potgrowthQcPQ)

### potgrowth_qc_5 -------------------------------------------------------

table(DataRaw$potgrowth_qc_5)
DataClean$dv_potgrowthQcPCQ <- NA
DataClean$dv_potgrowthQcPCQ <- DataRaw$potgrowth_qc_5 / 10
table(DataClean$dv_potgrowthQcPCQ)

## attitude_leftvsright --------------------------------------------------

attributes(DataRaw$attitude_leftvsright_1)
table(DataRaw$attitude_leftvsright_1)
DataClean$dv_attitudeLeftvsRight <- NA
DataClean$dv_attitudeLeftvsRight <- DataRaw$attitude_leftvsright_1 / 10 
table(DataClean$dv_attitudeLeftvsRight)

## attitude_party --------------------------------------------------------

attributes(DataRaw$attitude_party)
table(DataRaw$attitude_party)
DataClean$dv_partyId <- NA
DataClean$dv_partyId <- DataRaw$attitude_party / 10
table(DataClean$dv_partyId)

## people pred CPC ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_1)
table(DataRaw$people_pred_fed_1)
DataClean$dv_peoplePredCPC <- NA
DataClean$dv_peoplePredCPC <- DataRaw$people_pred_fed_1 / 100
table(DataClean$dv_peoplePredCPC)

## people pred LPC ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_2)
table(DataRaw$people_pred_fed_2)
DataClean$dv_peoplePredLPC <- NA
DataClean$dv_peoplePredLPC <- DataRaw$people_pred_fed_2 / 100
table(DataClean$dv_peoplePredLPC)


## people pred NDP ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_3)
table(DataRaw$people_pred_fed_3)
DataClean$dv_peoplePredNDP <- NA
DataClean$dv_peoplePredNDP <- DataRaw$people_pred_fed_3 / 100
table(DataClean$dv_peoplePredNDP)

## people pred BQ ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_4)
table(DataRaw$people_pred_fed_4)
DataClean$dv_peoplePredBQ <- NA
DataClean$dv_peoplePredBQ <- DataRaw$people_pred_fed_4 / 100
table(DataClean$dv_peoplePredBQ)

## people pred GPC ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_5)
table(DataRaw$people_pred_fed_5)
DataClean$dv_peoplePredGPC <- NA
DataClean$dv_peoplePredGPC <- DataRaw$people_pred_fed_5 / 100
table(DataClean$dv_peoplePredGPC)

## people pred PPC ----------------------------------------------------------

attributes(DataRaw$people_pred_fed_6)
table(DataRaw$people_pred_fed_6)
DataClean$dv_peoplePredPPC <- NA
DataClean$dv_peoplePredPPC <- DataRaw$people_pred_fed_6 / 100
table(DataClean$dv_peoplePredPPC)


## people pred majority ----------------------------------------------------------

attributes(DataRaw$people_pred_maj_1)
table(DataRaw$people_pred_maj_1)
DataClean$dv_peoplePredMajority <- NA
DataClean$dv_peoplePredMajority <- DataRaw$people_pred_maj_1 / 100
table(DataClean$dv_peoplePredMajority)




