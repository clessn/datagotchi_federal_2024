# DV

## turnout ---------------------------------------------------------------

table(DataRaw$turnout_1)
DataClean$dv_turnout <- NA
DataClean$dv_turnout <- DataRaw$turnout_1 / 10
table(DataClean$dv_turnout)

## vote_choice -----------------------------------------------------------

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


