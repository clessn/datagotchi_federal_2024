# DV

## turnout ---------------------------------------------------------------

table(Data_raw$turnout_1)
Data_clean$dv_turnout <- NA
Data_clean$dv_turnout <- Data_raw$turnout_1 / 10
table(Data_clean$dv_turnout)

## vote_choice -----------------------------------------------------------

table(Data_raw$vote_choice)
Data_clean$dv_voteChoice <- NA
Data_clean$dv_voteChoice[Data_raw$vote_choice == 1] <- "lpc"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 2] <- "cpc"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 3] <- "ndp"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 4] <- "bq"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 5] <- "gpc"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 6] <- "other"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 7] <- "other"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 8] <- "other"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 9] <- "other"
Data_clean$dv_voteChoice[Data_raw$vote_choice == 10] <- "other"
table(Data_clean$dv_voteChoice)

## vote_choice_raw -------------------------------------------------------

Data_clean$dv_voteChoiceAllOptions <- NA
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 1] <- "lpc"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 2] <- "cpc"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 3] <- "ndp"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 4] <- "bq"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 5] <- "gpc"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 6] <- "ppc"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 7] <- "other"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 8] <- "would_not_vote"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 9] <- "would_spoil_ballot"
Data_clean$dv_voteChoiceAllOptions[Data_raw$vote_choice == 10] <- "dk"
table(Data_clean$dv_voteChoiceAllOptions)

## vote_certainty --------------------------------------------------------

table(Data_raw$vote__certainty)
Data_clean$dv_voteCertainty <- NA
Data_clean$dv_voteCertainty[Data_raw$vote__certainty == 1] <- 0
Data_clean$dv_voteCertainty[Data_raw$vote__certainty == 2] <- 0.33
Data_clean$dv_voteCertainty[Data_raw$vote__certainty == 3] <- 0.67
Data_clean$dv_voteCertainty[Data_raw$vote__certainty == 4] <- 1
table(Data_clean$dv_voteCertainty)

## potgrowth_fed ---------------------------------------------------------

table(Data_raw$potgrowth_fed_1)
Data_clean$dv_potgrowthLPC <- NA
Data_clean$dv_potgrowthLPC <- Data_raw$potgrowth_fed_1 / 10
table(Data_clean$dv_potgrowthLPC)

## potgrowth_fed_1 -------------------------------------------------------

table(Data_raw$potgrowth_fed_2)
Data_clean$dv_potgrowthCPC <- NA
Data_clean$dv_potgrowthCPC <- Data_raw$potgrowth_fed_2 / 10
table(Data_clean$dv_potgrowthCPC)

## potgrowth_fed_2 -------------------------------------------------------

table(Data_raw$potgrowth_fed_3)
Data_clean$dv_potgrowthNDP <- NA
Data_clean$dv_potgrowthNDP <- Data_raw$potgrowth_fed_3 / 10
table(Data_clean$dv_potgrowthNDP)

## potgrowth_fed_3 -------------------------------------------------------

table(Data_raw$potgrowth_fed_4)
Data_clean$dv_potgrowthBQ <- NA
Data_clean$dv_potgrowthBQ <- Data_raw$potgrowth_fed_4 / 10
table(Data_clean$dv_potgrowthBQ)

## potgrowth_fed_4 -------------------------------------------------------

table(Data_raw$potgrowth_fed_5)
Data_clean$dv_potgrowthGPC <- NA
Data_clean$dv_potgrowthGPC <- Data_raw$potgrowth_fed_5 / 10
table(Data_clean$dv_potgrowthGPC)

## potgrowth_fed_5 -------------------------------------------------------

table(Data_raw$potgrowth_fed_6)
Data_clean$dv_potgrowthPPC <- NA
Data_clean$dv_potgrowthPPC <- Data_raw$potgrowth_fed_6 / 10
table(Data_clean$dv_potgrowthPPC)

## turnout_2021 ---------------------------------------------------------------

table(Data_raw$turnout_2021)
Data_clean$dv_turnout2021_bin <- NA
Data_clean$dv_turnout2021_bin[Data_raw$turnout_2021 == 1] <- 1
Data_clean$dv_turnout2021_bin[Data_raw$turnout_2021 == 2] <- 0
table(Data_clean$dv_turnout2021_bin)

## potgrowth_qc ----------------------------------------------------------

### potgrowth_qc_1 -------------------------------------------------------

table(Data_raw$potgrowth_qc_1)
Data_clean$dv_potgrowthQcCAQ <- NA
Data_clean$dv_potgrowthQcCAQ <- Data_raw$potgrowth_qc_1 / 10
table(Data_clean$dv_potgrowthQcCAQ)

### potgrowth_qc_2 -------------------------------------------------------

table(Data_raw$potgrowth_qc_2)
Data_clean$dv_potgrowthQcPLQ <- NA
Data_clean$dv_potgrowthQcPLQ <- Data_raw$potgrowth_qc_2 / 10
table(Data_clean$dv_potgrowthQcPLQ)

### potgrowth_qc_3 --------------------------------------------------------

table(Data_raw$potgrowth_qc_3)
Data_clean$dv_potgrowthQcQS <- NA
Data_clean$dv_potgrowthQcQS <- Data_raw$potgrowth_qc_3 / 10
table(Data_clean$dv_potgrowthQcQS)

### potgrowth_qc_4 -------------------------------------------------------

table(Data_raw$potgrowth_qc_4)
Data_clean$dv_potgrowthQcPQ <- NA
Data_clean$dv_potgrowthQcPQ <- Data_raw$potgrowth_qc_4 / 10
table(Data_clean$dv_potgrowthQcPQ)

### potgrowth_qc_5 -------------------------------------------------------

table(Data_raw$potgrowth_qc_5)
Data_clean$dv_potgrowthQcPCQ <- NA
Data_clean$dv_potgrowthQcPCQ <- Data_raw$potgrowth_qc_5 / 10
table(Data_clean$dv_potgrowthQcPCQ)

## attitude_leftvsright --------------------------------------------------

attributes(Data_raw$attitude_leftvsright_1)
table(Data_raw$attitude_leftvsright_1)
Data_clean$dv_attitudeLeftvsRight <- NA
Data_clean$dv_attitudeLeftvsRight <- Data_raw$attitude_leftvsright_1 / 10 
table(Data_clean$dv_attitudeLeftvsRight)

## attitude_party --------------------------------------------------------

attributes(Data_raw$attitude_party)
table(Data_raw$attitude_party)
Data_clean$dv_partyId <- NA
Data_clean$dv_partyId <- Data_raw$attitude_party / 10
table(Data_clean$dv_partyId)


