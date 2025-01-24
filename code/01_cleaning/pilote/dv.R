# DV

## turnout ---------------------------------------------------------------

table(data_raw$turnout_1)
data_clean$dv_turnout <- NA
data_clean$dv_turnout <- data_raw$turnout_1 / 10
table(data_clean$dv_turnout)

## vote_choice -----------------------------------------------------------

table(data_raw$vote_choice)
data_clean$dv_voteChoice <- NA
data_clean$dv_voteChoice[data_raw$vote_choice == 1] <- "lpc"
data_clean$dv_voteChoice[data_raw$vote_choice == 2] <- "cpc"
data_clean$dv_voteChoice[data_raw$vote_choice == 3] <- "ndp"
data_clean$dv_voteChoice[data_raw$vote_choice == 4] <- "bq"
data_clean$dv_voteChoice[data_raw$vote_choice == 5] <- "gpc"
data_clean$dv_voteChoice[data_raw$vote_choice == 6] <- "other"
data_clean$dv_voteChoice[data_raw$vote_choice == 7] <- "other"
data_clean$dv_voteChoice[data_raw$vote_choice == 8] <- "other"
data_clean$dv_voteChoice[data_raw$vote_choice == 9] <- "other"
data_clean$dv_voteChoice[data_raw$vote_choice == 10] <- "other"
table(data_clean$dv_voteChoice)

## vote_choice_raw -------------------------------------------------------

data_clean$dv_voteChoiceAllOptions <- NA
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 1] <- "lpc"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 2] <- "cpc"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 3] <- "ndp"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 4] <- "bq"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 5] <- "gpc"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 6] <- "ppc"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 7] <- "other"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 8] <- "would_not_vote"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 9] <- "would_spoil_ballot"
data_clean$dv_voteChoiceAllOptions[data_raw$vote_choice == 10] <- "dk"
table(data_clean$dv_voteChoiceAllOptions)

## vote_certainty --------------------------------------------------------

table(data_raw$vote__certainty)
data_clean$dv_voteCertainty <- NA
data_clean$dv_voteCertainty[data_raw$vote__certainty == 1] <- 0
data_clean$dv_voteCertainty[data_raw$vote__certainty == 2] <- 0.33
data_clean$dv_voteCertainty[data_raw$vote__certainty == 3] <- 0.67
data_clean$dv_voteCertainty[data_raw$vote__certainty == 4] <- 1
table(data_clean$dv_voteCertainty)

## potgrowth_fed ---------------------------------------------------------

table(data_raw$potgrowth_fed_1)
data_clean$dv_potgrowthLPC <- NA
data_clean$dv_potgrowthLPC <- data_raw$potgrowth_fed_1 / 10
table(data_clean$dv_potgrowthLPC)

## potgrowth_fed_1 -------------------------------------------------------

table(data_raw$potgrowth_fed_2)
data_clean$dv_potgrowthCPC <- NA
data_clean$dv_potgrowthCPC <- data_raw$potgrowth_fed_2 / 10
table(data_clean$dv_potgrowthCPC)

## potgrowth_fed_2 -------------------------------------------------------

table(data_raw$potgrowth_fed_3)
data_clean$dv_potgrowthNDP <- NA
data_clean$dv_potgrowthNDP <- data_raw$potgrowth_fed_3 / 10
table(data_clean$dv_potgrowthNDP)

## potgrowth_fed_3 -------------------------------------------------------

table(data_raw$potgrowth_fed_4)
data_clean$dv_potgrowthBQ <- NA
data_clean$dv_potgrowthBQ <- data_raw$potgrowth_fed_4 / 10
table(data_clean$dv_potgrowthBQ)

## potgrowth_fed_4 -------------------------------------------------------

table(data_raw$potgrowth_fed_5)
data_clean$dv_potgrowthGPC <- NA
data_clean$dv_potgrowthGPC <- data_raw$potgrowth_fed_5 / 10
table(data_clean$dv_potgrowthGPC)

## potgrowth_fed_5 -------------------------------------------------------

table(data_raw$potgrowth_fed_6)
data_clean$dv_potgrowthPPC <- NA
data_clean$dv_potgrowthPPC <- data_raw$potgrowth_fed_6 / 10
table(data_clean$dv_potgrowthPPC)

## turnout_2021 ---------------------------------------------------------------

table(data_raw$turnout_2021)
data_clean$dv_turnout2021_bin <- NA
data_clean$dv_turnout2021_bin[data_raw$turnout_2021 == 1] <- 1
data_clean$dv_turnout2021_bin[data_raw$turnout_2021 == 2] <- 0
table(data_clean$dv_turnout2021_bin)

## potgrowth_qc ----------------------------------------------------------

### potgrowth_qc_1 -------------------------------------------------------

table(data_raw$potgrowth_qc_1)
data_clean$dv_potgrowthQcCAQ <- NA
data_clean$dv_potgrowthQcCAQ <- data_raw$potgrowth_qc_1 / 10
table(data_clean$dv_potgrowthQcCAQ)

### potgrowth_qc_2 -------------------------------------------------------

table(data_raw$potgrowth_qc_2)
data_clean$dv_potgrowthQcPLQ <- NA
data_clean$dv_potgrowthQcPLQ <- data_raw$potgrowth_qc_2 / 10
table(data_clean$dv_potgrowthQcPLQ)

### potgrowth_qc_3 --------------------------------------------------------

table(data_raw$potgrowth_qc_3)
data_clean$dv_potgrowthQcQS <- NA
data_clean$dv_potgrowthQcQS <- data_raw$potgrowth_qc_3 / 10
table(data_clean$dv_potgrowthQcQS)

### potgrowth_qc_4 -------------------------------------------------------

table(data_raw$potgrowth_qc_4)
data_clean$dv_potgrowthQcPQ <- NA
data_clean$dv_potgrowthQcPQ <- data_raw$potgrowth_qc_4 / 10
table(data_clean$dv_potgrowthQcPQ)

### potgrowth_qc_5 -------------------------------------------------------

table(data_raw$potgrowth_qc_5)
data_clean$dv_potgrowthQcPCQ <- NA
data_clean$dv_potgrowthQcPCQ <- data_raw$potgrowth_qc_5 / 10
table(data_clean$dv_potgrowthQcPCQ)

## attitude_leftvsright --------------------------------------------------

attributes(data_raw$attitude_leftvsright_1)
table(data_raw$attitude_leftvsright_1)
data_clean$dv_attitudeLeftvsRight <- NA
data_clean$dv_attitudeLeftvsRight <- data_raw$attitude_leftvsright_1 / 10 
table(data_clean$dv_attitudeLeftvsRight)

## attitude_party --------------------------------------------------------

attributes(data_raw$attitude_party)
table(data_raw$attitude_party)
data_clean$dv_partyId <- NA
data_clean$dv_partyId <- data_raw$attitude_party / 10
table(data_clean$dv_partyId)


