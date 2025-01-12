# DV

## turnout ---------------------------------------------------------------

table(data_raw$turnout_1)
data_clean$dv_turnout <- NA
data_clean$dv_turnout <- data_raw$turnout_1 / 10
table(data_clean$dv_turnout)

## vote_choice -----------------------------------------------------------

table(data_raw$vote_choice)
data_clean$dv_vote_choice <- NA
data_clean$dv_vote_choice[data_raw$vote_choice == 1] <- "lpc"
data_clean$dv_vote_choice[data_raw$vote_choice == 2] <- "cpc"
data_clean$dv_vote_choice[data_raw$vote_choice == 3] <- "ndp"
data_clean$dv_vote_choice[data_raw$vote_choice == 4] <- "bq"
data_clean$dv_vote_choice[data_raw$vote_choice == 5] <- "gpc"
data_clean$dv_vote_choice[data_raw$vote_choice == 6] <- "other"
data_clean$dv_vote_choice[data_raw$vote_choice == 7] <- "other"
data_clean$dv_vote_choice[data_raw$vote_choice == 8] <- "other"
data_clean$dv_vote_choice[data_raw$vote_choice == 9] <- "other"
data_clean$dv_vote_choice[data_raw$vote_choice == 10] <- "other"
table(data_clean$dv_vote_choice)

## vote_choice_raw -------------------------------------------------------

data_clean$dv_vote_choice_raw <- NA
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 1] <- "lpc"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 2] <- "cpc"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 3] <- "ndp"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 4] <- "bq"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 5] <- "gpc"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 6] <- "ppc"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 7] <- "other"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 8] <- "would_not_vote"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 9] <- "would_spoil_ballot"
data_clean$dv_vote_choice_raw[data_raw$vote_choice == 10] <- "dk"
table(data_clean$dv_vote_choice_raw)

## vote_certainty --------------------------------------------------------

table(data_raw$vote__certainty)
data_clean$dv_vote_certainty <- NA
data_clean$dv_vote_certainty[data_raw$vote__certainty == 1] <- 0
data_clean$dv_vote_certainty[data_raw$vote__certainty == 2] <- 0.33
data_clean$dv_vote_certainty[data_raw$vote__certainty == 3] <- 0.67
data_clean$dv_vote_certainty[data_raw$vote__certainty == 4] <- 1
table(data_clean$dv_vote_certainty)

## potgrowth_fed ---------------------------------------------------------

table(data_raw$potgrowth_fed_1)
data_clean$dv_potgrowth_lpc <- NA
data_clean$dv_potgrowth_lpc <- data_raw$potgrowth_fed_1 / 10
table(data_clean$dv_potgrowth_lpc)

## potgrowth_fed_1 -------------------------------------------------------

table(data_raw$potgrowth_fed_2)
data_clean$dv_potgrowth_cpc <- NA
data_clean$dv_potgrowth_cpc <- data_raw$potgrowth_fed_2 / 10
table(data_clean$dv_potgrowth_cpc)

## potgrowth_fed_2 -------------------------------------------------------

table(data_raw$potgrowth_fed_3)
data_clean$dv_potgrowth_ndp <- NA
data_clean$dv_potgrowth_ndp <- data_raw$potgrowth_fed_3 / 10
table(data_clean$dv_potgrowth_ndp)

## potgrowth_fed_3 -------------------------------------------------------

table(data_raw$potgrowth_fed_4)
data_clean$dv_potgrowth_bq <- NA
data_clean$dv_potgrowth_bq <- data_raw$potgrowth_fed_4 / 10
table(data_clean$dv_potgrowth_bq)

## potgrowth_fed_4 -------------------------------------------------------

table(data_raw$potgrowth_fed_5)
data_clean$dv_potgrowth_gpc <- NA
data_clean$dv_potgrowth_gpc <- data_raw$potgrowth_fed_5 / 10
table(data_clean$dv_potgrowth_gpc)

## potgrowth_fed_5 -------------------------------------------------------

table(data_raw$potgrowth_fed_6)
data_clean$dv_potgrowth_ppc <- NA
data_clean$dv_potgrowth_ppc <- data_raw$potgrowth_fed_6 / 10
table(data_clean$dv_potgrowth_ppc)

## turnout_2021 ---------------------------------------------------------------

table(data_raw$turnout_2021)
data_clean$dv_turnout_2021_bin <- NA
data_clean$dv_turnout_2021_bin[data_raw$turnout_2021 == 1] <- 1
data_clean$dv_turnout_2021_bin[data_raw$turnout_2021 == 2] <- 0
table(data_clean$dv_turnout_2021_bin)

## potgrowth_qc ----------------------------------------------------------

### potgrowth_qc_1 -------------------------------------------------------

table(data_raw$potgrowth_qc_1)
data_clean$dv_potgrowth_qc_caq <- NA
data_clean$dv_potgrowth_qc_caq <- data_raw$potgrowth_qc_1 / 10
table(data_clean$dv_potgrowth_qc_caq)

### potgrowth_qc_2 -------------------------------------------------------

table(data_raw$potgrowth_qc_2)
data_clean$dv_potgrowth_qc_plq <- NA
data_clean$dv_potgrowth_qc_plq <- data_raw$potgrowth_qc_2 / 10
table(data_clean$dv_potgrowth_qc_plq)

### potgrowth_qc_3 --------------------------------------------------------

table(data_raw$potgrowth_qc_3)
data_clean$dv_potgrowth_qc_qs <- NA
data_clean$dv_potgrowth_qc_qs <- data_raw$potgrowth_qc_3 / 10
table(data_clean$dv_potgrowth_qc_qs)

### potgrowth_qc_4 -------------------------------------------------------

table(data_raw$potgrowth_qc_4)
data_clean$dv_potgrowth_qc_pq <- NA
data_clean$dv_potgrowth_qc_pq <- data_raw$potgrowth_qc_4 / 10
table(data_clean$dv_potgrowth_qc_pq)

### potgrowth_qc_5 -------------------------------------------------------

table(data_raw$potgrowth_qc_5)
data_clean$dv_potgrowth_qc_pcq <- NA
data_clean$dv_potgrowth_qc_pcq <- data_raw$potgrowth_qc_5 / 10
table(data_clean$dv_potgrowth_qc_pcq)

## attitude_leftvsright --------------------------------------------------

table(data_raw$attitude_leftvsright_1)
data_clean$dv_attitude_leftvsright <- NA
data_clean$dv_attitude_leftvsright <- data_raw$attitude_leftvsright_1 / 10 
table(data_clean$dv_attitude_leftvsright)

## attitude_party --------------------------------------------------------

attributes(data_raw$attitude_party)
table(data_raw$attitude_party)
data_clean$dv_party_id <- NA
data_clean$dv_party_id <- data_raw$attitude_party / 10
table(data_clean$dv_party_id)


