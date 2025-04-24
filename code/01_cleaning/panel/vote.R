# fed pid
attributes(DataRaw$fed_pid)
DataClean$dv_voteChoice <- NA
DataClean$dv_voteChoice[DataRaw$fed_pid == 1] <- "lpc"
DataClean$dv_voteChoice[DataRaw$fed_pid == 2] <- "cpc"
DataClean$dv_voteChoice[DataRaw$fed_pid == 3] <- "ndp"
DataClean$dv_voteChoice[DataRaw$fed_pid == 5] <- "bq"
DataClean$dv_voteChoice[DataRaw$fed_pid == 6] <- "other"
DataClean$dv_voteChoice[DataRaw$fed_pid == 7] <- "other"
table(DataClean$dv_voteChoice)

