# lifestyle

## Débat vs Hockey -------------------------------------------------------------

attributes(DataRaw$debat3)
table(DataRaw$debat3)

DataClean$debat_vs_hockey <- NA
DataClean$debat_vs_hockey[DataRaw$debat3 == 1] <- "canadien_game"
DataClean$debat_vs_hockey[DataRaw$debat3 == 2] <- "leaders_debate"
DataClean$debat_vs_hockey[DataRaw$debat3 == 3] <- "both"
DataClean$debat_vs_hockey[DataRaw$debat3 == 4] <- "neither"
table(DataClean$debat_vs_hockey)
