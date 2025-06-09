### Abortion

## Do you support the adoption of a legislation to restrict abortion rights in Canada?
table(DataRaw$tactical_abort_restrict)
DataClean$tactical_restrictAbort <- NA
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Strongly disagree"] <- 0
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Somewhat disagree"] <- 0.33
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Somewhat agree"] <- 0.66
DataClean$tactical_restrictAbort[DataRaw$tactical_abort_restrict == "Strongly agree"] <- 1
table(DataClean$tactical_restrictAbort)

## Access to abortion in Canada is under threat
table(DataRaw$tactical_abortion_threat)
DataClean$tactical_abortionThreatExists <- NA
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Strongly disagree"] <- 0
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Somewhat disagree"] <- 0.33
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Somewhat agree"] <- 0.66
DataClean$tactical_abortionThreatExists[DataRaw$tactical_abortion_threat == "Strongly agree"] <- 1
table(DataClean$tactical_abortionThreatExists)
