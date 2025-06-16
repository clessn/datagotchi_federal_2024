library(dplyr)

# Data Raw (only relevant variables) --------------------------------------
DataRaw <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250506.csv")
DataRaw <- DataRaw %>% 
  select(-starts_with("tactical"), 
         tactical_abort_restrict,
         tactical_abortion_threat) %>%
  filter(X_time <= as.Date("2025-04-28"))

# Data Clean (only relevant variables) ------------------------------------
DataClean <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/df_latest.rds")
DataClean <- DataClean %>% 
  select(-starts_with("tactical"), 
         tactical_abortionThreatExists,
         tactical_restrictAbort) %>%
  filter(dateCompletion <= as.Date("2025-04-28"))

# Save DF -----------------------------------------------------------------

write.csv(DataRaw, "_SharedFolder_datagotchi_federal_2024/data/appPartners/ChantalBayard_abortionDataRaw", row.names = FALSE)
write.csv(DataClean, "_SharedFolder_datagotchi_federal_2024/data/appPartners/ChantalBayard_abortionDataClean", row.names = FALSE)
