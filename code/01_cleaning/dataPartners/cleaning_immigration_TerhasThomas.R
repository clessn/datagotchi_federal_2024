library(dplyr)

# Data Raw (only relevant variables) --------------------------------------
DataRaw <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250506.csv")
DataRaw <- DataRaw %>% 
  select(-starts_with("tactical"), 
         -X_valid, 
         -X_errors,
         -prevision,
         -prediction,
         -skin,
         -hair_color,
         -hair_style,
         -beard_color,
         -accessories,
         -confirmation,
         tactical_home_situation,
         tactical_immigration_type.immigrants.live,
         tactical_immigration_type.immigrants.agricultural,
         tactical_immigration_type.immigrants.construction) %>%
  filter(X_time <= as.Date("2025-04-28"))

# Data Clean (only relevant variables) ------------------------------------
DataClean <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/df_latest.rds")
DataClean <- DataClean %>% 
  select(-starts_with("tactical"),
         -date,
         -dv_peoplePred,
         -dv_peoplePred_num, 
         tactical_dwellingSituation,
         tactical_moreImmigrantsAgriculture,
         tactical_moreImmigrantsConstruction,
         tactical_moreImmigrantsLive) %>%
  filter(dateCompletion <= as.Date("2025-04-28"))

# Save DF -----------------------------------------------------------------

write.csv(DataRaw, "_SharedFolder_datagotchi_federal_2024/data/appPartners/TerhasThomas_immigrationDataRaw", row.names = FALSE)
write.csv(DataClean, "_SharedFolder_datagotchi_federal_2024/data/appPartners/TerhasThomas_immigrationDataClean", row.names = FALSE)
