# lifestyle

## exercise --------------------------------------------------------------

table(data_raw$exercise)
data_clean$exercise_clean <- NA
data_clean$exercise_clean[data_raw$exercise == 1] <- 1
data_clean$exercise_clean[data_raw$exercise == 2] <- 2
data_clean$exercise_clean[data_raw$exercise == 3] <- 3
data_clean$exercise_clean[data_raw$exercise == 4] <- 4
data_clean$exercise_clean[data_raw$exercise == 5] <- 5
data_clean$exercise_clean[data_raw$exercise == 6] <- 6
data_clean$exercise_clean[data_raw$exercise == 7] <- 7
data_clean$exercise_clean[data_raw$exercise == 8] <- 8
table(data_clean$exercise_clean)


## activity_1 ------------------------------------------------------------

table(data_raw$activity_1) 
data_clean$lifestyle_fishing_freq <- NA
data_raw$activity_1 <- as.numeric(data_raw$activity_1)
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 1] <- 1
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 2] <- 2
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 3] <- 3
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 4] <- 4
data_clean$lifestyle_fishing_freq[data_raw$activity_1 == 5] <- 5
table(data_clean$lifestyle_fishing_freq)

## factor

table(data_clean$lifestyle_fishing_freq)
data_clean$lifestyle_fishing_freq_factor <- NA
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 1] <- "never"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 2] <- "almost_never"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 3] <- "sometimes"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 4] <- "often"
data_clean$lifestyle_fishing_freq_factor[data_raw$activity_1 == 5] <- "very_often"
data_clean$lifestyle_fishing_freq_factor <- factor(data_clean$lifestyle_fishing_freq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(data_clean$lifestyle_fishing_freq_factor)

## numeric

data_clean$lifestyle_fishing_freq_numeric <- NA 
data_clean$lifestyle_fishing_freq_numeric <- (data_raw$activity_1 - 1) / 4
table(data_clean$lifestyle_fishing_freq_numeric)

## bin

data_clean$lifestyle_fishing_bin <- NA
data_clean$lifestyle_fishing_bin[data_raw$activity_1  == 1] <- 0
data_clean$lifestyle_fishing_bin[data_raw$activity_1  %in% c(2, 3, 4, 5)] <- 1
table(data_clean$lifestyle_fishing_bin) 

## activity_2 ------------------------------------------------------------




## activity_3 ------------------------------------------------------------




## activity_4 -----------------------------------------------------------




## activity_5 ------------------------------------------------------------




## activity_6 ------------------------------------------------------------




## activity_7 ------------------------------------------------------------




## activity_8 ------------------------------------------------------------




## activity_9 ------------------------------------------------------------




## activity_10 -----------------------------------------------------------




## activity_11 -----------------------------------------------------------




## activity_12 -----------------------------------------------------------




## activity_13 -----------------------------------------------------------




## activity_14 -----------------------------------------------------------




## type_transport --------------------------------------------------------




## choice_transport ------------------------------------------------------




## field_occupation ------------------------------------------------------




## type_occupation -------------------------------------------------------




## clothes_consumption ---------------------------------------------------




## mode_attitude ---------------------------------------------------------




## meat_consumption ------------------------------------------------------




## meal_time -------------------------------------------------------------




## fridge ----------------------------------------------------------------

### fridge_1 -------------------------------------------------------------




### fridge_2 -------------------------------------------------------------




### fridge_3 -------------------------------------------------------------




### fridge_4 -------------------------------------------------------------




### fridge_5 -------------------------------------------------------------




### fridge_6 -------------------------------------------------------------




### fridge_7 -------------------------------------------------------------




### fridge_8 -------------------------------------------------------------




### fridge_9 -------------------------------------------------------------




### fridge_10 ------------------------------------------------------------




## coffee_consumption ---------------------------------------------------




## coffee_machine --------------------------------------------------------




## cons_pet --------------------------------------------------------------




## smoking ---------------------------------------------------------------




## alcool_type -----------------------------------------------------------




## alcool_frequency ------------------------------------------------------




## marijuana_frequency ---------------------------------------------------




## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------




## social_media_time -----------------------------------------------------




## clothing_style --------------------------------------------------------




## number_tattoos --------------------------------------------------------




## chronotype ------------------------------------------------------------




## trip ------------------------------------------------------------------


