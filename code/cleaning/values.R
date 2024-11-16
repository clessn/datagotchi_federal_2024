# values & perceptions


## values ----------------------------------------------------------------

### values_1 -------------------------------------------------------------

attributes(data_raw$values_1)
table(data_raw$values_1)

#### bin
data_clean$values_conformity_vs_freethinking <- NA
data_clean$values_conformity_vs_freethinking[data_raw$values_1 == 1] <- 0
data_clean$values_conformity_vs_freethinking[data_raw$values_1 == 2] <- 1
table(data_clean$values_conformity_vs_freethinking)



### values_2 -------------------------------------------------------------

attributes(data_raw$values_2)
table(data_raw$values_2)
#### bin
data_clean$values_curiosity_vs_goodmanners <- NA
data_clean$values_curiosity_vs_goodmanners[data_raw$values_2 == 1] <- 0 
data_clean$values_curiosity_vs_goodmanners[data_raw$values_2 == 2] <- 1
table(data_clean$values_curiosity_vs_goodmanners) 



### values_3 -------------------------------------------------------------

attributes(data_raw$values_3)
table(data_raw$values_3)

#### bin
data_clean$values_selfreliance_vs_obedience <- NA
data_clean$values_selfreliance_vs_obedience[data_raw$values_3 == 1] <- 0 
data_clean$values_selfreliance_vs_obedience[data_raw$values_3 == 2] <- 1
table(data_clean$values_selfreliance_vs_obedience) 

### values_4 -------------------------------------------------------------

attributes(data_raw$values_4)
table(data_raw$values_4)

#### bin
data_clean$values_considerate_vs_wellbehaved <- NA
data_clean$values_considerate_vs_wellbehaved[data_raw$values_4 == 1] <- 0 
data_clean$values_considerate_vs_wellbehaved[data_raw$values_4 == 2] <- 1
table(data_clean$values_considerate_vs_wellbehaved) 



## op_viewworld ----------------------------------------------------------

attributes(data_raw$op_viewworld_1)
table(data_raw$op_viewworld_1)

data_clean$values_world_is_dangerous <- NA
data_clean$values_world_is_dangerous <- data_raw$op_viewworld_1 / 10
table(data_clean$values_world_is_dangerous)

## isolationnisme_pol ----------------------------------------------------

attributes(data_raw$isolationnisme_pol_1)
table(data_raw$isolationnisme_pol_1)

data_clean$values_isolationnisme_pol <- NA
data_clean$values_isolationnisme_pol <- data_raw$isolationnisme_pol_1 / 10
table(data_clean$values_isolationnisme_pol)



### Polarization - Liberals -------------------------------------------------------------

attributes(data_raw$polarization_aff_1)
table(data_raw$polarization_aff_1)

data_clean$values_polarization_liberals <- NA
data_clean$values_polarization_liberals <- data_raw$polarization_aff_1 / 10
table(data_clean$values_polarization_liberals)


### Polarization - Conservatives -------------------------------------------------------------

attributes(data_raw$polarization_aff_2)
table(data_raw$polarization_aff_2)

data_clean$values_polarization_conservatives <- NA
data_clean$values_polarization_conservatives <- data_raw$polarization_aff_2 / 10
table(data_clean$values_polarization_conservatives)


### Polarization - Neo-democrats -------------------------------------------------------------

attributes(data_raw$polarization_aff_3)
table(data_raw$polarization_aff_3)

data_clean$values_polarization_neo_dem <- NA
data_clean$values_polarization_neo_dem <- data_raw$polarization_aff_3 / 10
table(data_clean$values_polarization_neo_dem)


### Polarization - Bloc Québécois -------------------------------------------------------------

attributes(data_raw$polarization_aff_4)
table(data_raw$polarization_aff_4)

data_clean$values_polarization_bloc_queb <- NA
data_clean$values_polarization_bloc_queb <- data_raw$polarization_aff_4 / 10
table(data_clean$values_polarization_bloc_queb)


### Polarization - Green -------------------------------------------------------------

attributes(data_raw$polarization_aff_5)
table(data_raw$polarization_aff_5)

data_clean$values_polarization_green <- NA
data_clean$values_polarization_green <- data_raw$polarization_aff_5 / 10
table(data_clean$values_polarization_green)


### Polarization - People's Party -------------------------------------------------------------

attributes(data_raw$polarization_aff_6)
table(data_raw$polarization_aff_6)

data_clean$values_polarization_people_party <- NA
data_clean$values_polarization_people_party <- data_raw$polarization_aff_6 / 10
table(data_clean$values_polarization_people_party)

