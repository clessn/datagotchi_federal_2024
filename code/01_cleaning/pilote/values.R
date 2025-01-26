# values & perceptions

## values ----------------------------------------------------------------

### values_1 -------------------------------------------------------------

attributes(Data_raw$values_1)
table(Data_raw$values_1)

#### bin
Data_clean$values_conformityFreethinking <- NA
Data_clean$values_conformityFreethinking[Data_raw$values_1 == 1] <- "conformity"
Data_clean$values_conformityFreethinking[Data_raw$values_1 == 2] <- "freethink"
Data_clean$values_conformityFreethinking <- factor(Data_clean$values_conformityFreethinking)
table(Data_clean$values_conformityFreethinking)

### values_2 -------------------------------------------------------------

attributes(Data_raw$values_2)
table(Data_raw$values_2)
#### bin
Data_clean$values_curiosityGoodmanners <- NA
Data_clean$values_curiosityGoodmanners[Data_raw$values_2 == 1] <- "curiosity" 
Data_clean$values_curiosityGoodmanners[Data_raw$values_2 == 2] <- "goodmanners"
Data_clean$values_curiosityGoodmanners <- factor(Data_clean$values_curiosityGoodmanners)
table(Data_clean$values_curiosityGoodmanners) 

### values_3 -------------------------------------------------------------

attributes(Data_raw$values_3)
table(Data_raw$values_3)

#### bin
Data_clean$values_selfrelianceObedience <- NA
Data_clean$values_selfrelianceObedience[Data_raw$values_3 == 1] <- "selfreliance" 
Data_clean$values_selfrelianceObedience[Data_raw$values_3 == 2] <- "obedience"
Data_clean$values_selfrelianceObedience <- factor(Data_clean$values_selfrelianceObedience)
table(Data_clean$values_selfrelianceObedience) 

### values_4 -------------------------------------------------------------

attributes(Data_raw$values_4)
table(Data_raw$values_4)

#### bin
Data_clean$values_considerateWellbehaved <- NA
Data_clean$values_considerateWellbehaved[Data_raw$values_4 == 1] <- "considerate" 
Data_clean$values_considerateWellbehaved[Data_raw$values_4 == 2] <- "wellbehaved"
Data_clean$values_considerateWellbehaved <- factor(Data_clean$values_considerateWellbehaved)
table(Data_clean$values_considerateWellbehaved) 

## op_viewworld ----------------------------------------------------------

attributes(Data_raw$op_viewworld_1)
table(Data_raw$op_viewworld_1)

Data_clean$values_worldDangerous <- NA
Data_clean$values_worldDangerous <- Data_raw$op_viewworld_1 / 10
table(Data_clean$values_worldDangerous)

## isolationnisme_pol ----------------------------------------------------

attributes(Data_raw$isolationnisme_pol_1)
table(Data_raw$isolationnisme_pol_1)

Data_clean$values_isolationismPol <- NA
Data_clean$values_isolationismPol <- Data_raw$isolationnisme_pol_1 / 10
table(Data_clean$values_isolationismPol)

### Polarization - Liberals -------------------------------------------------------------

attributes(Data_raw$polarization_aff_1)
table(Data_raw$polarization_aff_1)

Data_clean$values_polarizationLiberals <- NA
Data_clean$values_polarizationLiberals <- Data_raw$polarization_aff_1 / 10
table(Data_clean$values_polarizationLiberals)

### Polarization - Conservatives -------------------------------------------------------------

attributes(Data_raw$polarization_aff_2)
table(Data_raw$polarization_aff_2)

Data_clean$values_polarizationConservatives <- NA
Data_clean$values_polarizationConservatives <- Data_raw$polarization_aff_2 / 10
table(Data_clean$values_polarizationConservatives)


### Polarization - Neo-democrats -------------------------------------------------------------

attributes(Data_raw$polarization_aff_3)
table(Data_raw$polarization_aff_3)

Data_clean$values_polarizationNeoDem <- NA
Data_clean$values_polarizationNeoDem <- Data_raw$polarization_aff_3 / 10
table(Data_clean$values_polarizationNeoDem)


### Polarization - Bloc Québécois -------------------------------------------------------------

attributes(Data_raw$polarization_aff_4)
table(Data_raw$polarization_aff_4)

Data_clean$values_polarizationBloc <- NA
Data_clean$values_polarizationBloc <- Data_raw$polarization_aff_4 / 10
table(Data_clean$values_polarizationBloc)


### Polarization - Green -------------------------------------------------------------

attributes(Data_raw$polarization_aff_5)
table(Data_raw$polarization_aff_5)

Data_clean$values_polarizationGreen <- NA
Data_clean$values_polarizationGreen <- Data_raw$polarization_aff_5 / 10
table(Data_clean$values_polarizationGreen)


### Polarization - People's Party -------------------------------------------------------------

attributes(Data_raw$polarization_aff_6)
table(Data_raw$polarization_aff_6)

Data_clean$values_polarizationPeopleParty <- NA
Data_clean$values_polarizationPeopleParty <- Data_raw$polarization_aff_6 / 10
table(Data_clean$values_polarizationPeopleParty)


