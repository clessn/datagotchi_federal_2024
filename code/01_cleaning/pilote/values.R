# values & perceptions

## values ----------------------------------------------------------------

### values_1 -------------------------------------------------------------

attributes(data_raw$values_1)
table(data_raw$values_1)

#### bin
data_clean$values_conformityFreethinking <- NA
data_clean$values_conformityFreethinking[data_raw$values_1 == 1] <- "conformity"
data_clean$values_conformityFreethinking[data_raw$values_1 == 2] <- "freethink"
data_clean$values_conformityFreethinking <- factor(data_clean$values_conformityFreethinking)
table(data_clean$values_conformityFreethinking)

### values_2 -------------------------------------------------------------

attributes(data_raw$values_2)
table(data_raw$values_2)
#### bin
data_clean$values_curiosityGoodmanners <- NA
data_clean$values_curiosityGoodmanners[data_raw$values_2 == 1] <- "curiosity" 
data_clean$values_curiosityGoodmanners[data_raw$values_2 == 2] <- "goodmanners"
data_clean$values_curiosityGoodmanners <- factor(data_clean$values_curiosityGoodmanners)
table(data_clean$values_curiosityGoodmanners) 

### values_3 -------------------------------------------------------------

attributes(data_raw$values_3)
table(data_raw$values_3)

#### bin
data_clean$values_selfrelianceObedience <- NA
data_clean$values_selfrelianceObedience[data_raw$values_3 == 1] <- "selfreliance" 
data_clean$values_selfrelianceObedience[data_raw$values_3 == 2] <- "obedience"
data_clean$values_selfrelianceObedience <- factor(data_clean$values_selfrelianceObedience)
table(data_clean$values_selfrelianceObedience) 

### values_4 -------------------------------------------------------------

attributes(data_raw$values_4)
table(data_raw$values_4)

#### bin
data_clean$values_considerateWellbehaved <- NA
data_clean$values_considerateWellbehaved[data_raw$values_4 == 1] <- "considerate" 
data_clean$values_considerateWellbehaved[data_raw$values_4 == 2] <- "wellbehaved"
data_clean$values_considerateWellbehaved <- factor(data_clean$values_considerateWellbehaved)
table(data_clean$values_considerateWellbehaved) 

## op_viewworld ----------------------------------------------------------

attributes(data_raw$op_viewworld_1)
table(data_raw$op_viewworld_1)

data_clean$values_worldDangerous <- NA
data_clean$values_worldDangerous <- data_raw$op_viewworld_1 / 10
table(data_clean$values_worldDangerous)

## isolationnisme_pol ----------------------------------------------------

attributes(data_raw$isolationnisme_pol_1)
table(data_raw$isolationnisme_pol_1)

data_clean$values_isolationismPol <- NA
data_clean$values_isolationismPol <- data_raw$isolationnisme_pol_1 / 10
table(data_clean$values_isolationismPol)

### Polarization - Liberals -------------------------------------------------------------

attributes(data_raw$polarization_aff_1)
table(data_raw$polarization_aff_1)

data_clean$values_polarizationLiberals <- NA
data_clean$values_polarizationLiberals <- data_raw$polarization_aff_1 / 10
table(data_clean$values_polarizationLiberals)

### Polarization - Conservatives -------------------------------------------------------------

attributes(data_raw$polarization_aff_2)
table(data_raw$polarization_aff_2)

data_clean$values_polarizationConservatives <- NA
data_clean$values_polarizationConservatives <- data_raw$polarization_aff_2 / 10
table(data_clean$values_polarizationConservatives)


### Polarization - Neo-democrats -------------------------------------------------------------

attributes(data_raw$polarization_aff_3)
table(data_raw$polarization_aff_3)

data_clean$values_polarizationNeoDem <- NA
data_clean$values_polarizationNeoDem <- data_raw$polarization_aff_3 / 10
table(data_clean$values_polarizationNeoDem)


### Polarization - Bloc Québécois -------------------------------------------------------------

attributes(data_raw$polarization_aff_4)
table(data_raw$polarization_aff_4)

data_clean$values_polarizationBloc <- NA
data_clean$values_polarizationBloc <- data_raw$polarization_aff_4 / 10
table(data_clean$values_polarizationBloc)


### Polarization - Green -------------------------------------------------------------

attributes(data_raw$polarization_aff_5)
table(data_raw$polarization_aff_5)

data_clean$values_polarizationGreen <- NA
data_clean$values_polarizationGreen <- data_raw$polarization_aff_5 / 10
table(data_clean$values_polarizationGreen)


### Polarization - People's Party -------------------------------------------------------------

attributes(data_raw$polarization_aff_6)
table(data_raw$polarization_aff_6)

data_clean$values_polarizationPeopleParty <- NA
data_clean$values_polarizationPeopleParty <- data_raw$polarization_aff_6 / 10
table(data_clean$values_polarizationPeopleParty)


