# values & perceptions

## values ----------------------------------------------------------------

### values_1 -------------------------------------------------------------

attributes(DataRaw$values_1)
table(DataRaw$values_1)

#### bin
DataClean$values_conformityFreethinking <- NA
DataClean$values_conformityFreethinking[DataRaw$values_1 == 1] <- "conformity"
DataClean$values_conformityFreethinking[DataRaw$values_1 == 2] <- "freethink"
DataClean$values_conformityFreethinking <- factor(DataClean$values_conformityFreethinking)
table(DataClean$values_conformityFreethinking)

### values_2 -------------------------------------------------------------

attributes(DataRaw$values_2)
table(DataRaw$values_2)
#### bin
DataClean$values_curiosityGoodmanners <- NA
DataClean$values_curiosityGoodmanners[DataRaw$values_2 == 1] <- "curiosity" 
DataClean$values_curiosityGoodmanners[DataRaw$values_2 == 2] <- "goodmanners"
DataClean$values_curiosityGoodmanners <- factor(DataClean$values_curiosityGoodmanners)
table(DataClean$values_curiosityGoodmanners) 

### values_3 -------------------------------------------------------------

attributes(DataRaw$values_3)
table(DataRaw$values_3)

#### bin
DataClean$values_selfrelianceObedience <- NA
DataClean$values_selfrelianceObedience[DataRaw$values_3 == 1] <- "selfreliance" 
DataClean$values_selfrelianceObedience[DataRaw$values_3 == 2] <- "obedience"
DataClean$values_selfrelianceObedience <- factor(DataClean$values_selfrelianceObedience)
table(DataClean$values_selfrelianceObedience) 

### values_4 -------------------------------------------------------------

attributes(DataRaw$values_4)
table(DataRaw$values_4)

#### bin
DataClean$values_considerateWellbehaved <- NA
DataClean$values_considerateWellbehaved[DataRaw$values_4 == 1] <- "considerate" 
DataClean$values_considerateWellbehaved[DataRaw$values_4 == 2] <- "wellbehaved"
DataClean$values_considerateWellbehaved <- factor(DataClean$values_considerateWellbehaved)
table(DataClean$values_considerateWellbehaved) 

## op_viewworld ----------------------------------------------------------

attributes(DataRaw$op_viewworld_1)
table(DataRaw$op_viewworld_1)

DataClean$values_worldDangerous <- NA
DataClean$values_worldDangerous <- DataRaw$op_viewworld_1 / 10
table(DataClean$values_worldDangerous)

## isolationnisme_pol ----------------------------------------------------

attributes(DataRaw$isolationnisme_pol_1)
table(DataRaw$isolationnisme_pol_1)

DataClean$values_isolationismPol <- NA
DataClean$values_isolationismPol <- DataRaw$isolationnisme_pol_1 / 10
table(DataClean$values_isolationismPol)

### Polarization - Liberals -------------------------------------------------------------

attributes(DataRaw$polarization_aff_1)
table(DataRaw$polarization_aff_1)

DataClean$values_polarizationLiberals <- NA
DataClean$values_polarizationLiberals <- DataRaw$polarization_aff_1 / 10
table(DataClean$values_polarizationLiberals)

### Polarization - Conservatives -------------------------------------------------------------

attributes(DataRaw$polarization_aff_2)
table(DataRaw$polarization_aff_2)

DataClean$values_polarizationConservatives <- NA
DataClean$values_polarizationConservatives <- DataRaw$polarization_aff_2 / 10
table(DataClean$values_polarizationConservatives)


### Polarization - Neo-democrats -------------------------------------------------------------

attributes(DataRaw$polarization_aff_3)
table(DataRaw$polarization_aff_3)

DataClean$values_polarizationNeoDem <- NA
DataClean$values_polarizationNeoDem <- DataRaw$polarization_aff_3 / 10
table(DataClean$values_polarizationNeoDem)


### Polarization - Bloc Québécois -------------------------------------------------------------

attributes(DataRaw$polarization_aff_4)
table(DataRaw$polarization_aff_4)

DataClean$values_polarizationBloc <- NA
DataClean$values_polarizationBloc <- DataRaw$polarization_aff_4 / 10
table(DataClean$values_polarizationBloc)


### Polarization - Green -------------------------------------------------------------

attributes(DataRaw$polarization_aff_5)
table(DataRaw$polarization_aff_5)

DataClean$values_polarizationGreen <- NA
DataClean$values_polarizationGreen <- DataRaw$polarization_aff_5 / 10
table(DataClean$values_polarizationGreen)


### Polarization - People's Party -------------------------------------------------------------

attributes(DataRaw$polarization_aff_6)
table(DataRaw$polarization_aff_6)

DataClean$values_polarizationPeopleParty <- NA
DataClean$values_polarizationPeopleParty <- DataRaw$polarization_aff_6 / 10
table(DataClean$values_polarizationPeopleParty)


