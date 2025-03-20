#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01 Préparation initiales des données pour le clustering
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Variables communes entre pilote1 et app

# Déclaration et initialisation du vecteur contenant le nom des 
# variables qui sont communes entre le pilote1 et l'application
#
# Le pilot2 a des variables très différentes du pilot1 et de l'app, 
# il ne sera donc pas utilisé pour le clustering.
#
# Pour des fins de développement itératif :
# Les variables seront regroupées au fil du temps par sens communs
# Le début et la fin du groupement seront clairement indiqués
# Les groupements seront utilisés à l'étape 02 de la préparation des données
#
variables_act <- c(
  "id",
  "lifestyle_exerciseGym",
  "lifestyle_exerciseTeamSport",
  "lifestyle_exerciseWalk",
  "lifestyle_exerciseRun",
  "lifestyle_exerciseYoga",
  "lifestyle_exerciseSwim",
  "lifestyle_exerciseOther",
  "lifestyle_exerciseNone",
  "lifestyle_goFishingFreq_numeric",
  "lifestyle_goHuntingFreq_numeric",
  "lifestyle_goMuseumsFreq_numeric",
  "lifestyle_motorizedActFreq_numeric",
  "lifestyle_volunteeringFreq_numeric"
)

variables_style <- c(
  "id",
  "lifestyle_clothingStyleFormal",
  "lifestyle_clothingStyleClassic",
  "lifestyle_clothingStyleCasual",
  "lifestyle_clothingStyleSport",
  "lifestyle_clothingStyleElegant",
  "lifestyle_clothingStyleHippie",
  "lifestyle_clothingStylePunk",
  "lifestyle_clothingStyleRock",
  "lifestyle_clothingStyleOther",
  "lifestyle_hasTattoos",
  "lifestyle_ownPetCat",
  "lifestyle_ownPetDog",
  "lifestyle_ownPetOther",
  "lifestyle_ownPetCatAndDog",
  "lifestyle_ownPetFarmAnimals",
  "lifestyle_ownPetNone"
)

variables_sante <- c(
  "id",
  "lifestyle_eatMeatFreq",
  "lifestyle_favAlcoolRedWine",
  "lifestyle_favAlcoolWhiteWine",
  "lifestyle_favAlcoolRoseWine",
  "lifestyle_favAlcoolSpirits",
  "lifestyle_favAlcoolBubbleDrink",
  "lifestyle_favAlcoolBeer",
  "lifestyle_favAlcoolMicroBeer",
  "lifestyle_favAlcoolCocktail",
  "lifestyle_favAlcoolDontDrink",
  "lifestyle_smokeFreq"
)


variables_mode_de_vie <- c(
  "id",
  "ses_postalCode", # À transformer en rural, urbain, région, banlieue
  "ses_dwellingApp",
  "ses_dwellingLoft",
  "ses_dwellingCondo",
  "ses_dwellingTour",
  "ses_dwellingDetachedHouse",
  "ses_dwellingTownhouse",
  "ses_dwellingDuplex",
  "ses_dwellingCoop",
  "ses_dwellingHLM",
  "ses_dwellingMobile",
  "ses_dwellingOther",
  "lifestyle_typeTransportCar",
  "lifestyle_typeTransportSUV",
  "lifestyle_typeTransportMoto",
  "lifestyle_typeTransportWalk",
  "lifestyle_typeTransportBicycle",
  "lifestyle_typeTransportPublicTransit"
)

variables_commerce <- c(
  "id",
  "lifestyle_consClothesFrip",
  "lifestyle_consClothesIndependent",
  "lifestyle_consClothesChain",
  "lifestyle_consClothesSuperstores",
  "lifestyle_consClothesDepartment",
  "lifestyle_consClothesOnline",
  "lifestyle_consClothesOther",
  "lifestyle_consCoffeeTimHortons",
  "lifestyle_consCoffeeStarbucks",
  "lifestyle_consCoffeeSecondCup",
  "lifestyle_consCoffeeMcDo",
  "lifestyle_consCoffeeOther",
  "lifestyle_consCoffeeIndependent",
  "lifestyle_consCoffeeNone"
)

variables_ses <- c(
  "id",
  "ses_genderMale",
  "ses_genderFemale",
  "ses_gender",
  "ses_ageGroup5Years",
  "ses_age_4Cat",
  "ses_region",
  "ses_language",
  "ses_educ_3Cat",
  "ses_income",
  "ses_income3Cat",
  "ses_immigrant",
  "ses_ethnicity",
  "ses_ethnicityWB",
  "ses_sexOrientation"
)

variables_vote <- c(
  "id",
  "dv_voteChoiceLPC",
  "dv_voteChoiceCPC",
  "dv_voteChoiceNDP",
  "dv_voteChoiceBQ",
  "dv_voteChoiceGPC",
  "dv_voteChoiceOther",
  "dv_potgrowthLPC",
  "dv_potgrowthCPC",
  "dv_potgrowthNDP",
  "dv_potgrowthBQ",
  "dv_potgrowthGPC",
  "dv_potgrowthPPC",
  "dv_turnout"
)

variables_communes <- Reduce(union, list(
  variables_act,
  variables_style,
  variables_sante,
  variables_mode_de_vie,
  variables_commerce,
  variables_ses,
  variables_vote
))
