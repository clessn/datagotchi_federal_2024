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
  "lifestyle_goFishingFreq",
  "lifestyle_goHuntingFreq",
  "lifestyle_goMuseumsFreq",
  "lifestyle_motorizedActFreq",
  "lifestyle_volunteeringFreq"
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
  "lifestyle_ownPetCatAndDog"
  "lifestyle_ownPetFarmAnimals",
  "lifestyle_ownPetNone"
)

variables_sante <- c(
  "id",
  "lifestyle_eatMeatFreq_never",
  "lifestyle_eatMeatFreq_almost_never",
  "lifestyle_eatMeatFreq_once_month",
  "lifestyle_eatMeatFreq_once_week",
  "lifestyle_eatMeatFreq_few_week",
  "lifestyle_eatMeatFreq_daily",
  "lifestyle_eatMeatFreq_few_daily",
  "lifestyle_favAlcoolRedWine",
  "lifestyle_favAlcoolWhiteWine",
  "lifestyle_favAlcoolRoseWine",
  "lifestyle_favAlcoolSpirits",
  "lifestyle_favAlcoolBubbleDrink",
  "lifestyle_favAlcoolBeer",
  "lifestyle_favAlcoolMicroBeer",
  "lifestyle_favAlcoolCocktail",
  "lifestyle_favAlcoolDontDrink",
  "lifestyle_smokeFreq_never",
  "lifestyle_smokeFreq_few_times_year",
  "lifestyle_smokeFreq_month",
  "lifestyle_smokeFreq_once_week",
  "lifestyle_smokeFreq_few_times_week",
  "lifestyle_smokeFreq_once_day",
  "lifestyle_smokeFreq_few_times_day"
)

variables_mode_de_vie <- c(
  "id",
  #"postal_code", # À transformer en rural, urbain, région, banlieue
  "ses_dwellingApp",
  "ses_dwellingLoft",
  "ses_dwellingCondo",
  "ses_dwellingTour",
  "ses_dwellingDetachedHouse",
  "ses_dwellingTownHouse",
  "ses_dwellingDuplex",
  "ses_dwellingCoop",
  "ses_dwellingHLM",
  "ses_dwellingMobile",
  "ses_dwellingOther",
  "act_typeTransportCar",
  "act_typeTransportSUV",
  "act_typeTransportMoto",
  "act_typeTransportWalk",
  "act_typeTransportBicycle",
  "act_typeTransportPublicTransit"
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
  "lifestyle_consCoffeeTim",
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
  "age34m",
  "age3554",
  "age55p",
  "langEn",
  "langFr",
  "ses_languageOther",
  "educBHS",
  "educCollege",
  "educUniv",
  "ses_income_None",
  "ses_income_i1to30",
  "ses_income_i31to60",
  "ses_income_i61to90",
  "ses_income_i91to110",
  "ses_income_i111to150",
  "ses_income_i151to200",
  "ses_income_i201toInf",
  "immigrant",
  "ses_ethn_White",
  "ses_ethn_Black",
  "ses_ethn_Other",
  "ses_hetero",
  "ses_gai",
  "ses_bisex",
  "ses_sexOri_other"
)

variables_vote <- c(
  "id",
  "op_intent",
  "op_intent_CAQ",
  "op_intent_PQ",
  "op_intent_PLQ",
  "op_intent_QS",
  "op_intent_PCQ",
  "op_intent_Other"
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
