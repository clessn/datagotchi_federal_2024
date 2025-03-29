#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 03 - Variables à utiliser pour le clustering
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(dplyr)

# Déclaration des variables de clustering
variables_act_clust <- c(
  "id",
  "lifestyle_exerciseGym",
  #"lifestyle_exerciseTeamSport",
  #"lifestyle_exerciseWalk",
  #"lifestyle_exerciseRun",
  "lifestyle_exerciseYoga",
  #"lifestyle_exerciseSwim",
  #"lifestyle_exerciseOther",
  "lifestyle_exerciseNone",
  #"lifestyle_goFishingFreq_numeric",
  "lifestyle_goHuntingFreq_numeric"
  #"lifestyle_goMuseumsFreq_numeric"
  #"lifestyle_motorizedActFreq_numeric"
  #"lifestyle_volunteeringFreq_numeric"
)

variables_style_clust <- c(
  "id",
  #"lifestyle_clothingStyleClassic",
  #"lifestyle_clothingStyleCasual",
  #"lifestyle_clothingStyleSport",
  "lifestyle_clothingStyleOther",
  "lifestyle_hasTattoos",
  "lifestyle_ownPetCat",
  "lifestyle_ownPetDog",
  #"lifestyle_ownPetOther",
  #"lifestyle_ownPetCatAndDog",
  "lifestyle_ownPetNone"
)

variables_sante_clust <- c(
  "id",
  "lifestyle_eatMeatFreq",
  "lifestyle_favAlcoolRedWine",
  "lifestyle_favAlcoolWhiteWine",
  #"lifestyle_favAlcoolRoseWine",
  #"lifestyle_favAlcoolSpirits",
  #"lifestyle_favAlcoolBubbleDrink",
  #"lifestyle_favAlcoolBeer",
  #"lifestyle_favAlcoolMicroBeer",
  #"lifestyle_favAlcoolCocktail",
  "lifestyle_favAlcoolDontDrink",
  "lifestyle_smokeFreq"
)

variables_mode_de_vie_clust <- c(
  "id",
  "ses_urban"
  #"ses_dwellingApp",
  #"ses_dwellingCondo",
  #"ses_dwellingDetachedHouse",
  #"ses_dwellingTownhouse",
  #"ses_dwellingDuplex",
  #"ses_dwellingOther",
  #"lifestyle_typeTransportCar",
  #"lifestyle_typeTransportNoCar"
)

variables_commerce_clust <- c(
  "id",
  "lifestyle_consClothesFrip",
  #"lifestyle_consClothesIndependent",
  #"lifestyle_consClothesChain",
  #"lifestyle_consClothesSuperstores",
  #"lifestyle_consClothesDepartment",
  #"lifestyle_consClothesOnline",
  #"lifestyle_consClothesOther",
  "lifestyle_consCoffeeTimHortons",
  #"lifestyle_consCoffeeStarbucks",
  #"lifestyle_consCoffeeMcDo",
  #"lifestyle_consCoffeeOther",
  "lifestyle_consCoffeeIndependent",
  "lifestyle_consCoffeeNone"
)

variables_ses_clust <- c(
  "id",
  #"ses_genderMale",
  "ses_age",
  #"ses_languageEnglish",
  "ses_languageFrench",
  #"ses_languageOther",
  "ses_educBHS",
  #"ses_educPostHS",
  "ses_educUniv",
  "ses_incomeLow",
  #"ses_incomeMid",
  "ses_incomeHigh",
  "ses_immigrant",
  #"ses_ethnicityWhite",
  #"ses_ethnicityMinority",
  #"ses_sexOrientationHetero",
  #"ses_sexOrientationQueer",
  #"ses_regionPrairies",
  #"ses_regionBC",
  "ses_regionAtlantic",
  "ses_regionOntario",
  "ses_regionQuebec",
  #"ses_regionTerritories"
  "ses_regionWest"
)

variables_dv_clust <- c(
  "dv_turnout",
  "dv_solidity",
  "dv_voteChoiceLPC",
  "dv_voteChoiceCPC",
  "dv_voteChoiceNDP",
  "dv_voteChoiceBQ",
  "dv_voteChoiceGPC",
  "dv_voteChoiceOther"
)

variables_clust <- Reduce(union, list(
  #variables_act_clust,
  #variables_style_clust,
  #variables_sante_clust,
  variables_mode_de_vie_clust,
  #variables_commerce_clust,
  variables_ses_clust,
  variables_dv_clust
))

