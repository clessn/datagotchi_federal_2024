# lifestyle

## exercise --------------------------------------------------------------


# Vérification des valeurs existantes
table(DataRaw$sport)

# Création et transformation de la variable lifestyle_exercise
DataClean$lifestyle_exercise <- NA
DataClean$lifestyle_exercise[DataRaw$sport == "Gym"] <- "gym"
DataClean$lifestyle_exercise[DataRaw$sport == "Team sport"] <- "play_a_team_sport"
DataClean$lifestyle_exercise[DataRaw$sport == "Walk"] <- "walk"
DataClean$lifestyle_exercise[DataRaw$sport == "Run"] <- "run"
DataClean$lifestyle_exercise[DataRaw$sport == "Yoga"] <- "yoga"
DataClean$lifestyle_exercise[DataRaw$sport == "Swim"] <- "swimming"
DataClean$lifestyle_exercise[DataRaw$sport == "Other"] <- "other"
DataClean$lifestyle_exercise[DataRaw$sport == "I don’t do physical activities"] <- "i_do_not_exercise"

# Conversion en facteur
DataClean$lifestyle_exercise <- factor(DataClean$lifestyle_exercise)

# Vérification des résultats
table(DataClean$lifestyle_exercise)

# Each sport bin
DataClean$lifestyle_exerciseGym <- NA
DataClean$lifestyle_exerciseGym[DataRaw$sport == "Gym"] <- 1
DataClean$lifestyle_exerciseGym[DataRaw$sport != "Gym"] <- 0
table(DataClean$lifestyle_exerciseGym)

DataClean$lifestyle_exerciseTeamSport <- NA
DataClean$lifestyle_exerciseTeamSport[DataRaw$sport == "Team sport"] <- 1
DataClean$lifestyle_exerciseTeamSport[DataRaw$sport != "Team sport"] <- 0
table(DataClean$lifestyle_exerciseTeamSport)

DataClean$lifestyle_exerciseWalk <- NA
DataClean$lifestyle_exerciseWalk[DataRaw$sport == "Walk"] <- 1
DataClean$lifestyle_exerciseWalk[DataRaw$sport != "Walk"] <- 0
table(DataClean$lifestyle_exerciseWalk)

DataClean$lifestyle_exerciseRun <- NA
DataClean$lifestyle_exerciseRun[DataRaw$sport == "Run"] <- 1
DataClean$lifestyle_exerciseRun[DataRaw$sport != "Run"] <- 0
table(DataClean$lifestyle_exerciseRun)

DataClean$lifestyle_exerciseYoga <- NA
DataClean$lifestyle_exerciseYoga[DataRaw$sport == "Yoga"] <- 1
DataClean$lifestyle_exerciseYoga[DataRaw$sport != "Yoga"] <- 0
table(DataClean$lifestyle_exerciseYoga)

DataClean$lifestyle_exerciseSwim <- NA
DataClean$lifestyle_exerciseSwim[DataRaw$sport == "Swim"] <- 1
DataClean$lifestyle_exerciseSwim[DataRaw$sport != "Swim"] <- 0
table(DataClean$lifestyle_exerciseSwim)

DataClean$lifestyle_exerciseOther <- NA
DataClean$lifestyle_exerciseOther[DataRaw$sport == "Other"] <- 1
DataClean$lifestyle_exerciseOther[DataRaw$sport != "Other"] <- 0
table(DataClean$lifestyle_exerciseOther)

DataClean$lifestyle_exerciseNone <- NA
DataClean$lifestyle_exerciseNone[DataRaw$sport == "I don’t do physical activities"] <- 1
DataClean$lifestyle_exerciseNone[DataRaw$sport != "I don’t do physical activities"] <- 0
table(DataClean$lifestyle_exerciseNone)

# 祖国万岁！

## activity_1 ------------------------------------------------------------

## activity_1 ------------------------------------------------------------

# Vérification des valeurs existantes
table(DataRaw$fishing)

# Assignation des valeurs numériques
DataClean$lifestyle_goFishingFreq <- NA
DataClean$lifestyle_goFishingFreq[DataRaw$fishing == "Never"] <- 1
DataClean$lifestyle_goFishingFreq[DataRaw$fishing == "Almost never"] <- 2
DataClean$lifestyle_goFishingFreq[DataRaw$fishing == "Sometimes"] <- 3
DataClean$lifestyle_goFishingFreq[DataRaw$fishing == "Often"] <- 4
DataClean$lifestyle_goFishingFreq[DataRaw$fishing == "Very often"] <- 5
table(DataClean$lifestyle_goFishingFreq)

## factor
DataClean$lifestyle_goFishingFreq_factor <- NA
DataClean$lifestyle_goFishingFreq_factor[DataRaw$fishing == "Never"] <- "never"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$fishing == "Almost never"] <- "almost_never"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$fishing == "Sometimes"] <- "sometimes"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$fishing == "Often"] <- "often"
DataClean$lifestyle_goFishingFreq_factor[DataRaw$fishing == "Very often"] <- "very_often"
DataClean$lifestyle_goFishingFreq_factor <- factor(DataClean$lifestyle_goFishingFreq_factor,
                                                 levels = c("never",
                                                            "almost_never",
                                                            "sometimes",
                                                            "often",
                                                            "very_often"),
                                                 ordered = TRUE)
table(DataClean$lifestyle_goFishingFreq_factor)

## numeric
DataClean$lifestyle_goFishingFreq_numeric <- NA
DataClean$lifestyle_goFishingFreq_numeric[DataRaw$fishing == "Never"] <- 0
DataClean$lifestyle_goFishingFreq_numeric[DataRaw$fishing == "Almost never"] <- 0.25
DataClean$lifestyle_goFishingFreq_numeric[DataRaw$fishing == "Sometimes"] <- 0.5
DataClean$lifestyle_goFishingFreq_numeric[DataRaw$fishing == "Often"] <- 0.75
DataClean$lifestyle_goFishingFreq_numeric[DataRaw$fishing == "Very often"] <- 1
table(DataClean$lifestyle_goFishingFreq_numeric)

## bin
DataClean$lifestyle_goFishingFreq_bin <- NA
DataClean$lifestyle_goFishingFreq_bin[DataRaw$fishing == "Never"] <- 0
DataClean$lifestyle_goFishingFreq_bin[DataRaw$fishing %in% c("Almost never", "Sometimes", "Often", "Very often")] <- 1
table(DataClean$lifestyle_goFishingFreq_bin)

## activity_2 ------------------------------------------------------------
# Vérification des valeurs existantes
table(DataRaw$hunting)

# Assignation des valeurs numériques
DataClean$lifestyle_goHuntingFreq <- NA
DataClean$lifestyle_goHuntingFreq[DataRaw$hunting == "Never"] <- 1
DataClean$lifestyle_goHuntingFreq[DataRaw$hunting == "Almost never"] <- 2
DataClean$lifestyle_goHuntingFreq[DataRaw$hunting == "Sometimes"] <- 3
DataClean$lifestyle_goHuntingFreq[DataRaw$hunting == "Often"] <- 4
DataClean$lifestyle_goHuntingFreq[DataRaw$hunting == "Very often"] <- 5
table(DataClean$lifestyle_goHuntingFreq)

## factor
DataClean$lifestyle_goHuntingFreq_factor <- NA
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$hunting == "Never"] <- "never"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$hunting == "Almost never"] <- "almost_never"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$hunting == "Sometimes"] <- "sometimes"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$hunting == "Often"] <- "often"
DataClean$lifestyle_goHuntingFreq_factor[DataRaw$hunting == "Very often"] <- "very_often"
DataClean$lifestyle_goHuntingFreq_factor <- factor(DataClean$lifestyle_goHuntingFreq_factor,
                                                  levels = c("never",
                                                             "almost_never",
                                                             "sometimes",
                                                             "often",
                                                             "very_often"),
                                                  ordered = TRUE)
table(DataClean$lifestyle_goHuntingFreq_factor)

## numeric
DataClean$lifestyle_goHuntingFreq_numeric <- NA
DataClean$lifestyle_goHuntingFreq_numeric[DataRaw$hunting == "Never"] <- 0
DataClean$lifestyle_goHuntingFreq_numeric[DataRaw$hunting == "Almost never"] <- 0.25
DataClean$lifestyle_goHuntingFreq_numeric[DataRaw$hunting == "Sometimes"] <- 0.5
DataClean$lifestyle_goHuntingFreq_numeric[DataRaw$hunting == "Often"] <- 0.75
DataClean$lifestyle_goHuntingFreq_numeric[DataRaw$hunting == "Very often"] <- 1
table(DataClean$lifestyle_goHuntingFreq_numeric)

## bin
DataClean$lifestyle_goHuntingFreq_bin <- NA
DataClean$lifestyle_goHuntingFreq_bin[DataRaw$hunting == "Never"] <- 0
DataClean$lifestyle_goHuntingFreq_bin[DataRaw$hunting %in% c("Almost never", "Sometimes", "Often", "Very often")] <- 1
table(DataClean$lifestyle_goHuntingFreq_bin)


## activity_3 ------------------------------------------------------------



## activity_4 -----------------------------------------------------------




## activity_5 ------------------------------------------------------------

# Vérification des valeurs existantes
table(DataRaw$art)

# Assignation des valeurs numériques
DataClean$lifestyle_goMuseumsFreq <- NA
DataClean$lifestyle_goMuseumsFreq[DataRaw$art == "Never"] <- 1
DataClean$lifestyle_goMuseumsFreq[DataRaw$art == "Almost never"] <- 2
DataClean$lifestyle_goMuseumsFreq[DataRaw$art == "Sometimes"] <- 3
DataClean$lifestyle_goMuseumsFreq[DataRaw$art == "Often"] <- 4
DataClean$lifestyle_goMuseumsFreq[DataRaw$art == "Very often"] <- 5
table(DataClean$lifestyle_goMuseumsFreq)

## factor
DataClean$lifestyle_goMuseumsFreq_factor <- NA
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$art == "Never"] <- "never"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$art == "Almost never"] <- "almost_never"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$art == "Sometimes"] <- "sometimes"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$art == "Often"] <- "often"
DataClean$lifestyle_goMuseumsFreq_factor[DataRaw$art == "Very often"] <- "very_often"
DataClean$lifestyle_goMuseumsFreq_factor <- factor(DataClean$lifestyle_goMuseumsFreq_factor,
                                                   levels = c("never",
                                                              "almost_never",
                                                              "sometimes",
                                                              "often",
                                                              "very_often"),
                                                   ordered = TRUE)
table(DataClean$lifestyle_goMuseumsFreq_factor)

## numeric
DataClean$lifestyle_goMuseumsFreq_numeric <- NA
DataClean$lifestyle_goMuseumsFreq_numeric[DataRaw$art == "Never"] <- 0
DataClean$lifestyle_goMuseumsFreq_numeric[DataRaw$art == "Almost never"] <- 0.25
DataClean$lifestyle_goMuseumsFreq_numeric[DataRaw$art == "Sometimes"] <- 0.5
DataClean$lifestyle_goMuseumsFreq_numeric[DataRaw$art == "Often"] <- 0.75
DataClean$lifestyle_goMuseumsFreq_numeric[DataRaw$art == "Very often"] <- 1
table(DataClean$lifestyle_goMuseumsFreq_numeric)

## bin
DataClean$lifestyle_goMuseumsFreq_bin <- NA
DataClean$lifestyle_goMuseumsFreq_bin[DataRaw$art == "Never"] <- 0
DataClean$lifestyle_goMuseumsFreq_bin[DataRaw$art %in% c("Almost never", "Sometimes", "Often", "Very often")] <- 1
table(DataClean$lifestyle_goMuseumsFreq_bin)

## activity_6 ------------------------------------------------------------




## activity_7 ------------------------------------------------------------


## activity_8 ------------------------------------------------------------
# Créer la variable numérique
DataClean$lifestyle_motorizedActFreq <- NA

# Conversion vers numérique en fonction des catégories dans outdoor_activities
DataClean$lifestyle_motorizedActFreq[DataRaw$outdoor_activities == "Never"] <- 1
DataClean$lifestyle_motorizedActFreq[DataRaw$outdoor_activities == "Almost never"] <- 2
DataClean$lifestyle_motorizedActFreq[DataRaw$outdoor_activities == "Sometimes"] <- 3
DataClean$lifestyle_motorizedActFreq[DataRaw$outdoor_activities == "Often"] <- 4
DataClean$lifestyle_motorizedActFreq[DataRaw$outdoor_activities == "Very often"] <- 5

# Vérifier les résultats
table(DataClean$lifestyle_motorizedActFreq)

## factor
DataClean$lifestyle_motorizedActFreq_factor <- NA
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$outdoor_activities == "Never"] <- "never"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$outdoor_activities == "Almost never"] <- "almost_never"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$outdoor_activities == "Sometimes"] <- "sometimes"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$outdoor_activities == "Often"] <- "often"
DataClean$lifestyle_motorizedActFreq_factor[DataRaw$outdoor_activities == "Very often"] <- "very_often"

DataClean$lifestyle_motorizedActFreq_factor <- factor(DataClean$lifestyle_motorizedActFreq_factor, 
                                                     levels = c("never", "almost_never", "sometimes", "often", "very_often"), 
                                                     ordered = TRUE)

# Vérifier les résultats
table(DataClean$lifestyle_motorizedActFreq_factor)

## numeric
DataClean$lifestyle_motorizedActFreq_numeric <- NA
DataClean$lifestyle_motorizedActFreq_numeric <- (as.numeric(factor(DataRaw$outdoor_activities, 
                                                levels = c("Never", "Almost never", "Sometimes", "Often", "Very often"))) - 1) / 4

# Vérifier les résultats
table(DataClean$lifestyle_motorizedActFreq_numeric)

## bin
DataClean$lifestyle_motorizedActFreq_bin <- NA
DataClean$lifestyle_motorizedActFreq_bin[DataRaw$outdoor_activities == "Never"] <- 0
DataClean$lifestyle_motorizedActFreq_bin[DataRaw$outdoor_activities %in% c("Almost never", "Sometimes", "Often", "Very often")] <- 1

# Vérifier les résultats
table(DataClean$lifestyle_motorizedActFreq_bin)

## activity_9 ------------------------------------------------------------

## activity_10 -----------------------------------------------------------

# Créer la variable numérique
DataClean$lifestyle_volunteeringFreq <- NA

# Conversion vers numérique en fonction des catégories dans volunteering
DataClean$lifestyle_volunteeringFreq[DataRaw$volunteering == "Never"] <- 1
DataClean$lifestyle_volunteeringFreq[DataRaw$volunteering == "Almost never"] <- 2
DataClean$lifestyle_volunteeringFreq[DataRaw$volunteering == "Sometimes"] <- 3
DataClean$lifestyle_volunteeringFreq[DataRaw$volunteering == "Often"] <- 4
DataClean$lifestyle_volunteeringFreq[DataRaw$volunteering == "Very often"] <- 5

# Vérifier les résultats
table(DataClean$lifestyle_volunteeringFreq)

## factor
DataClean$lifestyle_volunteeringFreq_factor <- NA
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$volunteering == "Never"] <- "never"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$volunteering == "Almost never"] <- "almost_never"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$volunteering == "Sometimes"] <- "sometimes"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$volunteering == "Often"] <- "often"
DataClean$lifestyle_volunteeringFreq_factor[DataRaw$volunteering == "Very often"] <- "very_often"

DataClean$lifestyle_volunteeringFreq_factor <- factor(DataClean$lifestyle_volunteeringFreq_factor, 
                                                      levels = c("never", 
                                                                 "almost_never", 
                                                                 "sometimes", 
                                                                 "often", 
                                                                 "very_often"), 
                                                      ordered = TRUE)

# Vérifier les résultats
table(DataClean$lifestyle_volunteeringFreq_factor)

## numeric
DataClean$lifestyle_volunteeringFreq_numeric <- NA
DataClean$lifestyle_volunteeringFreq_numeric <- (as.numeric(factor(DataRaw$volunteering, 
                                                         levels = c("Never", "Almost never", "Sometimes", "Often", "Very often"))) - 1) / 4

# Vérifier les résultats
table(DataClean$lifestyle_volunteeringFreq_numeric)

## bin
DataClean$lifestyle_volunteeringFreq_bin <- NA
DataClean$lifestyle_volunteeringFreq_bin[DataRaw$volunteering == "Never"] <- 0
DataClean$lifestyle_volunteeringFreq_bin[DataRaw$volunteering %in% c("Almost never", "Sometimes", "Often", "Very often")] <- 1

# Vérifier les résultats
table(DataClean$lifestyle_volunteeringFreq_bin)


## activity_11 -----------------------------------------------------------



## activity_12 -----------------------------------------------------------



## activity_13 -----------------------------------------------------------


## type_transport --------------------------------------------------------

# Regrouper les types de transport
DataClean$lifestyle_typeTransport <- NA

# Véhicules personnels (car, SUV, motorcycle) -> "car"
DataClean$lifestyle_typeTransport[DataRaw$transport %in% c("Car", "SUV", "Motorcycle")] <- "car"

# Transport actif (walk, bicycle) -> "active_transport"
DataClean$lifestyle_typeTransport[DataRaw$transport %in% c("Walk", "Bicycle")] <- "active_transport"

# Transport partagé/public (public transit) -> "shared_transport"
DataClean$lifestyle_typeTransport[DataRaw$transport %in% c("Public transit")] <- "shared_transport"

# Convertir en facteur
DataClean$lifestyle_typeTransport <- as.factor(DataClean$lifestyle_typeTransport)

# Vérifier les résultats
table(DataClean$lifestyle_typeTransport)

## Variables binaires pour chaque mode de transport
# Car
DataClean$lifestyle_typeTransportCar <- NA
DataClean$lifestyle_typeTransportCar[DataRaw$transport == "Car"] <- 1
DataClean$lifestyle_typeTransportCar[DataRaw$transport != "Car"] <- 0
table(DataClean$lifestyle_typeTransportCar)

# SUV
DataClean$lifestyle_typeTransportSUV <- NA
DataClean$lifestyle_typeTransportSUV[DataRaw$transport == "SUV"] <- 1
DataClean$lifestyle_typeTransportSUV[DataRaw$transport != "SUV"] <- 0
table(DataClean$lifestyle_typeTransportSUV)

# Motorcycle
DataClean$lifestyle_typeTransportMoto <- NA
DataClean$lifestyle_typeTransportMoto[DataRaw$transport == "Motorcycle"] <- 1
DataClean$lifestyle_typeTransportMoto[DataRaw$transport != "Motorcycle"] <- 0
table(DataClean$lifestyle_typeTransportMoto)

# Walk
DataClean$lifestyle_typeTransportWalk <- NA
DataClean$lifestyle_typeTransportWalk[DataRaw$transport == "Walk"] <- 1
DataClean$lifestyle_typeTransportWalk[DataRaw$transport != "Walk"] <- 0
table(DataClean$lifestyle_typeTransportWalk)

# Bicycle
DataClean$lifestyle_typeTransportBicycle <- NA
DataClean$lifestyle_typeTransportBicycle[DataRaw$transport == "Bicycle"] <- 1
DataClean$lifestyle_typeTransportBicycle[DataRaw$transport != "Bicycle"] <- 0
table(DataClean$lifestyle_typeTransportBicycle)

# Public Transit
DataClean$lifestyle_typeTransportPublicTransit <- NA
DataClean$lifestyle_typeTransportPublicTransit[DataRaw$transport == "Public transit"] <- 1
DataClean$lifestyle_typeTransportPublicTransit[DataRaw$transport != "Public transit"] <- 0
table(DataClean$lifestyle_typeTransportPublicTransit)

# Note: Carpooling et Carsharing ne semblent pas être présents dans la nouvelle variable


## choice_transport ------------------------------------------------------


## field_occupation ------------------------------------------------------


## type_occupation -------------------------------------------------------

## clothes_consumption ---------------------------------------------------


# Regrouper les types de magasins
DataClean$lifestyle_consClothes <- NA

# Grands détaillants (Department stores, Chain stores, Superstores)
DataClean$lifestyle_consClothes[DataRaw$shopping %in% c("Department stores (The Bay, Simons, etc.)", 
                                                       "Chain stores (Gap, Zara, etc.)", 
                                                       "Superstores (Walmart, Costco, etc.)")] <- "large_retailers"

# Petits magasins locaux (Independent stores, Thrift stores)
DataClean$lifestyle_consClothes[DataRaw$shopping %in% c("Independent stores", 
                                                       "Thrift stores")] <- "small_local_store"

# Magasins en ligne uniquement
DataClean$lifestyle_consClothes[DataRaw$shopping == "Online only stores"] <- "online_store"

# Autres
DataClean$lifestyle_consClothes[DataRaw$shopping == "Other"] <- "other"

# Convertir en facteur ordonné
DataClean$lifestyle_consClothes <- factor(DataClean$lifestyle_consClothes,
                                         levels = c("large_retailers",
                                                   "small_local_store",
                                                   "online_store",
                                                   "other"),
                                         ordered = TRUE)

# Vérifier les résultats
table(DataClean$lifestyle_consClothes)

## Variables binaires pour chaque type de magasin
# Department stores
DataClean$lifestyle_consClothesDepartment <- NA
DataClean$lifestyle_consClothesDepartment[DataRaw$shopping == "Department stores (The Bay, Simons, etc.)"] <- 1
DataClean$lifestyle_consClothesDepartment[DataRaw$shopping != "Department stores (The Bay, Simons, etc.)"] <- 0
table(DataClean$lifestyle_consClothesDepartment)

# Independent stores
DataClean$lifestyle_consClothesIndependent <- NA
DataClean$lifestyle_consClothesIndependent[DataRaw$shopping == "Independent stores"] <- 1
DataClean$lifestyle_consClothesIndependent[DataRaw$shopping != "Independent stores"] <- 0
table(DataClean$lifestyle_consClothesIndependent)

# Chain stores
DataClean$lifestyle_consClothesChain <- NA
DataClean$lifestyle_consClothesChain[DataRaw$shopping == "Chain stores (Gap, Zara, etc.)"] <- 1
DataClean$lifestyle_consClothesChain[DataRaw$shopping != "Chain stores (Gap, Zara, etc.)"] <- 0
table(DataClean$lifestyle_consClothesChain)

# Superstores
DataClean$lifestyle_consClothesSuperstores <- NA
DataClean$lifestyle_consClothesSuperstores[DataRaw$shopping == "Superstores (Walmart, Costco, etc.)"] <- 1
DataClean$lifestyle_consClothesSuperstores[DataRaw$shopping != "Superstores (Walmart, Costco, etc.)"] <- 0
table(DataClean$lifestyle_consClothesSuperstores)

# Online only stores
DataClean$lifestyle_consClothesOnline <- NA
DataClean$lifestyle_consClothesOnline[DataRaw$shopping == "Online only stores"] <- 1
DataClean$lifestyle_consClothesOnline[DataRaw$shopping != "Online only stores"] <- 0
table(DataClean$lifestyle_consClothesOnline)

# Thrift stores (anciennement "Second hand stores")
DataClean$lifestyle_consClothesFrip <- NA
DataClean$lifestyle_consClothesFrip[DataRaw$shopping == "Thrift stores"] <- 1
DataClean$lifestyle_consClothesFrip[DataRaw$shopping != "Thrift stores"] <- 0
table(DataClean$lifestyle_consClothesFrip)

# Other
DataClean$lifestyle_consClothesOther <- NA
DataClean$lifestyle_consClothesOther[DataRaw$shopping == "Other"] <- 1
DataClean$lifestyle_consClothesOther[DataRaw$shopping != "Other"] <- 0
table(DataClean$lifestyle_consClothesOther)
## mode_attitude_1 ---------------------------------------------------------



## meat_consumption ------------------------------------------------------

# Création d'une nouvelle variable dans DataClean pour la fréquence de consommation alimentaire
DataClean$lifestyle_eatMeatFreq <- NA

# Associer chaque valeur textuelle à une valeur numérique normalisée (entre 0 et 1)
# "Never" = 0 (jamais)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "Never"] <- 0

# "Once a month" = 0.17 (occasionnellement)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "Once a month"] <- 0.17

# "A few times per year" = 0.08 (très rarement)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "A few times per year"] <- 0.08

# "Once a week" = 0.33 (hebdomadaire)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "Once a week"] <- 0.33

# "A few times a week" = 0.67 (plusieurs fois par semaine)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "A few times a week"] <- 0.67

# "Once a day" = 0.83 (quotidien)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "Once a day"] <- 0.83

# "More than once a day" = 1 (plusieurs fois par jour)
DataClean$lifestyle_eatMeatFreq[DataRaw$food == "More than once a day"] <- 1

# Vérification de la distribution des valeurs après transformation
table(DataClean$lifestyle_eatMeatFreq)

## meal_time -------------------------------------------------------------



## fridge_1 ----------------------------------------------------------------



### fridge_2 -------------------------------------------------------------


### fridge_3 -------------------------------------------------------------



### fridge_4 -------------------------------------------------------------


### fridge_5 -------------------------------------------------------------


### fridge_6 -------------------------------------------------------------



### fridge_7 -------------------------------------------------------------



### fridge_8 -------------------------------------------------------------



### fridge_9 -------------------------------------------------------------



### fridge_10 -------------------------------------------------------------



### fridge_11 ------------------------------------------------------------




## coffee_consumption ---------------------------------------------------

# Création de la variable catégorielle pour les choix de café
DataClean$lifestyle_consCoffee <- NA
DataClean$lifestyle_consCoffee[DataRaw$coffee == "Tim Hortons"] <- "tim_hortons"
DataClean$lifestyle_consCoffee[DataRaw$coffee == "Starbucks"] <- "starbucks"
DataClean$lifestyle_consCoffee[DataRaw$coffee == "Second Cup"] <- "second_cup"
DataClean$lifestyle_consCoffee[DataRaw$coffee == "McDonald's"] <- "mcdonalds"
DataClean$lifestyle_consCoffee[DataRaw$coffee == "Other"] <- "other"

DataClean$lifestyle_consCoffee[DataRaw$coffee == "Independent coffee shops"] <- "independent"
DataClean$lifestyle_consCoffee[DataRaw$coffee == "I don’t go to coffee shops"] <- "no_coffee"

# Conversion en facteur ordonné
DataClean$lifestyle_consCoffee <- factor(DataClean$lifestyle_consCoffee,
  levels = c("tim_hortons",
            "starbucks",
            "second_cup",
            "mcdonalds",
            "other",
            "independent",
            "no_coffee"),
  ordered = TRUE)

# Vérification de la distribution
table(DataClean$lifestyle_consCoffee)

## Création des variables binaires pour chaque choix de café

# Tim Hortons
DataClean$lifestyle_consCoffeeTimHortons <- NA
DataClean$lifestyle_consCoffeeTimHortons[DataRaw$coffee == "Tim Hortons"] <- 1
DataClean$lifestyle_consCoffeeTimHortons[DataRaw$coffee != "Tim Hortons"] <- 0
table(DataClean$lifestyle_consCoffeeTimHortons)

# Starbucks
DataClean$lifestyle_consCoffeeStarbucks <- NA
DataClean$lifestyle_consCoffeeStarbucks[DataRaw$coffee == "Starbucks"] <- 1
DataClean$lifestyle_consCoffeeStarbucks[DataRaw$coffee != "Starbucks"] <- 0
table(DataClean$lifestyle_consCoffeeStarbucks)

# Second Cup
DataClean$lifestyle_consCoffeeSecondCup <- NA
DataClean$lifestyle_consCoffeeSecondCup[DataRaw$coffee == "Second Cup"] <- 1
DataClean$lifestyle_consCoffeeSecondCup[DataRaw$coffee != "Second Cup"] <- 0
table(DataClean$lifestyle_consCoffeeSecondCup)

# McDonald's
DataClean$lifestyle_consCoffeeMcDo <- NA
DataClean$lifestyle_consCoffeeMcDo[DataRaw$coffee == "McDonald's"] <- 1
DataClean$lifestyle_consCoffeeMcDo[DataRaw$coffee != "McDonald's"] <- 0
table(DataClean$lifestyle_consCoffeeMcDo)

# Other
DataClean$lifestyle_consCoffeeOther <- NA
DataClean$lifestyle_consCoffeeOther[DataRaw$coffee == "Other"] <- 1
DataClean$lifestyle_consCoffeeOther[DataRaw$coffee != "Other"] <- 0
table(DataClean$lifestyle_consCoffeeOther)

# Independent coffee shops
DataClean$lifestyle_consCoffeeIndependent <- NA
DataClean$lifestyle_consCoffeeIndependent[DataRaw$coffee == "Independent coffee shops"] <- 1
DataClean$lifestyle_consCoffeeIndependent[DataRaw$coffee != "Independent coffee shops"] <- 0
table(DataClean$lifestyle_consCoffeeIndependent)

# No coffee shops
DataClean$lifestyle_consCoffeeNone <- NA
DataClean$lifestyle_consCoffeeNone[DataRaw$coffee == "I don’t go to coffee shops"] <- 1
DataClean$lifestyle_consCoffeeNone[DataRaw$coffee != "I don’t go to coffee shops"] <- 0
table(DataClean$lifestyle_consCoffeeNone)  # Correction du nom de variable par rapport au code original

## coffee_machine --------------------------------------------------------


## cons_pet --------------------------------------------------------------

# Création de la variable catégorielle pour les animaux de compagnie
DataClean$lifestyle_ownPet <- NA
DataClean$lifestyle_ownPet[DataRaw$pets == "Cat(s)"] <- "cat"
DataClean$lifestyle_ownPet[DataRaw$pets == "Dog(s)"] <- "dog"
DataClean$lifestyle_ownPet[DataRaw$pets == "Cat(s) and dog(s)"] <- "cat_and_dog"
DataClean$lifestyle_ownPet[DataRaw$pets == "Other pets"] <- "other"  # "diverse_animals" au lieu de "other" selon mapping original
DataClean$lifestyle_ownPet[DataRaw$pets == "Farm animals"] <- "farm_animals"
DataClean$lifestyle_ownPet[DataRaw$pets == "I don’t have pets"] <- "none"

# Conversion en facteur ordonné
DataClean$lifestyle_ownPet <- factor(DataClean$lifestyle_ownPet,
  levels = c("cat",
            "dog",
            "cat_and_dog",
            "diverse_animals",
            "other",
            "farm_animals",
            "none"),
  ordered = TRUE)

# Vérification de la distribution
table(DataClean$lifestyle_ownPet)

# Variable binaire pour la possession d'animaux (oui/non)
DataClean$lifestyle_ownPet_bin <- NA
DataClean$lifestyle_ownPet_bin[DataRaw$pets == "I don’t have pets"] <- 0
DataClean$lifestyle_ownPet_bin[DataRaw$pets != "I don’t have pets"] <- 1
table(DataClean$lifestyle_ownPet_bin)

## Création des variables binaires pour chaque type d'animal

# Chat
DataClean$lifestyle_ownPetCat <- NA
DataClean$lifestyle_ownPetCat[DataRaw$pets == "Cat(s)"] <- 1
DataClean$lifestyle_ownPetCat[DataRaw$pets != "Cat(s)"] <- 0
table(DataClean$lifestyle_ownPetCat)

# Chien
DataClean$lifestyle_ownPetDog <- NA
DataClean$lifestyle_ownPetDog[DataRaw$pets == "Dog(s)"] <- 1
DataClean$lifestyle_ownPetDog[DataRaw$pets != "Dog(s)"] <- 0
table(DataClean$lifestyle_ownPetDog)

# Chat et chien
DataClean$lifestyle_ownPetCatAndDog <- NA
DataClean$lifestyle_ownPetCatAndDog[DataRaw$pets == "Cat(s) and dog(s)"] <- 1
DataClean$lifestyle_ownPetCatAndDog[DataRaw$pets != "Cat(s) and dog(s)"] <- 0
table(DataClean$lifestyle_ownPetCatAndDog)

# other
DataClean$lifestyle_ownPetOther <- NA
DataClean$lifestyle_ownPetOther[DataRaw$pets == "Other pets"] <- 1
DataClean$lifestyle_ownPetOther[DataRaw$pets != "Other pets"] <- 0
table(DataClean$lifestyle_ownPetOther)



# Animaux de ferme
DataClean$lifestyle_ownPetFarmAnimals <- NA
DataClean$lifestyle_ownPetFarmAnimals[DataRaw$pets == "Farm animals"] <- 1
DataClean$lifestyle_ownPetFarmAnimals[DataRaw$pets != "Farm animals"] <- 0
table(DataClean$lifestyle_ownPetFarmAnimals)

# Aucun animal
DataClean$lifestyle_ownPetNone <- NA
DataClean$lifestyle_ownPetNone[DataRaw$pets == "I don’t have pets"] <- 1
DataClean$lifestyle_ownPetNone[DataRaw$pets != "I don’t have pets"] <- 0
table(DataClean$lifestyle_ownPetNone)


## smoking ---------------------------------------------------------------


# Création de la variable pour la fréquence de tabagisme
DataClean$lifestyle_smokeFreq <- NA

# Association de chaque valeur textuelle à une valeur numérique normalisée (entre 0 et 1)
# "Never" = 0 (jamais)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "Never"] <- 0

# "Once a month" = 0.1667 (très occasionnellement)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "Once a month"] <- 0.1667

# "A few times per year" = 0.0833 (rarement - valeur ajustée pour être plus faible que mensuel)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "A few times per year"] <- 0.0833

# "Once a week" = 0.3333 (hebdomadaire)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "Once a week"] <- 0.3333

# "A few times per week" = 0.5 (plusieurs fois par semaine)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "A few times per week"] <- 0.5

# "Once a day" = 0.8333 (quotidien)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "Once a day"] <- 0.8333

# "More than once a day" = 1 (plusieurs fois par jour)
DataClean$lifestyle_smokeFreq[DataRaw$smoking == "More than once a day"] <- 1

# Vérification de la distribution des valeurs après transformation
table(DataClean$lifestyle_smokeFreq)

## alcool_type -----------------------------------------------------------

# Création de la variable catégorielle pour le type d'alcool préféré
DataClean$lifestyle_favAlcool <- NA

# Regroupement des types de vin
DataClean$lifestyle_favAlcool[DataRaw$alcohol %in% c("Red wine", "White wine", "Rosé wine", "Sparkling wine or champagne")] <- "wine"

# Regroupement des types de bière
DataClean$lifestyle_favAlcool[DataRaw$alcohol %in% c("Regular beer", "Craft and microbrewery beer")] <- "beer"

# Spiritueux
DataClean$lifestyle_favAlcool[DataRaw$alcohol == "Spirituous beverage"] <- "spirits"

# Cocktails
DataClean$lifestyle_favAlcool[DataRaw$alcohol == "Cocktail"] <- "cocktail"

# Ne boit pas d'alcool
DataClean$lifestyle_favAlcool[DataRaw$alcohol == "I do not drink alcohol"] <- "dont_drink"

# Conversion en facteur
DataClean$lifestyle_favAlcool <- factor(DataClean$lifestyle_favAlcool)
table(DataClean$lifestyle_favAlcool)

## Variables binaires pour chaque type d'alcool

# Vin rouge
DataClean$lifestyle_favAlcoolRedWine <- NA
DataClean$lifestyle_favAlcoolRedWine[DataRaw$alcohol == "Red wine"] <- 1
DataClean$lifestyle_favAlcoolRedWine[DataRaw$alcohol != "Red wine"] <- 0
table(DataClean$lifestyle_favAlcoolRedWine)

# Vin blanc
DataClean$lifestyle_favAlcoolWhiteWine <- NA
DataClean$lifestyle_favAlcoolWhiteWine[DataRaw$alcohol == "White wine"] <- 1
DataClean$lifestyle_favAlcoolWhiteWine[DataRaw$alcohol != "White wine"] <- 0
table(DataClean$lifestyle_favAlcoolWhiteWine)

# Vin rosé
DataClean$lifestyle_favAlcoolRoseWine <- NA
DataClean$lifestyle_favAlcoolRoseWine[DataRaw$alcohol == "Rosé wine"] <- 1
DataClean$lifestyle_favAlcoolRoseWine[DataRaw$alcohol != "Rosé wine"] <- 0
table(DataClean$lifestyle_favAlcoolRoseWine)

# Vin mousseux/champagne
DataClean$lifestyle_favAlcoolBubbleDrink <- NA
DataClean$lifestyle_favAlcoolBubbleDrink[DataRaw$alcohol == "Sparkling wine or champagne"] <- 1
DataClean$lifestyle_favAlcoolBubbleDrink[DataRaw$alcohol != "Sparkling wine or champagne"] <- 0
table(DataClean$lifestyle_favAlcoolBubbleDrink)

# Bière régulière
DataClean$lifestyle_favAlcoolBeer <- NA
DataClean$lifestyle_favAlcoolBeer[DataRaw$alcohol == "Regular beer"] <- 1
DataClean$lifestyle_favAlcoolBeer[DataRaw$alcohol != "Regular beer"] <- 0
table(DataClean$lifestyle_favAlcoolBeer)

# Bière artisanale
DataClean$lifestyle_favAlcoolMicroBeer <- NA
DataClean$lifestyle_favAlcoolMicroBeer[DataRaw$alcohol == "Craft and microbrewery beer"] <- 1
DataClean$lifestyle_favAlcoolMicroBeer[DataRaw$alcohol != "Craft and microbrewery beer"] <- 0
table(DataClean$lifestyle_favAlcoolMicroBeer)

# Spiritueux
DataClean$lifestyle_favAlcoolSpirits <- NA
DataClean$lifestyle_favAlcoolSpirits[DataRaw$alcohol == "Spirituous beverage"] <- 1
DataClean$lifestyle_favAlcoolSpirits[DataRaw$alcohol != "Spirituous beverage"] <- 0
table(DataClean$lifestyle_favAlcoolSpirits)

# Cocktail
DataClean$lifestyle_favAlcoolCocktail <- NA
DataClean$lifestyle_favAlcoolCocktail[DataRaw$alcohol == "Cocktail"] <- 1
DataClean$lifestyle_favAlcoolCocktail[DataRaw$alcohol != "Cocktail"] <- 0
table(DataClean$lifestyle_favAlcoolCocktail)

# Ne boit pas d'alcool
DataClean$lifestyle_favAlcoolDontDrink <- NA
DataClean$lifestyle_favAlcoolDontDrink[DataRaw$alcohol == "I do not drink alcohol"] <- 1
DataClean$lifestyle_favAlcoolDontDrink[DataRaw$alcohol != "I do not drink alcohol"] <- 0
table(DataClean$lifestyle_favAlcoolDontDrink)


## alcool_frequency ------------------------------------------------------


## marijuana_frequency ---------------------------------------------------




## musical_band ----------------------------------------------------------




## musical_style ---------------------------------------------------------




## movie_preference ------------------------------------------------------




## social_media_use ------------------------------------------------------



## social_media_time -----------------------------------------------------


## clothing_style --------------------------------------------------------
# Création de la variable pour le style vestimentaire désagrégé
DataClean$lifestyle_clothingStyle <- NA
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Formal"] <- "formal"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Classic"] <- "classic"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Casual"] <- "casual"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Sporty"] <- "sport"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Elegant"] <- "elegant"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Bohemian"] <- "hippie"  # Bohemian correspond à hippie
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Punk"] <- "punk"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Rock"] <- "rock"
DataClean$lifestyle_clothingStyle[DataRaw$clothing == "Other"] <- "other"
DataClean$lifestyle_clothingStyle <- factor(DataClean$lifestyle_clothingStyle)
table(DataClean$lifestyle_clothingStyle)

# Création de la variable pour le style vestimentaire regroupé
DataClean$lifestyle_clothingStyleGroups <- NA
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing %in% c("Formal", "Elegant")] <- "formal"
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing %in% c("Classic", "Casual", "Sporty")] <- "easygoing"
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing %in% c("Bohemian", "Punk", "Rock")] <- "edgy"
DataClean$lifestyle_clothingStyleGroups[DataRaw$clothing == "Other"] <- "other"
DataClean$lifestyle_clothingStyleGroups <- factor(DataClean$lifestyle_clothingStyleGroups)
table(DataClean$lifestyle_clothingStyleGroups)

# Variables binaires pour chaque style vestimentaire
# Formal
DataClean$lifestyle_clothingStyleFormal <- NA
DataClean$lifestyle_clothingStyleFormal[DataRaw$clothing == "Formal"] <- 1
DataClean$lifestyle_clothingStyleFormal[DataRaw$clothing != "Formal"] <- 0
table(DataClean$lifestyle_clothingStyleFormal)

# Classic
DataClean$lifestyle_clothingStyleClassic <- NA
DataClean$lifestyle_clothingStyleClassic[DataRaw$clothing == "Classic"] <- 1
DataClean$lifestyle_clothingStyleClassic[DataRaw$clothing != "Classic"] <- 0
table(DataClean$lifestyle_clothingStyleClassic)

# Casual
DataClean$lifestyle_clothingStyleCasual <- NA
DataClean$lifestyle_clothingStyleCasual[DataRaw$clothing == "Casual"] <- 1
DataClean$lifestyle_clothingStyleCasual[DataRaw$clothing != "Casual"] <- 0
table(DataClean$lifestyle_clothingStyleCasual)

# Sport
DataClean$lifestyle_clothingStyleSport <- NA
DataClean$lifestyle_clothingStyleSport[DataRaw$clothing == "Sporty"] <- 1
DataClean$lifestyle_clothingStyleSport[DataRaw$clothing != "Sporty"] <- 0
table(DataClean$lifestyle_clothingStyleSport)

# Elegant
DataClean$lifestyle_clothingStyleElegant <- NA
DataClean$lifestyle_clothingStyleElegant[DataRaw$clothing == "Elegant"] <- 1
DataClean$lifestyle_clothingStyleElegant[DataRaw$clothing != "Elegant"] <- 0
table(DataClean$lifestyle_clothingStyleElegant)

# Hippie (Bohemian)
DataClean$lifestyle_clothingStyleHippie <- NA
DataClean$lifestyle_clothingStyleHippie[DataRaw$clothing == "Bohemian"] <- 1
DataClean$lifestyle_clothingStyleHippie[DataRaw$clothing != "Bohemian"] <- 0
table(DataClean$lifestyle_clothingStyleHippie)

# Punk
DataClean$lifestyle_clothingStylePunk <- NA
DataClean$lifestyle_clothingStylePunk[DataRaw$clothing == "Punk"] <- 1
DataClean$lifestyle_clothingStylePunk[DataRaw$clothing != "Punk"] <- 0
table(DataClean$lifestyle_clothingStylePunk)

# Rock
DataClean$lifestyle_clothingStyleRock <- NA
DataClean$lifestyle_clothingStyleRock[DataRaw$clothing == "Rock"] <- 1
DataClean$lifestyle_clothingStyleRock[DataRaw$clothing != "Rock"] <- 0
table(DataClean$lifestyle_clothingStyleRock)

# Other
DataClean$lifestyle_clothingStyleOther <- NA
DataClean$lifestyle_clothingStyleOther[DataRaw$clothing == "Other"] <- 1
DataClean$lifestyle_clothingStyleOther[DataRaw$clothing != "Other"] <- 0
table(DataClean$lifestyle_clothingStyleOther)

## number_tattoos --------------------------------------------------------
# Création de la variable pour le nombre de tatouages
DataClean$lifestyle_numberTattoos <- NA

# Assignation des valeurs correctes
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "I don’t have tattoos"] <- 0  # 0 tatouage
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "1"] <- 1  # 1 tatouage
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "2"] <- 2  # 2 tatouages
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "3"] <- 3  # 3 tatouages
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "4"] <- 4  # 4 tatouages
DataClean$lifestyle_numberTattoos[DataRaw$tattoos == "5 or more"] <- 5  # 5+ tatouages traité comme 5

# Vérification des valeurs
table(DataClean$lifestyle_numberTattoos, useNA = "ifany")

# Vérification croisée - ceci devrait montrer la correspondance entre les données brutes et nettoyées
table(DataRaw$tattoos, DataClean$lifestyle_numberTattoos, useNA = "ifany")

# Créer la variable binaire 'has_tattoos'
DataClean$lifestyle_hasTattoos <- as.numeric(DataClean$lifestyle_numberTattoos > 0)

# Vérification de la variable binaire
table(DataClean$lifestyle_hasTattoos, useNA = "ifany")
