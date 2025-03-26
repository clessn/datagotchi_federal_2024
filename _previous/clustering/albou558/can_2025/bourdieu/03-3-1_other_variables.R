#############################
##    Chargement libraries ##
#############################
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)   # Pour reorder_within() et scale_x_reordered()

##################################
##   1) Importation du dataset  ##
##################################
df_pilot_2025 <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/can2025/03_pilot_2025.rds")

names(df_pilot_2025)
str(df_pilot_2025)
################################################################################
##   2) Liste des variables que l'on veut décrire (binaires ou continues)     ##
################################################################################
vars_clusters <- c(
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
  "lifestyle_volunteeringFreq_numeric",  
  "lifestyle_clothingStyleClassic",      
  "lifestyle_clothingStyleCasual",       
  "lifestyle_clothingStyleSport",        
  "lifestyle_clothingStyleOther",        
  "lifestyle_hasTattoos",                
  "lifestyle_ownPetCat",                 
  "lifestyle_ownPetDog",                 
  "lifestyle_ownPetOther",               
  "lifestyle_ownPetCatAndDog",           
  "lifestyle_ownPetNone",                
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
  "lifestyle_smokeFreq",                 
  "ses_dwellingApp",                     
  "ses_dwellingCondo",                   
  "ses_dwellingDetachedHouse",           
  "ses_dwellingTownhouse",               
  "ses_dwellingDuplex",                  
  "ses_dwellingOther",                   
  "lifestyle_typeTransportCar",          
  "lifestyle_typeTransportSUV",          
  "lifestyle_typeTransportPublicTransit",
  "lifestyle_typeTransportActive",       
  "lifestyle_consClothesFrip",           
  "lifestyle_consClothesIndependent",    
  "lifestyle_consClothesChain",          
  "lifestyle_consClothesSuperstores",    
  "lifestyle_consClothesDepartment",     
  "lifestyle_consClothesOnline",         
  "lifestyle_consClothesOther",          
  "lifestyle_consCoffeeTimHortons",      
  "lifestyle_consCoffeeStarbucks",       
  "lifestyle_consCoffeeMcDo",            
  "lifestyle_consCoffeeOther",           
  "lifestyle_consCoffeeIndependent",     
  "lifestyle_consCoffeeNone",            
  "ses_genderMale",                      
  "ses_genderFemale",
  "ses_immigrant",
  "ses_educBHS",                         
  "ses_educPostHS",                      
  "ses_educUniv",                        
  "income_no_income",                    
  "income_1_30000",                      
  "income_30001_60000",                  
  "income_60001_90000",                  
  "income_90001_110000",                 
  "income_110001_150000",                
  "income_150001_200000",                
  "income_more_than_200000",             
  "ses_languageEnglish",                 
  "ses_languageFrench",                  
  "ses_languageOther",                   
  "ses_ethnicityWhite",                  
  "ses_ethnicityBlack",                  
  "ses_ethnicityOther",                  
  "ses_sexOrientationHetero",            
  "ses_sexOrientationQueer"
)

##################################################################
##   3) Fonction pour calculer l'importance des variables       ##
##      selon leur différenciation entre clusters               ##
##################################################################
calculate_importance <- function(data, variables) {
  importance_scores <- sapply(variables, function(var) {
    overall_mean  <- mean(data[[var]], na.rm = TRUE)
    # Moyenne de la variable var dans chaque cluster
    cluster_means <- tapply(data[[var]], data$cluster, mean, na.rm = TRUE)

    # Différence absolue moyenne, en unités d'écart-type
    abs_diff <- abs(cluster_means - overall_mean) / sd(data[[var]], na.rm = TRUE)
    mean(abs_diff, na.rm = TRUE)
  })

  # Normaliser pour que la somme des scores = 1
  importance_scores <- importance_scores / sum(importance_scores, na.rm = TRUE)
  return(importance_scores)
}

######################################################
##   4) Fonction pour décrire (moyenne / sd) par    ##
##      cluster pour les variables analysées        ##
######################################################
describe_personas <- function(data, variables) {
  persona_summary <- data %>%
    group_by(cluster) %>%
    summarise(across(all_of(variables),
                     list(X_mean = ~mean(.x, na.rm = TRUE),
                          X_sd   = ~sd(.x, na.rm = TRUE))))
  return(persona_summary)
}

###################################################################
##   5) Calcul des scores d'importance et description globale    ##
###################################################################
importance_scores <- calculate_importance(df_pilot_2025, vars_clusters)

# Ranger par ordre décroissant et arrondir
importance_scores <- importance_scores[order(desc(importance_scores))]
importance_scores <- signif(importance_scores, 4)

# Affichage
cat(paste(names(importance_scores), ":", importance_scores, collapse = "\n"))

# Description : moyennes/écart-types par cluster
describe_personas(df_pilot_2025, vars_clusters)

###############################################################################
##   6) Table contenant la moyenne par cluster et la moyenne globale (overall)
###############################################################################
df_means <- df_pilot_2025 %>%
  select(all_of(vars_clusters), cluster) %>%
  pivot_longer(
    cols      = -cluster,
    names_to  = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(mean_overall = mean(value, na.rm = TRUE)) %>%
  group_by(variable, cluster) %>%
  summarise(
    mean_per_cluster = mean(value, na.rm = TRUE),
    mean_overall     = first(mean_overall),
    .groups          = "drop"
  )

#################################################
##   7) Fonctions pour tester la "couleur"      ##
##      (significativité statistique)           ##
#################################################

### 7.1. Pour une variable binaire
calculate_binary_color <- function(cluster_data, df, var) {
  prop_cluster <- mean(cluster_data[[var]], na.rm = TRUE)
  prop_overall <- mean(df[[var]], na.rm = TRUE)

  # Test de proportion
  test_prop <- prop.test(
    x = c(sum(cluster_data[[var]]), sum(df[[var]])),
    n = c(nrow(cluster_data), nrow(df))
  )
  p_value   <- test_prop$p.value
  prop_diff <- abs(prop_cluster - prop_overall)

  # Seuil : p < 0.05 et différence > 0.3 => vert ou rouge
  if (p_value < 0.05 && prop_diff > 0.3) {
    if (prop_cluster > prop_overall) "green" else "red"
  } else {
    "black"
  }
}

### 7.2. Pour une variable continue
calculate_continuous_color <- function(cluster_data, df, var) {
  # Test t de Student
  t_test <- t.test(cluster_data[[var]], df[[var]])
  p_value <- t_test$p.value

  # Taille d'effet (Cohen's d) par rapport à l'écart-type global
  effect_size <- abs(t_test$estimate[1] - t_test$estimate[2]) / sd(df[[var]], na.rm = TRUE)

  # Seuil : p < 0.05 et d > 0.25 => vert ou rouge
  if (p_value < 0.05 && effect_size > 0.25) {
    if (t_test$estimate[1] > t_test$estimate[2]) "green" else "red"
  } else {
    "black"
  }
}

################################################################
##   8) Appliquer la coloration à chaque variable de chaque    ##
##      cluster et construire un df "df_colors"               ##
################################################################
dfUsedForClustering        <- df_pilot_2025
variablesUsedForClustering <- vars_clusters

df_colors <- data.frame(
  cluster = character(),
  variable = character(),
  color = character(),
  stringsAsFactors = FALSE
)

# Pour chaque cluster, on teste chaque variable
for (cluster_value in unique(dfUsedForClustering$cluster)) {
  cluster_data <- dfUsedForClustering %>% filter(cluster == cluster_value)

  # On évalue si la variable est binaire (0/1) ou continue
  characteristic_vars <- sapply(variablesUsedForClustering, function(var) {
    # Vérifie si la variable ne contient que des 0 et 1
    if (all(dfUsedForClustering[[var]] %in% c(0, 1), na.rm = TRUE)) {
      calculate_binary_color(cluster_data, dfUsedForClustering, var)
    } else {
      calculate_continuous_color(cluster_data, dfUsedForClustering, var)
    }
  })

  cluster_results <- data.frame(
    cluster  = rep(cluster_value, length(variablesUsedForClustering)),
    variable = variablesUsedForClustering,
    color    = characteristic_vars,
    stringsAsFactors = FALSE
  )

  df_colors <- bind_rows(df_colors, cluster_results)
}

##############################################################
##   9) Fusion avec df_means pour préparer la visualisation ##
##############################################################
df_joined <- df_means %>%
  left_join(df_colors, by = c("variable", "cluster"))

# Tri et préparation finale pour le ggplot
all_data_viz <- df_joined %>%
  group_by(cluster) %>%
  arrange(desc(mean_per_cluster)) %>%
  ungroup() %>%
  mutate(
    cluster = as.factor(cluster),
    variable = as.character(variable)
  )

#########################################
##   10) Visualisation finale (ggplot)  ##
#########################################
ggplot(all_data_viz,
       aes(x = reorder_within(variable, mean_per_cluster, cluster),
           y = mean_per_cluster,
           fill = color)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_identity() +
  labs(title = "Visualisation des personas",
       x = "",
       y = "Moyenne / Proportion") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ cluster, scales = "free_y") +
  scale_x_reordered()

options(jupyter.plot_mimetypes = "image/png")
options(repr.plot.width = 15, repr.plot.height = 8)

# Filtrer pour ne conserver que les variables distinctives (couleurs vertes ou rouges)
all_data_viz_signif <- all_data_viz %>%
  filter(color %in% c("green", "red"))

# Afficher le graphique avec ggplot
ggplot(all_data_viz_signif, aes(x = reorder_within(variable, mean_per_cluster, cluster), 
                                y = mean_per_cluster, fill = color)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_identity() +
  labs(title = "Variables distinctives par cluster",
       x = "",
       y = "Moyenne/Proportion") +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ cluster, scales = "free_y") +
  scale_x_reordered()

# Créer un dataframe ne contenant que les variables distinctives (color != "black")
df_distinctives_vars <- df_joined %>%
  filter(color != "black") %>%
  arrange(cluster, desc(mean_per_cluster))

df_distinctives_vars
