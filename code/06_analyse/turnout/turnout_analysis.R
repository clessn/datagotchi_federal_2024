#load packages

#load data

data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_Ponderee_20250330.rds")

subset <- data %>%
select(dv_turnout_bin)

labels_humains <- c(
  "ses_ethnicityWhite" = "Personne blanche",
  "ses_genderMale" = "Homme",
  "ses_genderFemale" = "Femme",
  "ses_dwellingDetachedHouse" = "Maison unifamiliale",
  "ses_dwellingApp" = "Appartement",
  "ses_regionQc" = "Réside au Québec",
  "lifestyle_goMuseumsFreq_bin" = "Va souvent au musée",
  "lifestyle_ownPet_bin" = "Possède un animal",
  "lifestyle_ownPetNone" = "Ne possède pas d’animal",
  "lifestyle_favAlcoholRedWine" = "Aime le vin rouge",
  "lifestyle_favAlcoholMicroBeer" = "Aime la bière artisanale",
  "lifestyle_favAlcoholCocktail" = "Aime les cocktails",
  "lifestyle_favAlcoholDontDrink" = "Ne boit pas d’alcool",
  "lifestyle_clothingStyleElegant" = "Style vestimentaire élégant",
  "lifestyle_clothingStyleCasual" = "Style vestimentaire décontracté",
  "lifestyle_consClothesDepartment" = "Achète ses vêtements en grand magasin",
  "lifestyle_consCoffeeTimHortons" = "Boit du café Tim Hortons",
  "lifestyle_consCoffeeIndependent" = "Boit du café indépendant",
  "lifestyle_consCoffeeNone" = "Ne boit pas de café",
  "lifestyle_motorizedActFreq_bin" = "Fait des activités motorisées",
  "lifestyle_typeTransportCar" = "Se déplace en voiture",
  "lifestyle_typeTransportPublicTransit" = "Utilise le transport en commun",
  "lifestyle_typeTransportWalk" = "Marche comme moyen de transport",
  "lifestyle_volunteeringFreq_bin" = "Fait du bénévolat régulièrement",
  "lifestyle_hasTattoos" = "A des tatouages"
)
moyennes <- moyennes %>%
  mutate(label = recode(variable, !!!labels_humains, .default = variable))



library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Sélection des binaires
binaires <- data %>%
  select(where(~ all(na.omit(.) %in% c(0, 1)))) %>%
  select(-dv_turnout_bin)

# 2. Fusion avec la variable cible
df_bin <- bind_cols(dv_turnout_bin = data$dv_turnout_bin, binaires)

# 3. Calcul des moyennes par groupe
moyennes <- df_bin %>%
  group_by(dv_turnout_bin) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(-dv_turnout_bin, names_to = "variable", values_to = "moyenne") %>%
  pivot_wider(names_from = dv_turnout_bin, values_from = moyenne, names_prefix = "vote_")

# 4. Calcul des différences et tri des plus discriminantes
moyennes <- moyennes %>%
  mutate(
    difference = vote_1 - vote_0,
    abs_diff = abs(difference)
  ) %>%
  arrange(desc(abs_diff)) %>%
  slice(1:25)

# 5. Appliquer les noms lisibles
moyennes$label <- labels_humains[moyennes$variable]
moyennes$label[is.na(moyennes$label)] <- moyennes$variable[is.na(moyennes$label)]

# 6. Graphique final avec dégradé
ggplot(moyennes, aes(x = difference, y = reorder(label, difference), fill = difference)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  annotate("text", x = -0.21, y = 0.5, label = "Abstention", color = "black", size = 4.5, hjust = 0) +
  annotate("text", x = 0.21, y = 0.5, label = "Vote", color = "black", size = 4.5, hjust = 1) +
  scale_fill_gradient2(high = "yellow", mid = "white", low = "deeppink", midpoint = 0) +
  labs(
    title = "Proportion de gens qui votent ou s’abstiennent selon leurs caractéristiques personnelles",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

