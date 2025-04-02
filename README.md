# Transfert de connaissances: modèle Datagotchi

Date: 7 janvier 2025
Description: Document de notes pour le transfert de connaissances par rapport au modèle Datagotchi
⚡ Projet.s : Datagotchi (https://www.notion.so/Datagotchi-7566c06c954a404bb4d8b2957799d83f?pvs=21), Datagotchi Canada (https://www.notion.so/Datagotchi-Canada-4bc674b7086b481c80a461f67ae6c05e?pvs=21)
Participant.e.s: Hubert Cadieux, Sarah-Jane Vincent, Nicholas Gaudet, Alexandre Bouillon, Étienne Proulx
État: En cours d'utilisation
Archivé: No

# A) Étapes générales

## 1) Type de modèle

À skipper pour cette édition: c’est un modèle multinomial

1. Avec le package `nnet`, la fonction `multinom` 
2. Équivalent de `lm` ou `glm`, mais pour une variable dépendante catégorielle (dans notre cas: 5 partis NON-ORDONNÉS) 
3. On va pas tester d’autre type de modèle

## 2) Décider des variables à inclure dans le modèle

On est limités par le nombre de variables dans l’app. On doit donc choisir un nombre `X` de variables qui seront incluses dans le modèle

## 3) Décider des types de variables à garder

ses_gender_factor VS ses_gender_numeric: on garde laquelle?

Même principe qu’à l’étape 2.

**Comment?** Faire une loop à travers `M` modèles en changeant aléatoirement les variables indépendantes du modèle ; à chaque modèle, on calcule la ***qualité du modèle*** (voire section plus bas pour des infos là-dessus) et on garde l’info. Par exemple, à chaque modèle, on enregistre une dataframe avec `J` rangées (nombre de variables indépendantes dans le modèle) et 3-4 colonnes: identifiant du modèle (un chiffre), nom de la variable indépendante (`ses_gender_factor`), et le score de qualité du modèle. En répétant ça `M` fois, on s’assure d’avoir chaque variable indépendante au moins 25-30 fois et c’est facile après de voir quelles variables ont été incluses dans des meilleurs modèles.

## 4) Décider des interactions à inclure dans le modèle

Partie pas mal théorique

Pour USA 2024, j’ai fait le sandbox ([https://clessn.shinyapps.io/sandbox_datagotchi/](https://clessn.shinyapps.io/sandbox_datagotchi/)) qui nous permettait de tester différents modèles et de voir si le modèle faisait du sens selon notre intuition.

Ça pourrait être une bonne idée d’adapter le code pour en faire un pour les fédérales rendu à cette étape.

J’avais fait du code pour tester plusieurs interactions automatiquement, mais ça avait pas donné grand chose

ses_gender * ses_income

ses_income * lifestyle_consCoffee

lifestyle_fishing_freq * lifestyle_hunting_freq

ses_age * ses_education

income * lifestyle_eatMeatFreq

ses_education * lifestyle_eatMeatFreq

ses_education * lifestyle_hunting_freq

lifestyle_consClothes * lifestyle_favAlcool

lifestyle_pets * lifestyle_eat_meat_freq

lifestyle_typeTransport * lifestyle_motorized_freq

lifestyle_yoga_freq * lifestyle_consCoffee

lifestyle_consClothes * lifestyle_volunteeringsocial_freq

ses_income * lifestyle_videogame_freq

ses_gender * lifestyle_eatMeatFreq

age * museums

education * museums

income * museums

exercise * lifestyle_smokeFreq

age * lifestyle_typeTransport

income * lifestyle_consClothes

income * lifestyle_favAlcool

educ * lifestyle_favAlcool

age * tattoo

age * volunteering

age * lifestyle_consCoffee

---

---

## 5) Intégrer le modèle dans l’app

[https://docs.google.com/spreadsheets/d/1iEmkmlymw6HeoVw5iGVIil-scA14ds7KOQjG7jAYuLo/edit?gid=854931957#gid=854931957](https://docs.google.com/spreadsheets/d/1iEmkmlymw6HeoVw5iGVIil-scA14ds7KOQjG7jAYuLo/edit?gid=854931957#gid=854931957)

Il faut transformer le modèle en coefficients, accorder les coefficients avec le mapping (dans le Google Sheets) et mettre les coefficients dans la feuille à cet effet

# B) Concept important : Tester sur de nouvelles données

Quand on veut savoir quel modèle performe le mieux, il est essentiel de le tester sur des **nouvelles données** (qu’il n’a jamais vues). Pourquoi ?

- Pour s'assurer que le modèle **généralise bien** et qu’il n’est pas juste bon pour les données sur lesquelles il a été entraîné (**overfitting**).

### Comment faire ?

1. **Diviser les données** :
    - **Training set** : pour entraîner le modèle.
    - **Test set** : pour évaluer la performance sur des données inédites.
2. **Validation croisée**: Répéter le processus sur plusieurs sous-ensembles pour garantir une évaluation plus robuste.
    1. Donc, dans vos loops, voici les étapes
        1. Séparer le jeu de données en training set (80% du jeu de données) et testing set (20% du jeu de données)
        2. Séparer le testing set en 3 à 5 sous-ensembles.
        3. Mesurer la performance du modèle à travers les 3 à 5 sous-ensemble

**Le but :** Choisir le modèle qui performe bien **sur des données non vues**, pas juste sur les données d'entraînement.

# C) Autre concept important: évaluer la qualité d’un modèle multinomial

Contrairement aux USA, notre variable dépendante est **catégorielle non-ordonnée**. On doit donc avoir une mesure . Je ne crois pas qu’il est suffisant de simplement regarder si le modèle s’est trompé (1) ou non (0). Il y a une différence si le modèle prédit un NPD au PLC que s’il prédit un NPD au PCC. Il faudra prendre en compte quand vous établissez un indice de “qualité” du modèle. Contactez Nadjim qui a fait les derniers modèles au fédéral et au provincial. Il n’y a pas UNE réponse à cette question, ça varie selon nos besoins (communiquez avec Yannick)