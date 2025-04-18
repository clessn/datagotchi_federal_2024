Tentative de reproduction de : Kilibarda, Anja, Clifton Linden, et Yannick Dufresne. 2020. « Do Campaign Events Matter? New Evidence from Voting Advice Applications ». Political Science Quarterly 135 (2): 259‑80. doi:10.1002/polq.13034.

Sur le cas du débat des chefs.

En résumé, voici les étapes appliquées ici:

1. Raking quotidien: pondération par jour basée sur les marges de la population par province
2. Moyenne pondérée quotidienne de l'IRC (transformé en probabilités?), une observation par jour.
3. Régression interventionnelle Box-Tiao sur cette time serie.


Si je veux intégrer le bayésien:

Option A:

- On garde les étapes 1 et 2 pareilles.
- Le modèle de l'étape 3 est un modèle bayésien qui utilise les données du pilote comme priors

Option B:

- Au lieu de faire une time series avec une observation par jour, on fait un modèle sur les réponses individuelles en incluant l'effet du jour comme intercepts aléatoires avec un prior sur les jours avant le débat.