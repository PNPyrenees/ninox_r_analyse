# Script R analyse capteurs NINOX
Script R d'analyse des capteurs NINOX permettant de mesurer la brillance du fond de ciel nocturne

# Résultats
Script offrant une première analyse des données issues de capteurs ninox.

A partir d'un fichier de données, le script génère un rapport succint des données, et produit des graphiques.

```
SITE : Site test
FICHIER : donnees_exemples/ninox_measure.csv
MEAN : 19.6627550706753
MEDIAN : 20.48
MODAL : 69
Nb de jours avec mesure : 69
```

Il offre une méthode d'extraction des "meilleurs nuits"

<p float="left">
<img src="docs/img/Site_test_meilleurs_nuits_densite2.jpg" width=200 alt="Graphique densité des meilleurs nuits">
<img src="docs/img/Site_test_meilleurs_nuits_densite.jpg" width=200 alt="Graphique densité des meilleurs nuits">
<img src="docs/img/Site_test_meilleurs_nuits_magnitude.jpg" width=200 alt="Graphique magnitude des meilleurs nuits">
</p>
<p float="left">
<img src="docs/img/Site_test_toutes_les_nuits_densite2.jpg" width=200 alt="Graphique densité de toutes les nuits">
<img src="docs/img/Site_test_toutes_les_nuits_densite.jpg" width=200 alt="Graphique densité de toutes les nuits">
<img src="docs/img/Site_test_toutes_les_nuits_magnitude.jpg" width=200 alt="Graphique magnitude de toutes les nuits">
</p>

# Extraction des "meilleurs nuits"

L'extraction automatique des "meilleurs nuits" est réalisée avec la procédure suivante :

 * Extraction des données durant les périodes de nuits noires. Pour extraire ces données nous nous basons sur la valeur de l'altitude du soleil au moment de la mesure qui doit être inférieure à -230
 * Récupération des jours où les valeurs de sqm_mag sont comprises entre 21 et 22.2 et où l'écart entre les deux valeurs ne dépasse pas 1
 * Extraction de l'ensemble des valeurs dans le fichier d'origine des jours sélectionnés précédemment

Les valeurs ont été choisies de façon arbitraire et sont définies sous forme de constante que l'on peut modifier lors des appels aux fonctions.

# Utilisation du script
## Prerequis
Installer les packages R suivants :

```R
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
```


## Utilisation du script

Toutes les fonctions sont contenues dans le fichier `ninox_functions.R`. Il y a une une fonction principale qui peut être utilisée, ou il est possible d'appeler individuellement les fonctions.

```R
source("ninox_functions.R")

NOM_FICHIER <- "donnees_exemples/ninox_measure.csv"
NOM_SITE <- "Site test"

#########################
# Utilisation de la fonction globale
process_all(NOM_FICHIER, NOM_SITE)
```

```R
source("ninox_functions.R")

NOM_FICHIER <- "donnees_exemples/ninox_measure.csv"
NOM_SITE <- "Site test"

# Chargement et prétraitrement du fichier
all_data <- load_and_process_file(NOM_FICHIER)
# Selection des meilleurs nuits
best_night <- get_best_night(all_data)
# Modal
sqm_mag_mod <- get_modal_sqm_mag_value(all_data)
# Génération des graphiques
generate_graph(best_night, NOM_SITE, "meilleurs nuits", sqm_mag_mod)
generate_graph(all_data, NOM_SITE, "toutes les nuits", sqm_mag_mod)
```

# Constantes
Pour extraire les "meilleurs nuits" un ensemble de valeurs par défaut ont été défini.

```R
# Altitude du soleil au moment de la mesure *10 en degré pour filtrer les données
SUN_ALT_MIN <- -250
# Valeur mininale de sqm_mag utilisée pour filtrer les données
MIN_SQM_MAG_VAL <- 21
# Valeur maximale de sqm_mag utilisée pour filtrer les données
MAX_SQM_MAG_VAL <- 22.2
# Différence maximale entre sqm_mag min et max par nuit utilisée pour filtrer les données
DIFF_SQM_MAG <- 1
```

Ces valeurs peuvent être changer lors de l'appel aux fonctions
```R
# Fonction globale
process_all(file_name, nom_site, sun_alt_min = SUN_ALT_MIN, diff_sqm_mag = DIFF_SQM_MAG, min_sqm_mag_val = MIN_SQM_MAG_VAL, max_sqm_mag_val = MAX_SQM_MAG_VAL)

# Fonction de calcul des "meilleurs nuits"
get_best_night(data_in, sun_alt_min = SUN_ALT_MIN, diff_sqm_mag = DIFF_SQM_MAG, min_sqm_mag_val = MIN_SQM_MAG_VAL, max_sqm_mag_val = MAX_SQM_MAG_VAL)
```