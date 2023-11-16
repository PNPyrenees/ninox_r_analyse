source("ninox_functions.R")

NOM_FICHIER <- "donnees_exemples/ninox_measure.csv"
NOM_SITE <- "Site test"

#########################
# Utilisation de la fonction globale
process_all(NOM_FICHIER, NOM_SITE)

#########################
# Pas à pas

# Chargement et prétraitrement du fichier
all_data <- load_and_process_file(NOM_FICHIER)
# Selection des meilleurs nuits
best_night <- get_best_night(all_data)

# Rapport du lot de données
report_path <- sprintf("%s.txt", NOM_SITE)
# moyenne :
mean <- mean(all_data$sqm_mag)
# mediane :
median <- median(all_data$sqm_mag)
# Modal
sqm_mag_mod <- get_modal_sqm_mag_value(all_data)
# CALCUL DU nombre jour avec des données :
n_day <- n_distinct(all_data$ymd)

report_txt <- c(
  sprintf("SITE : %s", NOM_SITE),
  sprintf("FICHIER : %s", NOM_FICHIER),
   sprintf("MEAN : %s", round(mean,2)),
    sprintf("MEDIAN : %s",round( median,2)),
    sprintf("MODAL : %s", round(sqm_mag_mod, 2)),
    sprintf("Numbre of days with measurements : %s", n_day)
)
writeLines(report_txt, report_path)

# Génération des graphiques
generate_graph(best_night, NOM_SITE, "", sqm_mag_mod)
generate_graph(all_data, NOM_SITE, "All data", sqm_mag_mod)
