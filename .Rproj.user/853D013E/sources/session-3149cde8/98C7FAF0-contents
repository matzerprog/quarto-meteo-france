library(data.table)

meteo <- fread("synop.csv.gz")
emdat <- fread("catastrophes.csv")


meteo[, temperature_C := t - 273.15]
meteo_monthly <- meteo[, .(avg_temp_C = mean(temperature_C)), by = .(year, month)]

monthly_avg <- meteo_monthly[, .(overall_avg = mean(avg_temp_C)), by = month]

# On suppose que ta data frame s'appelle meteo
unique_year_month <- unique(meteo[, .(year, month)])
# Vérifie le résultat
head(unique_year_month)

# Calcul de la moyenne mensuelle par année
monthly_avg <- meteo[, .(mean_celsius = mean(temperature_C, na.rm = TRUE)), by = .(year, month)]
# Vérifie le résultat
head(monthly_avg)


# Calcul de la moyenne par mois (toutes années confondues)
month_global_avg <- meteo[, .(global_month_avg = mean(temperature_C, na.rm = TRUE)), by = month]
# Vérifie le résultat
month_global_avg

# Fusionne la moyenne mensuelle avec la moyenne globale du mois
final_df <- merge(monthly_avg, month_global_avg, by = "month")
# Réorganise les colonnes pour avoir year, month, mean_celsius, global_month_avg
final_df <- final_df[, .(year, month, mean_celsius, global_month_avg)]
final_df <- final_df[order(year, month)]




### etape 2 ###



# Filtrage des années et mois valides
emdat_filtered <- emdat[year >= 1975 & (year < 2022 | (year == 2022 & start_month <= 11))]
emdat_fr <- emdat_filtered[country == "France"]

# Colonnes des coûts finissant par "_thousand_usd"
usd_cols <- grep("_thousand_usd$", names(emdat_fr), value = TRUE)
for (col in usd_cols) {
  set(emdat_fr, which(is.na(emdat_fr[[col]])), col, 0)
}
# NA en décès remplacés par 0
emdat_fr[is.na(total_deaths), total_deaths := 0]

emdat_monthly <- emdat_fr[, .(
  total_disasters = .N,
  total_damages = rowSums(.SD),
  total_deaths = sum(total_deaths)
), by = .(year, start_month), .SDcols = usd_cols]

setnames(emdat_monthly, "start_month", "month")

final_df <- merge(final_df, emdat_monthly, by = c("year", "month"), all.x = TRUE)

# Remplacer NA par 0 dans les colonnes fusionnées
final_df[is.na(total_disasters), total_disasters := 0]
final_df[is.na(total_damages), total_damages := 0]
final_df[is.na(total_deaths), total_deaths := 0]




final_df[, year := as.integer(year)]
final_df[, month := as.integer(month)]
emdat_monthly[, year := as.integer(year)]
emdat_monthly[, month := as.integer(month)]


# Supprimer les doublons et garder la première occurrence
final_df_unique <- final_df[!duplicated(final_df[, .(year, month)])]




fwrite(final_df_unique, file = "C:\\Users\\Morgane\\Documents\\meteo france\\final_dataframe.csv")




