# How to get started with passenger data by Verkehrsbetriebe
#     Zürich (VBZ) from the open data platform of the City of Zurich
#     https://data.stadt-zuerich.ch/dataset/vbz_fahrgastzahlen_ogd
#     There you can also find further explanations (only in German)
# Author: Verkehrsbetriebe Zürich (C. Baur)

# Load packages
library(dplyr)

# Define where the ogd-data is to find on your disk
path_to_data <- ("./data/")

# Load the data 
# You will find the datasets at
# https://data.stadt-zuerich.ch/dataset/vbz_fahrgastzahlen_ogd

# Load matching tables
df_stops <- read.csv(paste0(path_to_data,"haltestellen.csv"),
                     encoding="UTF-8",
                     sep=";")


df_day_type <- read.csv(paste0(path_to_data,"tagtyp.csv"),
                        encoding="UTF-8",
                        sep=";")

df_lines <- read.csv(paste0(path_to_data,"linie.csv"),
                       encoding="UTF-8",
                       sep=";")

df_vehicle_capacities <- read.csv(paste0(path_to_data,"gefaessgroesse.csv"),
                                  encoding="UTF-8",
                                  sep=";")

# Load table "REISENDE": the passenger data is stored there
df_count <- read.csv(paste0(path_to_data,"reisende.csv"),
                     encoding="UTF-8",
                     sep=";")

# Matching
# In order to get the complete main passengers table, you need to match df_count with df_stops, df_day_type and df_lines

REISENDE_full <- df_count%>%
  # drop the column "Linienname" from df_count
  select(-Linienname)%>%
  #join df_stops
  left_join(df_stops,
            by=c("Haltestellen_Id"="Haltestellen_Id"))%>%
  #join df_day_type
  left_join(df_day_type,
            by=c("Tagtyp_Id"="Tagtyp_Id"))%>%
  #join df_lines
  left_join(df_lines,
            by=c("Linien_Id"="Linien_Id"))%>%
  #join df_vehicle_capacities
  left_join(df_vehicle_capacities,
            by=c("Plan_Fahrt_Id"="Plan_Fahrt_Id"))


# Example Analyses

# Passengers per line
# hint: "Linien_Id" (= lines ids) is only unique within one year.
# Use "Linienname" (= lines names) to compare lines between years

# passengers per line per year
# group by Linien_Id etc., multiply Einsteiger (= passengers getting into the vehicle) with Tage_DTV
# (= extrapolation factor for the whole year) and calculate the sum
pax_line_year <- REISENDE_full%>%
  group_by(Linien_Id, Linienname, Linienname_Fahrgastauskunft)%>%
  summarise(pax_per_year=sum(Einsteiger*Tage_DTV, na.rm = TRUE))


# passengers per
# - average Monday-Friday work day (DWV)
# - average Monday-Sunday day (DTV)
# - average Saturday (Sa)
# - average Sunday (So)
# - average Saturday night (Sa_N)
# - average Sunday night (So_N)
pax_line_year_day_type <- REISENDE_full%>%
  group_by(Linien_Id, Linienname, Linienname_Fahrgastauskunft)%>%
  summarise(pax_per_DTV=sum((Einsteiger*Tage_DTV)/365, na.rm = TRUE),
    pax_per_DWV=sum((Einsteiger*Tage_DWV)/251, na.rm = TRUE),
    pax_per_Sa=sum((Einsteiger*Tage_SA)/52, na.rm = TRUE),
    pax_per_So=sum((Einsteiger*Tage_SO)/62, na.rm = TRUE),
    pax_per_Sa_N=sum((Einsteiger*Tage_SA_N)/52, na.rm = TRUE),
    pax_per_So_N=sum((Einsteiger*Tage_SO_N)/52, na.rm = TRUE))

# Passengers per stop
# hint: "Haltestellen_Id" (= stops ids) is only unique within one year.
# Use "Haltestellennummer" (= stops numbers) to compare stops between years.
# Haltestellennummer" is more stable and unique within all published datasets.

# passengers per stop per year
# group by Haltestellen_Id etc., multiply Einsteiger (= passengers getting into the vehicle) with Tage_DTV
# (= extrapolation factor for the whole year) and calculate the sum
pax_stop_year <- REISENDE_full%>%
  group_by(Haltestellen_Id, Haltestellennummer, Haltestellenlangname)%>%
  summarise(pax_per_year=sum(Einsteiger*Tage_DTV, na.rm = TRUE))

# passengers per:
# - average Monday-Friday work day (DWV)
# - average Monday-Sunday day (DTV)
# - average Saturday (Sa)
# - average Sunday (So)
# - average Saturday night (Sa_N)
# - average Sunday night (So_N)
pax_stops_year_day_type <- REISENDE_full%>%
  group_by(Haltestellen_Id, Haltestellennummer, Haltestellenlangname)%>%
  summarise(pax_per_DTV=sum((Einsteiger*Tage_DTV)/365, na.rm = TRUE),
    pax_per_DWV=sum((Einsteiger*Tage_DWV)/251, na.rm = TRUE),
    pax_per_Sa=sum((Einsteiger*Tage_SA)/52, na.rm = TRUE),
    pax_per_So=sum((Einsteiger*Tage_SO)/62, na.rm = TRUE),
    pax_per_Sa_N=sum((Einsteiger*Tage_SA_N)/52, na.rm = TRUE),
    pax_per_So_N=sum((Einsteiger*Tage_SO_N)/52, na.rm = TRUE))

