# How to get started with travel time data from Verkehrsbetriebe Zürich (VBZ) available on the open data platform of the City of Zurich. 
#     This guide is based on this example dataset for 2020: https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd_2020/resource/743a5c63-e636-4cd2-9ff3-05f668adb209.
#     There you can also find further explanations (only in German).
#     Update: These datasets are no longer maintained on Open Data Zurich. Travel times of VBZ in the planned vs. actual comparison after 29.07.2023, can be found in the open data 
#     catalog on opentransportdata.swiss at https://opentransportdata.swiss/en/dataset/istdaten.
# Author: Verkehrsbetriebe Zürich (C. Baur)


# Load packages
library(dplyr)
library(tidyr)
library(readr)


# Define where the ogd-data is to find on your disk
path_to_data <- ("./data/")


# Load the data
# You will find the datasets e.g. here:
# https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd_2020

# Matching tables
df_stop_point <- read_csv(paste0(path_to_data,"haltepunkt.csv"))
df_stop <- read_csv(paste0(path_to_data,"haltestelle.csv"))


# Load table "fahrzeiten": the travel time data is stored there
df_travel_time <- read_csv(paste0(path_to_data,"fahrzeiten_soll_ist_20200809_20200815.csv"))



# Matching
# In order to get the full stops information, you need to match df_travel_time with df_stop_point and df_stop

traveltime_stpoint_st <- df_travel_time%>%
  # match df_travel_time with the stop points from df_stop_point according to the departure stop point
  # ("from"; german: "von")
  left_join(df_stop_point,
            by=c("halt_punkt_id_von"="halt_punkt_id",
                 "halt_punkt_diva_von"="halt_punkt_diva",
                 "halt_id_von"="halt_id"))%>%
  #rename new columns by adding suffix "von"
  rename(GPS_Latitude_von=GPS_Latitude,
         GPS_Longitude_von=GPS_Longitude,
         GPS_Bearing_von=GPS_Bearing,
         halt_punkt_ist_aktiv_von=halt_punkt_ist_aktiv)%>%
  #join df_stop_point to "to" ("nach")
  left_join(df_stop_point,
            by=c("halt_punkt_id_nach"="halt_punkt_id",
                 "halt_punkt_diva_nach"="halt_punkt_diva",
                 "halt_id_nach"="halt_id"))%>%
  #rename new columns by adding suffix "nach"
  rename(GPS_Latitude_nach=GPS_Latitude,
         GPS_Longitude_nach=GPS_Longitude,
         GPS_Bearing_nach=GPS_Bearing,
         halt_punkt_ist_aktiv_nach=halt_punkt_ist_aktiv)%>%
  #join df_stop to "from" ("von")
  left_join(df_stop,
            by=c("halt_id_von"="halt_id",
                 "halt_diva_von"="halt_diva",
                 "halt_kurz_von1"="halt_kurz"))%>%
  #rename new columns by adding suffix "von"
  rename(halt_lang_von=halt_lang,
         halt_ist_aktiv_von=halt_ist_aktiv)%>%
  #join df_stop to "to" ("nach")
  left_join(df_stop,
            by=c("halt_id_nach"="halt_id",
                 "halt_diva_nach"="halt_diva",
                 "halt_kurz_nach1"="halt_kurz"))%>%
  #rename new columns by adding suffix "nach"
  rename(halt_lang_nach=halt_lang,
         halt_ist_aktiv_nach=halt_ist_aktiv)
  

# Calculate the punctuality per line
#     According to the punctuality definition of VBZ, a ride is considered on time (punctual) when the actual arrival time at the stop
#     does not exceed the scheduled arrival time by more than 2 minutes (otherwise defined as "delayed") or the actual
#     departure at a stop does not happen more than 1 minute earlier than the scheduled departure (otherwise defined as
#     "too early")


punctuality <- traveltime_stpoint_st%>%
  # first calculate the difference between actual ("ist") and scheduled ("soll") arrival ("an") / departure ("ab")
  mutate(diff_arr=(ist_an_nach1-soll_an_nach),
         diff_dep=(ist_ab_nach-soll_ab_nach))%>%
  # and then assign the punctuality categories accordingly
  mutate(punct_cat = case_when(diff_arr>=120 ~ "delayed",
                         diff_dep<= -60 ~ "to early",
                       TRUE~ "punctual"))%>%
  # count the occurrences per line of each category
  group_by(linie, punct_cat)%>%
  summarise(count=n())%>%
  # calculate the proportions
  mutate(percent_punct=100*(count/sum(count)))


# transform to wide format
punctuality_transf <- punctuality%>%
  select(linie,
         punct_cat,
         percent_punct)%>%
  spread(key=punct_cat,
         value=percent_punct)
