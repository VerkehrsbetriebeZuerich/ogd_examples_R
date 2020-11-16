
rm(list=ls())
gc()

##### Description: How to get started with travel time data (Verkehrsbetriebe
#     Zürich VBZ) from the open data platform of the city of Zurich
#     https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd
#     There you can also find further descriptions (only in German at the moment)


##### Author: Verkehrsbetriebe Zürich (C.Baur)

##### Date: 04.09.2020


#________________________________________________________________
#####                   Setup                               #####
#________________________________________________________________

# Libraries
library(dplyr)
library(tidyr)
library(readr)


# Input-path (where the ogd-data is to find on your disk)
input_path <- ("01_Data/01_Input/")


#_________________________________________________________________
#####                 Main                                   #####
#_________________________________________________________________


#### >Load the data ####
# You will find the data sets at 
# https://data.stadt-zuerich.ch/dataset/vbz_fahrzeiten_ogd

#Matchingtables
haltepunkt <- read_csv(paste0(input_path,"haltepunkt.csv"))

haltestelle <- read_csv(paste0(input_path,"haltestelle.csv"))


#Main table

#how many "fahrzeiten*" files exist in your folder?
#be carefull, one "fahrzeiten"-dataset is about 250 MB
#make sure your working-directory is set up correct so R can find your files
file_list <- list.files(input_path,
                        pattern = "fahrzeiten")

for (file in file_list){
  # if the merged dataset does exist, append to it
  if (exists("fahrzeiten")){
    temp_dataset <- read_csv(paste0(input_path,file))
    fahrzeiten<-rbind(fahrzeiten, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("fahrzeiten")){
    fahrzeiten <- read_csv(paste0(input_path,file))
  }
  
}


#### >Some examples ####
#### >> Matching ####
fahrzeiten_full <- fahrzeiten%>%
  #join haltepunkt to "from" ("von")
  left_join(haltepunkt,
            by=c("halt_punkt_id_von"="halt_punkt_id",
                 "halt_punkt_diva_von"="halt_punkt_diva",
                 "halt_id_von"="halt_id"))%>%
  #rename new columns it with suffix "von"
  rename(GPS_Latitude_von=GPS_Latitude,
         GPS_Longitude_von=GPS_Longitude,
         GPS_Bearing_von=GPS_Bearing,
         halt_punkt_ist_aktiv_von=halt_punkt_ist_aktiv)%>%
  #join haltepunkt to "to" ("nach")
  left_join(haltepunkt,
            by=c("halt_punkt_id_nach"="halt_punkt_id",
                 "halt_punkt_diva_nach"="halt_punkt_diva",
                 "halt_id_nach"="halt_id"))%>%
  #rename new columns it with suffix "nach"
  rename(GPS_Latitude_nach=GPS_Latitude,
         GPS_Longitude_nach=GPS_Longitude,
         GPS_Bearing_nach=GPS_Bearing,
         halt_punkt_ist_aktiv_nach=halt_punkt_ist_aktiv)%>%
  #join haltestelle to "from" ("von")
  left_join(haltestelle,
            by=c("halt_id_von"="halt_id",
                 "halt_diva_von"="halt_diva",
                 "halt_kurz_von1"="halt_kurz"))%>%
  #rename new columns it with suffix "von"
  rename(halt_lang_von=halt_lang,
         halt_ist_aktiv_von=halt_ist_aktiv)%>%
  #join haltestelle to "to" ("nach")
  left_join(haltestelle,
            by=c("halt_id_nach"="halt_id",
                 "halt_diva_nach"="halt_diva",
                 "halt_kurz_nach1"="halt_kurz"))%>%
  #rename new columns it with suffix "nach"
  rename(halt_lang_nach=halt_lang,
         halt_ist_aktiv_nach=halt_ist_aktiv)
  

#### >> Punctuality per line ####
# whats punctuality? 
# VBZ define an belated arrival lower than 2min as "on time"
# also the departure must not be more than 1min before the scheduled time 
# < -60s "not on time/too early"
# <= -60s ... <= 120s "on time"
# > 120s "not on time/delayed"

punctuality <- fahrzeiten_full%>%
  #Calculate gap between scheduled and acuatal arrival/departuretimes
  # at "nach" stop
  mutate(gap_arr=(ist_an_nach1-soll_an_nach),
         gap_dep=(ist_ab_nach-soll_ab_nach))%>%
  #Calculate punctual category
  mutate(punct_cat = case_when(gap_arr>=120 ~ "delayed",
                         gap_dep<= -60 ~ "to early",
                       TRUE~ "punctual"))%>%
  #group per line and punctual category
  group_by(linie, punct_cat)%>%
  #sum it up
  summarise(count=n())%>%
  #and create percentage value
  mutate(percentage=100*(count/sum(count)))


#may you want to spread the punct_cat?
punctuality_spread <- punctuality%>%
  #select relevant columns
  select(linie,
         punct_cat,
         percentage)%>%
  #use spread
  spread(key=punct_cat,
         value=percentage)
