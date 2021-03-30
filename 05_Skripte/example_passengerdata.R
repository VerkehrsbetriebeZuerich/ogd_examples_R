
rm(list=ls())
gc()

##### Description: How to get started with passenger data (Verkehrsbetriebe
#     Zürich VBZ) from the open data platform of the city of Zurich
#     https://data.stadt-zuerich.ch/dataset/vbz_fahrgastzahlen_ogd 
#     There you can also find further descriptions (only in German)


##### Author: Verkehrsbetriebe Zürich (C.Baur)

##### Date: 16.11.2020


#________________________________________________________________
#####                   Setup                               #####
#________________________________________________________________

# Libraries
library(dplyr)


# Input-path (where the ogd-data is to find on your disk)
input_path <- ("01_Data/01_Input/")


#_________________________________________________________________
#####                 Main                                   #####
#_________________________________________________________________


#### >Load the data ####
# You will find the data sets at 
# https://data.stadt-zuerich.ch/dataset/vbz_fahrgastzahlen_ogd 

#Matching tables
HALTESTELLEN <- read.csv(paste0(input_path,"HALTESTELLEN.csv"),
                         encoding="UTF-8",
                         sep=";")

TAGTYP <- read.csv(paste0(input_path,"TAGTYP.csv"),
                         encoding="UTF-8",
                         sep=";")

LINIE <- read.csv(paste0(input_path,"LINIE.csv"),
                         encoding="UTF-8",
                         sep=";")

GEFAESSGROESSE <- read.csv(paste0(input_path,"GEFAESSGROESSE.csv"),
                           encoding="UTF-8",
                           sep=";")

#Main table
REISENDE <- read.csv(paste0(input_path,"REISENDE.csv"),
                         encoding="UTF-8",
                         sep=";")


#### >Some examples ####
#### >> Matching ####
REISENDE_full <- REISENDE%>%
  #remove column Linienname from REISENDE
  select(-Linienname)%>%
  #join HALTESTELLEN
  left_join(HALTESTELLEN,
            by=c("Haltestellen_Id"="Haltestellen_Id"))%>%
  #join TAGTYP
  left_join(TAGTYP,
            by=c("Tagtyp_Id"="Tagtyp_Id"))%>%
  #join LINIE
  left_join(LINIE,
            by=c("Linien_Id"="Linien_Id"))%>%
  #join GEFAESSGROESSE
  left_join(GEFAESSGROESSE,
            by=c("Plan_Fahrt_Id"="Plan_Fahrt_Id"))

#### >> Passenger per line ####
# hint: "Linien_Id" is only unique within one year. Use "Linienname" to compare lines
# within several years

# whole year Einsteiger (=passengers getting into the vehicle)
# Tage_DTV = extrapolation factor for the whole year 
df1 <- REISENDE_full%>%
  group_by(Linien_Id, Linienname, Linienname_Fahrgastauskunft)%>%
  summarise(pax_per_year=sum(Einsteiger*Tage_DTV, na.rm = TRUE))

# per typical workday (DWV) / typical day (DTV) / typical saturday (Sa) and so on
df2 <- REISENDE_full%>%
  group_by(Linien_Id, Linienname, Linienname_Fahrgastauskunft)%>%
  summarise(#avg. daily traffic Mo-So (DTV)
            pax_per_DTV=sum((Einsteiger*Tage_DTV)/365, na.rm = TRUE),
            #avg. traffic per workday Mo-Fr (DWV)
            pax_per_DWV=sum((Einsteiger*Tage_DWV)/251, na.rm = TRUE),
            #avg. traffic per Saturday (Sa)
            pax_per_Sa=sum((Einsteiger*Tage_SA)/52, na.rm = TRUE),
            #avg. traffic per Sunday (So)
            pax_per_So=sum((Einsteiger*Tage_SO)/62, na.rm = TRUE),
            #avg. traffic per Night Friday to Saturday (Sa_N)
            pax_per_Sa_N=sum((Einsteiger*Tage_SA_N)/52, na.rm = TRUE),
            #avg. traffic per Night Saturday to Sunday (So_N)
            pax_per_So_N=sum((Einsteiger*Tage_SO_N)/52, na.rm = TRUE))


#### >> Passenger per stop ####
#hint: "Haltestellen_Id" is only unique within one year. Use "Haltestellennummer" 
# to compare lines within several years.
# "Haltestellennummer" is more stable and unique within all published datasets


# whole year Einsteiger (=passengers getting into the vehicle)
# Tage_DTV = extrapolation factor for the whole year
df3 <- REISENDE_full%>%
  group_by(Haltestellen_Id, Haltestellennummer, Haltestellenlangname)%>%
  summarise(pax_per_year=sum(Einsteiger*Tage_DTV, na.rm = TRUE))

# Einsteiger (=passengers getting into the vehicle) per average work day Monday-Friday (DWV) / average day Monday-Sunday (DTV) / typical Saturday (Sa) and so on
df4 <- REISENDE_full%>%
  group_by(Haltestellen_Id, Haltestellennummer, Haltestellenlangname)%>%
  summarise(#avg. daily traffic Mo-So (DTV)
    pax_per_DTV=sum((Einsteiger*Tage_DTV)/365, na.rm = TRUE),
    #avg. traffic per workday Mo-Fr (DWV)
    pax_per_DWV=sum((Einsteiger*Tage_DWV)/251, na.rm = TRUE),
    #avg. traffic per Saturday (Sa)
    pax_per_Sa=sum((Einsteiger*Tage_SA)/52, na.rm = TRUE),
    #avg. traffic per Sunday (So)
    pax_per_So=sum((Einsteiger*Tage_SO)/62, na.rm = TRUE),
    #avg. traffic per Night Friday to Saturday (Sa_N)
    pax_per_Sa_N=sum((Einsteiger*Tage_SA_N)/52, na.rm = TRUE),
    #avg. traffic per Night Saturday to Sunday (So_N)
    pax_per_So_N=sum((Einsteiger*Tage_SO_N)/52, na.rm = TRUE))
