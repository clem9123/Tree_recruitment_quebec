##################################################################
# This script is used to import the data from the PEP database.
# It loads all the libraries.
# Select the data for the studied zone Basalm fir-yellow birch
##################################################################

#LIBRAIRY
library(sf)
library(mapview)
library(tidyverse)
library(lme4)
library(lubridate)
library(glmmTMB)
library(gganimate)

# ENCODING FUNCTION
# Use UTF8 to have the accent
encoding <- function(df) {
    for (col in colnames(df)) {
      if (is.character(df[[col]])) {
        Encoding(df[[col]]) <- "UTF-8"
      }
    }
    return(df)
}

# IMPORT DATA
zone <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "classi_eco_pe") %>%
    encoding()
gaule <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "dendro_gaules") %>%
    encoding()
tree <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "dendro_arbres") %>%
    encoding()
semis <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "station_semis") %>%
    encoding()
perturbation <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond") %>%
    encoding()
all_placette <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "placette") %>%
    encoding()

placette_mes <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "placette_mes") %>%
    encoding()
station_pe <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "station_pe") %>%
    encoding()
station_sol <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "station_sol") %>%
    encoding()

placette_mes <- merge(placette_mes, station_pe)
placette_mes <- merge(placette_mes, station_sol, all.x = TRUE)
placette_mes <- merge(placette_mes, all_placette)
placette_mes <- placette_mes %>% st_as_sf(sf_column_name = "geom")
placette_mes <- placette_mes %>% mutate(year = year(date_sond))

# SELECT THE ZONE
gaule <- gaule %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)
tree <- tree %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)
semis <- semis %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)
perturbation <- perturbation %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)
placette_mes <- placette_mes %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)
placette <- all_placette %>%
    filter(id_pe %in% zone[which(zone$dom_bio == 4),]$id_pe)


# CREATE GAULE DATA

# tableau avec les décompte de gaule même null
all_gaule <- merge(
    gaule %>% 
     select(-st_ha, -tige_ha) %>%
     pivot_wider(names_from = essence, values_from = nb_tige, values_fill = 0),
    placette_mes %>%
     select(id_pe, id_pe_mes, no_mes, year, latitude, longitude),
    all = TRUE)
all_gaule[is.na(all_gaule)] <- 0

# remettre les espèce en une colonne, les classes de taille en 4 colonnes
all_gaule <- all_gaule %>%
    pivot_longer(cols = -c(id_pe, id_pe_mes, no_mes, year, latitude, longitude,
        cl_dhp, geom),
        names_to = "essence", values_to = "nb_tige") %>%
    pivot_wider(names_from="cl_dhp", values_from="nb_tige", values_fill=0) %>%
    select(-`0`) %>%
    rename(cl2 = `002`, cl4 = `004`, cl6 = `006`, cl8 = `008`) %>%
    mutate(all_cl = cl2 + cl4 + cl6 + cl8)

# CREATE PERTURBATION DATA

# Placette et année avant mesure
an_mesure <- placette_mes %>% select(id_pe, no_mes, year) %>% data.frame() %>%
    pivot_wider(names_from = no_mes, values_from = year) %>% data_frame ()
colnames(an_mesure) <- c("id_pe","geom", "m1", "m2", "m3", "m4", "m5", "m6", "m7")

# create columns m12, m23, m34, m45, m56, m67 with NA
an_mesure <- an_mesure %>%
    mutate(m12 = NA, m23 = NA, m34 = NA, m45 = NA, m56 = NA, m67 = NA)

# recreat the mutate above with a loop
for(i in 1:nrow(an_mesure)){
    if(!is.na(an_mesure[i,]$m1) & !is.na(an_mesure[i,]$m2)){
        an_mesure[i,"m12"] <- toString(list(an_mesure[i,]$m1:an_mesure[i,]$m2)[[1]])
        #print("yes")
    }
    if(!is.na(an_mesure[i,]$m2) & !is.na(an_mesure[i,]$m3)){
        an_mesure[i,"m23"] <- toString(list(an_mesure[i,]$m2:an_mesure[i,]$m3)[[1]])
        #print("yes")
    }
    if(!is.na(an_mesure[i,]$m3) & !is.na(an_mesure[i,]$m4)){
        an_mesure[i,"m34"] <- toString(list(an_mesure[i,]$m3:an_mesure[i,]$m4)[[1]])
        #print("yes")
    }
    if(!is.na(an_mesure[i,]$m4) & !is.na(an_mesure[i,]$m5)){
        an_mesure[i,"m45"] <- toString(list(an_mesure[i,]$m4:an_mesure[i,]$m5)[[1]])
        #print("yes")
    }
    if(!is.na(an_mesure[i,]$m5) & !is.na(an_mesure[i,]$m6)){
        an_mesure[i,"m56"] <- toString(list(an_mesure[i,]$m5:an_mesure[i,]$m6)[[1]])
        #print("yes")
    }
    if(!is.na(an_mesure[i,]$m6) & !is.na(an_mesure[i,]$m7)){
        an_mesure[i,"m67"] <- toString(list(an_mesure[i,]$m6:an_mesure[i,]$m7)[[1]])
        #print("yes")
    }
}

an_mesure <- an_mesure %>% select (id_pe,"m12","m23","m34","m45","m56","m67")
colnames(an_mesure) = c("id_pe", "2", "3", "4", "5", "6", "7")
an_mesure <- an_mesure %>%
    pivot_longer(cols = c("2", "3", "4", "5", "6", "7"),
    names_to = "no_mes", values_to = "year") %>%
    mutate(year = strsplit(year,", ")) %>%
    unnest(cols = year) %>%
    mutate(year = as.numeric(year)) %>%
    data.frame()

# il n'y a que des perturbations dont on connait la date

p_partielle <- perturbation %>% select(id_pe, perturb, an_perturb) %>%
    na.omit() %>%
    unique() %>%
    rename(year = an_perturb)
p_totale <- perturbation %>% select(id_pe, origine, an_origine) %>%
    na.omit() %>%
    unique() %>%
    rename(year = an_origine)

p_partielle <- merge(p_partielle, an_mesure, full.x = TRUE)
p_totale <- merge(p_totale, an_mesure, full.x = TRUE)

## type perturbation totale
p_totale <- p_totale %>%
  mutate(type = case_when(
    origine == "BR" ~ "burn",
    origine %in% c("CBA", "CBT", "CDV", "CPH", "CPR", "CPT", "CRB", "CRS",
        "CS", "CT", "ETR", "RPS") ~ "logging",
    origine %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
    origine %in% c("P", "PLN", "PLR", "ENS") ~ "plantation",
    origine == "ES" ~ "severe_outbreak",
    origine == "FR" ~ "wasteland"))
    #is.na(origine) ~ "None"))
p_totale$type <- as.factor(p_totale$type)

## type de perturbation partielle
p_partielle <- p_partielle %>%
  mutate(type = case_when(
    perturb == "BRP" ~ "partial_burn",
    perturb %in% c("CA", "CAM", "CB","CD","CDL","CE", "CEA", "CIP", "CJ",
        "CJG", "CJP", "CJT", "CP", "CPC", "CPF", "CPI", "CPM", "CPS", "CPX",
        "CTR", "DEG", "DLD", "DRM", "EC", "ECE", "EPC", "ESI", "PCP") ~
            "partial_logging",
    perturb == "EL" ~ "light_outbreak",
    perturb %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP=dépérissement
    perturb %in% c("ENR", "RR",  "RRG") ~ "partial_plantation"))
p_partielle$type <- as.factor(p_partielle$type)

# récupéré les données geom

p_totale <- p_totale %>%
    merge(all_placette %>% select(id_pe, latitude, longitude), all.x = TRUE) %>%
    st_as_sf(sf_column_name = "geom")
p_partielle <- p_partielle %>%
    merge(all_placette %>% select(id_pe, latitude, longitude), all.x = TRUE) %>%
    st_as_sf(sf_column_name = "geom")

# les données des perturbation qu'entre 1970 et 2023
p_totale <- p_totale %>% filter(year >= 1970 & year <= 2023)%>%
  mutate(id_pe_mes = paste(id_pe, no_mes, sep = "0"))
p_partielle <- p_partielle %>% filter(year >= 1970 & year <= 2023)%>%
  mutate(id_pe_mes = paste(id_pe, no_mes, sep = "0"))

# RECRUES
recrutement <- tree %>%
    filter(etat %in% c(40, 42, 44, 46)) %>%
    group_by(id_pe, essence, no_mes) %>%
    summarize(recrutement = n()) %>%
    ungroup()

# MERGE RECRUES AND GAULE
gaule_rec <- all_gaule %>%
    merge(recrutement, all.x = TRUE) %>%
    mutate(recrutement = ifelse(is.na(recrutement), 0, recrutement))

# remove all data exept important species and data_perturb
# rm(list = setdiff(ls(), c("gaule_rec", "p_partielle", "p_totale")))