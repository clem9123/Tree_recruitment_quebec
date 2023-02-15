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
# il n'y a que des perturbations dont on connait la date
perturbation <- perturbation %>%
    filter(an_perturb %in% 1971:2023 | an_origine %in% 1971:2023) %>%
    select(id_pe, no_mes, perturb, an_perturb, origine, an_origine) %>%
    unique()

## perturbation d'origine
perturbation <- perturbation %>%
  mutate(ptotale = case_when(
    origine == "BR" ~ "burn",
    origine %in% c("CBA", "CBT", "CDV", "CPH", "CPR", "CPT", "CRB", "CRS",
        "CS", "CT", "ETR", "RPS") ~ "logging",
    origine %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
    origine %in% c("P", "PLN", "PLR", "ENS") ~ "plantation",
    origine == "ES" ~ "severe_outbreak",
    origine == "FR" ~ "wasteland"))
    #is.na(origine) ~ "None"))
perturbation$ptotale <- as.factor(perturbation$ptotale)

## perturbation partielle
perturbation <- perturbation %>%
  mutate(ppartielle = case_when(
    perturb == "BRP" ~ "partial_burn",
    perturb %in% c("CA", "CAM", "CB","CD","CDL","CE", "CEA", "CIP", "CJ",
        "CJG", "CJP", "CJT", "CP", "CPC", "CPF", "CPI", "CPM", "CPS", "CPX",
        "CTR", "DEG", "DLD", "DRM", "EC", "ECE", "EPC", "ESI", "PCP") ~
            "partial_logging",
    perturb == "EL" ~ "light_outbreak",
    perturb %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP=dépérissement
    perturb %in% c("ENR", "RR",  "RRG") ~ "partial_plantation"))
perturbation$ppartielle <- as.factor(perturbation$ppartielle)

# MERGE GAULE AND PERTRUBATION
gaule_perturb <- all_gaule  %>%
    merge(perturbation, by = "id_pe", all.x = TRUE) %>%
    mutate(is_perturb = ifelse(is.na(origine) & is.na(perturb),FALSE, TRUE)) %>%
    mutate(is_gaule = ifelse(all_cl == 0,FALSE,TRUE))

# RECRUES
recrutement <- tree %>%
    filter(etat %in% c(40, 42, 44, 46)) %>%
    group_by(id_pe, essence, no_mes) %>%
    summarize(recrutement = n()) %>%
    ungroup()

# MERGE RECRUES AND GAULE
gaule_perturb_rec <- gaule_perturb %>%
    merge(recrutement, all.x = TRUE) %>%
    mutate(recrutement = ifelse(is.na(recrutement), 0, recrutement))


# ARBRE ADULTE
tree_adult <- tree %>%
    filter(etat %in% c(10,30,50)) %>%
    group_by(id_pe, essence, no_mes) %>%
    summarize(nb_adult = n()) %>%
    mutate(no_mes = no_mes + 1) %>%
    ungroup()

# MERGE ARBRE ADULTE AND GAULE
data_perturb <- gaule_perturb_rec %>%
    merge(tree_adult, all.x = TRUE) %>%
    mutate(nb_adult = ifelse(is.na(nb_adult), 0, nb_adult)) %>%
    mutate(is_adult = ifelse(nb_adult > 0, TRUE, FALSE))

# AJOUTER LES PERIODES
p1 <- placette_mes[which(placette_mes$year %in% 1970:1981),]$id_pe
p2 <- placette_mes[which(placette_mes$year %in% 2005:2021),]$id_pe

data_perturb <- data_perturb %>%
    mutate(periode = case_when(
        year %in% p1 ~ 1,
        year %in% p2 ~ 2,
        TRUE ~ 0)) %>%
    mutate(is_periode = ifelse(id_pe %in% p2 & id_pe %in% p1, TRUE, FALSE))

# SELECT IMPORTANT SPECIES
essence_imp <- c("SAB","EPN","BOP","ERR","ERS","BOJ")

# remove all data exept important species and data_perturb
rm(list = setdiff(ls(), c("data_perturb", "essence_imp")))