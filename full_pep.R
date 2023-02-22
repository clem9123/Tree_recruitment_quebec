library(tidyverse)
library(lme4)
library(rgdal)
library(sf)

# ENCODING FUNCTION FOR UTF8
encoding <- function(df) {
    for (col in colnames(df)) {
      if (is.character(df[[col]])) {
        Encoding(df[[col]]) <- "UTF-8"
      }
    }
    return(df)
}

# DATA
env_data <- readRDS("data/env_data_fev2023.RDS") %>% encoding()
tree_data <- readRDS("data/tree_data_fev2023.RDS") %>% encoding()
sap_data <- readRDS("data/sap_data_fev2023.RDS") %>% encoding()
pep_xy <- st_read("data/pep_xy32198_fev2023.gpkg",
    layer = "pep_xy32198_fev2023") %>% encoding()
version_inv <- readRDS("data/version_inv.RDS") %>% encoding()
growth <- readRDS("data/growth_data_fev2023.RDS") %>% encoding()
# recrutement
recru_data <- tree_data %>%
    filter(etat %in% c(40, 42, 44, 46)) %>%
    group_by(id_pe_mes, essence) %>%
    summarize(recrues = n()) %>%
    ungroup()

# arbre adulte vivant
adulte_data <- tree_data %>%
# est ce que j'ajoute les recrues vivante 40 ? de la période de la période précédente ?
# Pour la periode précédente ça parait logique, mais pour la période actuelle, je ne sais pas
# Pour l'étude des recrues ça parait un peu bête mais pour les gaules ça peut paraitre logique
# Bon ça doir pas être énormémént d'arbre c'est 10% quand même
    filter(etat %in% c(10,30,40,50)) %>%
    group_by(id_pe_mes, essence) %>%
    summarize(arbres = n()) %>%
    ungroup()

# merge sap pert avec adulte et recru
sap_pert <- sap_pert %>%
    merge(adulte_data, all.x = TRUE) %>%
    merge(recru_data, all.x = TRUE)

# in sap pert columns recrues et arbres change na to 0
sap_pert[is.na(sap_pert$recrues), "recrues"] <- 0
sap_pert[is.na(sap_pert$arbres), "arbres"] <- 0

# récuperer que essence SAB

sap_pert <- sap_pert %>%
    filter(essence == "SAB")

# nouvelle colonne presance absence de gaule avec all_gaule > 0
sap_pert <- sap_pert %>% mutate(presence_gaule = ifelse(all_cl > 0, 1, 0))
