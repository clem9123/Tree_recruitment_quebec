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
perturb_data <- readRDS("data/perturbation_data_fev2023.RDS") %>% encoding()

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
sap_full <- sap_data %>%
    merge(adulte_data, all.x = TRUE) %>%
    merge(recru_data, all.x = TRUE)

# in sap pert columns recrues et arbres change na to 0
sap_full[is.na(sap_full$recrues), "recrues"] <- 0
sap_full[is.na(sap_full$arbres), "arbres"] <- 0

# nouvelle colonne presance absence de gaule avec all_gaule > 0
sap_full <- sap_full %>% mutate(presence_gaule = ifelse(all_cl > 0, 1, 0))

# récuperer que essence SAB
sap_full <- sap_full %>%
    filter(essence == "SAB")

## LES PERTURBATIONS

# deux choses : le tableau a double entrée : id_pe mes et perturb (le nom)
# l'association de la perturbation avec le tableau de sap

# tableau de perturbation rows id_pe_mes et columns perturb avec year en valeur
perturbation <- perturb_data %>%
    mutate(id_pe_mes = paste0(id_pe,"0", no_mes)) %>%
    select(id_pe, no_mes, id_pe_mes, perturb, year) %>%
    unique() %>%
    group_by(id_pe_mes) %>%
    pivot_wider(names_from = perturb, values_from = year, values_fn = ~ max(.x, na.rm = TRUE), values_fill = "0")

# il y a peut-être un problème avec le choix de ne prendre que les perturbs qui ont une date

full_data <- sap_full %>%
    merge(perturbation, all.x = TRUE)

# na from full_data to 0

full_data <- full_data %>%
    select(-`NA`) %>%
    mutate(
        partial_logging = as.numeric(partial_logging),
        partial_windfall = as.numeric(partial_windfall),
        light_outbreak = as.numeric(light_outbreak),
        partial_burn = as.numeric(partial_burn),
        partial_plantation = as.numeric(partial_plantation),
        logging = as.numeric(logging),
        plantation = as.numeric(plantation),
        burn = as.numeric(burn),
        severe_outbreak = as.numeric(severe_outbreak),
        windfall = as.numeric(windfall),
        year_measured = as.numeric(year_measured)
    ) %>%
    mutate(
        partial_logging = ifelse(is.na(partial_logging), NA, year_measured - partial_logging),
        partial_windfall = ifelse(is.na(partial_windfall), NA, year_measured - partial_windfall),
        light_outbreak = ifelse(is.na(light_outbreak), NA, year_measured - light_outbreak),
        partial_burn = ifelse(is.na(partial_burn), NA, year_measured - partial_burn),
        partial_plantation = ifelse(is.na(partial_plantation), NA, year_measured - partial_plantation),
        logging = ifelse(is.na(logging), NA, year_measured - logging),
        plantation = ifelse(is.na(plantation), NA, year_measured - plantation),
        burn = ifelse(is.na(burn), NA, year_measured - burn),
        severe_outbreak = ifelse(is.na(severe_outbreak), NA, year_measured - severe_outbreak),
        windfall = ifelse(is.na(windfall), NA, year_measured - windfall)
    )

full_data <- full_data %>% merge(env_data, all.x = TRUE)
full_data <- full_data %>% filter(id_pe %in% pep_xy$id_pe)
full_data <- full_data %>% merge(pep_xy %>% data.frame() %>% select(id_pe, longitude, latitude), all.x = TRUE)

# MODEL

# trop de NA X(

model <- glm(presence_gaule ~ partial_logging + partial_windfall +
    light_outbreak + partial_burn + partial_plantation + logging + plantation +
    burn + severe_outbreak + windfall + year_measured + latitude + longitude +
    typehumus + epmatorg + ph_humus + cl_age + cl_drai2 + age_mean,
    data = full_data %>% filter(essence == "SAB"), family = binomial)
summary(model)

# step

step(model, direction = "both")

full_data %>% filter(essence == "SAB") %>% nrow()
