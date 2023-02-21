#### Getting non climatic environmental variables from Quebec forest inventory data ####

# Marie-Helene Brice
# June 13th 2018

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# Environmental variables of interest:
# Soil variables (humus type, humus ph, pierrosity, drainage)
# Disturbances (logging, burning, windfall, insect outbreak)
# Forest age


### PACKAGES ####

require(sf)
require(dplyr)
require(tidyr)
require(data.table)
library(lubridate)



## DATA ####

### Formatted species data with xy coordinates

tree_data <- readRDS("data/tree_data_fev2023.RDS")

# check the list of layers in the gpkg

st_layers("raw_data/PEP_GPKG/PEP.gpkg")

# Layer containing soil variables (humus, texture, ph)
pep_sol <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_sol")

# Layer containing disturbance variables and drainage
## Field data
pep_pe <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_pe")

## photo-interpretation (better for disturbances)
pep_ori <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")

# Layer containing age of selected trees in all PE MES
pep_arb <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "dendro_arbres_etudes")

# For soil type, take the measurements for each PEP MES (humus type, organic matter depth, sometimes vary through year mainly because of disturbances)
# For analysis, we could decide to take only the last measurements or the mean (for quantitative variable)

placette_mes <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "placette_mes") %>%
  filter(id_pe %in% tree_data$id_pe) %>%
  mutate(year = year(date_sond))

### SELECT VARIABLES ####

# % of complete cases
apply(pep_sol, 2, function(x) length(which(complete.cases(x))) / nrow(pep_sol) * 100)

apply(pep_pe, 2, function(x) length(which(complete.cases(x))) / nrow(pep_pe) * 100)

apply(pep_ori, 2, function(x) length(which(complete.cases(x))) / nrow(pep_ori) * 100)

apply(pep_arb, 2, function(x) length(which(complete.cases(x))) / nrow(pep_arb) * 100)


# SOIL VARIABLES
pep_sol <- pep_sol %>%
  select(id_pe, id_pe_mes, typehumus, epmatorg, ph_humus, ph_horizb, pourcpierr)

# DISTURBANCE VARIABLES
# field data
pep_pe <- pep_pe %>%
  select(id_pe, id_pe_mes, origine, perturb, cl_age,
         cl_drai, altitude, versant, pc_pent, exposition, dep_sur, nature_dep)
# photo-interpretation data
pep_ori <- pep_ori %>%
  select(id_pe, id_pe_mes, origine, an_origine, perturb, an_perturb, cl_age,
         cl_drai, dep_sur) %>%
  rename_with(~paste0(., "_ori"), origine:dep_sur)


# DISTURBANCE VARIABLES
pep_arb <- pep_arb %>%
  select(id_pe, id_pe_mes, id_arbre, id_arb_mes, age)

# XY

pep_xy <- st_read("data/pep_xy32198_fev2023.gpkg")



# ASSOCIATE MEASURE AND PERTURBATION ORI

# Placette et année avant mesure
an_mesure <- placette_mes %>% select(id_pe, no_mes, year) %>% data.frame() %>%
    pivot_wider(names_from = no_mes, values_from = year) %>% data_frame ()
colnames(an_mesure) <- c("id_pe","geom", "m1", "m2", "m3", "m4", "m5", "m6") #, "m7")

# create columns m12, m23, m34, m45, m56, m67 with NA
an_mesure <- an_mesure %>%
    mutate(m12 = NA, m23 = NA, m34 = NA, m45 = NA, m56 = NA) #, m67 = NA)

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
    #if(!is.na(an_mesure[i,]$m6) & !is.na(an_mesure[i,]$m7)){
    #    an_mesure[i,"m67"] <- toString(list(an_mesure[i,]$m6:an_mesure[i,]$m7)[[1]])
    #    #print("yes")
    #}
}

an_mesure <- an_mesure %>% select (id_pe,"m12","m23","m34","m45","m56")#,"m67")
colnames(an_mesure) = c("id_pe", "2", "3", "4", "5", "6")#, "7")
an_mesure <- an_mesure %>%
    pivot_longer(cols = c("2", "3", "4", "5", "6"), # "7"),
    names_to = "no_mes", values_to = "year") %>%
    mutate(year = strsplit(year,", ")) %>%
    unnest(cols = year) %>%
    mutate(year = as.numeric(year)) %>%
    data.frame()

# il n'y a que des perturbations dont on connait la date

p_partielle <- pep_ori %>% select(id_pe, perturb, an_perturb) %>%
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
    origine %in% c("CHT","DT") ~ "windfall", # DT: dépérissement
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
    perturb %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP: dépérissement
    perturb %in% c("ENR", "RR",  "RRG") ~ "partial_plantation"))
p_partielle$type <- as.factor(p_partielle$type)


### JOIN VARIBALES ####

env_data <- tree_data %>%
  ungroup() %>%
  select(id_pe, id_pe_mes, year_measured) %>%
  distinct() %>%
  left_join(pep_sol, by = c("id_pe", "id_pe_mes")) %>%
  left_join(pep_pe, by = c("id_pe", "id_pe_mes")) %>%
# mais du coup là ce ne sont pas forcement les bonnes mesures FUCK
  left_join(pep_ori, by = c("id_pe", "id_pe_mes"))

age_data <- tree_data %>%
  select(id_pe, id_pe_mes, id_arbre, id_arb_mes) %>%
  left_join(pep_arb, by = c("id_pe", "id_pe_mes", "id_arbre", "id_arb_mes"))

### RECLASSIFY VARIABLES ####

table(pep_pe$origine) # 4096 plots with coupe total
table(pep_ori$origine_ori) # 4341 plots with coupe total
# ORIGINE => perturbations naturelles et interventions anthropiques d’origine 
# qui ont éliminé plus de 75 % de la surface terrière du peuplement précédent

## DRAINAGE
pep_pe <- pep_pe %>%
  mutate(cl_drai2 = case_when(cl_drai %in% 0 ~ "excessif",
                              cl_drai %in% c(10:14) ~ "rapide",
                              cl_drai == 16 ~ "complexe",
                              cl_drai %in% c(20:24) ~ "bon",
                              cl_drai %in% c(30:34) ~ "modere",
                              cl_drai %in% c(40:44) ~ "imparfait",
                              cl_drai %in% c(50:54) ~ "mauvais",
                              cl_drai %in% c(60:64) ~ "tres_mauvais")) %>%
  mutate(cl_drai_ori2 = case_when(cl_drai_ori %in% c(0, "00") ~ "excessif",
                                  cl_drai_ori %in% c(10:14) ~ "rapide",
                                  cl_drai_ori == 16 ~ "complexe",
                                  cl_drai_ori %in% c(20:24) ~ "bon",
                                  cl_drai_ori %in% c(30:34) ~ "modere",
                                  cl_drai_ori %in% c(40:44) ~ "imparfait",
                                  cl_drai_ori %in% c(50:54) ~ "mauvais",
                                  cl_drai_ori %in% c(60:64) ~ "tres_mauvais"))
pep_pe$cl_drai2 <- as.factor(pep_pe$cl_drai2)
pep_pe$cl_drai_ori2 <- as.factor(pep_pe$cl_drai_ori2)


## PERTURBATION D'ORIGINE
logging <- c(
  "CBA", "CBT", "CDV", "CEF",
  "CPH", "CPR", "CPT", "CRB", "CRS",
  "CS", "CT", "ETR", "RPS"
)
env_data <- env_data %>%
  mutate(origine_ori2 = case_when(origine_ori == "BR" ~ "burn",
                                  origine_ori %in% logging ~ "logging",
                                  origine_ori %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
                                  origine_ori %in% c("P", "PLN", "PLR", "PRR", "ENS", "REA") ~ "plantation",
                                  origine_ori == "ES" ~ "severe_outbreak",
                                  origine_ori == "FR" ~ "wasteland"))
env_data$origine_ori2 <- as.factor(env_data$origine_ori2)

## PERTURBATION PARTIELLE
partial_logging <- c(
  "CA", "CAM", "CB", "CD", "CDL", "CE", "CEA", "CIP",
  "CJ", "CJG", "CJP", "CJT",
  "CP", "CPC", "CPF", "CPI", "CPM", "CPS", "CPX", "CTR",
  "DEG", "DLD", "DRM", "EC", "ECE", "EPC", "ESI", "PCP"
)
env_data <- env_data %>%
  mutate(perturb_ori2 = case_when(perturb_ori == "BRP" ~ "partial_burn",
                                  perturb_ori %in% partial_logging ~ "partial_logging",
                                perturb_ori == "EL" ~ "light_outbreak",
                                perturb_ori %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP = dépérissement
                                perturb_ori %in% c("ENR", "RR",  "RRG") ~ "partial_plantation")) 
env_data$perturb_ori2 <- as.factor(env_data$perturb_ori2)

## AGE
age_data <- age_data %>%
  ungroup() %>%
  group_by(id_pe_mes) %>%
  summarise(age_mean = mean(as.integer(age), na.rm = TRUE)) %>%
  replace_na(list(age_mean = NA))

env_data <- env_data %>% 
  left_join(age_data, by = "id_pe_mes")

# Order and select last soil measures

env_data <- env_data %>% 
  select(id_pe:year_measured, 
         origine_ori:an_perturb_ori, origine_ori2, perturb_ori2,
         origine:perturb, 
         age_mean, cl_age,
         typehumus:pourcpierr, 
         cl_drai, cl_drai2, cl_drai_ori2, 
         altitude:nature_dep) 

env_data <- env_data %>% 
  group_by(id_pe) %>%
  arrange(year_measured, .by_group = TRUE) %>% 
  mutate_at(vars(typehumus:nature_dep), last) %>%
  ungroup()

### SAVE ####

saveRDS(env_data, "data/env_data_fev2023.RDS")




