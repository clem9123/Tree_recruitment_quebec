library(sf)
library(dplyr)

# Layer containing disturbance variables and drainage
## Field data
pep_pe <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_pe")

## photo-interpretation (better for disturbances)
pep_ori <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")


# DISTURBANCE VARIABLES
# field data
pep_pe <- pep_pe %>%
  select(id_pe, id_pe_mes, origine, perturb, cl_age,
         cl_drai, altitude, versant, pc_pent, exposition, dep_sur, nature_dep)
# photo-interpretation data
pep_ori <- pep_ori %>%
  select(id_pe, id_pe_mes, origine, an_origine, perturb, an_perturb, cl_age,
         cl_drai, dep_sur)

# XY

pep_xy <- st_read("data/pep_xy32198_fev2023.gpkg")



# ASSOCIATE MEASURE AND PERTURBATION ORI

# Placette et année avant mesure
an_mesure <- placette_mes %>% select(id_pe, no_mes, year) %>% data.frame() %>%
    pivot_wider(names_from = no_mes, values_from = year) %>% data.frame ()
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