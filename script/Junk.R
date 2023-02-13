head(tree_mes_xy)
head(sap_mes_xy)

tree <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "dendro_arbres")
zone_pe <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "classi_eco_pe")
gaule <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "dendro_gaules")
semis <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "station_semis")
perturbation <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")
mesure <- st_read("data/PEP_GPKG/PEP.gpkg", layer = "placette_mes")

head(zone_pe)


# Les perturbations
