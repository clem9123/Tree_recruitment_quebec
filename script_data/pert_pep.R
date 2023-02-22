# Layer containing disturbance variables and drainage
## Field data
pep_pe <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_pe")

## photo-interpretation (better for disturbances)
pep_ori <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")

