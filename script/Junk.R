################################################################################
# NOT SURE ABOUT THIS PART YET

# MERGE GAULE AND PERTRUBATION
gaule_perturb <- all_gaule  %>%
    merge(perturbation, by = "id_pe", all.x = TRUE) %>%
    mutate(is_perturb = ifelse(is.na(origine) & is.na(perturb),FALSE, TRUE)) %>%
    mutate(is_gaule = ifelse(all_cl == 0,FALSE,TRUE))

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

################################################################################
# ANIMATED PLOTS

anim <- p_totale %>%
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, color = type)) +
  transition_states(year) +
  ggtitle("{closest_state}",
          subtitle = "Frame {frame} of {nframes}")
anim_save("perturb_totale.gif",
  animate(anim, fps = 10, width = 800, height = 400))