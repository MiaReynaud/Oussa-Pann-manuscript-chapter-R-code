

################################################################################
#
#        CODE POUR LE CHAPITRE DE MANUSCRIT SUR LES SESSIONS DE JEU
#                  Traitement des données du jeu pañn
# 
# 
# M. Reynaud
# 2026-08-03
# 
################################################################################



# test




# Initialisation
rm(list = ls())

# Packages
library(ggplot2)   # pour les graphique
library(tidyverse) # pour manipuler les bases de donnees
library(dplyr)     # pour manipuler les bases de donnees
library(Epi)       # pour manipuler les bases de donnees
library(Hmisc)     # pour graphiques
library(gridExtra) # pour organisation graph ggplot
library(readxl)    # pour chargement data

# Fonctions
mean.rm <- function(x){mean(x,na.rm=T)} # moyenne ne tenant pas compte des NA
sd.rm <- function(x){sd(x,na.rm=T)} # écart-type ne tenant pas compte des NA

# Dossier de travail où sont les data (A CHANGER) ---------------
setwd("/Mes Donnees/Analyses")

# lecture de la base de données dans le fichier XLSX (VERIFIER LA VERSION) -------------
# toutes les donnees sont dans le même fichier Excel, sur des feuilles différentes
file <- "base_de_donnees_jeu_pann-concession_20260511.xlsx"

# base peche
data <- read_excel(file, sheet="BDD_peche_&_SMA")
head(data)

# base joueur·euses
dataPlayersAll <- read_excel(file, sheet="BDD_joueurses")
head(dataPlayersAll)

# base taille
taille <- read_excel(file, sheet="BDD_stock_SMA")

taille <- taille[
  (
    taille$id_partie %in% 3:46 &
      taille$id_partie != 4 &
      taille$id_partie != 3 &
      taille$id_partie != 11 
  )
  |
    taille$village == "sma_aleatoire",
]

head(taille)

# base stock
stock <- read_excel(file, sheet="BDD_stock_SMA")
head(stock)

stock <- stock[
  stock$id_partie %in% 3:46 &          # garder parties 3 à 16
    stock$id_partie != 4 &             # enlever partie 4 (seulement 7 années)
    stock$id_partie != 3 &            # une avait déjà joué
    stock$id_partie != 11,            # enlever partie 11 (seulement 4 joueuses)
]

## selection des donnees
## on retire les 2 premières parties, non comparables car règles de jeu différentes, et on retire aussi tous les résultats de simulation
# personnage avec profils différents pour les parties 1:2

data <- data[
  (
    data$id_partie %in% 3:46 &
      data$id_partie != 4 &
      data$id_partie != 3 &
      data$id_partie != 11 &
      data$nom_personnage != "noire"
  )
  |
    data$village == "sma_aleatoire",
]

d <- data[
  (
    data$id_partie %in% 3:46 &
      data$id_partie != 4 &
      data$id_partie != 3 &
      data$id_partie != 11 &
      data$nom_personnage != "noire"
  )
  |
    data$village == "sma_aleatoire",
]

d <- d[d$annee <= 8, ]

dataPlayers <- dataPlayersAll[
  dataPlayersAll$id_partie %in% 3:16 &          # garder parties 3 à 16
    dataPlayersAll$id_partie != 4 &             # enlever partie 4 (seulement 7 années)
    dataPlayersAll$id_partie != 3 &            # une avait déjà joué
    dataPlayersAll$id_partie != 11,            # enlever partie 11 (seulement 4 joueuses)
]

## Statistiques des parties

### Gains totaux et nombre d'années jouées
tab_gains <- stat.table(index=list(id_partie), list(sum(quantite), max(annee)), data=d)
tab_gains

### Gains totaux par localisation par partie
tab_gains_selon_loc <- stat.table(index=list(id_partie, factor(localisation)), list(sum(quantite)), data=d)
tab_gains_selon_loc

## Fréquence de visite de chaque zone par partie
d$visite <- 1
tab_visites_selon_loc <- stat.table(
  index = list(id_partie, factor(localisation)),
  contents = list(sum(visite)),
  data = d)
tab_visites_selon_loc



# On nettoie la BDD
dd <- d %>%
  filter(
    !is.na(statu_familial),
    !is.na(localisation),
    !is.na(annee)
  )






















































































################################################################################
################################################################################
#
## METHODE
#
################################################################################
################################################################################










################################################################################
## CORPUS DE DONNEES
################################################################################

# Nombre de sessions
nb_sessions <- dataPlayers %>%
  summarise(
    sessions = n_distinct(id_partie)
  )

nb_sessions

# Nombre total de joueuses-session
nrow(dataPlayers)

# Durée des sessions
library(dplyr)

duree_sessions <- dataPlayers %>%
  distinct(id_partie, heure_debut, heure_fin) %>%
  mutate(
    duree_min = as.numeric(
      difftime(heure_fin, heure_debut, units = "mins")
    )
  )

summary(duree_sessions$duree_min)

min(duree_sessions$duree_min)
max(duree_sessions$duree_min)
mean(duree_sessions$duree_min)



## Corpus de données
# Nombre d'actions de jeu des parties observées
library(dplyr)

nb_actions_sessions <- d %>%
  filter(id_partie <= 16) %>%
  nrow()

nb_actions_sessions

# pareil mais pour les simulations
nb_actions_simulations <- d %>%
  filter(id_partie >= 17) %>%
  nrow()

nb_actions_simulations


# Nombre d'heures d'enregistrement
library(dplyr)

duree_totale <- data %>%
  filter(id_partie >= 1, id_partie <= 16) %>%
  distinct(id_partie, heure_debut, heure_fin) %>%
  mutate(
    duree_min = as.numeric(difftime(heure_fin, heure_debut, units = "mins"))
  ) %>%
  summarise(
    total_min = sum(duree_min),
    total_h = total_min / 60
  )

duree_totale






## Description des données issues des sessions de jeu
# prépa data
library(dplyr)
library(ggplot2)
library(patchwork)

# Une ligne par participant
participants <- dataPlayers %>%
  distinct(id_partie, nom_joueuse, .keep_all = TRUE)

# Une ligne par session
duree_sessions <- dataPlayers %>%
  distinct(id_partie, village, heure_debut, heure_fin) %>%
  mutate(
    duree_min = as.numeric(difftime(heure_fin, heure_debut, units = "mins"))
  )

# graph A
g1 <- participants %>%
  mutate(sexe = factor(sexe, levels = c("femme", "homme"))) %>%
  dplyr::count(sexe) %>%
  mutate(pct = round(100 * n / sum(n))) %>%
  ggplot(aes(sexe, n)) +
  geom_col(fill = "grey70", colour = "black", width = .7) +
  geom_text(
    aes(label = paste0(pct, "%")),
    vjust = 1.1,
    colour = "black"
  ) +
  scale_x_discrete(labels = c("femme" = "Femme",
                              "homme" = "Homme")) +
  labs(x = NULL, y = "Nombre de participant·es") +
  theme_classic(base_size = 12)

# B
g2 <- participants %>%
  mutate(statu_familial = factor(
    statu_familial,
    levels = c("enfant", "parent", "grand_parent")
  )) %>%
  dplyr::count(statu_familial) %>%
  ggplot(aes(statu_familial, n)) +
  geom_col(fill = "grey70", colour = "black", width = .7) +
  geom_text(
    aes(label = paste0(round(100*n/sum(n)), "%")),
    vjust = 1.1,
    colour = "black"
  ) +
  scale_x_discrete(labels = c("enfant" = "Enfant",
                              "parent" = "Parent",
                              "grand_parent" = "Grand\n-parent")) +
  labs(x = NULL, y = "Nombre de participant·es") +
  theme_classic(base_size = 12)

# C
g3 <- ggplot(duree_sessions,
             aes(x = "", y = duree_min)) +
  geom_boxplot(fill = "grey90",
               colour = "black",
               width = .35,
               outlier.shape = NA) +
  geom_jitter(aes(colour = village),
              width = .08,
              size = 2.8,
              alpha = 0.9) +
  scale_colour_manual(
    values = c(
      "Falia" = "grey35",
      "Niodior" = "grey70"
    ),
    name = "Village :"
  ) +
  guides(
    colour = guide_legend(nrow = 1)
  ) +
  labs(
    x = NULL,
    y = "Durée des sessions (min)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -12),
    legend.box.margin = margin(t = -12),
  legend.title = element_text(size = 11, face = "plain"),
  legend.text = element_text(size = 11)
  )

# D
g4 <- participants %>%
  dplyr::count(village) %>%
  ggplot(aes(village, n, fill = village)) +
  geom_col(width = .7) +
  geom_text(aes(label = n), vjust = -.3) +
  labs(x = NULL, y = "Nombre de participant·es") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

# Figure finale
(g1 + g2 + g3)












































































































################################################################################
################################################################################
#
## RESULTATS
#
################################################################################
################################################################################







##################################################################################
# EMERGENCE D'UNE CRISE CHRONIQUE
##################################################################################

## Figure x. Émergence d'une crise socio-écologique affectant le stock de coquillages (A.), la taille des coquillages (B.) et les gains économiques (C.) au cours des sessions de jeu. Les enveloppes indiquent l'intervalle entre les valeurs minimale et maximale observées d'une session à l'autre pour chaque village (variabilité). La ligne rouge en pointillés marque le point de bascule de la dynamique, distinguant la période précédant la crise chronique de celle durant laquelle elle sévit.
library(tidyverse)
library(readxl)
library(ggplot2)

# Couleurs personnalisées
cols_villages <- c(
  "Falia" = "#6A51A3",
  "Niodior" = "#E76FAD",
  "sma_aleatoire" = "#5FC2FC"
)

# Import
stock <- read_excel(file, sheet = "BDD_stock_SMA")

# Filtrer
stock_clean <- stock %>%
  
  filter(
    id_partie >= 4,
    id_partie <= 46,
    !id_partie %in% c(4, 11),
    annee <= 8
  )

# stock total
stock_total <- stock_clean %>%
  
  group_by(village, id_partie, annee) %>%
  
  summarise(
    stock_total = sum(quantite, na.rm = TRUE),
    .groups = "drop"
  )

# Moyenne + min/max
stock_summary <- stock_total %>%
  
  group_by(village, annee) %>%
  
  summarise(
    mean_stock = mean(stock_total, na.rm = TRUE),
    min_stock  = min(stock_total, na.rm = TRUE),
    max_stock  = max(stock_total, na.rm = TRUE),
    .groups = "drop"
  )

# Import
stock <- read_excel(file, sheet = "BDD_stock_SMA")

# Filtrer
stock_clean <- stock %>%
  
  filter(
    id_partie >= 4,
    id_partie <= 46,
    !id_partie %in% c(4, 11),
    annee <= 8
  )

# Taille moyenne pondérée par partie et année
taille_partie <- stock_clean %>%
  
  group_by(village, id_partie, annee) %>%
  
  summarise(
    
    taille_moyenne =
      sum(classe_taille * quantite, na.rm = TRUE) /
      sum(quantite, na.rm = TRUE),
    
    .groups = "drop"
  )

# Résumé par village et année
taille_summary <- taille_partie %>%
  
  group_by(village, annee) %>%
  
  summarise(
    mean_size = mean(taille_moyenne, na.rm = TRUE),
    min_size  = min(taille_moyenne, na.rm = TRUE),
    max_size  = max(taille_moyenne, na.rm = TRUE),
    .groups = "drop"
  )

# Import
data <- read_excel(file, sheet = "BDD_peche_&_SMA")

# Filtrer
data_clean <- data %>%
  
  filter(
    id_partie >= 4,
    id_partie <= 46,
    !id_partie %in% c(4, 11),
    annee <= 8
  )

# Gains totaux par joueuse et par tour
gains_joueuse <- data_clean %>%
  
  group_by(village, id_partie, nom_personnage, annee) %>%
  
  summarise(
    gain_total = sum(quantite, na.rm = TRUE),
    .groups = "drop"
  )

# Moyenne + min/max par village et tour
gains_summary <- gains_joueuse %>%
  
  group_by(village, annee) %>%
  
  summarise(
    mean_gain = mean(gain_total, na.rm = TRUE),
    min_gain  = min(gain_total, na.rm = TRUE),
    max_gain  = max(gain_total, na.rm = TRUE),
    .groups = "drop"
  )

theme_game <- theme_minimal(base_size = 14) +
  
  theme(
    
    # légende
    legend.position = "bottom",
    legend.title = element_blank(),
    
    # titres des panels
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "grey20"
    ),
    
    # axes
    axis.title = element_text(
      color = "grey20"
    ),
    
    axis.text = element_text(
      color = "grey35"
    ),
    
    # grille claire
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.4
    ),
    
    panel.grid.minor = element_blank(),
    
    # traits axes plus doux
    axis.line = element_line(
      color = "grey70",
      linewidth = 0.3
    )
  )

## cette fois avec des enveloppes plutot que des moustaches

stock_summary <- stock_summary %>%
  mutate(alpha_ribbon = ifelse(village == "sma_aleatoire", 0.05, 0.18))

gains_summary <- gains_summary %>%
  mutate(alpha_ribbon = ifelse(village == "sma_aleatoire", 0.05, 0.18))

taille_summary <- taille_summary %>%
  mutate(alpha_ribbon = ifelse(village == "sma_aleatoire", 0.05, 0.18))

p1 <- ggplot(stock_summary,
             aes(x = annee,
                 y = mean_stock,
                 color = village,
                 fill = village)) +
  geom_vline(
    xintercept = 4,
    color = "red3",
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.8
  ) +
  geom_ribbon(aes(
    ymin = min_stock,
    ymax = max_stock,
    alpha = ifelse(village == "sma_aleatoire", 0, alpha_ribbon)
  ), color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.3) +
  scale_alpha_identity() +
  scale_color_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_fill_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "A. Stock de coquillage",
    x = NULL,
    y = "Stock moyen"
  ) +
  theme_game +
  theme(
    axis.text.x = element_text(size = 9),
    axis.ticks.x = element_line(),
    axis.title.x = element_blank()
  ) +
  guides(
    fill = "none",  # On enlève la légende pour le fill
    color = guide_legend(override.aes = list(
      shape = c(16, 16, 16),  # Point pour tous
      linetype = c(1, 1, 1),  # Ligne pour tous
      fill = c(cols_villages["Falia"], cols_villages["Niodior"], NA)  # Fond pour Falia et Niodior, rien pour sma_aleatoire
    ))
  )

p2 <- ggplot(gains_summary,
             aes(x = annee,
                 y = mean_gain,
                 color = village,
                 fill = village)) +
  geom_vline(
    xintercept = 5,
    color = "red3",
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.8
  ) +
  geom_ribbon(aes(
    ymin = min_gain,
    ymax = max_gain,
    alpha = ifelse(village == "sma_aleatoire", 0, alpha_ribbon)
  ), color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.3) +
  scale_alpha_identity() +
  scale_color_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_fill_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "C. Gains économiques",
    x = NULL,
    y = "Gains moyen"
  ) +
  theme_game +
  theme(
    axis.text.x = element_text(size = 9),
    axis.ticks.x = element_line(),
    axis.title.x = element_text(hjust = 0.5)
  ) +
  guides(
    fill = "none",
    color = guide_legend(override.aes = list(
      shape = c(16, 16, 16),
      linetype = c(1, 1, 1),
      fill = c(cols_villages["Falia"], cols_villages["Niodior"], NA)
    ))
  )

p3 <- ggplot(taille_summary,
             aes(x = annee,
                 y = mean_size,
                 color = village,
                 fill = village)) +
  geom_vline(
    xintercept = 4,
    color = "red3",
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.8
  ) +
  geom_ribbon(aes(
    ymin = min_size,
    ymax = max_size,
    alpha = ifelse(village == "sma_aleatoire", 0, alpha_ribbon)
  ), color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.3) +
  scale_alpha_identity() +
  scale_color_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_fill_manual(
    values = cols_villages,
    labels = c(
      "Falia" = "Falia",
      "Niodior" = "Niodior",
      "sma_aleatoire" = "Simulation aléatoire"
    )
  ) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "B. Taille de coquillage",
    x = NULL,
    y = "Taille moyenne"
  ) +
  theme_game +
  theme(
    axis.text.x = element_text(size = 9),
    axis.ticks.x = element_line(),
    axis.title.x = element_text(hjust = 0.5)
  ) +
  guides(
    fill = "none",
    color = guide_legend(override.aes = list(
      shape = c(16, 16, 16),
      linetype = c(1, 1, 1),
      fill = c(cols_villages["Falia"], cols_villages["Niodior"], NA)
    ))
  )

final_plot <- (p1 | p3 | p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot









































##################################################################################
# STRATEGIES IDENTIFIEES
##################################################################################

## Limiter les risques
# variable village
library(dplyr)

dd <- dd %>%
  mutate(
    village_choice = ifelse(localisation == "village", 1, 0)
  )

# pourcentage pêche vs village par partie
peche_partie <- dd %>%
  mutate(
    periode = ifelse(annee < 4, "debut", "fin"),
    peche = localisation != "village"
  ) %>%
  group_by(id_partie, periode) %>%
  summarise(prop_peche = mean(peche), .groups="drop")

library(tidyr)

peche_wide <- peche_partie %>%
  pivot_wider(
    names_from = periode,
    values_from = prop_peche
  )

wilcox.test(
  peche_wide$debut,
  peche_wide$fin,
  paired = TRUE
)














## GLMM : le village augmente-t-il au cours du temps ?
dd$village_choice <- ifelse(dd$localisation=="village",1,0)

library(lme4)

mod_village <- glmer(
  village_choice ~ annee +
    (1|id_partie) +
    (1|id_personnage),
  data=dd,
  family=binomial
)

library(car)

Anova(mod_village)

summary(mod_village)















## La pêche reste majoritaire ?
# Proportions par année
prop <- dd %>%
  group_by(annee) %>%
  summarise(
    peche = mean(localisation!="village"),
    village = mean(localisation=="village")
  )

prop

all(prop$peche > 0.5)

# graph
library(ggplot2)

ggplot(prop,
       aes(annee, peche))+
  geom_point(size=3)+
  geom_line(size=1)+
  ylim(0,1)+
  ylab("Proportion de décisions de pêche")+
  xlab("Tour")

# Autre graph
prop_long <- prop %>%
  pivot_longer(
    -annee,
    names_to="Activite",
    values_to="Proportion"
  )

ggplot(prop_long,
       aes(annee,
           Proportion,
           colour=Activite))+
  geom_line(size=1.2)+
  geom_point(size=3)+
  ylim(0,1)+
  theme_classic()

















## Moyennes
peche_wide %>%
  summarise(
    debut = mean(debut),
    fin = mean(fin),
    diff = mean(fin - debut)
  )



































































##################################################################################
# DETERMINANT DES DECISIONS
##################################################################################

### INFORMATIONS ECOLOGIQUES

## Impact des gains précédents
library(dplyr)

switch_data <- dd %>%
  arrange(id_partie, id_personnage, annee) %>%
  group_by(id_partie, id_personnage) %>%
  mutate(
    localisation_prev = lag(localisation),
    gain_prev = lag(quantite),
    switch = ifelse(
      !is.na(localisation_prev) &
        localisation != localisation_prev,
      1,
      0
    )
  ) %>%
  ungroup()

table(switch_data$switch, useNA="ifany")

# enlève le premier tour
switch_data <- switch_data %>%
  filter(!is.na(gain_prev))

# Vérifier l'effet du gain précédent sur le changement
library(lme4)

mod_switch <- glmer(
  switch ~ gain_prev +
    (1|id_partie) +
    (1|id_personnage),
  data = switch_data,
  family = binomial
)

summary(mod_switch)

# significativité
library(car)

Anova(mod_switch)

# augmentation ?

switch_data <- switch_data %>%
  mutate(
    stay = ifelse(switch == 0, 1, 0)
  )

mod_stay <- glmer(
  stay ~ gain_prev +
    (1|id_partie) +
    (1|id_personnage),
  data = switch_data,
  family = binomial
)

summary(mod_stay)

# vérification graphique
library(ggplot2)

switch_data %>%
  group_by(gain_prev) %>%
  summarise(
    prop_rester = mean(stay)
  ) %>%
  ggplot(aes(gain_prev, prop_rester))+
  geom_point(size=3)+
  geom_smooth(method="glm",
              method.args=list(family="binomial"))+
  theme_classic()+
  ylab("Probabilité de rester dans la même zone")+
  xlab("Gain au tour précédent")

























## zone la plus rentable
library(dplyr)

# Quantité de grosses pann par zone, année et partie
stock_opt <- stock %>%
  filter(classe_taille == 4,
         localisation %in% c("jaune","vert","rouge")) %>%
  group_by(id_partie, annee, localisation) %>%
  summarise(stock_grosses = sum(quantite), .groups = "drop")

zone_opt <- stock_opt %>%
  group_by(id_partie, annee) %>%
  slice_max(stock_grosses, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(zone_optimale = localisation)

choix <- dd %>%
  select(id_partie, annee, localisation)

comparaison <- choix %>%
  left_join(zone_opt,
            by = c("id_partie","annee"))

comparaison %>%
  summarise(
    n = n(),
    n_optimales = sum(localisation == zone_optimale),
    pourcentage = 100 * n_optimales / n
  )
























## effet pénibilité sur changement de zone

dd2 <- dd %>%
  arrange(id_partie,
          nom_joueuse,
          annee) %>%
  group_by(id_partie,
           nom_joueuse) %>%
  mutate(
    localisation_suivante = lead(localisation),
    changement = localisation != localisation_suivante
  ) %>%
  ungroup()

dd2 <- dd2 %>%
  filter(!is.na(changement))

modele <- glm(changement ~ penibilite,
              family = binomial,
              data = dd2)

summary(modele)

























## vers une optimisation écologique ?
# Créer la variable "choix optimal"
comparaison <- dd %>%
  left_join(zone_opt, by = c("id_partie", "annee")) %>%
  mutate(optimal = localisation == zone_optimale)

# Visualiser la proportion de choix optimaux par année
comparaison %>%
  group_by(annee) %>%
  summarise(
    proportion = mean(optimal),
    n = n()
  )

comparaison %>%
  group_by(annee) %>%
  summarise(proportion = mean(optimal)) %>%
  ggplot(aes(annee, proportion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Année",
    y = "Proportion de choix optimaux"
  ) +
  theme_classic()

# Tester statistiquement la tendance
modele <- glm(optimal ~ annee,
              family = binomial,
              data = comparaison)

summary(modele)

# avec un modèle mixte parce que la même joueuse prend des décisions succesives
library(lme4)

modele_mixte <- glmer(
  optimal ~ annee + (1 | id_partie/nom_joueuse),
  family = binomial,
  data = comparaison
)

summary(modele_mixte)

























## abandon peche si mauvais gains
library(dplyr)

village_data <- dd %>%
  arrange(id_partie, id_personnage, annee) %>%
  group_by(id_partie, id_personnage) %>%
  mutate(
    gain_prev = lag(quantite)
  ) %>%
  ungroup() %>%
  filter(!is.na(gain_prev))

summary(village_data$gain_prev)

village_data <- village_data %>%
  mutate(
    village_choice = ifelse(localisation == "village", 1, 0)
  )

table(village_data$village_choice)

library(lme4)

mod_village_gain <- glmer(
  village_choice ~ gain_prev +
    (1|id_partie) +
    (1|id_personnage),
  data = village_data,
  family = binomial
)

summary(mod_village_gain)

mod_village_annee <- glmer(
  village_choice ~ annee +
    (1|id_partie) +
    (1|id_personnage),
  data = village_data,
  family = binomial
)

summary(mod_village_annee)

mod_village <- glmer(
  village_choice ~ gain_prev + annee +
    (1|id_partie) +
    (1|id_personnage),
  data = village_data,
  family = binomial
)

summary(mod_village)

library(car)

Anova(mod_village)

village_data %>%
  group_by(gain_prev) %>%
  summarise(
    proportion_village = mean(village_choice),
    n = n()
  )

# avec gains en variable catégorielle
mod_village_gain_facteur <- glmer(
  village_choice ~ factor(gain_prev) + annee +
    (1|id_partie) +
    (1|id_personnage),
  data = village_data,
  family = binomial
)

summary(mod_village_gain_facteur)































































##################################################################################
# PROFILS DE JOUEUR·EUSES
##################################################################################

## validation des profils qualitatif ?
library(dplyr)
library(tidyr)
library(ggplot2)

# Nombre de tours de pêche par joueuse
freq_pecheuse <- dd %>%
  distinct(id_partie, id_personnage, nom_personnage) %>%
  left_join(
    dd %>%
      filter(localisation != "village") %>%
      count(id_partie, id_personnage, name = "nb_tours_peche"),
    by = c("id_partie", "id_personnage")
  ) %>%
  mutate(
    nb_tours_peche = replace_na(nb_tours_peche, 0)
  )


summary(freq_pecheuse$nb_tours_peche)


# Distribution de l'engagement dans la pêche
ggplot(freq_pecheuse, aes(nb_tours_peche)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_continuous(breaks = 0:8) +
  theme_classic() +
  labs(
    x = "Nombre de tours de pêche",
    y = "Nombre de joueuses"
  )

# k-means 3 profils
set.seed(123)

km3 <- kmeans(
  freq_pecheuse["nb_tours_peche"],
  centers = 3
)

freq_pecheuse$profil_kmeans <- factor(km3$cluster)

# Moyenne des groupes
freq_pecheuse %>%
  group_by(profil_kmeans) %>%
  summarise(
    n = n(),
    moyenne = mean(nb_tours_peche),
    min = min(nb_tours_peche),
    max = max(nb_tours_peche)
  )

# Visualisation
ggplot(
  freq_pecheuse,
  aes(profil_kmeans, nb_tours_peche, fill = profil_kmeans)
) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = "Profil",
    y = "Nombre de tours de pêche"
  )

# Modèles de mélange gaussien
library(mclust)

set.seed(123)

mc_auto <- Mclust(freq_pecheuse$nb_tours_peche)

summary(mc_auto)

# Nombre optimal de groupes selon BIC
mc_auto$G

# Comparaison avec 3 profils imposés
mc3 <- Mclust(
  freq_pecheuse$nb_tours_peche,
  G = 3
)

mc_auto$bic
mc3$bic


# Visualisation BIC
plot(
  mc_auto,
  what = "BIC"
)

# Modèle mélange de Poisson
library(flexmix)

set.seed(123)

mods <- lapply(
  1:5,
  function(k)
    flexmix(
      nb_tours_peche ~ 1,
      data = freq_pecheuse,
      k = k,
      model = FLXMRglm(family = "poisson")
    )
)

BICs <- sapply(mods, BIC)

data.frame(
  nombre_profils = 1:5,
  BIC = BICs
)

plot(
  1:5,
  BICs,
  type = "b",
  pch = 19,
  xlab = "Nombre de profils",
  ylab = "BIC"
)























## Figure engagement dans la pêche
library(patchwork)

# Catégories uniquement pour visualisation
freq_pecheuse <- dd %>%
  distinct(id_partie, id_personnage, nom_personnage, village) %>%
  left_join(
    dd %>%
      filter(localisation != "village") %>%
      dplyr::count(id_partie, id_personnage, name = "nb_tours_peche"),
    by = c("id_partie", "id_personnage")
  ) %>%
  mutate(
    nb_tours_peche = tidyr::replace_na(nb_tours_peche, 0)
  )

n_total <- nrow(freq_pecheuse)


freq_pecheuse <- freq_pecheuse %>%
  mutate(
    engagement = factor(
      case_when(
        nb_tours_peche <= 2 ~ "Faible engagement",
        nb_tours_peche <= 5 ~ "Engagement intermédiaire",
        TRUE ~ "Fort engagement"
      ),
      levels = c(
        "Faible engagement",
        "Engagement intermédiaire",
        "Fort engagement"
      )
    )
  )

effectifs <- freq_pecheuse %>%
  distinct(id_partie, id_personnage, village) %>%
  group_by(village) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

cols_engagement <- c(
  "Faible engagement" = "#D9D9D9",
  "Engagement intermédiaire" = "#8C8C8C",
  "Fort engagement" = "#252525"
)

# A. Distribution générale
p1 <- ggplot(
  freq_pecheuse,
  aes(nb_tours_peche, fill = engagement)
) +
  geom_histogram(
    binwidth = 1,
    color = "white",
    boundary = -0.5
  ) +
  scale_fill_manual(values = cols_engagement) +
  scale_x_continuous(
    breaks = 0:8,
    limits = c(0, 8)
  ) +
  theme_classic() +
  labs(
    title = "A. Distribution de l'effort de pêche",
    x = "Nombre de tours à la pêche",
    y = "Nombre de joueur·euses",
    fill = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  annotate(
    "text",
    x = 0.2,
    y = Inf,
    label = paste0("n = ", n_total),
    hjust = 0,
    vjust = 1.5,
    size = 4
  )

# B. Comparaison entre villages
prop_profils <- freq_pecheuse %>%
  left_join(
    dd %>%
      distinct(id_partie, village),
    by = "id_partie"
  ) %>%
  dplyr::count(village, engagement) %>%
  group_by(village) %>%
  mutate(
    prop = n/sum(n)
  )

p2 <- ggplot(
  prop_profils,
  aes(village, prop, fill = engagement)
) +
  geom_col(color = "white") +
  scale_fill_manual(values = cols_engagement) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(0, 1.12), clip = "off") +
  labs(
    title = "B. Répartition des niveaux d'engagement\nau sein des villages",
    x = "Village",
    y = "Proportion",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(t = 15, r = 10, b = 5, l = 5)
  ) +
  geom_text(
    data = effectifs,
    aes(
      x = village,
      y = 1.08,
      label = paste0("n = ", n)
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  )

p1 + p2 +
  plot_layout(guides = "collect")






















































##################################################################################
# PROJECTIONS DANS LE JEU
##################################################################################

### "La pêche"
# pourcentage de joueur·euses sur chaque localisation au premier tour

dd %>%
  filter(annee == 1) %>%
  count(localisation) %>%
  mutate(
    pourcentage = 100 * n / sum(n)
  )








### Les zones de pêche
## Choix au premier tour
library(dplyr)

# Choix au premier tour
tour1 <- dd %>%
  filter(annee == 1,
         localisation != "village")

tour1 %>%
  dplyr::count(localisation) %>%
  mutate(
    proportion = n / sum(n) * 100
  )














## schéma d'exploitation et efficacité économique
# Données : uniquement les zones de pêche
obs <- dd %>%
  filter(localisation %in% c("jaune", "vert", "rouge")) %>%
  dplyr::count(localisation) %>%
  arrange(match(localisation, c("jaune", "vert", "rouge")))

obs

# Effectifs observés
observes <- obs$n

# Probabilités attendues selon la productivité (2:4:6)
attendues <- c(2, 4, 6) / sum(c(2, 4, 6))

# Test du chi² d'adéquation
test <- chisq.test(x = observes, p = attendues)

test

resultats <- data.frame(
  localisation = c("jaune", "vert", "rouge"),
  observes = observes,
  prop_observee = observes / sum(observes),
  prop_attendue = attendues,
  ratio = (observes / sum(observes)) / attendues
)

resultats

cat("Chi² =", round(test$statistic, 2), "\n")
cat("ddl =", test$parameter, "\n")
cat("p =", format.pval(test$p.value), "\n")



















































































































































### Gains totaux et nombre d'années jouées
tab_total <- stat.table(index=list(id_partie), list(sum(quantite), max(nb_annee)), data=d)
tab_total

### Gains totaux par localisation par année
tab_total_loc <- stat.table(index=list(id_partie, factor(localisation)), list(sum(quantite)), data=d)
tab_total_loc


## gains cumulé, par partie en fonction de la localisation -------
## attention au nombre d'années et nombre de joueurs différents
ggplot(d, aes(x=id_partie, y=quantite, label=quantite))+
  geom_bar(aes(fill=localisation), stat="identity")+
  scale_fill_manual(values=cols_villages)+
  ylim(0,100)+
  # geom_text(aes(label = quantite), position=position_stack(vjust = 0.5))+
  labs(fill="Localisation")


## gains cumulé, par partie en fonction de la localisation -------
## attention pas le même nombre de joueurs
## ATTENTION partie 11 on n'a eu que 4 joueuses
ggplot(d, aes(x=id_partie, y=quantite/nb_annee/nb_joueuses, label=quantite))+
  geom_bar(aes(fill=localisation), stat="identity")+
  scale_fill_manual(values=mycolors_game)+
  # ylim(0,10)+
  labs(fill="Localisation")


## gains par joueuse par partie en fonction de la localisation -------
ggplot(d, aes(x=annee, y=quantite, fill=factor(localisation, levels = c("dakar","jaune","vert","rouge","village"))))+
  geom_bar(stat="identity")+
  geom_point(aes(x=annee, y=penibilite), shape=17, size=3, col="red")+
  guides(shape=NA, size=NA)+
  ylim(0,3)+
  scale_fill_manual(values=mycolors)+
  labs(fill="Localisation")+
  facet_grid(factor(nom_personnage, levels = c("rouge","verte","jaune","grise","rose"))~id_partie)


## gains cumulés par partie par localisation -------------------------
ggplot(d, aes(localisation, fill=factor(localisation, levels = c("dakar","jaune","vert","rouge","village"))))+
  geom_bar()+
  scale_fill_manual(values=mycolors_game)+
  labs(fill="Localisation")+
  facet_grid(.~id_partie)


## Taille max chaque année par saré ----------------------------------
partie <- 3:16
t <- taille[taille$id_partie%in%partie,]
ggplot(t, aes(x=annee, y=id_taille_finale, fill=factor(localisation, levels = c("jaune","vert","rouge"))))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("yellow","#009900","red"))+
  labs(fill="Localisation")+
  facet_grid(factor(localisation, levels = c("jaune","vert","rouge"))~id_partie)


## Trajectoire des joueuses ----------------------

### trajectoires individuelles -------------------
# on regarde les gains cumulés de chaque joueuse à chaque partie
par(mar=c(4,4,1,1), mfrow=c(4,4), mgp=c(2,1,0))
for (p in 3:16){
plot(1,1, xlim=c(0,11), ylim=c(0,30), pch=NA, xlab="Year", ylab="Cumulated income")
  legend("topleft", legend=p, bty="n")
  partie <-p
  d <- data[data$id_partie%in%partie,]
  names <- unique(d$nom_personnage)
  for (i in names){
    x <- c(0,unique(data[data$id_partie%in%partie & data$nom_personnage==i,]$annee))
    y <- c(0,cumsum(data[data$id_partie%in%partie & data$nom_personnage==i,]$quantite))
    points(x, y, type="l")
    abline(1,2, col="red") # on ajoute la courbe cumulée de gain = 2 par année
  }
}


## évolution de la pénibilité et des gains moyens individuels par année, par village -----------------------
## ajout des simulations SMA

# load data (feuille différente de la BDD concession)
# les parties simulées sont ajoutées à la suite des partie jouées
sma <- read_excel(file, sheet="BDD_peche_&_SMA")
head(sma)

# identifiant des parties sélectionnées ; on retire les ârties 1 et 2 non comparables à cause des profils
partie <-3:46 

# on selectionne les parties (!=1:3)
d <- sma[sma$id_partie%in%partie,] 
# uniquement les parties ayant duré moins de 9 ans
# ATTENTION une partie une seule partie à 7 ans !!!!
d <- d[d$annee<9,] 

t <- taille[taille$id_partie%in%partie,] # on selectionne les parties (!=1:3) et uniquement les partie ayant duré moins de 9 ans
t <- t[t$annee<9,] # on ne garde que 8 ans car peu de parties au-delà

s <- stock[stock$id_partie%in%partie,] # on selectionne les parties
s <- s[s$annee<9,] # on ne garde que 8 ans car peu de parties au-delà

## Création des data.frame pour recueillir les résultats
df_income <- data.frame(village=NA, annee=NA, y=NA, sd=NA)
df_penibilite <- data.frame(village=NA, annee=NA, y=NA, sd=NA)
df_taille <-  data.frame(village=NA, annee=NA, y=NA, sd=NA)
df_stock <-  data.frame(village=NA, annee=NA, localisation=NA, classe_taille=NA, y=NA, sd=NA)
df_village <- data.frame(village=NA, annee=NA, y=NA, sd=NA)

## Boucle sur les villages
for (v in unique(d$village)){
  dd <- d[d$village==v,]
  # gains
  y <- tapply(dd$quantite, dd$annee, mean.rm)
  sd <- tapply(dd$quantite, dd$annee, sd.rm)
  df.tmp <- data.frame(village= rep(v,length(y)), annee=as.numeric(names(y)), y=y, sd=sd)
  df_income <- rbind(df_income, df.tmp)
  
  # pénibilité
  dd <- dd[dd$localisation!="village",] # on retire les villages pour le calcul de la pénibilité
  dd$penibilite[is.na(dd$penibilite)] <- 0 # on remplace les case vides par des 0 pour le calcul des moyennes
  y <- tapply(dd$penibilite, dd$annee, mean.rm)
  sd <- tapply(dd$penibilite, dd$annee, sd.rm)
  df.tmp <- data.frame(village= v, annee=as.numeric(names(y)), y=y, sd=sd)
  df_penibilite <- rbind(df_penibilite, df.tmp)
  
  # taille
  dd <- t[t$village==v,]
  y <- tapply(dd$id_taille_finale, dd$annee, mean.rm)
  sd <- tapply(dd$id_taille_finale, dd$annee, sd.rm)
  df.tmp <- data.frame(village= rep(v,length(y)), annee=as.numeric(names(y)), y=y, sd=sd)
  df_taille <- rbind(df_taille, df.tmp)
  
  # village
  dd <- d[d$village==v & d$localisation=="village",] # on garde uniquement les villages
  y <- tapply(dd$quantite, dd$annee, mean.rm)
  sd <- tapply(dd$quantite, dd$annee, sd.rm)
  df.tmp <- data.frame(village= v, annee=as.numeric(names(y)), y=y, sd=sd)
  df_village <- rbind(df_village, df.tmp)
  
  # stock
  # if(v!="sma_alea"){
    for (a in 1:8){ # boucle sur les années
      for(l in unique(s$localisation)){# boucle sur les localisation
        ss.tmp <- s[s$village==v,]
        ss <- ss.tmp[ss.tmp$annee==a & ss.tmp$localisation==l,]
        y <- tapply(ss$quantite, ss$classe_taille, mean.rm)
        sd <- tapply(ss$quantite, ss$classe_taille, sd.rm)
        df.tmp <- data.frame(village=v, annee=a, localisation=l, classe_taille=1:4, y=y, sd=sd)
        df_stock <- rbind(df_stock, df.tmp)
      # }
    }
  }
}

# on retire les NA des data.frame
df_income <- df_income[!is.na(df_income$village),]
df_penibilite <- df_penibilite[!is.na(df_penibilite$village),]
df_taille <- df_taille[!is.na(df_taille$village),]
df_stock <- df_stock[!is.na(df_stock$village),]
df_village <- df_village[!is.na(df_village$village),]


# ajout des sommes cumulées  pour income et pénibilité
df_income$y_cum <- NA
df_penibilite$y_cum <- NA
for (i in unique(df_income$village)){
  d <- df_income[df_income$village==i,]$y
  df_income[df_income$village==i,]$y_cum <- cumsum(d)
  
  d <- df_penibilite[df_penibilite$village==i,]$y
  df_penibilite[df_penibilite$village==i,]$y_cum <- cumsum(d)
}


# Plot des gains moyens par joueuse par année pour chaque village
p1 <- ggplot(df_income, aes(x=annee, y=y, color=village, shape=village))+
  geom_point(aes(x=annee, y=y), size=3)+
  geom_smooth(linewidth=0, aes(fill=village), alpha=.2)+
  geom_line(size=1.5)+
  # geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2, position=position_dodge(0.05))+
  xlab("Year")+
  ylab("Income")+
  scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  theme_minimal()+
  labs(color="Village", shape="Village", fill="Village")

# Plot des gains moyens par joueuse cumulés par année pour chaque village
p1bis <- ggplot(df_income, aes(x=annee, y=y_cum, color=village, shape=village))+
  geom_line(size=1.5)+
  # geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2, position=position_dodge(0.05))+
  xlab("Year")+
  ylab("Income")+
  scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  theme_minimal()+
  labs(color="Village", shape="Village", fill="Village")

# Plot des pénibilités moyennes par joueuse par année pour chaque village
p2 <- ggplot(df_penibilite, aes(x=annee, y=y, fill=village))+
  geom_bar(stat="identity", position=position_dodge())+
  # geom_errorbar(aes(ymin=y, ymax=y+sd), position=position_dodge(.9), width=0)+
  scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  xlab("")+
  ylab("Arduousness")+
  theme_minimal()

# Plot des pénibilités moyennes par joueuse cumulées par année pour chaque village
p2bis <- ggplot(df_penibilite, aes(x=annee, y=y_cum, color=village))+
  geom_line(size=1.5)+
  # geom_errorbar(aes(ymin=y, ymax=y+sd), position=position_dodge(.9), width=0)+
  # scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  xlab("Year")+
  ylab("Arduousness")+
  theme_minimal()+
  labs(color="Village")

## Pénibilité moyenne par village ------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot(df_penibilite, aes(x=village, y=y, fill=village))+
  geom_boxplot()+
  xlab("")+
  ylab("Arduousness")+
  theme(legend.position="none")



# Plot des tailles moyennes annuelles par village
p3 <- ggplot(df_taille, aes(x=annee, y=y, fill=village))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=y, ymax=y+sd), position=position_dodge(.9), width=0)+
  scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  xlab("")+
  ylab("Shell size")+
  theme_minimal()

# résultat moyens
grid.arrange(p3, p1, p2, heights = c(1,2,1))


# Moyenne des passages au village par année par village
p4 <- ggplot(df_village, aes(x=annee, y=y, color=village))+
  geom_point(aes(x=annee, y=y), size=3)+
  geom_smooth(linewidth=0, aes(fill=village), alpha=.2)+
  geom_line(size=1.5)+
  # geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2, position=position_dodge(0.05))+
  xlab("Year")+
  ylab("Village income")+
  scale_x_continuous(limits=c(.5, max(df_income$annee)+.5), breaks=seq(1,10))+
  theme_minimal()+
  labs(color="Village", shape="Village", fill="Village")


# gains moyens au village
p4

## résultat cumulés
grid.arrange(p1bis, p2bis, heights = c(1,1))





## distribution sur les localisations ---------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
d <- sma[sma$id_partie%in%partie,] # on selectionne les parties
d <- d[d$annee<9,] # on selectionne les parties
dd <- d[d$village%in%c("Falia","Niodior"),]

ggplot(dd, aes(y=localisation, fill=village))+
  geom_bar(position=position_dodge())+
  coord_flip()

dd <- dd[dd$id_localisation!=5,]
ggplot(dd, aes(x=id_localisation, y=quantite, color=village))+
  geom_point()+
  stat_ellipse()


## évolution des stocks par classe de taille -----------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot(df_stock, aes(x=annee, y=y, color=factor(village)))+
  geom_point()+
  geom_line()+
  # geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=0.2)+
  xlab("Year")+
  ylab("Shell density")+
  facet_grid(factor(localisation, level=c("jaune","vert","rouge"))~classe_taille)+
  labs(color="Village")











































































########################################################################################
# Diversité spatiale
########################################################################################

library(dplyr)
library(vegan)

# Nombre de joueuses par localisation à chaque tour
tab <- dd %>%
  group_by(id_partie, annee, localisation) %>%
  summarise(n = n(), .groups="drop")

# Passage en tableau large
tab_wide <- tidyr::pivot_wider(
  tab,
  names_from = localisation,
  values_from = n,
  values_fill = 0
)

# Shannon
tab_wide$shannon <- diversity(tab_wide[,c("jaune","vert","rouge","village")],
                              index="shannon")

library(lme4)

m <- lmer(shannon ~ annee + (1|id_partie), data=tab_wide)

summary(m)








































# Rotation des zones
dd <- dd %>%
  arrange(id_partie,id_personnage,annee)

dd <- dd %>%
  group_by(id_partie,id_personnage) %>%
  mutate(changement =
           localisation != lag(localisation))

rotation <- dd %>%
  group_by(id_partie,annee) %>%
  summarise(prop_change = mean(changement, na.rm=TRUE))

lmer(prop_change ~ annee + (1|id_partie), data=rotation)













































# Imitation de zone
choix_zone ~ succes_zone_tour_precedent + (1|id_partie)

























































#######################################################################################
# Dynamiques économiques
#######################################################################################



################################################
# Evolution des gains (selon leur origine)

library(dplyr)
library(ggplot2)

## Préparation des données
graph <- d %>%
  filter(id_partie %in% c(1:16),
         !(id_partie %in% c(3,4,11)),
         annee <= 8) %>%
  mutate(
    type_gain = ifelse(localisation == "village", "Village", "Pêche"),
    type_gain = factor(type_gain, levels = c("Village", "Pêche"))
  )

## Gains par partie, tour et type de localisation
gains <- graph %>%
  group_by(id_partie, annee, type_gain) %>%
  summarise(
    gains = sum(quantite, na.rm = TRUE),
    .groups = "drop"
  )

## Pénibilité totale par partie et tour
pen <- graph %>%
  group_by(id_partie, annee) %>%
  summarise(
    penibilite = sum(penibilite, na.rm = TRUE),
    .groups = "drop"
  )

## Facteur de mise à l'échelle
coef <- max(gains$gains) / max(pen$penibilite)

lab_parties <- c(
  "5"  = "P5N",
  "6"  = "P6N",
  "7"  = "P7N",
  "8"  = "P8N",
  "9"  = "P9F",
  "10" = "P10F",
  "12" = "P12F",
  "13" = "P13F",
  "14" = "P14F",
  "15" = "P15F",
  "16" = "P16F"
)
## Figure
ggplot() +
  
  ## Gains
  geom_col(
    data = gains,
    aes(x = annee,
        y = gains,
        fill = type_gain),
    width = 0.8
  ) +
  
  scale_fill_manual(
    name = "Origine des gains :",
    values = c(
      "Village" = "#4daf4a",
      "Pêche"   = "#377eb8"
    )
  ) +
  
  ## Pénibilité
  geom_line(
    data = pen,
    aes(x = annee,
        y = penibilite,
        colour = "Pénibilité",
        group = 1),
    linewidth = 0.9
  ) +
  
  geom_point(
    data = pen,
    aes(x = annee,
        y = penibilite,
        colour = "Pénibilité"),
    size = 2
  ) +
  
  facet_wrap(
    ~id_partie,
    ncol = 4,
    labeller = labeller(id_partie = lab_parties)
  ) +
  
  scale_colour_manual(
    name = "",
    values = c("Pénibilité" = "black")
  ) +
  
  scale_x_continuous(breaks = 1:8) +
  
  labs(
    x = "Tour de jeu",
    y = "Nombre de gains"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +

  theme_classic() +
  
  theme(
    legend.position = "bottom",
    
    ## Cadre autour de chaque graphique
    panel.border = element_rect(
      colour = "gray",
      fill = NA,
      linewidth = 0.5
    ),
    
    ## Plus de fond gris autour du nom de la partie
    strip.background = element_blank(),
    
    ## Texte des facettes
    strip.text = element_text(
      face = "bold",
      size = 10
    )
  )





































#######################################
# visites du village

visites <- graph %>%
  mutate(
    village = localisation == "village",
    site = ifelse(id_partie <= 8, "Niodior", "Falia")
  ) %>%
  group_by(id_partie, site, annee) %>%
  summarise(
    prop_village = mean(village),
    .groups = "drop"
  )

visites_moy <- visites %>%
  group_by(site, annee) %>%
  summarise(
    moyenne = mean(prop_village),
    se = sd(prop_village)/sqrt(n()),
    .groups = "drop"
  )































































######################################################################################
## FIGURE profils selon effort de pêche
######################################################################################
library(patchwork)
library(ggplot2)
library(scales)

cols_villages <- c(
  "Falia" = "#6A51A3",
  "Niodior" = "#E76FAD",
  "sma_aleatoire" = "#5FC2FC"
)

cols_profils <- c(
  "Low fishing effort" = "#D9D9D9",
  "Intermediate fishing effort" = "#8C8C8C",
  "High fishing effort" = "#252525"
)

p1 <- ggplot(freq_pecheuse, aes(nb_sorties)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "white") +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "A. Distribution of fishing effort",
    x = "Fishing trips (0–8)",
    y = "Number of fishers"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  ) +
  theme(
    plot.title = element_text(face = "bold")
  )

p2 <- ggplot(freq_pecheuse,
             aes(nb_sorties, fill = village)) +
  geom_histogram(binwidth = 1,
                 position = "fill",
                 color = "white") +
  scale_fill_manual(values = cols_villages) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "B. Fishing effort by village",
    x = "Fishing trips",
    y = "Proportion of fishers"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  ) 

freq_pecheuse$village <- trimws(freq_pecheuse$village)

freq_pecheuse$village <- factor(freq_pecheuse$village,
                                levels = names(cols_villages))

freq_pecheuse$profil <- factor(
  km$cluster,
  levels = c(1, 3, 2),   # ordre des centres : 0.5 ; 4.36 ; 6.89
  labels = c(
    "Low fishing effort",
    "Intermediate fishing effort",
    "High fishing effort"
  )
)

p3 <- ggplot(freq_pecheuse,
             aes(nb_sorties, fill = profil)) +
  geom_histogram(binwidth = 1,
                 color = "white",
                 position = "stack") +
  scale_fill_manual(values = cols_profils) +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "A. Distribution of fishing effort",
    x = "Number of fishing trips during the game",
    y = "Number of fishers",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  )

p4 <- ggplot(freq_pecheuse,
             aes(nb_sorties, fill = profil)) +
  geom_histogram(binwidth = 1,
                 position = "fill",
                 color = "white") +
  facet_wrap(~village) +
  scale_fill_manual(values = cols_profils) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "B. Behavioral profiles by village",
    x = "Fishing trips",
    y = "Proportion within village"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  ) +
  scale_x_continuous(breaks = 0:8) +
  guides(fill = "none")

final_fig <- ( p3 | p4) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold")
  )

final_fig































## avec proportion de chaque profils dans les villages
library(dplyr)

freq_pecheuse <- freq_pecheuse %>%
  mutate(
    profil = case_when(
      nb_sorties <= 2 ~ "Low fishing effort",
      nb_sorties <= 5 ~ "Intermediate fishing effort",
      nb_sorties <= 8 ~ "High fishing effort"
    ),
    profil = factor(
      profil,
      levels = c(
        "Low fishing effort",
        "Intermediate fishing effort",
        "High fishing effort"
      )
    )
  )

prop_profils <- freq_pecheuse %>%
  dplyr::count(village, profil) %>%
  dplyr::group_by(village) %>%
  dplyr::mutate(prop = n / sum(n))

p3 <- ggplot(freq_pecheuse,
             aes(nb_sorties, fill = profil)) +
  geom_histogram(binwidth = 1,
                 color = "white",
                 position = "stack") +
  scale_fill_manual(
    values = cols_profils,
    labels = c(
      "Low fishing effort" = "Faible effort de pêche",
      "Intermediate fishing effort" = "Effort de pêche intermédiaire",
      "High fishing effort" = "Fort effort de pêche"
    )
  ) +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "A. Distribution de l'effort de pêche",
    x = "Nombre de sorties de pêche pendant le jeu",
    y = "Nombre de pêcheuses",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  )

p4 <- ggplot(prop_profils,
             aes(x = village,
                 y = prop,
                 fill = profil)) +
  geom_col(color = "white") +
  scale_fill_manual(
    values = cols_profils,
    labels = c(
      "Low fishing effort" = "Faible effort de pêche",
      "Intermediate fishing effort" = "Effort de pêche intermédiaire",
      "High fishing effort" = "Fort effort de pêche"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "B. Profils d'effort de pêche selon le village",
    x = "Village",
    y = "Proportion de pêcheuses",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(b = 8)
    )
  )

n_parties <- length(unique(prop_partie$id_partie))

final_fig <- (p3 | p4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = paste0("n = ", n_parties)
  ) &
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 1,
      size = 11,
      margin = margin(t = -5, r = 5, b = 0)
    )
  )

final_fig
























## Les types de profils au sein de chaque partie
prop_partie <- freq_pecheuse %>%
  dplyr::count(id_partie, profil) %>%
  dplyr::group_by(id_partie) %>%
  dplyr::mutate(prop = n / sum(n))

ggplot(prop_partie,
       aes(x = factor(id_partie),
           y = prop,
           fill = profil)) +
  geom_col(color = "white") +
  scale_fill_manual(values = cols_profils) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Behavioral profiles by game",
    x = "Game",
    y = "Proportion of fishers",
    fill = NULL
  ) +
  theme_classic()








## même avec les parties non conformes
library(dplyr)

freq_partie <- data %>%
  mutate(sortie_peche = localisation %in% c("jaune", "vert", "rouge")) %>%
  group_by(id_partie, nom_joueuse, village) %>%
  summarise(
    nb_sorties = sum(sortie_peche),
    .groups = "drop"
  )

freq_partie <- freq_partie %>%
  mutate(
    profil = case_when(
      nb_sorties <= 2 ~ "Low fishing effort",
      nb_sorties <= 5 ~ "Intermediate fishing effort",
      TRUE ~ "High fishing effort"
    ),
    profil = factor(
      profil,
      levels = c(
        "Low fishing effort",
        "Intermediate fishing effort",
        "High fishing effort"
      )
    )
  )

prop_partie <- freq_partie %>%
  dplyr::filter(id_partie %in% 1:16) %>%
  dplyr::group_by(id_partie, profil) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::group_by(id_partie) %>%
  dplyr::mutate(prop = n / sum(n))

p_sessions <- ggplot(prop_partie,
                     aes(x = factor(id_partie),
                         y = prop,
                         fill = profil)) +
  geom_col(color = "white") +
  scale_fill_manual(
    values = cols_profils,
    labels = c(
      "Low fishing effort" = "Faible effort de pêche",
      "Intermediate fishing effort" = "Effort de pêche intermédiaire",
      "High fishing effort" = "Fort effort de pêche"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "C. Répartition des profils dans les sessions de jeu",
    x = "Session de jeu",
    y = "Proportion de pêcheuses",
    fill = NULL,
    caption = paste0("n = ", length(unique(prop_partie$id_partie)))
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(
      hjust = 1,
      size = 11,
      margin = margin(t = 5)
    )
  )

p_sessions































## lien entre diversité des profils et réussite dans la session
reussite_partie <- data %>%
  filter(
    id_partie %in% 3:16,
    id_partie != 4,
    id_partie != 3,
    id_partie != 11,
    nom_personnage != "noire"
  ) %>%
  group_by(id_partie) %>%
  summarise(
    gains_totaux = sum(quantite, na.rm = TRUE),
    reussite = gains_totaux >= 80,
    .groups = "drop"
  )

diversite_profils <- freq_partie %>%
  filter(id_partie %in% 1:16) %>%
  group_by(id_partie) %>%
  summarise(
    nb_profils = n_distinct(profil),
    .groups = "drop"
  )

analyse_profils <- reussite_partie %>%
  left_join(diversite_profils, by = "id_partie")

n_sessions <- length(unique(analyse_profils$id_partie))

ggplot(analyse_profils,
       aes(x = factor(nb_profils),
           y = gains_totaux)) +
  geom_boxplot(fill = "grey80") +
  geom_jitter(width = 0.1,
              size = 3) +
  geom_hline(yintercept = 80,
             linetype = "dashed",
             color = "red") +
  labs(
    x = "Nombre de profils d'effort présents dans la session",
    y = "Gains totaux de la session",
    title = "Lien entre diversité des profils et réussite collective",
    caption = paste0("n = ", n_sessions)
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(
      hjust = 1,
      size = 11,
      margin = margin(t = 5)
    )
  )

kruskal.test(
  gains_totaux ~ nb_profils,
  data = analyse_profils
)



























































## changement dans pratiques de pêche avant et après le tour 4
# avec les parties restriction max 

## Sélection des parties et création des variables
dd2 <- dd %>%
  filter(
    !id_partie %in% c(1, 2, 3, 4, 8, 11, 16)
  ) %>%
  mutate(
    peche = localisation %in% c("jaune", "vert", "rouge"),
    periode = ifelse(annee <= 4, "avant", "apres")
  )

## Vérification des parties conservées
sort(unique(dd2$id_partie))

## Vérification du nombre de personnages par partie
dd2 %>%
  group_by(id_partie) %>%
  summarise(
    n_personnages = n_distinct(nom_personnage),
    .groups = "drop"
  )

## Calcul de la fréquence de pêche par personnage et par période
freq_peche <- dd2 %>%
  group_by(id_partie, nom_personnage, periode) %>%
  summarise(
    nb_tours = n(),
    nb_peche = sum(peche),
    prop_peche = mean(peche),
    .groups = "drop"
  )

## Vérification : une seule ligne par personnage et par période
freq_peche %>%
  dplyr::count(id_partie, nom_personnage, periode)

## Passage au format large
freq_peche_wide <- freq_peche %>%
  select(id_partie, nom_personnage, periode, prop_peche) %>%
  pivot_wider(
    names_from = periode,
    values_from = prop_peche
  )

## Vérification : 5 personnages par partie
freq_peche_wide %>%
  group_by(id_partie) %>%
  summarise(
    n_personnages = n(),
    .groups = "drop"
  )

## Test de Wilcoxon apparié
wilcox.test(
  freq_peche_wide$avant,
  freq_peche_wide$apres,
  paired = TRUE
)

## Moyennes avant / après
freq_peche_wide %>%
  summarise(
    moyenne_avant = mean(avant, na.rm = TRUE),
    moyenne_apres = mean(apres, na.rm = TRUE),
    difference = moyenne_apres - moyenne_avant
  )

## Graphique
ggplot(freq_peche,
       aes(x = periode, y = prop_peche)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  theme_classic() +
  labs(
    x = "",
    y = "Proportion de visites en pêche"
  )






















































# Evolution des visites des différentes zones au cours du temps

library(dplyr)
library(ggplot2)

visites <- d %>%
  filter(
    annee <= 8,
    localisation %in% c("jaune","vert","rouge","village")
  ) %>%
  group_by(annee, localisation) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

ggplot(visites,
       aes(x = annee,
           y = n,
           colour = localisation)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    x = "Round",
    y = "Number of visits"
  )






















# Evolution des captures selon la zone

captures <- d %>%
  filter(
    annee <= 8,
    localisation %in% c("jaune","vert","rouge","village")
  ) %>%
  group_by(annee, localisation) %>%
  summarise(
    gain_moyen = mean(quantite),
    .groups = "drop"
  )

ggplot(captures,
       aes(annee,
           gain_moyen,
           colour = localisation)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    x = "Round",
    y = "Mean harvest"
  )
























# Test du retour dans la même zone après un bon succès

library(dplyr)

learn <- d %>%
  arrange(id_partie,
          nom_personnage,
          annee) %>%
  group_by(id_partie, nom_personnage) %>%
  mutate(
    succes_precedent = lag(quantite),
    zone_precedente = lag(localisation),
    retour_meme_zone = localisation == zone_precedente
  ) %>%
  ungroup() %>%
  filter(!is.na(succes_precedent))





# LMM (ou plutôt GLMM car la réponse est binaire)
library(lme4)

m1 <- glmer(
  retour_meme_zone ~ succes_precedent +
    (1 | id_partie),
  data = learn,
  family = binomial
)

summary(m1)

summary(m1)

anova(m1)

drop1(m1, test = "Chisq")





# vérification visuelle

learn %>%
  mutate(
    succes_cat = cut(
      succes_precedent,
      breaks = c(-Inf,2,4,6,Inf)
    )
  ) %>%
  group_by(succes_cat) %>%
  summarise(
    p_retour = mean(retour_meme_zone)
  ) %>%
  ggplot(aes(succes_cat, p_retour)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(
    x = "Previous harvest",
    y = "Probability of returning to the same zone"
  )






















# une mauvaise capture augmente la probabilité de changer de zone ?
library(dplyr)
library(lme4)

learn2 <- learn %>%
  mutate(
    succes_cat = cut(
      succes_precedent,
      breaks = c(-Inf, 1, 2, 3, Inf),
      labels = c("0", "1", "2", "3")
    ),
    changement_zone = !retour_meme_zone
  )

m3 <- glmer(
  changement_zone ~ succes_cat +
    (1 | id_partie),
  data = learn2,
  family = binomial
)

summary(m3)
drop1(m3, test = "Chisq")

learn2 %>%
  group_by(succes_cat) %>%
  summarise(
    p_changement = mean(changement_zone),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(succes_cat, p_changement)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Previous harvest",
    y = "Probability of changing fishing area"
  ) +
  theme_classic()




























