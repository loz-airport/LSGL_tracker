# Suivi des vols de l'aéroport de Lausanne 🦁✈️

![Bannière](LAFT_image.png)

Un projet de collecte de données qui suit tous les vols arrivant et partant de l'aéroport de Lausanne-Blécherette (LSGL) à l'aide de l'API OpenSky Network. Le projet collecte automatiquement les données historiques de vol, y compris les positions des aéronefs, les indicatifs d'appel et les trajectoires de vol.

Ce projet a été devéloppé en R originellement. Il est maintenu, vibe codé et documenté à l'aide d’Antigravity & Gemini.  

## 📊 Ce que fait ce repo

Ce dépôt :
- **Récupère automatiquement** les données de vol depuis l'[API OpenSky Network](https://opensky-network.org) pour l'aéroport LSGL
- **S'exécute quotidiennement** via GitHub Actions pour collecter et mettre à jour les enregistrements de vols historiques
- **Stocke** à la fois les informations de base sur les vols (arrivées/départs) et les données de position détaillées (vecteurs d'état)
- **Maintient** un ensemble de données historiques complet remontant à septembre 2022

## 🗂️ Structure du dépôt

```
.
├── data_raw/                  # Toutes les données de vol collectées (fichiers CSV)
│   ├── bl_arr_YYYY_MM_DD.csv  # Vols d'arrivée quotidiens
│   ├── bl_dep_YYYY_MM_DD.csv  # Vols de départ quotidiens
│   ├── bl_arr_SV_YYYY_MM_DD.csv  # Vecteurs d'état d'arrivée quotidiens (positions GPS)
│   ├── bl_dep_SV_YYYY_MM_DD.csv  # Vecteurs d'état de départ quotidiens
│   ├── bl_arr_all.csv         # Arrivées consolidées
│   ├── bl_dep_all.csv         # Départs consolidés
│   ├── bl_arr_SV_all.csv      # Vecteurs d'état d'arrivée consolidés
│   └── bl_dep_SV_all.csv      # Vecteurs d'état de départ consolidés
├── R/                         # Scripts R pour la collecte de données
│   ├── _helper.R              # Fonctions auxiliaires pour l'API OpenSky
│   ├── save_historical_osn.R  # Script principal pour récupérer et sauvegarder les données
│   └── getAndViz.Rmd          # Notebook d'exploration et de visualisation des données
├── .github/workflows/         # Collecte de données automatisée
│   └── main.yml               # Workflow GitHub Actions (s'exécute deux fois par jour)
└── README.md                  # Ce fichier
```

## 📁 Structure des données

### Fichiers de données de vol

#### Arrivées et Départs (`bl_arr_*.csv` et `bl_dep_*.csv`)

Informations de base sur le vol pour chaque arrivée ou départ.

**Colonnes :**
| Colonne | Type | Description |
|--------|------|-------------|
| `ICAO24` | Chaîne | Identifiant unique d'aéronef ICAO 24-bit (ex. : `4b4326`) |
| `call_sign` | Chaîne | Indicatif d'appel du vol (ex. : `HBZLE`) |
| `departure_time` | DateTime | Heure réelle de départ (UTC) |
| `departure_date` | Date | Date de départ |
| `arrival_time` | DateTime | Heure réelle d'arrivée (UTC) |
| `arrival_date` | Date | Date d'arrivée |
| `departure_airport_ICAO` | Chaîne | Code ICAO de l'aéroport de départ (ex. : `LSGL`) |
| `destination_airport_ICAO` | Chaîne | Code ICAO de l'aéroport de destination |
| `id` | Chaîne | Identifiant unique de vol : `{ICAO24}_{departure_time}` |

**Exemple :**
```csv
ICAO24,call_sign,departure_time,departure_date,arrival_time,arrival_date,departure_airport_ICAO,destination_airport_ICAO,id
4b4326,HBZLE,2022-09-10T07:12:33Z,2022-09-10,2022-09-10T08:11:58Z,2022-09-10,LSZM,LSGL,4b4326_2022-09-10 09:12:33
```

#### Vecteurs d'état (`bl_arr_SV_*.csv` et `bl_dep_SV_*.csv`)

Données de position GPS échantillonnées à intervalles réguliers (généralement toutes les 3 minutes) pendant chaque vol.

**Colonnes :**
| Colonne | Type | Description |
|--------|------|-------------|
| `ICAO24` | Chaîne | Identifiant de l'aéronef |
| `longitude` | Flottant | Longitude GPS (degrés décimaux) |
| `latitude` | Flottant | Latitude GPS (degrés décimaux) |
| `requested_time` | Entier | Horodatage Unix de la mesure de position |
| `geo_altitude` | Flottant | Altitude en mètres (barométrique) |
| `velocity` | Flottant | Vitesse au sol en m/s |
| `special_purpose_indicator` | Booléen | Indicateur d'usage spécial |
| `origin_country` | Chaîne | Pays d'immatriculation de l'aéronef |
| `id` | Chaîne | Identifiant de vol (correspond aux fichiers de données de vol) |
| `arrival_date` | Date | Date d'arrivée du vol |
| `departure_date` | Date | Date de départ du vol |

**Exemple :**
```csv
ICAO24,longitude,latitude,requested_time,geo_altitude,velocity,special_purpose_indicator,origin_country,id,arrival_date,departure_date
4b43ad,6.22727,46.38366,1666246299,NA,0,FALSE,NA,4b43ad_2022-10-20 08:11:39,2022-10-20,2022-10-20
```

#### Fichiers de métadonnées (`aircraft_metadata.csv` et `airport_metadata.csv`)

Métadonnées détaillées pour les aéronefs et les aéroports rencontrés dans les données de vol (mises à jour pour les 30 derniers jours).

**Métadonnées des aéronefs (`aircraft_metadata.csv`) :**
| Colonne | Type | Description |
|--------|------|-------------|
| `ICAO24` | Chaîne | Identifiant unique d'aéronef ICAO 24-bit |
| `model` | Chaîne | Modèle d'aéronef (ex. : `Pilatus PC-12/47E`) |
| `origin_country` | Chaîne | Pays d'immatriculation |
| `photo_url` | Chaîne | URL vers une photo miniature de l'aéronef (provenant de Planespotters.net) |

**Métadonnées des aéroports (`airport_metadata.csv`) :**
| Colonne | Type | Description |
|--------|------|-------------|
| `ICAO` | Chaîne | Code aéroportuaire ICAO (ex. : `LSGL`) |
| `IATA` | Chaîne | Code aéroportuaire IATA (ex. : `QLS`) |
| `name` | Chaîne | Nom de l'aéroport |
| `city` | Chaîne | Ville desservie par l'aéroport |
| `country` | Chaîne | Pays où se trouve l'aéroport |
| `longitude` | Flottant | Longitude de l'aéroport |
| `latitude` | Flottant | Latitude de l'aéroport |
| `altitude` | Flottant | Altitude de l'aéroport (mètres) |

### Convention de nommage des fichiers

- `bl_arr_YYYY_MM_DD.csv` - Arrivées pour une date spécifique
- `bl_dep_YYYY_MM_DD.csv` - Départs pour une date spécifique
- `bl_arr_SV_YYYY_MM_DD.csv` - Vecteurs d'état pour les arrivées à une date spécifique
- `bl_dep_SV_YYYY_MM_DD.csv` - Vecteurs d'état pour les départs à une date spécifique
- Fichiers `*_all.csv` - Données consolidées sur toutes les dates

## 🚀 Utilisation des données

### Démarrage rapide avec R

```r
# Charger les données d'arrivée
library(readr)
arrivals <- read_csv("data_raw/bl_arr_all.csv")

# Charger les données de départ
departures <- read_csv("data_raw/bl_dep_all.csv")

# Charger les vecteurs d'état pour les arrivées
arrival_positions <- read_csv("data_raw/bl_arr_SV_all.csv")
```

### Démarrage rapide avec Python

```python
import pandas as pd

# Charger les données d'arrivée
arrivals = pd.read_csv("data_raw/bl_arr_all.csv")

# Charger les données de départ
departures = pd.read_csv("data_raw/bl_dep_all.csv")

# Charger les vecteurs d'état pour les arrivées
arrival_positions = pd.read_csv("data_raw/bl_arr_SV_all.csv")
```

### Cas d'utilisation courants

#### 1. Compter les vols par jour
```r
library(dplyr)

daily_traffic <- departures %>%
  group_by(departure_date) %>%
  summarize(
    num_flights = n(),
    unique_aircraft = n_distinct(ICAO24)
  )
```

#### 2. Trouver les routes les plus courantes
```r
popular_routes <- departures %>%
  count(departure_airport_ICAO, destination_airport_ICAO) %>%
  arrange(desc(n))
```

#### 3. Tracer les trajectoires de vol
```r
library(ggplot2)

# Obtenir les vecteurs d'état pour un vol spécifique
flight_path <- arrival_positions %>%
  filter(id == "4b43ad_2022-10-20 08:11:39")

# Tracer la trajectoire
ggplot(flight_path, aes(x = longitude, y = latitude)) +
  geom_path() +
  geom_point() +
  theme_minimal() +
  labs(title = "Trajectoire de vol", x = "Longitude", y = "Latitude")
```

## 🔄 Mises à jour des données

Les données sont mises à jour automatiquement **deux fois par jour** à :
- 04:27 UTC
- 08:27 UTC

via le workflow GitHub Actions. Le workflow récupère les données des 30 derniers jours et met à jour les dates manquantes.

## 🔐 Exécution locale

Pour exécuter les scripts de collecte de données localement :

1. **Installer les dépendances R :**
   ```r
   # Installer renv pour la gestion des paquets
   install.packages("renv")
   renv::restore()
   ```

2. **Configurer les identifiants OpenSky :**
   Créez un fichier `.Renviron` à la racine du projet :
   ```bash
   OPENSKY_USR="votre_nom_utilisateur"
   OPENSKY_PWD="votre_mot_de_passe"
   ```
   > **Note :** Inscrivez-vous pour un compte gratuit sur [OpenSky Network](https://opensky-network.org) pour accéder aux données historiques

3. **Exécuter le script de collecte de données :**
   ```r
   Rscript R/save_historical_osn.R
   ```

> ⚠️ **Attention :** La collecte complète des données avec les vecteurs d'état peut prendre plusieurs heures pour 30 jours de données.

## 📖 À propos de LSGL

**L'aéroport de Lausanne-Blécherette** (ICAO : LSGL) est un petit aéroport régional en Suisse principalement utilisé pour :
- L'aviation générale
- L'instruction en vol
- Les aéronefs privés
- De petits vols commerciaux occasionnels

Compte tenu de sa nature, le volume de vol est relativement faible par rapport aux grands aéroports (typiquement 5 à 20 vols par jour).

## 🔗 Liens utiles

- [Profil de l'aéroport OpenSky Network pour LSGL](https://opensky-network.org/airport-profile?icao=LSGL)
- [Documentation de l'API OpenSky Network](https://openskynetwork.github.io/opensky-api/)
- [Paquet R openSkies](https://CRAN.R-project.org/package=openSkies)

## 📝 Licence

Licence GPL-3.0 - Voir le fichier [LICENSE](LICENSE) pour plus de détails

## 🤝 Contribution

Il s'agit principalement d'un projet personnel de collecte de données, mais les problèmes (issues) et les suggestions sont les bienvenus !

---

**Source de données :** [OpenSky Network](https://opensky-network.org) - Le réseau OpenSky est une association à but non lucratif qui fournit des données de trafic aérien ouvertes à des fins de recherche et non commerciales.
