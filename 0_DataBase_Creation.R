### 0. IMPORTING DATASETS ----

library(dplyr)
library(stringr)
library(sf)
library(stringi)

Camera_1987 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1987/Camera-19870614.txt", sep=";")
Senato_1987 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/1987/Senato-19870614.txt", sep=";")

Camera_1992 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1992/camera-19920405.txt", sep=";")
Senato_1992 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/1992/senato-19920405.txt", sep=";")

Camera_1994_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1994/camera-19940327_Proporzionale.txt", sep=";")
Camera_1994_VA <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1994/Camera_19940327_Uninom_Scrutini.txt", sep=";")
Senato_1994 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/1994/senato-19940327.txt", sep=";")

Camera_1996_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1996/camera-19960421_Proporzionale.txt", sep=";")
Camera_1996_VA <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/1996/Camera_19960421_Uninom_Scrutini.txt", sep=";")
Senato_1996 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/1996/senato-19960421.txt", sep=";")

Camera_2001_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2001/camera-20010513_Proporzionale.txt", sep=";")
Camera_2001_VA <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2001/Camera_20010513_Uninom_Scrutini.txt", sep=";")
Senato_2001 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2001/senato-20010513.txt", sep=";")

Camera_2006_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2006/camera_italia-20060409.txt", sep=";", fileEncoding = "latin1")
Camera_2006_VA_Trentino <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2006/camera_vaosta-20060409.txt")
Senato_2006_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2006/senato_italia-20060409.txt", sep=";")
Senato_2006_VA_Trentino <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2006/senato_vaosta_trentino-20060409.txt", sep = ";")

Senato_2008_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2008/senato_italia-20080413.txt", sep=";")
Senato_2008_VA_Trentino <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2008/senato_vaosta_trentino_20080413.txt", sep = ";")
Camera_2008_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2008/camera_italia-20080413.txt", sep=";")
Camera_2008_VA_Trentino <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2008/camera_vaosta-20080413.txt")

Senato_2013_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2013/senato_italia-20130224.txt", sep=";")
Senato_2013_VA_Trentino <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2013/senato_vaosta_trentino-20130224.txt", sep = ";")
Camera_2013_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2013/camera_italia-20130224.txt", sep=";")
Camera_2013_VA_Trentino <- read.csv2("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2013/camera_vaosta-20130224.txt")

Senato_2018 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2018/Senato2018_livComune.txt", sep=";", fileEncoding = "latin1")
Camera_2018 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2018/Camera2018_livComune.txt", sep=";", fileEncoding = "latin1")

# --- Fix 2018 Camera ---
Camera_2018 <- Camera_2018 %>%
  # Safely renames CIRCOSCRIZIONE to REGIONE only if it actually exists
  rename(any_of(c(REGIONE = "CIRCOSCRIZIONE", REGIONE = "CIRC.REG"))) %>%
  mutate(
    # If the region contains Aosta, overwrite VOTI_LISTA with VOTI_CANDIDATO
    VOTI_LISTA = case_when(
      grepl("AOSTA", REGIONE, ignore.case = TRUE) ~ as.character(VOTI_CANDIDATO),
      TRUE ~ as.character(VOTI_LISTA)
    ),
    # Safely convert the final column to integer
    VOTI_LISTA = as.integer(VOTI_LISTA)
  )

# --- Fix 2018 Senato ---
Senato_2018 <- Senato_2018 %>%
  rename(any_of(c(REGIONE = "CIRCOSCRIZIONE", REGIONE = "CIRC.REG"))) %>%
  mutate(
    VOTI_LISTA = case_when(
      grepl("AOSTA", REGIONE, ignore.case = TRUE) ~ as.character(VOTI_CANDIDATO),
      TRUE ~ as.character(VOTI_LISTA)
    ),
    VOTI_LISTA = as.integer(VOTI_LISTA)
  )

Camera_2022_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2022/Camera_Italia_LivComune.csv", sep=";")
Camera_2022_VA_Trentino <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/CAMERA/2022/Camera_VAosta_LivComune.csv", sep=";")
Senato_2022_Italia <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2022/Senato_Italia_LivComune.csv", sep=";")
Senato_2022_VA_Trentino <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2022/Senato_VAosta&Trentino_LivComune.csv", sep=";")
# we need file 'scrutini' for matching the numbers of voters
scrutini_2022 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Dataset/SENATO/2022/Senato_VAosta&Trentino_livComune_Scrutini.csv", sep=";")


### 1. JOINING 'VALLE D'AOSTA' e 'TRENTINO' -----

### 1994

Camera_1994_Italia_clean <- Camera_1994_Italia %>%
  # Safely rename VOTILISTA to VOTI_LISTA only if VOTILISTA exists
  rename(any_of(c(VOTI_LISTA = "VOTILISTA"))) %>%
  mutate(
    REGIONE = CIRCOSCRIZIONE, 
    PROVINCIA = NA_character_, # FIX: Initialize missing column to match Valle d'Aosta
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI, ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA)

Camera_1994_VA_clean <- Camera_1994_VA %>%
  filter(grepl("AOSTA", circ, ignore.case = TRUE)) %>%
  mutate(
    COMUNE = toupper(comune),
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA",
    TIPO_ELEZIONE = "Camera",
    LISTA = "UNINOMINALE"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = elettoritot, 
         ELETTORI_MASCHI = elettorimaschi, 
         VOTANTI = NUMVOTANTITOTALI, 
         VOTI_LISTA = VOTIVALIDI, 
         LISTA)

Camera_1994 <- bind_rows(Camera_1994_Italia_clean, Camera_1994_VA_clean)

### 1996

Camera_1996_Italia_clean <- Camera_1996_Italia %>%
  rename(any_of(c(VOTI_LISTA = "VOTILISTA"))) %>%
  mutate(
    REGIONE = CIRCOSCRIZIONE, 
    PROVINCIA = NA_character_, 
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI, ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA)

Camera_1996_VA_clean <- Camera_1996_VA %>%
  filter(grepl("AOSTA", circ, ignore.case = TRUE)) %>%
  mutate(
    COMUNE = toupper(comune),
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA",
    TIPO_ELEZIONE = "Camera",
    LISTA = "UNINOMINALE"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = elettoritot, 
         ELETTORI_MASCHI = elettorimaschi, 
         VOTANTI = NUMVOTANTITOTALI, 
         VOTI_LISTA = VOTIVALIDI, 
         LISTA)

Camera_1996 <- bind_rows(Camera_1996_Italia_clean, Camera_1996_VA_clean)


### 2001

Camera_2001_Italia_clean <- Camera_2001_Italia %>%
  rename(any_of(c(VOTI_LISTA = "VOTILISTA"))) %>%
  mutate(
    REGIONE = CIRCOSCRIZIONE, 
    PROVINCIA = NA_character_, 
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI, ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA)

Camera_2001_VA_clean <- Camera_2001_VA %>%
  filter(grepl("AOSTA", circ, ignore.case = TRUE)) %>%
  mutate(
    COMUNE = toupper(comune),
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA",
    TIPO_ELEZIONE = "Camera",
    LISTA = "UNINOMINALE"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = elettoritot, 
         ELETTORI_MASCHI = elettorimaschi, 
         VOTANTI = NUMVOTANTITOTALI, 
         VOTI_LISTA = VOTIVALIDI, 
         LISTA)

Camera_2001 <- bind_rows(Camera_2001_Italia_clean, Camera_2001_VA_clean)

### 2006

Camera_2006_Italia_clean <- Camera_2006_Italia %>%
  mutate(
    REGIONE = CIRCOSCRIZIONE, # The national file uses CIRCOSCRIZIONE instead of REGIONE
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI,  ELETTORI_MASCHI, VOTANTI, VOTI_LISTA = VOTILISTA, LISTA)

Camera_2006_VA_clean <- Camera_2006_VA_Trentino %>%
  mutate(
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA", # Aosta is a single province
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = ELETTORI_TOTALI, ELETTORI_MASCHI, VOTANTI = VOTANTI_TOTALI, 
         VOTI_LISTA, LISTA)

Camera_2006 <- bind_rows(Camera_2006_Italia_clean, Camera_2006_VA_clean)

Senato_2006_Italia_clean <- Senato_2006_Italia %>%
  mutate(TIPO_ELEZIONE = "Senato") %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = ELETTORI_TOTALI,  ELETTORI_MASCHI, VOTANTI = VOTANTI_TOTALI,
         VOTI_LISTA, LISTA)

Senato_2006_VA_Trentino_clean <- Senato_2006_VA_Trentino %>%
  mutate(
    PROVINCIA = case_when(
      REGIONE == "VALLE D'AOSTA" ~ "AOSTA",
      # Precise mapping for Trentino-Alto Adige districts
      REGIONE == "TRENTINO-ALTO ADIGE" & COLLEGIO %in% c("1 - ROVERETO", "2 - TRENTO", "3 - PERGINE VALSUGANA") ~ "TRENTO",
      REGIONE == "TRENTINO-ALTO ADIGE" & COLLEGIO %in% c("4 - BOLZANO", "5 - MERANO", "6 - BRESSANONE") ~ "BOLZANO",
      # Fallback: if names are slightly different, check for keywords
      REGIONE == "TRENTINO-ALTO ADIGE" & grepl("ROVERETO|TRENTO|PERGINE", COLLEGIO) ~ "TRENTO",
      REGIONE == "TRENTINO-ALTO ADIGE" & grepl("BOLZANO|MERANO|BRESSANONE", COLLEGIO) ~ "BOLZANO",
      TRUE ~ "SCONOSCIUTA" 
    ),
    TIPO_ELEZIONE = "Senato"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI,  ELETTORI_MASCHI, VOTANTI = VOTANTI_TOTALI,
         VOTI_LISTA, LISTA)


Senato_2006 <- bind_rows(Senato_2006_Italia_clean, Senato_2006_VA_Trentino_clean)

### 2008 

Camera_2008_Italia_clean <- Camera_2008_Italia %>%
  mutate(TIPO_ELEZIONE = "Camera") %>%
  select(COMUNE, PROVINCIA, REGIONE = CIRCOSCRIZIONE, TIPO_ELEZIONE, ELETTORI, 
         ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA)

Camera_2008_VA_clean <- Camera_2008_VA_Trentino %>%
  # CAMERA: We ONLY want Valle d'Aosta.
  mutate(
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA", 
    TIPO_ELEZIONE = "Camera",
    VOTI_LISTA = as.integer(as.numeric(VOTI_LISTA))
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, ELETTORI = ELETTORI_TOTALI, 
         ELETTORI_MASCHI, VOTANTI = VOTANTI_TOTALI, VOTI_LISTA, LISTA)

Camera_2008 <- bind_rows(Camera_2008_Italia_clean, Camera_2008_VA_clean)

Senato_2008_Italia_clean <- Senato_2008_Italia %>%
  mutate(TIPO_ELEZIONE = "Senato") %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, ELETTORI, 
         ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA)

Senato_2008_VA_clean <- Senato_2008_VA_Trentino %>%
  # SENATO: We need BOTH regions. The PROVINCIA column already exists here.
  mutate(
    PROVINCIA = toupper(PROVINCIA), 
    TIPO_ELEZIONE = "Senato",
    VOTI_LISTA = as.integer(as.numeric(VOTI_LISTA))
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = ELETTORI_TOTALI, ELETTORI_MASCHI, 
         VOTANTI = VOTANTI_TOTALI, VOTI_LISTA, LISTA)

Senato_2008 <- bind_rows(Senato_2008_Italia_clean, Senato_2008_VA_clean)


### 2013 

# 1. Camera Italia
Camera_2013_Italia_clean <- Camera_2013_Italia %>%
  mutate(
    TIPO_ELEZIONE = "Camera",
    REGIONE = CIRCOSCRIZIONE # We create REGIONE from CIRCOSCRIZIONE here
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI, ELETTORI_MASCHI, 
         VOTANTI, VOTI_LISTA, LISTA)

# 2. Camera Valle d'Aosta
Camera_2013_VA_clean <- Camera_2013_VA_Trentino %>%
  # Filter ONLY using COLLEGIO since that's the only geo column present
  filter(grepl("AOSTA", COLLEGIO, ignore.case = TRUE)) %>%
  mutate(
    REGIONE = "VALLE D'AOSTA",
    PROVINCIA = "AOSTA", 
    TIPO_ELEZIONE = "Camera"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = ELETTORI_TOTALI, ELETTORI_MASCHI, 
         VOTANTI = VOTANTI_TOTALI, VOTI_LISTA, LISTA)

Camera_2013 <- bind_rows(Camera_2013_Italia_clean, Camera_2013_VA_clean)


# 3. Senato Italia
Senato_2013_Italia_clean <- Senato_2013_Italia %>%
  mutate(TIPO_ELEZIONE = "Senato") %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI, ELETTORI_MASCHI, 
         VOTANTI, VOTI_LISTA, LISTA)

# 4. Senato Valle d'Aosta & Trentino
Senato_2013_VA_clean <- Senato_2013_VA_Trentino %>%
  mutate(
    PROVINCIA = toupper(PROVINCIA), # The column is already there! Just standardize text.
    TIPO_ELEZIONE = "Senato"
  ) %>%
  select(COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, 
         ELETTORI = ELETTORI_TOTALI, ELETTORI_MASCHI, 
         VOTANTI = VOTANTI_TOTALI, VOTI_LISTA, LISTA)

Senato_2013 <- bind_rows(Senato_2013_Italia_clean, Senato_2013_VA_clean)


### 2022 

scrutini_clean <- scrutini_2022 %>%
  mutate(
    # Fix the mismatched region name so the join succeeds!
    REGIONE = if_else(REGIONE == "TRENTINO-ALTO ADIGE/SUDTIROL", "TRENTINO-ALTO ADIGE", REGIONE)
  ) %>%
  select(
    REGIONE, 
    COMUNE, 
    ELETTORI = NUMELETTORI, 
    ELETTORI_MASCHI = NUMELETTORIMASCHI, 
    VOTANTI = VOTANTITOT
  ) %>%
  distinct()

Camera_2022_Italia_clean <- Camera_2022_Italia %>%
  mutate(
    TIPO_ELEZIONE = "Camera",
    PROVINCIA = NA_character_ 
  ) %>%
  select(
    COMUNE, PROVINCIA, REGIONE = CIRC.REG, TIPO_ELEZIONE, 
    ELETTORI = ELETTORITOT, ELETTORI_MASCHI = ELETTORIM, 
    VOTANTI = VOTANTITOT, VOTI_LISTA = VOTILISTA, LISTA = DESCRLISTA
  )

Camera_2022_VA_clean <- Camera_2022_VA_Trentino %>%
  # 1. Safely filter using ONLY the REGIONE column
  filter(grepl("AOSTA", REGIONE, ignore.case = TRUE)) %>%
  
  # 2. Join with scrutini to get the voter numbers
  left_join(scrutini_clean, by = c("REGIONE", "COMUNE")) %>%
  
  # 3. Standardize columns
  mutate(
    TIPO_ELEZIONE = "Camera",
    PROVINCIA = "AOSTA", # Forced to Aosta since we filtered the rest out
    VOTI_LISTA = as.integer(as.numeric(TOTVOTI)) 
  ) %>%
  select(
    COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, ELETTORI, 
    ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA = CONTRASSEGNO
  )

Senato_2022_Italia_clean <- Senato_2022_Italia %>%
  mutate(
    TIPO_ELEZIONE = "Senato",
    PROVINCIA = NA_character_
  ) %>%
  select(
    COMUNE, PROVINCIA, REGIONE = CIRC.REG, TIPO_ELEZIONE, 
    ELETTORI = ELETTORITOT, ELETTORI_MASCHI = ELETTORIM, 
    VOTANTI = VOTANTITOT, VOTI_LISTA = VOTILISTA, LISTA = DESCRLISTA
  )

Senato_2022_VA_clean <- Senato_2022_VA_Trentino %>%
  left_join(scrutini_clean, by = c("REGIONE", "COMUNE")) %>%
  mutate(
    TIPO_ELEZIONE = "Senato",
    # SENATO: We need BOTH regions, so we keep the TRENTO/BOLZANO logic here
    PROVINCIA = case_when(
      REGIONE == "VALLE D'AOSTA" ~ "AOSTA",
      REGIONE == "TRENTINO-ALTO ADIGE" & grepl("TRENTO", COLLEGIO, ignore.case = TRUE) ~ "TRENTO",
      REGIONE == "TRENTINO-ALTO ADIGE" & grepl("BOLZANO", COLLEGIO, ignore.case = TRUE) ~ "BOLZANO",
      TRUE ~ NA_character_
    ),
    VOTI_LISTA = as.integer(as.numeric(TOTVOTI))
  ) %>%
  select(
    COMUNE, PROVINCIA, REGIONE, TIPO_ELEZIONE, ELETTORI, 
    ELETTORI_MASCHI, VOTANTI, VOTI_LISTA, LISTA = CONTRASSEGNO
  )

Camera_2022 <- bind_rows(Camera_2022_Italia_clean, Camera_2022_VA_clean)
Senato_2022 <- bind_rows(Senato_2022_Italia_clean, Senato_2022_VA_clean)

rm(Camera_2006_Italia, Camera_2006_Italia_clean, Camera_2006_VA_clean, Camera_2006_VA_Trentino,
   Camera_2008_Italia, Camera_2008_Italia_clean, Camera_2008_VA_clean, Camera_2008_VA_Trentino,
   Camera_2013_Italia, Camera_2013_Italia_clean, Camera_2013_VA_clean, Camera_2013_VA_Trentino,
   Camera_2022_Italia, Camera_2022_Italia_clean, Camera_2022_VA_clean, Camera_2022_VA_Trentino, 
   Senato_2006_Italia, Senato_2006_Italia_clean, Senato_2006_VA_Trentino_clean, Senato_2006_VA_Trentino,
   Senato_2008_Italia, Senato_2008_Italia_clean, Senato_2008_VA_clean, Senato_2008_VA_Trentino,
   Senato_2013_Italia, Senato_2013_Italia_clean, Senato_2013_VA_clean, Senato_2013_VA_Trentino,
   Senato_2022_Italia, Senato_2022_Italia_clean, Senato_2022_VA_clean, Senato_2022_VA_Trentino,
   scrutini_clean, scrutini_2022)

### 2. GEO-JOIN / MATCHING 'regioni' -> 'province' -> 'comuni' ----

clean_geo_names <- function(name_vector) {
  name_vector %>%
    toupper() %>%                                
    str_replace_all("'", " ") %>%                
    str_replace_all("-", " ") %>%    
    str_replace_all("/", " ") %>%   # THE MAGIC FIX FOR BILINGUAL PROVINCES
    stri_trans_general(id = "Latin-ASCII") %>%   # Aggressively strips all accents (e.g., CITTÀ -> CITTA)
    str_remove_all("[^A-Z ]") %>%                
    str_squish()                                 
}

### 2.1. Shapefile ISTAT (Comuni)
conf_comuni_91 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti1991/Com1991/Com1991_WGS84.shp")
conf_comuni_01 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti2001_g/Com2001_g/Com2001_g_WGS84.shp")
conf_comuni_06 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012006_g/Com01012006_g/Com01012006_g_WGS84.shp")
conf_comuni_08 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012008_g/Com01012008_g/Com01012008_g_WGS84.shp")
conf_comuni_13 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012013_g/Com01012013_g/Com01012013_g_WGS84.shp")
conf_comuni_18 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012018_g/Com01012018_g/Com01012018_g_WGS84.shp")
conf_comuni_22 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012022_g/Com01012022_g/Com01012022_g_WGS84.shp")

# Shapefile ISTAT (Province)
conf_province_91 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti1991/Prov1991/Prov1991_WGS84.shp")
conf_province_01 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti2001_g/Prov2001_g/Prov2001_g_WGS84.shp")
conf_province_06 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012006_g/Prov01012006_g/Prov01012006_g_WGS84.shp")
conf_province_08 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012008_g/Prov01012008_g/Prov01012008_g_WGS84.shp")
conf_province_13 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012013_g/Prov01012013_g/Prov01012013_g_WGS84.shp")
conf_province_18 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012018_g/ProvCM01012018_g/ProvCM01012018_g_WGS84.shp")

conf_province_18$DEN_PROV <- conf_province_18$DEN_PCM

conf_province_22 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012022_g/ProvCM01012022_g/ProvCM01012022_g_WGS84.shp")

# Shapefile ISTAT (Regioni)
conf_regioni_91 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti1991/Reg1991/Reg1991_WGS84.shp")
conf_regioni_01 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti2001_g/Reg2001_g/Reg2001_g_WGS84.shp")
conf_regioni_06 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012006_g/Reg01012006_g/Reg01012006_g_WGS84.shp")
conf_regioni_08 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012008_g/Reg01012008_g/Reg01012008_g_WGS84.shp")
conf_regioni_13 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012013_g/Reg01012013_g/Reg01012013_g_WGS84.shp")
conf_regioni_18 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012018_g/Reg01012018_g/Reg01012018_g_WGS84.shp")
conf_regioni_22 <- st_read("C:/Users/massi/OneDrive/Desktop/TESI/Confini/Limiti01012022_g/Reg01012022_g/Reg01012022_g_WGS84.shp")


# 1. Create REGIONE_CLEAN by cleaning CIRCOSCRIZIONE
Camera_1994 <- Camera_1994 %>%
  mutate(
    # Remove numbers and trailing spaces (e.g., "LOMBARDIA 1" -> "LOMBARDIA")
    REGIONE_CLEAN = str_remove_all(REGIONE, "[0-9]"),
    REGIONE_CLEAN = clean_geo_names(REGIONE_CLEAN),
    
    # Fix specific naming mismatches with ISTAT
    REGIONE_CLEAN = case_when(
      REGIONE_CLEAN == "ABRUZZI" ~ "ABRUZZO",
      REGIONE_CLEAN == "FRIULI VENEZIA GIULIA" ~ "FRIULI VENEZIA GIULIA", # Standard
      TRUE ~ REGIONE_CLEAN
    )
  )

Camera_1996 <- Camera_1996 %>%
  mutate(
    # Remove numbers and trailing spaces (e.g., "LOMBARDIA 1" -> "LOMBARDIA")
    REGIONE_CLEAN = str_remove_all(REGIONE, "[0-9]"),
    REGIONE_CLEAN = clean_geo_names(REGIONE_CLEAN),
    
    # Fix specific naming mismatches with ISTAT
    REGIONE_CLEAN = case_when(
      REGIONE_CLEAN == "ABRUZZI" ~ "ABRUZZO",
      REGIONE_CLEAN == "FRIULI VENEZIA GIULIA" ~ "FRIULI VENEZIA GIULIA", # Standard
      TRUE ~ REGIONE_CLEAN
    )
  )

################################################################################
### 2.2. 1987 - 1992 - 1994 - 1996 ---> confini_1991 
# 1. Build the 1991 Lookups 

library(dplyr)
library(stringr)
library(stringi)
library(sf)

# 1. The Robust Cleaner
clean_geo_names <- function(name_vector) {
  name_vector %>%
    toupper() %>%                                
    str_replace_all("'", " ") %>%                
    str_replace_all("-", " ") %>%                
    str_replace_all("/", " ") %>%   
    stri_trans_general(id = "Latin-ASCII") %>%   
    str_remove_all("[^A-Z ]") %>%                
    str_squish()                                 
}

# 2. Build the ISTAT Lookup with a matching PROVINCIA_CLEAN column
lookup_comuni_91 <- conf_comuni_91 %>%
  st_drop_geometry() %>% 
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  # Join with Province names to get the ISTAT province string
  left_join(
    conf_province_91 %>% 
      st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>%
      select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  select(COMUNE_CLEAN, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM, PRO_COM_T) %>%
  distinct()

# 3. Clean, Patch, and Join Camera_1987
Camera_1987 <- Camera_1987 %>%
  mutate(
    COMUNE_CLEAN = clean_geo_names(COMUNE),
    PROVINCIA_CLEAN = clean_geo_names(PROVINCIA),
    # patch provinces
      PROVINCIA_CLEAN = case_when(
        PROVINCIA_CLEAN == "AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
        PROVINCIA_CLEAN == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
        PROVINCIA_CLEAN == "BOLZANO" ~ "BOLZANO BOZEN",
        # Fix for that one Santa Lucia row we found
        COMUNE_CLEAN == "SANTA LUCIA" & PROVINCIA_CLEAN == "SALERNO" ~ "AVELLINO",
        TRUE ~ PROVINCIA_CLEAN
      ),
      
      # 2. PATCH COMUNI (Your Master List)
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "SANTA LUCIA" ~ "SANTA LUCIA DI SERINO",
        COMUNE_CLEAN == "CERRETTO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "BASTIA" ~ "BASTIA UMBRA",
        COMUNE_CLEAN == "PAGANICO" ~ "PAGANICO SABINO",
        COMUNE_CLEAN == "CASTELLO LAVAZZO" ~ "CASTELLAVAZZO",
        COMUNE_CLEAN == "MONSERRATO" ~ "CAGLIARI", 
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA", 
        COMUNE_CLEAN == "CREVOLA D OSSOLA" ~ "CREVOLADOSSOLA",
        COMUNE_CLEAN == "CIANO D ENZA" ~ "CANOSSA", 
        COMUNE_CLEAN == "ALBISSOLA MARINA" ~ "ALBISOLA MARINA", 
        COMUNE_CLEAN == "VENARIA" ~ "VENARIA REALE",
        COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
        COMUNE_CLEAN == "GARNIGA" ~ "GARNIGA TERME",
        COMUNE_CLEAN == "SANT ORSOLA" ~ "SANT ORSOLA TERME",
        COMUNE_CLEAN == "CIVITACASTELLANA" ~ "CIVITA CASTELLANA",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "POZZAGLIA SABINO" ~ "POZZAGLIA SABINA",
        COMUNE_CLEAN == "MONTE GRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "CAMAGNA" ~ "CAMAGNA MONFERRATO",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        COMUNE_CLEAN == "AGLIANO" ~ "AGLIANO TERME",
        COMUNE_CLEAN == "SANNICANDRO DI BARI" ~ "SAN NICANDRO DI BARI",
        COMUNE_CLEAN == "DANTA" ~ "DANTA DI CADORE",
        COMUNE_CLEAN == "TELESE" ~ "TELESE TERME",
        COMUNE_CLEAN == "GRIZZANA" ~ "GRIZZANA MORANDI",
        COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
        COMUNE_CLEAN == "CEGLIE MESSAPICO" ~ "CEGLIE MESSAPICA",
        COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
        COMUNE_CLEAN == "RIPALIMOSANO" ~ "RIPALIMOSANI",
        COMUNE_CLEAN == "SAN POLOMATESE" ~ "SAN POLO MATESE",
        COMUNE_CLEAN == "GALLO" ~ "GALLO MATESE",
        COMUNE_CLEAN == "IONADI" ~ "JONADI",
        COMUNE_CLEAN == "MONTICELLO" ~ "MONTICELLO BRIANZA",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "PERSICO D OSIMO" ~ "PERSICO DOSIMO",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "RO FERRARESE" ~ "RO",
        COMUNE_CLEAN == "BARBERINO VAL D ELSA" ~ "BARBERINO DI VAL D ELSA",
        COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "OLLASTRA SIMAXIS" ~ "OLLASTRA",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "ZEME LOMELLINA" ~ "ZEME",
        COMUNE_CLEAN == "FARINI D OLMO" ~ "FARINI",
        COMUNE_CLEAN == "NUGHEDU DI SAN NICOLO" ~ "NUGHEDU SAN NICOLO",
        COMUNE_CLEAN == "FORGARIA NEL FRIULI" ~ "FORGARIA DEL FRIULI",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "SAINT RHEMY" ~ "SAINT RHEMY EN BOSSES",
        COMUNE_CLEAN == "JESOLO" ~ "IESOLO",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        TRUE ~ COMUNE_CLEAN
      )
    )

# 3. Clean, Patch, and Join Camera_1987
Camera_1992 <- Camera_1992 %>%
  mutate(
    COMUNE_CLEAN = clean_geo_names(COMUNE),
    PROVINCIA_CLEAN = clean_geo_names(PROVINCIA),
    # patch provinces
    PROVINCIA_CLEAN = case_when(
      PROVINCIA_CLEAN == "AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
      PROVINCIA_CLEAN == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
      PROVINCIA_CLEAN == "BOLZANO" ~ "BOLZANO BOZEN",
      PROVINCIA_CLEAN == "REGGIO EMILIA" ~ "REGGIO NELL EMILIA",
      # Fix for that one Santa Lucia row we found
      COMUNE_CLEAN == "SANTA LUCIA" & PROVINCIA_CLEAN == "SALERNO" ~ "AVELLINO",
      TRUE ~ PROVINCIA_CLEAN
    ),
    
    # 2. PATCH COMUNI (Your Master List)
    COMUNE_CLEAN = case_when(
      COMUNE_CLEAN == "SANTA LUCIA" ~ "SANTA LUCIA DI SERINO",
      COMUNE_CLEAN == "CERRETTO DELLE LANGHE" ~ "CERRETO LANGHE",
      COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETO LANGHE",
      COMUNE_CLEAN == "BASTIA" ~ "BASTIA UMBRA",
      COMUNE_CLEAN == "PAGANICO" ~ "PAGANICO SABINO",
      COMUNE_CLEAN == "CASTELLO LAVAZZO" ~ "CASTELLAVAZZO",
      COMUNE_CLEAN == "MONSERRATO" ~ "CAGLIARI", 
      COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
      COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
      COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
      COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA", 
      COMUNE_CLEAN == "CREVOLA D OSSOLA" ~ "CREVOLADOSSOLA",
      COMUNE_CLEAN == "CIANO D ENZA" ~ "CANOSSA", 
      COMUNE_CLEAN == "ALBISSOLA MARINA" ~ "ALBISOLA MARINA", 
      COMUNE_CLEAN == "VENARIA" ~ "VENARIA REALE",
      COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
      COMUNE_CLEAN == "GARNIGA" ~ "GARNIGA TERME",
      COMUNE_CLEAN == "SANT ORSOLA" ~ "SANT ORSOLA TERME",
      COMUNE_CLEAN == "CIVITACASTELLANA" ~ "CIVITA CASTELLANA",
      COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
      COMUNE_CLEAN == "POZZAGLIA SABINO" ~ "POZZAGLIA SABINA",
      COMUNE_CLEAN == "MONTE GRIMANO" ~ "MONTE GRIMANO TERME",
      COMUNE_CLEAN == "CAMAGNA" ~ "CAMAGNA MONFERRATO",
      COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
      COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
      COMUNE_CLEAN == "AGLIANO" ~ "AGLIANO TERME",
      COMUNE_CLEAN == "SANNICANDRO DI BARI" ~ "SAN NICANDRO DI BARI",
      COMUNE_CLEAN == "DANTA" ~ "DANTA DI CADORE",
      COMUNE_CLEAN == "TELESE" ~ "TELESE TERME",
      COMUNE_CLEAN == "GRIZZANA" ~ "GRIZZANA MORANDI",
      COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
      COMUNE_CLEAN == "CEGLIE MESSAPICO" ~ "CEGLIE MESSAPICA",
      COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
      COMUNE_CLEAN == "RIPALIMOSANO" ~ "RIPALIMOSANI",
      COMUNE_CLEAN == "SAN POLOMATESE" ~ "SAN POLO MATESE",
      COMUNE_CLEAN == "GALLO" ~ "GALLO MATESE",
      COMUNE_CLEAN == "IONADI" ~ "JONADI",
      COMUNE_CLEAN == "MONTICELLO" ~ "MONTICELLO BRIANZA",
      COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
      COMUNE_CLEAN == "PERSICO D OSIMO" ~ "PERSICO DOSIMO",
      COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
      COMUNE_CLEAN == "RO FERRARESE" ~ "RO",
      COMUNE_CLEAN == "BARBERINO VAL D ELSA" ~ "BARBERINO DI VAL D ELSA",
      COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
      COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
      COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
      COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
      COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
      COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
      COMUNE_CLEAN == "OLLASTRA SIMAXIS" ~ "OLLASTRA",
      COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
      COMUNE_CLEAN == "ZEME LOMELLINA" ~ "ZEME",
      COMUNE_CLEAN == "FARINI D OLMO" ~ "FARINI",
      COMUNE_CLEAN == "NUGHEDU DI SAN NICOLO" ~ "NUGHEDU SAN NICOLO",
      COMUNE_CLEAN == "FORGARIA NEL FRIULI" ~ "FORGARIA DEL FRIULI",
      COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
      COMUNE_CLEAN == "SAINT RHEMY" ~ "SAINT RHEMY EN BOSSES",
      COMUNE_CLEAN == "JESOLO" ~ "IESOLO",
      COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
      TRUE ~ COMUNE_CLEAN
    )
  )

# 4. Join and Diagnostic
Camera_1987_geo <- Camera_1987 %>%
  left_join(lookup_comuni_91, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

Camera_1992_geo <- Camera_1992 %>%
  left_join(lookup_comuni_91, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

unmatched_C_1987 <- Camera_1987 %>%
  anti_join(lookup_comuni_91, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN")) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct() %>%
  arrange(PROVINCIA_CLEAN, COMUNE_CLEAN)

unmatched_C_1992 <- Camera_1992 %>%
  anti_join(lookup_comuni_91, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN")) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct() %>%
  arrange(PROVINCIA_CLEAN, COMUNE_CLEAN)


print(unmatched_C_1987)
print(unmatched_C_1992)

################################################################################

# 1. Create a quick Region dictionary from the 1991 ISTAT shapefile
lookup_regioni_91 <- conf_regioni_91 %>%
  st_drop_geometry() %>%
  mutate(REGIONE_ISTAT = clean_geo_names(DEN_REG)) %>%
  select(COD_REG, REGIONE_ISTAT) %>%
  distinct()

# 2. Attach Region names to Camera data and extract the "Bridge"
bridge_1987 <- Camera_1987_geo %>%
  left_join(lookup_regioni_91, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

bridge_1992 <- Camera_1992_geo %>%
  left_join(lookup_regioni_91, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

patch_senato_91 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      REGIONE_CLEAN = clean_geo_names(REGIONE),
      
      # 1. Align Ministry Region names with ISTAT Region names
      REGIONE_CLEAN = case_when(
        REGIONE_CLEAN == "VALLE D AOSTA" ~ "VALLE D AOSTA",
        # Fix the Trentino mismatch once we know the exact ISTAT string
        REGIONE_CLEAN == "TRENTINO ALTO ADIGE" ~ "TRENTINO ALTO ADIGE SUDTIROL",
        TRUE ~ REGIONE_CLEAN
      ),
      
      # 2. Specific Comune Patches (From your unmatched list)
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "GALLO" ~ "GALLO MATESE",
        COMUNE_CLEAN == "TELESE" ~ "TELESE TERME",
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "CIANO D ENZA" ~ "CANOSSA",
        COMUNE_CLEAN == "FARINI D OLMO" ~ "FARINI",
        COMUNE_CLEAN == "GRIZZANA" ~ "GRIZZANA MORANDI",
        COMUNE_CLEAN == "CIVITACASTELLANA" ~ "CIVITA CASTELLANA",
        COMUNE_CLEAN == "POZZAGLIA SABINO" ~ "POZZAGLIA SABINA",
        COMUNE_CLEAN == "PAGANICO" ~ "PAGANICO SABINO",
        COMUNE_CLEAN == "MONTE GRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
        COMUNE_CLEAN == "RIPALIMOSANO" ~ "RIPALIMOSANI",
        COMUNE_CLEAN == "SAN POLOMATESE" ~ "SAN POLO MATESE",
        COMUNE_CLEAN == "AGLIANO" ~ "AGLIANO TERME",
        COMUNE_CLEAN == "CAMAGNA" ~ "CAMAGNA MONFERRATO",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "CREVOLA D OSSOLA" ~ "CREVOLADOSSOLA",
        COMUNE_CLEAN == "VENARIA" ~ "VENARIA REALE",
        COMUNE_CLEAN == "CEGLIE MESSAPICO" ~ "CEGLIE MESSAPICA",
        COMUNE_CLEAN == "SANNICANDRO DI BARI" ~ "SAN NICANDRO DI BARI",
        COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
        COMUNE_CLEAN == "NUGHEDU DI SAN NICOLO" ~ "NUGHEDU SAN NICOLO",
        COMUNE_CLEAN == "OLLASTRA SIMAXIS" ~ "OLLASTRA",
        COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
        COMUNE_CLEAN == "BARBERINO VAL D ELSA" ~ "BARBERINO DI VAL D ELSA",
        COMUNE_CLEAN == "MONSERRATO" ~ "CAGLIARI", # Or leave unmatched if it wasn't a comune yet
        
        # Also include the ones you already had in the Camera patch!
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "RO FERRARESE" ~ "RO",
        COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "PERSICO D OSIMO" ~ "PERSICO DOSIMO",
        COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
        COMUNE_CLEAN == "ZEME LOMELLINA" ~ "ZEME",
        COMUNE_CLEAN == "CERRETTO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "IONADI" ~ "JONADI",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "FORGARIA NEL FRIULI" ~ "FORGARIA DEL FRIULI",
        COMUNE_CLEAN == "MONTICELLO" ~ "MONTICELLO BRIANZA",
        
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
        COMUNE_CLEAN == "GARNIGA" ~ "GARNIGA TERME",
        COMUNE_CLEAN == "SANT ORSOLA" ~ "SANT ORSOLA TERME",
        COMUNE_CLEAN == "SAINT RHEMY" ~ "SAINT RHEMY EN BOSSES",
        COMUNE_CLEAN == "DANTA" ~ "DANTA DI CADORE",
        COMUNE_CLEAN == "JESOLO" ~ "IESOLO",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        COMUNE_CLEAN == "BASTIA" ~ "BASTIA UMBRA",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        TRUE ~ COMUNE_CLEAN
      )
    )
}

# Apply the patch
Senato_1987_clean <- patch_senato_91(Senato_1987)
Senato_1992_clean <- patch_senato_91(Senato_1992)

# Join with the Camera Bridge
Senato_1987_geo <- Senato_1987_clean %>%
  left_join(bridge_1987, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

Senato_1992_geo <- Senato_1992_clean %>%
  left_join(bridge_1992, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

# --- 1987 Unmatched Check ---
unmatched_S_1987 <- Senato_1987_clean %>%
  anti_join(bridge_1987, by = c("COMUNE_CLEAN" = "COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

# --- 1992 Unmatched Check ---
unmatched_S_1992 <- Senato_1992_clean %>%
  anti_join(bridge_1992, by = c("COMUNE_CLEAN" = "COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

print(unmatched_S_1987)
print(unmatched_S_1992)



### 1994-1996

lookup_comuni_regione_91 <- conf_comuni_91 %>% 
  st_drop_geometry() %>%
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_91 %>% st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>% select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  left_join(lookup_regioni_91, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

patch_94_96 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      
      # Safely grab the region regardless of whether it's called REGIONE or REGIONE_CLEAN in the raw data
      REG_TEMP = if("REGIONE_CLEAN" %in% names(.)) REGIONE_CLEAN else REGIONE,
      REGIONE_CLEAN = clean_geo_names(REG_TEMP),
      
      # 1. Align Ministry Region names with ISTAT Region names
      REGIONE_CLEAN = case_when(
        REGIONE_CLEAN == "TRENTINO ALTO ADIGE" ~ "TRENTINO ALTO ADIGE SUDTIROL",
        TRUE ~ REGIONE_CLEAN
      ),
      
      # 2. Strip "PARTE DI COMUNE" prefixes BEFORE evaluating the city names
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, "^PARTE DEL COMUNE DI |^PARTE DI COMUNE DI |^PARTE DI COMUNE "),
      
      # 3. Handle major city splits AND specific Comune fixes
      COMUNE_CLEAN = case_when(
        
        # --- ADD THESE 3 EXEMPTIONS FIRST! ---
        COMUNE_CLEAN == "BARI SARDO" ~ "BARI SARDO",
        COMUNE_CLEAN == "FERRARA DI MONTE BALDO" ~ "FERRARA DI MONTE BALDO",
        COMUNE_CLEAN == "CASTELLO LAVAZZO" ~ "CASTELLAVAZZO",
        
        # City splits
        str_detect(COMUNE_CLEAN, "^ROMA ") ~ "ROMA",
        str_detect(COMUNE_CLEAN, "^FIRENZE ") ~ "FIRENZE",
        str_detect(COMUNE_CLEAN, "^NAPOLI ") ~ "NAPOLI",
        str_detect(COMUNE_CLEAN, "^BOLOGNA ") ~ "BOLOGNA",
        str_detect(COMUNE_CLEAN, "^GENOVA ") ~ "GENOVA",
        str_detect(COMUNE_CLEAN, "^VENEZIA ") ~ "VENEZIA",
        str_detect(COMUNE_CLEAN, "^PALERMO ") ~ "PALERMO",
        str_detect(COMUNE_CLEAN, "^CATANIA ") ~ "CATANIA",
        str_detect(COMUNE_CLEAN, "^MESSINA ") ~ "MESSINA",
        str_detect(COMUNE_CLEAN, "^BARI ") ~ "BARI",
        str_detect(COMUNE_CLEAN, "^TARANTO ") ~ "TARANTO",
        str_detect(COMUNE_CLEAN, "^VERONA ") ~ "VERONA",
        str_detect(COMUNE_CLEAN, "^TRIESTE ") ~ "TRIESTE",
        str_detect(COMUNE_CLEAN, "^PADOVA ") ~ "PADOVA",
        str_detect(COMUNE_CLEAN, "^CAGLIARI ") ~ "CAGLIARI",
        str_detect(COMUNE_CLEAN, "^PERUGIA ") ~ "PERUGIA",
        str_detect(COMUNE_CLEAN, "^SALERNO ") ~ "SALERNO",
        str_detect(COMUNE_CLEAN, "^FOGGIA ") ~ "FOGGIA",
        str_detect(COMUNE_CLEAN, "^FERRARA ") ~ "FERRARA",
        str_detect(COMUNE_CLEAN, "^MODENA ") ~ "MODENA",
        str_detect(COMUNE_CLEAN, "^REGGIO CALABRIA") ~ "REGGIO DI CALABRIA",
        
        # Specific Comune fixes
        COMUNE_CLEAN == "GALLO" ~ "GALLO MATESE",
        COMUNE_CLEAN == "TELESE" ~ "TELESE TERME",
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "CIANO D ENZA" ~ "CANOSSA",
        COMUNE_CLEAN == "FARINI D OLMO" ~ "FARINI",
        COMUNE_CLEAN == "GRIZZANA" ~ "GRIZZANA MORANDI",
        COMUNE_CLEAN == "CIVITACASTELLANA" ~ "CIVITA CASTELLANA",
        COMUNE_CLEAN == "POZZAGLIA SABINO" ~ "POZZAGLIA SABINA",
        COMUNE_CLEAN == "PAGANICO" ~ "PAGANICO SABINO",
        COMUNE_CLEAN == "MONTE GRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
        COMUNE_CLEAN == "RIPALIMOSANO" ~ "RIPALIMOSANI",
        COMUNE_CLEAN == "SAN POLOMATESE" ~ "SAN POLO MATESE",
        COMUNE_CLEAN == "AGLIANO" ~ "AGLIANO TERME",
        COMUNE_CLEAN == "CAMAGNA" ~ "CAMAGNA MONFERRATO",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "CREVOLA D OSSOLA" ~ "CREVOLADOSSOLA",
        COMUNE_CLEAN == "VENARIA" ~ "VENARIA REALE",
        COMUNE_CLEAN == "CEGLIE MESSAPICO" ~ "CEGLIE MESSAPICA",
        COMUNE_CLEAN == "SANNICANDRO DI BARI" ~ "SAN NICANDRO DI BARI",
        COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
        COMUNE_CLEAN == "NUGHEDU DI SAN NICOLO" ~ "NUGHEDU SAN NICOLO",
        COMUNE_CLEAN == "OLLASTRA SIMAXIS" ~ "OLLASTRA",
        COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
        COMUNE_CLEAN == "BARBERINO VAL D ELSA" ~ "BARBERINO DI VAL D ELSA",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "RO FERRARESE" ~ "RO",
        COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "PERSICO D OSIMO" ~ "PERSICO DOSIMO",
        COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
        COMUNE_CLEAN == "ZEME LOMELLINA" ~ "ZEME",
        COMUNE_CLEAN == "CERRETTO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "IONADI" ~ "JONADI",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "FORGARIA NEL FRIULI" ~ "FORGARIA DEL FRIULI",
        COMUNE_CLEAN == "MONTICELLO" ~ "MONTICELLO BRIANZA",
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
        COMUNE_CLEAN == "GARNIGA" ~ "GARNIGA TERME",
        COMUNE_CLEAN == "SANT ORSOLA" ~ "SANT ORSOLA TERME",
        COMUNE_CLEAN == "SAINT RHEMY" ~ "SAINT RHEMY EN BOSSES",
        COMUNE_CLEAN == "DANTA" ~ "DANTA DI CADORE",
        COMUNE_CLEAN == "JESOLO" ~ "IESOLO",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        COMUNE_CLEAN == "BASTIA" ~ "BASTIA UMBRA",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        COMUNE_CLEAN == "MONSERRATO" ~ "ASSEMINI",
        COMUNE_CLEAN == "STATTE" ~ "TARANTO",
        COMUNE_CLEAN == "XXXXX" ~ "COSSATO",
        COMUNE_CLEAN == "FIUMICINO" ~ "ROMA",
        
        # --- 1996 Typo & Abbreviation Fixes ---
        COMUNE_CLEAN == "ALBISSOLA MARINA" ~ "ALBISOLA MARINA", # Only one 'S'
        COMUNE_CLEAN == "BREMBATE DI SORA" ~ "BREMBATE DI SOPRA",
        COMUNE_CLEAN == "SAN ELLEGRINO TERME" ~ "SAN PELLEGRINO TERME",
        COMUNE_CLEAN == "BALDISSERO ALBA" ~ "BALDISSERO D ALBA",
        COMUNE_CLEAN == "BELLINGAZO NOVARESE" ~ "BELLINZAGO NOVARESE",
        COMUNE_CLEAN == "MASSIMO VISCONTI" ~ "MASSINO VISCONTI",
        COMUNE_CLEAN == "OLTRONA SAN MAMETTE" ~ "OLTRONA DI SAN MAMETTE",
        COMUNE_CLEAN == "PIANELLO LARIO" ~ "PIANELLO DEL LARIO",
        COMUNE_CLEAN == "SAN BARTOLOMEO VC" ~ "SAN BARTOLOMEO VAL CAVARGNA",
        COMUNE_CLEAN == "SAN FERMO D BATTAGLIA" ~ "SAN FERMO DELLA BATTAGLIA",
        COMUNE_CLEAN == "SAN NAZZARO VC" ~ "SAN NAZZARO VAL CAVARGNA",
        COMUNE_CLEAN == "VALREZZO" ~ "VAL REZZO",
        COMUNE_CLEAN == "ACQUASANTA" ~ "ACQUASANTA TERME",
        COMUNE_CLEAN == "VILLAGRANDE STR" ~ "VILLAGRANDE STRISAILI",
        
        # --- 1996 "Time Machine" Fixes (Reverting to 1991 ISTAT names) ---
        COMUNE_CLEAN == "DUE CARRARE" ~ "CARRARA SAN GIORGIO", # Merged in 1995
        COMUNE_CLEAN == "PORTO VIRO" ~ "CONTARINA", # Merged in 1995
        COMUNE_CLEAN == "PADRU" ~ "BUDDUSO", # Separated in 1996
        # Eligendo sometimes applies 1998 names retroactively, so we revert these too:
        COMUNE_CLEAN == "MONTIGLIO MONFERRATO" ~ "MONTIGLIO",
        COMUNE_CLEAN == "MOSSO" ~ "MOSSO SANTA MARIA",
        
        TRUE ~ COMUNE_CLEAN
      )
    ) %>%
    # Drop the temporary region column
    select(-REG_TEMP)
}


# 1. Apply the Patch
Camera_1994_clean <- patch_94_96(Camera_1994)
Senato_1994_clean <- patch_94_96(Senato_1994)

# 2. Perform the Geographical Join
Camera_1994_geo <- Camera_1994_clean %>%
  left_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

Senato_1994_geo <- Senato_1994_clean %>%
  left_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

# 1. Apply the Patch
Camera_1996_clean <- patch_94_96(Camera_1996)
Senato_1996_clean <- patch_94_96(Senato_1996)

# 2. Perform the Geographical Join
Camera_1996_geo <- Camera_1996_clean %>%
  left_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

Senato_1996_geo <- Senato_1996_clean %>%
  left_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))


# 3. Check for unmatched rows (Camera)
unmatched_C_1994 <- Camera_1994_clean %>%
  anti_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

unmatched_C_1996 <- Camera_1996_clean %>%
  anti_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

unmatched_S_1994 <- Senato_1994_clean %>%
  anti_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

unmatched_S_1996 <- Senato_1996_clean %>%
  anti_join(lookup_comuni_regione_91, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

print(unmatched_C_1994)
print(unmatched_S_1994)
print(unmatched_S_1996)
print(unmatched_C_1996)


################################################################################
### 2.3. 2001 ---> confini_2001 

# 1. Create the 2001 Region Dictionary
lookup_regioni_01 <- conf_regioni_01 %>%
  st_drop_geometry() %>%
  mutate(REGIONE_ISTAT = clean_geo_names(DEN_REG)) %>%
  select(COD_REG, REGIONE_ISTAT) %>%
  distinct()

lookup_comuni_regione_01 <- conf_comuni_01 %>% 
  st_drop_geometry() %>%
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_01 %>% st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>% select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  left_join(lookup_regioni_01, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

patch_01 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      
      # Safely grab the region regardless of whether it's called REGIONE or REGIONE_CLEAN in the raw data
      REG_TEMP = if("REGIONE_CLEAN" %in% names(.)) REGIONE_CLEAN else REGIONE,
      REGIONE_CLEAN = clean_geo_names(REG_TEMP),
      
      # 1. Align Ministry Region names with ISTAT Region names
      REGIONE_CLEAN = case_when(
        REGIONE_CLEAN == "TRENTINO ALTO ADIGE" ~ "TRENTINO ALTO ADIGE SUDTIROL",
        REGIONE_CLEAN == "VALLE D AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
        TRUE ~ REGIONE_CLEAN
      ),
      
      # 2. Strip "PARTE DI COMUNE" prefixes BEFORE evaluating the city names
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, "^PARTE DEL COMUNE DI |^PARTE DI COMUNE DI |^PARTE DI COMUNE "),
      
      # 3. Handle major city splits AND specific Comune fixes
      COMUNE_CLEAN = case_when(
        
        # --- ADD THESE 3 EXEMPTIONS FIRST! ---
        COMUNE_CLEAN == "BARI SARDO" ~ "BARI SARDO",
        COMUNE_CLEAN == "FERRARA DI MONTE BALDO" ~ "FERRARA DI MONTE BALDO",
        COMUNE_CLEAN == "CASTELLO LAVAZZO" ~ "CASTELLAVAZZO",
        
        # City splits
        str_detect(COMUNE_CLEAN, "^PARMA ") ~ "PARMA",
        str_detect(COMUNE_CLEAN, "^ROMA ") ~ "ROMA",
        str_detect(COMUNE_CLEAN, "^FIRENZE ") ~ "FIRENZE",
        str_detect(COMUNE_CLEAN, "^NAPOLI ") ~ "NAPOLI",
        str_detect(COMUNE_CLEAN, "^BOLOGNA ") ~ "BOLOGNA",
        str_detect(COMUNE_CLEAN, "^GENOVA ") ~ "GENOVA",
        str_detect(COMUNE_CLEAN, "^VENEZIA ") ~ "VENEZIA",
        str_detect(COMUNE_CLEAN, "^PALERMO ") ~ "PALERMO",
        str_detect(COMUNE_CLEAN, "^CATANIA ") ~ "CATANIA",
        str_detect(COMUNE_CLEAN, "^MESSINA ") ~ "MESSINA",
        str_detect(COMUNE_CLEAN, "^BARI ") ~ "BARI",
        str_detect(COMUNE_CLEAN, "^TARANTO ") ~ "TARANTO",
        str_detect(COMUNE_CLEAN, "^VERONA ") ~ "VERONA",
        str_detect(COMUNE_CLEAN, "^TRIESTE ") ~ "TRIESTE",
        str_detect(COMUNE_CLEAN, "^PADOVA ") ~ "PADOVA",
        str_detect(COMUNE_CLEAN, "^CAGLIARI ") ~ "CAGLIARI",
        str_detect(COMUNE_CLEAN, "^PERUGIA ") ~ "PERUGIA",
        str_detect(COMUNE_CLEAN, "^SALERNO ") ~ "SALERNO",
        str_detect(COMUNE_CLEAN, "^FOGGIA ") ~ "FOGGIA",
        str_detect(COMUNE_CLEAN, "^FERRARA ") ~ "FERRARA",
        str_detect(COMUNE_CLEAN, "^MODENA ") ~ "MODENA",
        str_detect(COMUNE_CLEAN, "^REGGIO CALABRIA") ~ "REGGIO DI CALABRIA",
        
        COMUNE_CLEAN == "BUIA" ~ "BUJA",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "MONTALBANO IONICO " ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "MONTEBELLO IONICO " ~ "MONTEBELLO JONICO",
        COMUNE_CLEAN == "NOCERA TIRINESE" ~ "NOCERA TERINESE",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
        COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        COMUNE_CLEAN == "GARNIGA" ~ "GARNIGA TERME",
        
        # --- 2001 Specific Comune Fixes ---
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "MONTEBELLO IONICO" ~ "MONTEBELLO JONICO",
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE" ~ "SAN DORLIGO DELLA VALLE DOLINA",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "PAGANICO" ~ "PAGANICO SABINO",
        COMUNE_CLEAN == "ALBISOLA MARINA" ~ "ALBISSOLA MARINA", # ISTAT uses two S's here
        COMUNE_CLEAN == "AQUILA DI ARROSCIA" ~ "AQUILA D ARROSCIA",
        COMUNE_CLEAN == "COSIO DI ARROSCIA" ~ "COSIO D ARROSCIA",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "PERSICO D OSIMO" ~ "PERSICO DOSIMO",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETTO LANGHE", 
        COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
        COMUNE_CLEAN == "CALATAFIMI TERME" ~ "CALATAFIMI SEGESTA", # Name changed in 1997
        COMUNE_CLEAN == "CALATAFIMI" ~ "CALATAFIMI SEGESTA",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "SANTO STINO DI LIVENZA" ~ "SAN STINO DI LIVENZA",
        TRUE ~ COMUNE_CLEAN
      )
    ) %>%
    # Drop the temporary region column
    select(-REG_TEMP)
}

# 1. Apply the Patch
Camera_2001_clean <- patch_01(Camera_2001)
Senato_2001_clean <- patch_01(Senato_2001)

# 2. Perform the Geographical Join
Camera_2001_geo <- Camera_2001_clean %>%
  left_join(lookup_comuni_regione_01, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

Senato_2001_geo <- Senato_2001_clean %>%
  left_join(lookup_comuni_regione_01, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

# --- Camera 2001 Unmatched ---
unmatched_C_2001 <- Camera_2001_clean %>%
  anti_join(lookup_comuni_regione_01, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

# --- Senato 2001 Unmatched ---
unmatched_S_2001 <- Senato_2001_clean %>%
  anti_join(lookup_comuni_regione_01, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

print(unmatched_C_2001)
print(unmatched_S_2001)

################################################################################
### 2.4. 2006 ---> confini_2006 

# 1. Build the ISTAT Lookup (Same as yours, but adding REGIONE for safety)
lookup_comuni_06 <- conf_comuni_06 %>%
  st_drop_geometry() %>% 
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_06 %>% 
      st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>%
      select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  # IMPORTANT: Select only the 'CLEAN' versions and the codes
  # We drop the original 'COMUNE' from the shapefile here
  select(COMUNE_CLEAN, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM, PRO_COM_T) %>%
  distinct()


patch_2006 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      PROVINCIA_CLEAN = clean_geo_names(PROVINCIA),
      
      # 1. FIX PROVINCE NAMES (Mapping old provinces to 2006 ISTAT reality)
      # We move towns to the new provinces so the join finds them
      PROVINCIA_CLEAN = case_when(
          COMUNE_CLEAN %in% c("MARGHERITA DI SAVOIA", "SAN FERDINANDO DI PUGLIA", "TRINITAPOLI") ~ "FOGGIA",
          
          PROVINCIA_CLEAN == "BARLETTA ANDRIA TRANI" ~ "BARI",
          PROVINCIA_CLEAN == "CARBONIA IGLES" ~ "CARBONIA IGLESIAS",
          PROVINCIA_CLEAN == "MONZA" ~ "MILANO",
          PROVINCIA_CLEAN == "FERMO" ~ "ASCOLI PICENO",
          PROVINCIA_CLEAN == "AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
          PROVINCIA_CLEAN == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
          PROVINCIA_CLEAN == "BOLZANO" ~ "BOLZANO BOZEN",
          PROVINCIA_CLEAN == "REGGIO EMILIA" ~ "REGGIO NELL EMILIA",
        TRUE ~ PROVINCIA_CLEAN
      ),
      
      # 2. STRIP PROVINCE SUFFIXES (TN, CT, etc.)
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, " TN$"),
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, " CT$"),
      
      # 3. SPECIFIC TOWN FIXES
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "MONTEBELLO IONICO" ~ "MONTEBELLO JONICO",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "RONCEGNO" ~ "RONCEGNO TERME",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "RUFFRE" ~ "RUFFRE MENDOLA",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "NOCERA TIRINESE" ~ "NOCERA TERINESE",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETTO LANGHE",
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "VALVERDE" ~ "VALVERDE", # Suffix removed above
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        COMUNE_CLEAN == "AQUILA DI ARROSCIA" ~ "AQUILA D ARROSCIA",
        COMUNE_CLEAN == "COSIO DI ARROSCIA" ~ "COSIO D ARROSCIA",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
        COMUNE_CLEAN == "MONACIILIONI" ~ "MONACILIONI",
        COMUNE_CLEAN == "COLLEDANCHISE" ~ "COLLE D ANCHISE",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "SERRA S ABBONDIO" ~ "SERRA SANT ABBONDIO",
        COMUNE_CLEAN == "MONTEGRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "SANTA VITTORIA IN MATEMANO" ~ "SANTA VITTORIA IN MATENANO",
        COMUNE_CLEAN == "TRUGGIO" ~ "TRIUGGIO",
        COMUNE_CLEAN == "SANTA TERESA DI GALLURA" ~ "SANTA TERESA GALLURA",
        COMUNE_CLEAN == "GONNASFANADIGA" ~ "GONNOSFANADIGA",
        COMUNE_CLEAN == "SANTO STINO DI LIVENZA" ~ "SAN STINO DI LIVENZA",
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE" ~ "SAN DORLIGO DELLA VALLE DOLINA",
        COMUNE_CLEAN == "BUIA" ~ "BUJA",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "MONGUELFO" ~ "MONGUELFO TESIDO",
        TRUE ~ COMUNE_CLEAN
      )
    )
}

Camera_2006 <- patch_2006(Camera_2006)

Camera_2006_geo <- Camera_2006 %>%
  left_join(lookup_comuni_06, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

Senato_2006 <- patch_2006(Senato_2006)

Senato_2006_geo <- Senato_2006 %>%
  left_join(lookup_comuni_06, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

# 4. Diagnostic
unmatched_C_2006 <- Camera_2006_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

unmatched_S_2006 <- Senato_2006_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

print(unmatched_C_2006)
print(unmatched_S_2006)

################################################################################
### 2.5. 2008 ---> confini_2008 

# 1. Build the ISTAT Lookup (Same as yours, but adding REGIONE for safety)
lookup_comuni_08 <- conf_comuni_08 %>%
  st_drop_geometry() %>% 
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_08 %>% 
      st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>%
      select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  # IMPORTANT: Select only the 'CLEAN' versions and the codes
  # We drop the original 'COMUNE' from the shapefile here
  select(COMUNE_CLEAN, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM, PRO_COM_T) %>%
  distinct()


patch_2008 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      PROVINCIA_CLEAN = clean_geo_names(PROVINCIA),
      
      # 1. FIX PROVINCE NAMES (Mapping old provinces to 2008 ISTAT reality)
      # We move towns to the new provinces so the join finds them
      PROVINCIA_CLEAN = case_when(
        COMUNE_CLEAN %in% c("MARGHERITA DI SAVOIA", "SAN FERDINANDO DI PUGLIA", "TRINITAPOLI") ~ "FOGGIA",
        
        PROVINCIA_CLEAN == "BARLETTA ANDRIA TRANI" ~ "BARI",
        PROVINCIA_CLEAN == "CARBONIA IGLES" ~ "CARBONIA IGLESIAS",
        PROVINCIA_CLEAN == "MONZA E DELLA BRIANZA" ~ "MILANO",
        PROVINCIA_CLEAN == "FERMO" ~ "ASCOLI PICENO",
        PROVINCIA_CLEAN == "AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
        PROVINCIA_CLEAN == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
        PROVINCIA_CLEAN == "BOLZANO" ~ "BOLZANO BOZEN",
        PROVINCIA_CLEAN == "REGGIO EMILIA" ~ "REGGIO NELL EMILIA",
        TRUE ~ PROVINCIA_CLEAN
      ),
      
      # 2. STRIP PROVINCE SUFFIXES (TN, CT, etc.)
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, " TN$"),
      COMUNE_CLEAN = str_remove(COMUNE_CLEAN, " CT$"),
      
      # 3. SPECIFIC TOWN FIXES
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "MONTEBELLO IONICO" ~ "MONTEBELLO JONICO",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "MONTECOMPATRI" ~ "MONTE COMPATRI",
        COMUNE_CLEAN == "RONCEGNO" ~ "RONCEGNO TERME",
        COMUNE_CLEAN == "REANA DEL ROIALE" ~ "REANA DEL ROJALE",
        COMUNE_CLEAN == "RUFFRE" ~ "RUFFRE MENDOLA",
        COMUNE_CLEAN == "MASSALUBRENSE" ~ "MASSA LUBRENSE",
        COMUNE_CLEAN == "NOCERA TIRINESE" ~ "NOCERA TERINESE",
        COMUNE_CLEAN == "POIANA MAGGIORE" ~ "POJANA MAGGIORE",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETO LANGHE",
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "VALVERDE" ~ "VALVERDE", # Suffix removed above
        COMUNE_CLEAN == "TORELLA DE LOMBARDI" ~ "TORELLA DEI LOMBARDI",
        COMUNE_CLEAN == "COSTA DI SERINA" ~ "COSTA SERINA",
        COMUNE_CLEAN == "MONTE CASTELLO" ~ "MONTECASTELLO",
        COMUNE_CLEAN == "AQUILA DI ARROSCIA" ~ "AQUILA D ARROSCIA",
        COMUNE_CLEAN == "COSIO DI ARROSCIA" ~ "COSIO D ARROSCIA",
        COMUNE_CLEAN == "BAIARDO" ~ "BAJARDO",
        COMUNE_CLEAN == "MONTALBANO IONICO" ~ "MONTALBANO JONICO",
        COMUNE_CLEAN == "CASTEL MOLA" ~ "CASTELMOLA",
        COMUNE_CLEAN == "BOIANO" ~ "BOJANO",
        COMUNE_CLEAN == "MONACIILIONI" ~ "MONACILIONI",
        COMUNE_CLEAN == "COLLEDANCHISE" ~ "COLLE D ANCHISE",
        COMUNE_CLEAN == "CASTRONUOVO DI SICILIA" ~ "CASTRONOVO DI SICILIA",
        COMUNE_CLEAN == "SERRA S ABBONDIO" ~ "SERRA SANT ABBONDIO",
        COMUNE_CLEAN == "MONTEGRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "SANTA VITTORIA IN MATEMANO" ~ "SANTA VITTORIA IN MATENANO",
        COMUNE_CLEAN == "TRUGGIO" ~ "TRIUGGIO",
        COMUNE_CLEAN == "SANTA TERESA DI GALLURA" ~ "SANTA TERESA GALLURA",
        COMUNE_CLEAN == "GONNASFANADIGA" ~ "GONNOSFANADIGA",
        COMUNE_CLEAN == "SANTO STINO DI LIVENZA" ~ "SAN STINO DI LIVENZA",
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE" ~ "SAN DORLIGO DELLA VALLE DOLINA",
        COMUNE_CLEAN == "BUIA" ~ "BUJA",
        COMUNE_CLEAN == "CERRETO DELLE LANGHE" ~ "CERRETTO LANGHE",
        COMUNE_CLEAN == "CERRETO LANGHE" ~ "CERRETTO LANGHE",
        COMUNE_CLEAN == "CASSANO ALLO IONIO" ~ "CASSANO ALL IONIO",
        COMUNE_CLEAN == "MONGUELFO" ~ "MONGUELFO TESIDO",
        
        # --- Add these to the COMUNE_CLEAN section of your patch ---
        COMUNE_CLEAN == "ZEME LOMELLINA" ~ "ZEME",
        COMUNE_CLEAN == "SANNICANDRO GARGANICO" ~ "SAN NICANDRO GARGANICO",
        COMUNE_CLEAN == "MONTE GRIMANO" ~ "MONTE GRIMANO TERME",
        COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
        COMUNE_CLEAN == "MONTICELLO" ~ "MONTICELLO BRIANZA",
        COMUNE_CLEAN == "RO FERRARESE" ~ "RO",
        COMUNE_CLEAN == "SAN REMO" ~ "SANREMO",
        COMUNE_CLEAN == "LONATO" ~ "LONATO DEL GARDA", # Name changed in 2007
        TRUE ~ COMUNE_CLEAN
      )
    )
}

Camera_2008 <- patch_2008(Camera_2008)

Camera_2008_geo <- Camera_2008 %>%
  left_join(lookup_comuni_08, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

Senato_2008 <- patch_2008(Senato_2008)

Senato_2008_geo <- Senato_2008 %>%
  left_join(lookup_comuni_08, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

# 4. Diagnostic
unmatched_C_2008 <- Camera_2008_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

unmatched_S_2008 <- Senato_2008_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

print(unmatched_C_2008)
print(unmatched_S_2008)

################################################################################
### 2.6. 2013 ---> confini_2013 


# 1. Build the ISTAT Lookup (Same as yours, but adding REGIONE for safety)
lookup_comuni_13 <- conf_comuni_13 %>%
  st_drop_geometry() %>% 
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_13 %>% 
      st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>%
      select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  # IMPORTANT: Select only the 'CLEAN' versions and the codes
  # We drop the original 'COMUNE' from the shapefile here
  select(COMUNE_CLEAN, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM, PRO_COM_T) %>%
  distinct()

patch_2013 <- function(df) {
  df %>%
    mutate(
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      PROVINCIA_CLEAN = clean_geo_names(PROVINCIA),
      
      # 1. FIX PROVINCE NAMES (Mapping old provinces to 2013 ISTAT reality)
      PROVINCIA_CLEAN = case_when(
        PROVINCIA_CLEAN == "AOSTA" ~ "VALLE D AOSTA VALLEE D AOSTE",
        PROVINCIA_CLEAN == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
        PROVINCIA_CLEAN == "BOLZANO" ~ "BOLZANO BOZEN",
        TRUE ~ PROVINCIA_CLEAN
      ),
      
      # 2. FIX COMUNE NAMES
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "COSIO DI ARROSCIA" ~ "COSIO D ARROSCIA",
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE" ~ "SAN DORLIGO DELLA VALLE DOLINA",
        COMUNE_CLEAN == "CERRINA" ~ "CERRINA MONFERRATO",
        COMUNE_CLEAN == "VIALE D ASTI" ~ "VIALE",  
        COMUNE_CLEAN == "MONTEBELLO IONICO" ~ "MONTEBELLO JONICO",
        COMUNE_CLEAN == "IOLANDA DI SAVOIA" ~ "JOLANDA DI SAVOIA",
        COMUNE_CLEAN == "MASSAFISCAGLIA" ~ "MASSA FISCAGLIA",
        COMUNE_CLEAN == "PUEGNAGO DEL GARDA" ~ "PUEGNAGO SUL GARDA",
        TRUE ~ COMUNE_CLEAN
      )
    )
}



Camera_2013 <- patch_2013(Camera_2013)
Camera_2013_geo <- Camera_2013 %>%
  left_join(lookup_comuni_13, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

Senato_2013 <- patch_2013(Senato_2013)
Senato_2013_geo <- Senato_2013 %>%
  left_join(lookup_comuni_13, by = c("COMUNE_CLEAN", "PROVINCIA_CLEAN"))

# 4. Diagnostic
unmatched_C_2013 <- Camera_2013_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

unmatched_S_2013 <- Senato_2013_geo %>%
  filter(is.na(PRO_COM_T)) %>%
  select(PROVINCIA_CLEAN, COMUNE, COMUNE_CLEAN) %>% 
  distinct()

print(unmatched_C_2013)
print(unmatched_S_2013)

################################################################################
### 2.7. 2018 ---> confini_2018


# 1. Build the 2018 Region Dictionary
lookup_regioni_18 <- conf_regioni_18 %>%
  st_drop_geometry() %>%
  mutate(REGIONE_ISTAT = clean_geo_names(DEN_REG)) %>%
  select(COD_REG, REGIONE_ISTAT) %>%
  distinct()

lookup_comuni_regione_18 <- conf_comuni_18 %>% 
  st_drop_geometry() %>%
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_18 %>% st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>% select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  left_join(lookup_regioni_18, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

# 3. Create the 2018 Patcher (Skeleton ready for mergers)
patch_2018 <- function(df) {
  df %>%
    mutate(
      # 1. Strip the German translation from Alto Adige BEFORE cleaning
      COMUNE = str_remove(COMUNE, "/.*"),
      
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      REGIONE_CLEAN = clean_geo_names(REGIONE), 
      
      # 2. Bulletproof Region Matcher (Ignores encoding issues!)
      REGIONE_CLEAN = case_when(
        str_detect(REGIONE_CLEAN, "^TRENTINO ALTO ADIGE") ~ "TRENTINO ALTO ADIGE",
        REGIONE_CLEAN == "AOSTA" ~ "VALLE D AOSTA",
        TRUE ~ REGIONE_CLEAN
      ),
      
      # 3. Handle the 2018 Mergers and stragglers
      COMUNE_CLEAN = case_when(
        # Revert late-2018 mergers to their Jan 1st ISTAT polygons
        COMUNE_CLEAN == "FIUMICELLO VILLA VICENTINA" ~ "FIUMICELLO", 
        COMUNE_CLEAN == "TREPPO LIGOSULLO" ~ "TREPPO CARNICO", 
        COMUNE_CLEAN == "SN JAN DI FASSA" ~ "SEN JAN DI FASSA",
        COMUNE_CLEAN == "EMARSE" ~ "EMARESE",
        COMUNE_CLEAN == "FNIS" ~ "FENIS",
        COMUNE_CLEAN == "VERRS" ~ "VERRES",
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE DOLINA" ~ "SAN DORLIGO DELLA VALLE",
        TRUE ~ COMUNE_CLEAN
      )
    )
}

Camera_2018 <- patch_2018(Camera_2018)
Senato_2018 <- patch_2018(Senato_2018)

# Final Geo Join for 2018
Camera_2018_geo <- Camera_2018 %>%
  left_join(lookup_comuni_regione_18, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))
Senato_2018_geo <- Senato_2018 %>%
  left_join(lookup_comuni_regione_18, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

# 5. Run the Diagnostic (Camera)
unmatched_C_2018 <- Camera_2018 %>%
  anti_join(lookup_comuni_regione_18, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

# 6. Run the Diagnostic (Senato)
unmatched_S_2018 <- Senato_2018 %>%
  anti_join(lookup_comuni_regione_18, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

print(unmatched_C_2018)
print(unmatched_S_2018)

################################################################################
### 2.8. 2022 ---> confini_2022

# 1. Build the 2022 Region Dictionary
lookup_regioni_22 <- conf_regioni_22 %>%
  st_drop_geometry() %>%
  mutate(REGIONE_ISTAT = clean_geo_names(DEN_REG)) %>%
  select(COD_REG, REGIONE_ISTAT) %>%
  distinct()

# 2. Build the Region-Based Lookup for Comuni (2022)
lookup_comuni_regione_22 <- conf_comuni_22 %>% 
  st_drop_geometry() %>%
  mutate(COMUNE_CLEAN = clean_geo_names(COMUNE)) %>%
  left_join(
    conf_province_22 %>% st_drop_geometry() %>% 
      mutate(PROVINCIA_CLEAN = clean_geo_names(DEN_PROV)) %>% select(COD_PROV, PROVINCIA_CLEAN), 
    by = "COD_PROV"
  ) %>%
  left_join(lookup_regioni_22, by = "COD_REG") %>%
  # ADDED COD_REG AND COD_PROV HERE
  select(COMUNE_CLEAN, REGIONE_ISTAT, PROVINCIA_CLEAN, COD_REG, COD_PROV, PRO_COM_T) %>%
  distinct()

# 3. Create the 2022 Patcher (Skeleton ready for mergers)
patch_2022 <- function(df) {
  df %>%
    mutate(
      # 1. Strip the German translation from Alto Adige BEFORE cleaning
      COMUNE = str_remove(COMUNE, "/.*"),
      
      COMUNE_CLEAN = clean_geo_names(COMUNE),
      REGIONE_CLEAN = clean_geo_names(REGIONE), 
      
      # 2. Bulletproof Region Matcher (Ignores encoding issues!)
      REGIONE_CLEAN = case_when(
        str_detect(REGIONE_CLEAN, "^TRENTINO ALTO ADIGE") ~ "TRENTINO ALTO ADIGE",
        REGIONE_CLEAN == "AOSTA" ~ "VALLE D AOSTA",
        TRUE ~ REGIONE_CLEAN
      ),
      
      COMUNE_CLEAN = case_when(
        COMUNE_CLEAN == "SAN DORLIGO DELLA VALLE DOLINA" ~ "SAN DORLIGO DELLA VALLE", 
        COMUNE_CLEAN == "CASORZO MONFERRATO" ~ "CASORZO", 
        TRUE ~ COMUNE_CLEAN
      )
    )
}

Camera_2022 <- patch_2022(Camera_2022)
Senato_2022 <- patch_2022(Senato_2022)

Camera_2022_geo <- Camera_2022 %>%
  left_join(lookup_comuni_regione_22, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))
Senato_2022_geo <- Senato_2022 %>%
  left_join(lookup_comuni_regione_22, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT"))

# 5. Run the Diagnostic (Camera)
unmatched_C_2022 <- Camera_2022 %>%
  anti_join(lookup_comuni_regione_22, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

# 6. Run the Diagnostic (Senato)
unmatched_S_2022 <- Senato_2022 %>%
  anti_join(lookup_comuni_regione_22, by = c("COMUNE_CLEAN", "REGIONE_CLEAN" = "REGIONE_ISTAT")) %>%
  select(REGIONE_CLEAN, COMUNE, COMUNE_CLEAN) %>%
  distinct() %>%
  arrange(REGIONE_CLEAN, COMUNE_CLEAN)

print(unmatched_C_2022)
print(unmatched_S_2022)

###############################################################################

library(dplyr)
library(tidyr)

# 1. Gather all dataframes into a named list
list_dfs <- list(
  "Camera_1987" = Camera_1987_geo, "Senato_1987" = Senato_1987_geo,
  "Camera_1992" = Camera_1992_geo, "Senato_1992" = Senato_1992_geo,
  "Camera_1994" = Camera_1994_geo, "Senato_1994" = Senato_1994_geo,
  "Camera_1996" = Camera_1996_geo, "Senato_1996" = Senato_1996_geo,
  "Camera_2001" = Camera_2001_geo, "Senato_2001" = Senato_2001_geo,
  "Camera_2006" = Camera_2006_geo, "Senato_2006" = Senato_2006_geo,
  "Camera_2008" = Camera_2008_geo, "Senato_2008" = Senato_2008_geo,
  "Camera_2013" = Camera_2013_geo, "Senato_2013" = Senato_2013_geo,
  "Camera_2018" = Camera_2018_geo, "Senato_2018" = Senato_2018_geo,
  "Camera_2022" = Camera_2022_geo, "Senato_2022" = Senato_2022_geo
)

# 2. Build the master dataframe
df_storico_master <- bind_rows(list_dfs, .id = "DATASET") %>%
  
  # Split "Camera_1987" into TIPO_ELEZIONE ("Camera") and ANNO (1987)
  separate(DATASET, into = c("TIPO_ELEZIONE", "ANNO"), sep = "_", convert = TRUE) %>%
  
  mutate(
    # Unify Region names (Prioritize the cleaned versions, fall back to whatever the raw file used)
    REGIONE = coalesce(REGIONE_CLEAN, REGIONE, CIRCOSCRIZIONE),
    
    # Unify Province names
    PROVINCIA = coalesce(PROVINCIA_CLEAN, PROVINCIA),
    
    # Set the universal ISTAT code as the main PRO_COM
    PRO_COM = as.character(PRO_COM_T),
    
    # Ensure vote/elector counts are integers
    ELETTORI = as.integer(ELETTORI),
    ELETTORI_MASCHI = as.integer(ELETTORI_MASCHI),
    VOTANTI = as.integer(VOTANTI),
    VOTI_LISTA = as.integer(VOTI_LISTA)
  ) %>%
  
  # 3. Select ONLY your target columns
  select(
    COMUNE, 
    PROVINCIA, 
    REGIONE, 
    PRO_COM, 
    any_of(c("COD_PROV", "COD_REG")), # Safely grabs these where they exist, leaves NA where they don't
    ANNO, 
    TIPO_ELEZIONE, 
    ELETTORI, 
    ELETTORI_MASCHI, 
    VOTANTI, 
    LISTA, 
    VOTI_LISTA
  )

# ARRICCHIMENTO DATI (Feature Engineering)

df_storico_master <- df_storico_master %>%
  mutate(
    LEGGE_ELETTORALE = case_when(
      ANNO %in% c(1987, 1992)       ~ "Proporzionale",
      ANNO %in% c(1994, 1996, 2001) ~ "Mattarellum",
      ANNO %in% c(2006, 2008, 2013) ~ "Porcellum",
      ANNO %in% c(2018, 2022)       ~ "Rosatellum",
      TRUE ~ "Altro" 
    )
  )

dataset_da_tenere <- 
  c("Camera_1987_geo", "Camera_1992_geo", "Camera_1994_geo", "Camera_1996_geo", 
    "Camera_2001_geo", "Camera_2006_geo", "Camera_2008_geo", "Camera_2013_geo", 
    "Camera_2018_geo", "Camera_2022_geo", "Senato_1987_geo", "Senato_1992_geo", 
    "Senato_1994_geo", "Senato_1996_geo", "Senato_2001_geo", "Senato_2006_geo", 
    "Senato_2008_geo", "Senato_2013_geo", "Senato_2018_geo", "Senato_2022_geo", 
    "conf_comuni_91", "conf_comuni_01", "conf_comuni_06", "conf_comuni_08", "conf_comuni_13", "conf_comuni_18", "conf_comuni_22",
    "conf_province_91", "conf_province_01", "conf_province_06", "conf_province_08", "conf_province_13", "conf_province_18", "conf_province_22",
    "conf_regioni_91", "conf_regioni_01", "conf_regioni_06", "conf_regioni_08", "conf_regioni_13", "conf_regioni_18", "conf_regioni_22",
    "df_storico_master"
  )

rm(list = setdiff(ls(), dataset_da_tenere))


#################################################################################

### 3. COVARIATES using ISTAT INDICATORS ----

### 3.1. 8mila Census (1991, 2001, 2011)

library(readr)
library(dplyr)
library(stringi)
library(stringr)
# 1. Define formatting rules
ita_locale <- locale(encoding = "ISO-8859-1", decimal_mark = ",", grouping_mark = ".")
istat_na <- c("", "NA", "…")

# 2. EXPLICITLY name the columns to keep (fixes the subset error)
cols_to_keep <- c(
  "AnnoCP", 
  "Livello territoriale", 
  "Codice Regione 2011", 
  "Codice Provincia 2011", 
  "Codice Comune all'epoca", 
  "Codice comune 2011", 
  "Denominazione del territorio", 
  "P1", "P7", "P13", "SS4", "L12"
)

# 3. Import only the selected columns
indicators_01 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_01.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_02 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_02.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_03 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_03.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_04 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_04.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_05 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_05.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_06 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_06.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_07 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_07.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_08 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_08.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_09 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_09.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_10 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_10.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)

indicators_11 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_11.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_12 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_12.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_13 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_13.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_14 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_14.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_15 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_15.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_16 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_16.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_17 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_17.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_18 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_18.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_19 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_19.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
indicators_20 <- read_delim("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/confini-epoca_20.csv", delim = ";", locale = ita_locale, na = istat_na, col_types = cols(.default = "c"), col_select = all_of(cols_to_keep), trim_ws = TRUE)
# 4. Bind the streamlined dataframes
ISTAT_indicators <- bind_rows(
  indicators_01, indicators_02, indicators_03, indicators_04, indicators_05,
  indicators_06, indicators_07, indicators_08, indicators_09, indicators_10,
  indicators_11, indicators_12, indicators_13, indicators_14, indicators_15,
  indicators_16, indicators_17, indicators_18, indicators_19, indicators_20
) %>%
  filter(!is.na(AnnoCP)) # Filter out ghosts

# 5. Convert the kept indicator columns safely to numeric
ISTAT_indicators <- type.convert(ISTAT_indicators, as.is = TRUE, dec = ",")

# Filter the dataframe to keep only the relevant census years
ISTAT_indicators <- ISTAT_indicators %>%
  filter(AnnoCP %in% c(1991, 2001, 2011))

rm(list = c(
  "indicators_01", "indicators_02", "indicators_03", "indicators_04", "indicators_05",
  "indicators_06", "indicators_07", "indicators_08", "indicators_09", "indicators_10",
  "indicators_11", "indicators_12", "indicators_13", "indicators_14", "indicators_15",
  "indicators_16", "indicators_17", "indicators_18", "indicators_19", "indicators_20"
))

# 1. Create a "smart" cleaning function
fix_istat_numbers <- function(x) {
  # If the column is already a number (like P7, SS4, L12), leave it untouched!
  if(is.numeric(x)) {
    return(x)
  }
  
  # If it's text (like P1 and P13), fix the Italian formatting
  x_clean <- as.character(x)
  x_clean <- str_remove_all(x_clean, "\\.")      # Remove thousands dots ("1.250,5" -> "1250,5")
  x_clean <- str_replace_all(x_clean, ",", "\\.")  # Change decimal comma to dot ("1250,5" -> "1250.5")
  
  # Convert to standard numeric
  return(as.numeric(x_clean))
}

# 2. Apply it safely across all your indicator columns
ISTAT_indicators <- ISTAT_indicators %>%
  mutate(across(c(P1, P7, P13, SS4, L12), fix_istat_numbers))
### 3.2. Indicators constructed for years (2018, 2022)

## i) POPULATION DENSITY
# P7 = Pop. Totale / Superf. in km^2

carta_comuni <- read.csv("C:/Users/massi/Downloads/Carta Identità dei Comuni (IT1,DF_CARTA_IDENTITA,1.0).csv")

# 1. Build the 2018 Density Dataframe
density_2018 <- carta_comuni %>%
  # Select and instantly rename the long column names
  select(
    `Codice comune 2011` = Comune..ID.,  # Keeping the name consistent with historical data
    `Denominazione del territorio` = Comune,
    Pop_2018 = `X2018.Popolazione.al.1º.gennaio..numero.abitanti.`,
    Sup_2018 = `X2018.Superficie..kmq.`
  ) %>%
  # Add the Year and calculate P7 (Population / Area)
  mutate(
    AnnoCP = 2018,
    P7 = Pop_2018 / Sup_2018
  ) %>%
  # Drop the raw population and area columns since we only need P7 now
  select(`AnnoCP`, `Codice comune 2011`, `Denominazione del territorio`, P7)

# 2. Build the 2022 Density Dataframe
density_2022 <- carta_comuni %>%
  select(
    `Codice comune 2011` = Comune..ID.,
    `Denominazione del territorio` = Comune,
    Pop_2022 = `X2022.Popolazione.al.1º.gennaio..numero.abitanti.`,
    Sup_2022 = `X2022.Superficie..kmq.`
  ) %>%
  mutate(
    AnnoCP = 2022,
    P7 = Pop_2022 / Sup_2022
  ) %>%
  select(`AnnoCP`, `Codice comune 2011`, `Denominazione del territorio`, P7)

# 3. Bind them together into one clean modern density dataset!
density_18_22 <- bind_rows(density_2018, density_2022)

## ii) AGING INDEX
# P13 = (Popolazione > 64) / (Popolazione < 15) * 100

classi_eta_22 <- read.csv("C:/Users/massi/Downloads/Popolazione residente al 1° gennaio - Comuni per età (IT1,DF_22_289_COMUNIETA1,1.0) (3).csv")
classi_eta_19 <- read.csv("C:/Users/massi/Downloads/Popolazione residente al 1° gennaio - Comuni per età (IT1,DF_22_289_COMUNIETA1,1.0) (2).csv")

aging_2018 <- classi_eta_19 %>%
  # Group by row so we can sum horizontally
  rowwise() %>%
  mutate(
    # Sum columns 3 through 17 (Ages 0 to 14)
    Pop_0_14 = sum(c_across(3:17), na.rm = TRUE),
    
    # Sum columns 18 through 53 (Ages 65 to 100+)
    Pop_65_plus = sum(c_across(18:53), na.rm = TRUE)
  ) %>%
  ungroup() %>% # Always ungroup after rowwise()
  mutate(
    AnnoCP = 2018,
    
    # Calculate the Aging Index
    P13 = (Pop_65_plus / Pop_0_14) * 100,
    
    # Fix for tiny mountain villages: if there are literally 0 children, 
    # math returns "Inf". We convert that to NA to protect your future models.
    P13 = ifelse(is.infinite(P13), NA, P13)
  ) %>%
  # Keep only the essential columns and standardize names
  select(
    `AnnoCP`,
    `Codice comune 2011` = Comune..ID.,
    `Denominazione del territorio` = Comune,
    P13
  )

aging_2022 <- classi_eta_22 %>%
  rowwise() %>%
  mutate(
    Pop_0_14 = sum(c_across(3:17), na.rm = TRUE),
    Pop_65_plus = sum(c_across(18:53), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AnnoCP = 2022,
    P13 = (Pop_65_plus / Pop_0_14) * 100,
    P13 = ifelse(is.infinite(P13), NA, P13)
  ) %>%
  select(
    `AnnoCP`,
    `Codice comune 2011` = Comune..ID.,
    `Denominazione del territorio` = Comune,
    P13
  )

aging_18_22 <- bind_rows(aging_2018, aging_2022)

## iii) EMPLOYEMENT RATE
# L12 = (Occupati 15-64 anni) / (Popolazione 15-64 anni) * 100

employement_18 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/occupazione_2018.csv", sep=";", na = "..")
employement_22 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/occupazione_2022.csv", sep=";", na = "..")

employement_18 <- employement_18 %>%
  mutate(
    AnnoCP = 2018,
    # Strip any potential thousands-dots and force to numeric
    occupato = as.numeric(gsub("\\.", "", occupato)),
    totale = as.numeric(gsub("\\.", "", totale)),
    
    # Now the math will execute perfectly
    L12 = (occupato / totale) * 100
  ) %>%
  select(AnnoCP, Territorio, L12)

employement_22 <- employement_22 %>%
  mutate(
    AnnoCP = 2022,
    occupato = as.numeric(gsub("\\.", "", occupato)),
    totale = as.numeric(gsub("\\.", "", totale)),
    
    L12 = (occupato / totale) * 100
  ) %>%
  select(AnnoCP, Territorio, L12)

employement_18_22 <- bind_rows(employement_18, employement_22)

## iv) INCIDENCE OF UNIVERSITY GRADUATES
# SS4 (# laureati o titolo superiore) / (Popolazione > 8) * 100

graduated_18 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/istruzione_2018.csv", sep=";", na = "..", encoding = "latin1")
graduated_22 <- read.csv("C:/Users/massi/OneDrive/Desktop/TESI/Covariate/istruzione_2022.csv", sep=";", na = "..")

graduated_18 <- graduated_18 %>%
  mutate(
    AnnoCP = 2018,
    
    # NEW: We now add High School Diplomas (Col 5) + Bachelor's (Col 6) + Master's/PhDs (Col 7)
    Totale_Diplomati_Laureati = rowSums(across(c(
      diploma.di.istruzione.secondaria.di.II.grado.o.di.qualifica.professionale..corso.di.3.4.anni..compresi.IFTS,
      diploma.di.tecnico.superiore.ITS.o.titolo.di.studio.terziario.di.primo.livello,
      titolo.di.studio.terziario.di.secondo.livello.e.dottorato.di.ricerca
    )), na.rm = TRUE),
    
    # Calculate correct SS4
    SS4 = (Totale_Diplomati_Laureati / totale) * 100
  ) %>%
  select(AnnoCP, Territorio, SS4)

graduated_22 <- graduated_22 %>%
  mutate(
    AnnoCP = 2022,
    
    Totale_Diplomati_Laureati = rowSums(across(c(
      diploma.di.istruzione.secondaria.di.II.grado.o.di.qualifica.professionale..corso.di.3.4.anni..compresi.IFTS,
      diploma.di.tecnico.superiore.ITS.o.titolo.di.studio.terziario.di.primo.livello,
      titolo.di.studio.terziario.di.secondo.livello.e.dottorato.di.ricerca
    )), na.rm = TRUE),
    
    SS4 = (Totale_Diplomati_Laureati / totale) * 100
  ) %>%
  select(AnnoCP, Territorio, SS4)

graduated_18_22 <- bind_rows(graduated_18, graduated_22)


##############################################################################

### 1. FIRST FIX

filter_comuni_only <- function(df) {
  df %>%
    mutate(
      
      # 2. Extract and count the leading spaces
      leading_spaces = nchar(str_extract(Territorio, "^\\s*"))
    ) %>%
    # 3. Keep only the most indented rows (the Comuni)
    filter(leading_spaces == max(leading_spaces, na.rm = TRUE)) %>%
    # 4. Trim the spaces safely now that the encoding is fixed
    mutate(Territorio = str_trim(Territorio)) %>%
    select(-leading_spaces)
}

graduated_18_22 <- filter_comuni_only(graduated_18_22)
employement_18_22 <- filter_comuni_only(employement_18_22)

### 2. SECOND FIX

# 1. Create a MODERN dictionary from your aging dataset
# (Because the 2018/2022 aging dataset already contains the correct IDs for the new merged towns)
modern_dictionary <- aging_18_22 %>%
  select(AnnoCP, `Codice comune 2011`, `Denominazione del territorio`) %>%
  distinct()

# 2. Attach these modern codes to Education and Employment
graduated_18_22_full <- graduated_18_22 %>%
  rename(`Denominazione del territorio` = Territorio) %>%
  left_join(modern_dictionary, by = c("AnnoCP", "Denominazione del territorio"))

employement_18_22_full <- employement_18_22 %>%
  rename(`Denominazione del territorio` = Territorio) %>%
  left_join(modern_dictionary, by = c("AnnoCP", "Denominazione del territorio"))


modern_indicators <- density_18_22 %>%
  # Rename the ID column to reflect what it actually is (Current ID = ID all'epoca)
  rename(`Codice Comune all'epoca` = `Codice comune 2011`) %>%
  select(AnnoCP, `Codice Comune all'epoca`, `Denominazione del territorio`, P7) %>%
  
  # Join Aging
  left_join(
    aging_18_22 %>% 
      rename(`Codice Comune all'epoca` = `Codice comune 2011`) %>%
      select(AnnoCP, `Codice Comune all'epoca`, P13), 
    by = c("AnnoCP", "Codice Comune all'epoca")
  ) %>%
  
  # Join Education
  left_join(
    graduated_18_22_full %>% 
      rename(`Codice Comune all'epoca` = `Codice comune 2011`) %>%
      select(AnnoCP, `Codice Comune all'epoca`, SS4), 
    by = c("AnnoCP", "Codice Comune all'epoca")
  ) %>%
  
  # Join Employment
  left_join(
    employement_18_22_full %>% 
      rename(`Codice Comune all'epoca` = `Codice comune 2011`) %>%
      select(AnnoCP, `Codice Comune all'epoca`, L12), 
    by = c("AnnoCP", "Codice Comune all'epoca")
  )



# Stack the historical data and modern data together
final_covariates <- bind_rows(ISTAT_indicators, modern_indicators) %>%
  # Arrange nicely by the actual historical code, then by Year
  arrange(`Codice Comune all'epoca`, AnnoCP)



#################################################################################

## 2. SECOND FIX

# 1. Define the mapping between Election Year and Census Year
df_storico_master <- df_storico_master %>%
  mutate(
    AnnoCP = case_when(
      ANNO %in% c(1987, 1992, 1994, 1996) ~ 1991, # Use 1991 census for the 80s/90s elections
      ANNO %in% c(2001, 2006)             ~ 2001, # Use 2001 census for the early 2000s
      ANNO %in% c(2008, 2013)             ~ 2011, # Use 2011 census for 2008 and 2013
      ANNO == 2018                        ~ 2018, # Perfect modern match
      ANNO == 2022                        ~ 2022, # Perfect modern match
      TRUE ~ NA_real_
    )
  )

# 2. Prepare the covariates for the merge
covariates_clean <- final_covariates %>%
  # Strip any leading zeros to create a universal matching key
  mutate(MATCH_ID = as.character(as.numeric(`Codice Comune all'epoca`))) %>%
  # Drop redundant columns so we don't clutter your master dataset
  select(
    AnnoCP, 
    MATCH_ID, 
    P7_Density = P7, 
    P13_Aging = P13, 
    SS4_Education = SS4, 
    L12_Employment = L12
  )

# 3. Perform the Master Join!
df_storico_master <- df_storico_master %>%
  mutate(MATCH_ID = as.character(as.numeric(PRO_COM))) %>%
  left_join(covariates_clean, by = c("AnnoCP", "MATCH_ID")) %>%
  # Drop the temporary matching keys
  select(-MATCH_ID, -AnnoCP)

# Let's see exactly how many NA covariates we have per election year
na_summary <- df_storico_master %>%
  group_by(ANNO) %>%
  summarise(
    Total_Rows = n(),
    Missing_Covariates = sum(is.na(P7_Density)),
    Percentage_Missing = round((sum(is.na(P7_Density)) / n()) * 100, 2)
  )

print(na_summary)

###############################################################################

df_storico_master <- df_storico_master %>%
  mutate(
    REGIONE = case_when(
      # 1. Handle the tricky cross-regional circumscriptions first!
      REGIONE == "PERUGIA-TERNI-RIETI" & PROVINCIA == "RIETI" ~ "LAZIO",
      REGIONE == "PERUGIA-TERNI-RIETI" ~ "UMBRIA",
      REGIONE == "UDINE-BELLUNO-GORIZIA-PORDENONE" & PROVINCIA == "BELLUNO" ~ "VENETO",
      REGIONE == "UDINE-BELLUNO-GORIZIA-PORDENONE" ~ "FRIULI-VENEZIA GIULIA",
      
      # 2. Standardize all other circumscriptions and variations into the 20 official regions
      REGIONE %in% c("VALLE D AOSTA", "VALLE D AOSTA VALLEE D AOSTE", "AOSTA") ~ "VALLE D'AOSTA",
      REGIONE %in% c("ABRUZZO", "L'AQUILA-PESCARA-CHIETI-TERAMO") ~ "ABRUZZO",
      REGIONE %in% c("BASILICATA", "POTENZA-MATERA") ~ "BASILICATA",
      REGIONE %in% c("CALABRIA", "CATANZARO-COSENZA-REGGIO CALABRIA") ~ "CALABRIA",
      REGIONE %in% c("CAMPANIA", "CAMPANIA 1", "CAMPANIA 2", "BENEVENTO-AVELLINO-SALERNO", "NAPOLI-CASERTA") ~ "CAMPANIA",
      REGIONE %in% c("EMILIA-ROMAGNA", "EMILIA ROMAGNA", "BOLOGNA-FERRARA-RAVENNA-FORLI", "BOLOGNA-FERRARA-RAVENNA-FORLI'", "PARMA-MODENA-PIACENZA-REGGIO EMILIA") ~ "EMILIA-ROMAGNA",
      REGIONE %in% c("FRIULI-VENEZIA GIULIA", "FRIULI VENEZIA GIULIA", "TRIESTE") ~ "FRIULI-VENEZIA GIULIA",
      REGIONE %in% c("LAZIO", "LAZIO 1", "LAZIO 2", "ROMA-VITERBO-LATINA-FROSINONE") ~ "LAZIO",
      REGIONE %in% c("LIGURIA", "GENOVA-IMPERIA-LA SPEZIA-SAVONA") ~ "LIGURIA",
      REGIONE %in% c("LOMBARDIA", "LOMBARDIA 1", "LOMBARDIA 2", "LOMBARDIA 3", "BRESCIA-BERGAMO", "COMO-SONDRIO-VARESE", "MANTOVA-CREMONA", "MILANO-PAVIA") ~ "LOMBARDIA",
      REGIONE %in% c("MARCHE", "ANCONA-PESARO-MACERATA-ASCOLI PICENO") ~ "MARCHE",
      REGIONE %in% c("MOLISE", "CAMPOBASSO-ISERNIA") ~ "MOLISE",
      REGIONE %in% c("PIEMONTE", "PIEMONTE 1", "PIEMONTE 2", "CUNEO-ALESSANDRIA-ASTI", "TORINO-NOVARA-VERCELLI") ~ "PIEMONTE",
      REGIONE %in% c("PUGLIA", "BARI-FOGGIA", "LECCE-BRINDISI-TARANTO") ~ "PUGLIA",
      REGIONE %in% c("SARDEGNA", "CAGLIARI-SASSARI-NUORO-ORISTANO") ~ "SARDEGNA",
      REGIONE %in% c("SICILIA", "SICILIA 1", "SICILIA 2", "CATANIA-MESSINA-SIRACUSA-RAGUSA-ENNA", "PALERMO-TRAPANI-AGRIGENTO-CALTANISSETTA") ~ "SICILIA",
      REGIONE %in% c("TOSCANA", "FIRENZE-PISTOIA", "PISA-LIVORNO-LUCCA-MASSA CARRARA", "SIENA-AREZZO-GROSSETO") ~ "TOSCANA",
      REGIONE %in% c("TRENTINO-ALTO ADIGE", "TRENTINO ALTO ADIGE", "TRENTINO ALTO ADIGE SUDTIROL", "TRENTO-BOLZANO") ~ "TRENTINO-ALTO ADIGE",
      REGIONE %in% c("UMBRIA") ~ "UMBRIA",
      REGIONE %in% c("VENETO", "VENETO 1", "VENETO 2", "VENEZIA-TREVISO", "VERONA-PADOVA-VICENZA-ROVIGO") ~ "VENETO",
      
      # 3. Fallback just in case something new slips through
      TRUE ~ REGIONE
    )
  )

df_storico_master <- df_storico_master %>%
  mutate(
    SISTEMA_ELETTORALE = if_else(LEGGE_ELETTORALE == "Proporzionale", "Puro_Prop", "Misto")
  )


##################################################################################
## 3. THIRD FIX

library(tidyr)

df_storico_master <- df_storico_master %>%
  
  # 1. Fix the Ministry typos so the names match across all years
  mutate(
    COMUNE = case_when(
      COMUNE == "GONNASFANADIGA" ~ "GONNOSFANADIGA",
      COMUNE == "SANTA TERESA DI GALLURA" ~ "SANTA TERESA GALLURA",
      TRUE ~ COMUNE # Leave all other towns exactly as they are
    )
  ) %>%
  
  # 2. Group by the now-harmonized municipality names
  group_by(COMUNE) %>%
  
  # 3. Sort chronologically
  arrange(ANNO) %>%
  
  # 4. Fill the NAs forward
  fill(P13_Aging, P7_Density, SS4_Education, L12_Employment, .direction = "down") %>%
  
  ungroup()

# 5. The Final Victory Check!
df_storico_master %>%
  filter(ANNO == 2006, REGIONE == "SARDEGNA") %>%
  group_by(PROVINCIA, COD_PROV) %>%
  summarise(
    Total_Municipalities = n_distinct(PRO_COM),
    Missing_Employment = sum(is.na(L12_Employment)),
    .groups = 'drop'
  )

df_storico_master <- df_storico_master %>%
  mutate(
    # Patch the missing historical code
    PRO_COM = ifelse(COMUNE == "SESSA CILENTO" & is.na(PRO_COM), 65141, PRO_COM),
    COD_PROV = ifelse(COMUNE == "SESSA CILENTO" & is.na(COD_PROV), 65, COD_PROV),
    COD_REG = ifelse(COMUNE == "SESSA CILENTO" & is.na(COD_REG), 15, COD_REG)
  )

#"Camera_1987_geo", "Camera_1992_geo", "Camera_1994_geo", "Camera_1996_geo", 
#"Camera_2001_geo", "Camera_2006_geo", "Camera_2008_geo", "Camera_2013_geo", 
#"Camera_2018_geo", "Camera_2022_geo", "Senato_1987_geo", "Senato_1992_geo", 
#"Senato_1994_geo", "Senato_1996_geo", "Senato_2001_geo", "Senato_2006_geo", 
#"Senato_2008_geo", "Senato_2013_geo", "Senato_2018_geo", "Senato_2022_geo", 

# 1. The Bulletproof Override
df_storico_master <- df_storico_master %>%
  mutate(
    ELETTORI = case_when(
      as.numeric(PRO_COM) == 87009 & 
        as.numeric(ANNO) == 1996 & 
        grepl("SENATO", toupper(TIPO_ELEZIONE)) ~ 15000, # Matches SENATO, Senato, Senato  , etc.
      
      TRUE ~ ELETTORI
    )
  )

dataset_da_tenere <- 
  c("conf_comuni_91", "conf_comuni_01", "conf_comuni_06", "conf_comuni_08", "conf_comuni_13", "conf_comuni_18", "conf_comuni_22",
    "conf_province_91", "conf_province_01", "conf_province_06", "conf_province_08", "conf_province_13", "conf_province_18", "conf_province_22",
    "conf_regioni_91", "conf_regioni_01", "conf_regioni_06", "conf_regioni_08", "conf_regioni_13", "conf_regioni_18", "conf_regioni_22",
    "df_storico_master", "final_covariates"
  )

rm(list = setdiff(ls(), dataset_da_tenere))
