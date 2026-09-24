load("C:/Users/massi/OneDrive/Desktop/TESI/R codes/df_3.RData")

library(dplyr)
library(sf)
library(ggplot2)
library(scales) 
library(viridis)
library(stringr)
library(tidyr)


################################################################################
### 1.1 HISTORICAL VOTER TURNOUT (SENATO)###

# We process df_storico_master to get one clean turnout % per Province/Year
provincial_turnout <- df_storico_master %>%
  # Filter for the correct spelling and house
  filter(TIPO_ELEZIONE == "Camera") %>%
  
  # Remove party-level duplicates (LISTA) to get unique municipality totals
  distinct(ANNO, COD_PROV, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
  
  # Group by Year and Province Code
  group_by(ANNO, COD_PROV) %>%
  
  # Sum raw numbers for the whole province before calculating %
  summarise(
    Tot_Elettori = sum(ELETTORI, na.rm = TRUE),
    Tot_Votanti = sum(VOTANTI, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Calculate the true Turnout percentage
  mutate(
    TURNOUT = (Tot_Votanti / Tot_Elettori) * 100,
    COD_PROV = as.numeric(COD_PROV) # Ensure numeric for the join
  )

# We align the Coordinate Reference System (CRS) so they can be merged
prepare_shp <- function(shp) {
  shp %>% 
    mutate(COD_PROV = as.numeric(COD_PROV)) %>% 
    st_transform(4326)
}

conf_province_91_clean <- prepare_shp(conf_province_91)
conf_province_01_clean <- prepare_shp(conf_province_01)
conf_province_06_clean <- prepare_shp(conf_province_06)
conf_province_08_clean <- prepare_shp(conf_province_08)
conf_province_13_clean <- prepare_shp(conf_province_13)
conf_province_18_clean <- prepare_shp(conf_province_18)
conf_province_22_clean <- prepare_shp(conf_province_22)

# Join 1987 borders to years 1987 through 1996
map_87_96 <- conf_province_91_clean %>%
  left_join(provincial_turnout %>% filter(ANNO %in% c(1987, 1992, 1994, 1996)), by = "COD_PROV")

map_01 <- conf_province_01_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2001), by = "COD_PROV")

map_06 <- conf_province_06_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2006), by = "COD_PROV")

map_08 <- conf_province_08_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2008), by = "COD_PROV")

map_13 <- conf_province_13_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2013), by = "COD_PROV")

map_18 <- conf_province_18_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2018), by = "COD_PROV")

map_22 <- conf_province_22_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2022), by = "COD_PROV")

# Combine all eras into one master spatial object, removing empty rows
master_map_data <- bind_rows(map_87_96, map_01, map_06, map_08, map_13, map_18, map_22) %>%
  filter(!is.na(ANNO))

plot1 <- ggplot(master_map_data) +
  # Updated to match the thinner black borders of the posterior maps
  geom_sf(aes(fill = TURNOUT), color = "black", linewidth = 0.05) + 
  scale_fill_distiller(
    palette = "YlGnBu", 
    direction = 1, 
    name = "Turnout (%)",
    na.value = "grey80"
  ) +
  facet_wrap(~ ANNO, ncol = 5) +
  theme_void() +
  labs(
    title = "Historical Voter Turnout in Italy (Camera)",
    subtitle = "Analysis by Province using Historically Accurate Borders (1987-2022)",
    caption = "Source: ELIGENDO | Mapping: ISTAT Historical Borders"
  ) +
  theme(
    # Aligned title sizes with your posterior estimate maps
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30", margin = margin(b = 15)),
    
    # --- THIS CREATES THE BOXES AROUND THE YEARS AND PANELS ---
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_rect(fill = "#faebd7", color = "black", linewidth = 1),
    strip.text = element_text(size = 11, face = "bold", margin = margin(t = 5, b = 5)),
    
    # Legend styling matched to the MCMC script
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(2.5, "cm")
  )

print(plot1)

ggsave(
  filename = "Map_National_Turnout_Camera.png", 
  plot = plot1, 
  width = 11.7,       
  height = 8.3,       
  dpi = 300,          
  bg = "white"        
)

################################################################################
### 1.2 HISTORICAL VOTER TURNOUT (CAMERA) ###

# We process df_storico_master to get one clean turnout % per Province/Year
provincial_turnout <- df_storico_master %>%
  # Filter for the correct spelling and house
  filter(TIPO_ELEZIONE == "Camera") %>%
  
  # Remove party-level duplicates (LISTA) to get unique municipality totals
  distinct(ANNO, COD_PROV, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
  
  # Group by Year and Province Code
  group_by(ANNO, COD_PROV) %>%
  
  # Sum raw numbers for the whole province before calculating %
  summarise(
    Tot_Elettori = sum(ELETTORI, na.rm = TRUE),
    Tot_Votanti = sum(VOTANTI, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Calculate the true Turnout percentage
  mutate(
    TURNOUT = (Tot_Votanti / Tot_Elettori) * 100,
    COD_PROV = as.numeric(COD_PROV) # Ensure numeric for the join
  )

# We align the Coordinate Reference System (CRS) so they can be merged
prepare_shp <- function(shp) {
  shp %>% 
    mutate(COD_PROV = as.numeric(COD_PROV)) %>% 
    st_transform(4326)
}

# Join 1987 borders to years 1987 through 1996
map_87_96 <- conf_province_91_clean %>%
  left_join(provincial_turnout %>% filter(ANNO %in% c(1987, 1992, 1994, 1996)), by = "COD_PROV")

map_01 <- conf_province_01_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2001), by = "COD_PROV")

map_06 <- conf_province_06_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2006), by = "COD_PROV")

map_08 <- conf_province_08_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2008), by = "COD_PROV")

map_13 <- conf_province_13_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2013), by = "COD_PROV")

map_18 <- conf_province_18_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2018), by = "COD_PROV")

map_22 <- conf_province_22_clean %>%
  left_join(provincial_turnout %>% filter(ANNO == 2022), by = "COD_PROV")

# Combine all eras into one master spatial object, removing empty rows
master_map_data <- bind_rows(map_87_96, map_01, map_06, map_08, map_13, map_18, map_22) %>%
  filter(!is.na(ANNO))

plot2 <- ggplot(master_map_data) +
  geom_sf(aes(fill = TURNOUT), color = "grey30", size = 0.1) +
  scale_fill_distiller(
    palette = "YlGnBu", 
    direction = 1, 
    name = "Turnout (%)",
    na.value = "grey80"
  ) +
  facet_wrap(~ ANNO, ncol = 5) +
  theme_void() +
  labs(
    title = "Historical Voter Turnout in Italy (Camera)",
    subtitle = "Analysis by Province using Historically Accurate Borders (1987-2022)",
    caption = "Source: Ministero dell'Interno | Mapping: ISTAT Historical Borders"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(b = 20)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave(
  filename = "Map_National_Turnout_Camera.png", 
  plot = plot2, 
  width = 10,       
  height = 6,       
  dpi = 300,       
  bg = "white"     
)

################################################################################
### 2 HISTORICAL VOTER TURNOUT MAP (GIF) ###

# Load the animation libraries
library(gganimate)
library(gifski)

# Build the animated ggplot object
plot3 <- ggplot(master_map_data) +
  geom_sf(aes(fill = TURNOUT), color = "grey30", linewidth = 0.1) +
  scale_fill_distiller(
    palette = "YlGnBu", 
    direction = 1, 
    name = "Turnout (%)",
    na.value = "grey80"
  ) +
  theme_void() +
  labs(
    title = "Historical Voter Turnout in Italy (Senato)",
    # The {current_frame} syntax dynamically updates the year on each frame
    subtitle = "Year: {current_frame}", 
    caption = "Source: Ministero dell'Interno | Mapping: ISTAT Historical Borders"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    # Made the subtitle slightly larger and blue to emphasize the changing year
    plot.subtitle = element_text(size = 24, face = "bold", hjust = 0.5, color = "steelblue", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  ) +
  transition_manual(ANNO)

# Render the animation
anim <- animate(
  plot3, 
  fps = 5,          # Speed: 1.5 frames per second (slow enough to read the map)
  width = 10,         # 10 inches wide
  height = 10,        # 10 inches tall (maps are often better as squares or slightly tall)
  units = "in", 
  res = 300,          # 300 DPI for high-quality rendering
  bg = "white"        # Force white background
)

# Save the GIF to your computer
anim_save("Animated_National_Turnout.gif", animation = anim)

################################################################################
### 3.1 NATIONAL TURNOUT TREND ###

national_turnout <- df_storico_master %>%
  filter(TIPO_ELEZIONE == "Camera") %>%
  # Remove party-level duplicates to get unique municipality totals
  distinct(ANNO, PRO_COM, ELETTORI, VOTANTI) %>%
  # Group only by Year to get the National sum
  group_by(ANNO) %>%
  summarise(
    Naz_Elettori = sum(ELETTORI, na.rm = TRUE),
    Naz_Votanti = sum(VOTANTI, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate the National Turnout %
  mutate(
    TURNOUT_NAZ = (Naz_Votanti / Naz_Elettori) * 100
  )

trend_plot <- ggplot(national_turnout, aes(x = ANNO, y = TURNOUT_NAZ)) +
  geom_vline(xintercept = c(1993, 2005, 2017), linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 1993 - 0.5, y = 65, label = "Mattarellum", color = "red", angle = 90, size = 3) +
  annotate("text", x = 2005 - 0.5, y = 65, label = "Porcellum", color = "red", angle = 90, size = 3) +
  annotate("text", x = 2017 - 0.5, y = 65, label = "Rosatellum", color = "red", angle = 90, size = 3) +
  geom_area(fill = "grey50", alpha = 0.2) +
  geom_line(color = "grey50", linewidth = 1.2) +
  geom_point(color = "black", size = 3) +
  geom_text(aes(label = paste0(round(TURNOUT_NAZ, 1), "%")), vjust = -1.2, size = 4, fontface = "bold") +
  scale_x_continuous(breaks = national_turnout$ANNO) +
  scale_y_continuous(limits = c(60, 100), labels = scales::label_percent(scale = 1)) +
  theme_minimal() +
  labs(
    x = "Election Year",
    y = "Turnout (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11)
  )


ggsave(
  filename = "National_Turnout_Trend.png", 
  plot = trend_plot, 
  width = 10,       
  height = 6,       
  dpi = 300,        
  bg = "white"      
)

################################################################################
### 3.2 REGIONAL TURNOUT TRENDS ###

library(stringr)

# Define the updated generic function
plot_regional_trend <- function(region_name) {
  
  print(paste("Generating trend plot for:", region_name))

# 1. CALCULATE NATIONAL TURNOUT (THE BASELINE)
  national_turnout <- df_storico_master %>%
    filter(TIPO_ELEZIONE == "Camera") %>%
    distinct(ANNO, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
    group_by(ANNO) %>%
    summarise(
      Naz_Elettori = sum(ELETTORI, na.rm = TRUE),
      Naz_Votanti = sum(VOTANTI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(TURNOUT_NAZ = (Naz_Votanti / Naz_Elettori) * 100)
  

# 2. FILTER AND AGGREGATE REGIONAL DATA
  regional_turnout <- df_storico_master %>%
    filter(TIPO_ELEZIONE == "Camera") %>%
    filter(str_detect(toupper(REGIONE), toupper(region_name))) %>%
    distinct(ANNO, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
    group_by(ANNO) %>%
    summarise(
      Reg_Elettori = sum(ELETTORI, na.rm = TRUE),
      Reg_Votanti = sum(VOTANTI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(TURNOUT_REG = (Reg_Votanti / Reg_Elettori) * 100)
  
  # Safety Check
  if(nrow(regional_turnout) == 0) {
    stop(paste("No data found for region:", region_name, "- Please check spelling!"))
  }
  
  # Join the national baseline to the regional data
  plot_data <- regional_turnout %>%
    left_join(national_turnout, by = "ANNO")
  
# 3. DYNAMIC AXIS SCALING & HELPER DATAFRAME
  y_min <- min(c(plot_data$TURNOUT_REG, plot_data$TURNOUT_NAZ), na.rm = TRUE) - 5
  if (y_min > 60) y_min <- 60 
  
  # Create a tiny dataframe for the electoral laws so we can map them in the legend!
  laws_df <- data.frame(year = c(1993, 2005, 2017))
  
# 4. BUILD THE TIME SERIES PLOT
  p <- ggplot(plot_data, aes(x = ANNO)) +
    
    # 1. Electoral Law Lines (mapped inside aes() to trigger the legend)
    geom_vline(data = laws_df, aes(xintercept = year, color = "Electoral Law Change", linetype = "Electoral Law Change"), linewidth = 0.8) +
    
    # Keep the text annotations so readers know WHICH law is which
    annotate("text", x = 1993 - 0.5, y = y_min + 6, label = "Mattarellum (1993)", color = "red", angle = 90, size = 2.8) +
    annotate("text", x = 2005 - 0.5, y = y_min + 6, label = "Porcellum (2005)", color = "red", angle = 90, size = 2.8) +
    annotate("text", x = 2017 - 0.5, y = y_min + 6, label = "Rosatellum (2017)", color = "red", angle = 90, size = 2.8) +
    
    # 2. National Trend
    geom_line(aes(y = TURNOUT_NAZ, color = "National Average", linetype = "National Average"), linewidth = 1) +
    
    # 3. Regional Trend
    geom_area(aes(y = TURNOUT_REG), fill = "seagreen", alpha = 0.2) + # Left out of aes() to keep legend clean
    geom_line(aes(y = TURNOUT_REG, color = "Regional Turnout", linetype = "Regional Turnout"), linewidth = 1.2) +
    geom_point(aes(y = TURNOUT_REG, color = "Regional Turnout"), size = 3) +
    
    # 4. Text labels above the REGIONAL points
    geom_text(aes(y = TURNOUT_REG, label = paste0(round(TURNOUT_REG, 1), "%")), 
              vjust = -1.2, size = 4, fontface = "bold", color = "darkgreen") +
    
    # Styling the axes
    scale_x_continuous(breaks = plot_data$ANNO) +
    scale_y_continuous(limits = c(y_min, 100), labels = scales::label_percent(scale = 1)) +
    

# 5. UNIFY AND STYLE THE LEGEND
  # By giving both scales the exact same breaks and names, ggplot merges them into ONE legend
  scale_color_manual(
    name = NULL,
    breaks = c("Regional Turnout", "National Average", "Electoral Law Change"),
    values = c(
      "Regional Turnout" = "seagreen", 
      "National Average" = "grey40", 
      "Electoral Law Change" = "red"
    )
  ) +
    scale_linetype_manual(
      name = NULL,
      breaks = c("Regional Turnout", "National Average", "Electoral Law Change"),
      values = c(
        "Regional Turnout" = "solid", 
        "National Average" = "dashed", 
        "Electoral Law Change" = "dashed"
      )
    ) +
    
    theme_minimal() +
    labs(
      title = paste("Voter Turnout Trend in", str_to_title(region_name)),
      subtitle = "Camera dei Deputati (1987 - 2022)",
      x = "Election Year",
      y = "Turnout (%)",
      caption = "Source: Elaborazione su dati Ministero dell'Interno"
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key.width = unit(2, "cm") # Makes the dashed lines wider so they are easier to see!
    )
  
  return(p)
}

# LOOP TO GENERATE AND EXPORT ALL REGIONAL PLOTS

# Extract a clean list of all unique regions in your dataset (ignoring NAs)
all_regions <- df_storico_master %>%
  filter(!is.na(REGIONE)) %>%
  pull(REGIONE) %>%
  unique() %>%
  as.character()

# Loop through each region
for (reg in all_regions) {
  
  # Clean the region name to make it a safe filename 
  # (e.g., "VALLE D'AOSTA" becomes "VALLE_D_AOSTA")
  safe_filename <- str_replace_all(reg, "[^A-Za-z0-9]", "_")

  tryCatch({
    
    # Generate the plot using our updated function
    p <- plot_regional_trend(reg)
    
    # Save it to your computer
    ggsave(
      filename = paste0("Trend_Turnout_", safe_filename, ".png"), 
      plot = p, 
      width = 10,       
      height = 6,       
      dpi = 300,        
      bg = "white"      
    )
    
  }, error = function(e) {
    # If an error happens, print a warning in the console and move to the next region
    message(paste("Skipping", reg, "due to error:", e$message))
  })
}

print("All regional plots successfully exported!")

################################################################################
### 3.3 PROVINCIAL TURNOUT TRENDS ###

# Define the function for Provinces
plot_provincial_trend <- function(province_name) {
  
  print(paste("Generating trend plot for Province of:", province_name))
  
# 1. CALCULATE NATIONAL TURNOUT (THE BASELINE)
  national_turnout <- df_storico_master %>%
    filter(TIPO_ELEZIONE == "Camera") %>%
    distinct(ANNO, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
    group_by(ANNO) %>%
    summarise(
      Naz_Elettori = sum(ELETTORI, na.rm = TRUE),
      Naz_Votanti = sum(VOTANTI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(TURNOUT_NAZ = (Naz_Votanti / Naz_Elettori) * 100)
  
# 2. FILTER AND AGGREGATE PROVINCIAL DATA
  provincial_turnout <- df_storico_master %>%
    filter(TIPO_ELEZIONE == "Camera") %>%
    # STRICT MATCH to prevent overlapping names (e.g., Roma vs Romagna)
    filter(toupper(PROVINCIA) == toupper(province_name)) %>%
    distinct(ANNO, PRO_COM, COMUNE, ELETTORI, VOTANTI) %>%
    group_by(ANNO) %>%
    summarise(
      Prov_Elettori = sum(ELETTORI, na.rm = TRUE),
      Prov_Votanti = sum(VOTANTI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(TURNOUT_PROV = (Prov_Votanti / Prov_Elettori) * 100)
  
  # Safety Check
  if(nrow(provincial_turnout) == 0) {
    stop(paste("No data found for province:", province_name, "- Please check spelling!"))
  }
  
  # Join the national baseline to the provincial data
  plot_data <- provincial_turnout %>%
    left_join(national_turnout, by = "ANNO")
  
# 3. DYNAMIC AXIS SCALING & HELPER DATAFRAME
  y_min <- min(c(plot_data$TURNOUT_PROV, plot_data$TURNOUT_NAZ), na.rm = TRUE) - 5
  if (y_min > 60) y_min <- 60 
  
  laws_df <- data.frame(year = c(1993, 2005, 2017))
  
# 4. BUILD THE TIME SERIES PLOT
  p <- ggplot(plot_data, aes(x = ANNO)) +
    
    # Electoral Law Lines
    geom_vline(data = laws_df, aes(xintercept = year, color = "Electoral Law Change", linetype = "Electoral Law Change"), linewidth = 0.8) +
    annotate("text", x = 1993 - 0.5, y = y_min + 6, label = "Mattarellum (1993)", color = "red", angle = 90, size = 2.8) +
    annotate("text", x = 2005 - 0.5, y = y_min + 6, label = "Porcellum (2005)", color = "red", angle = 90, size = 2.8) +
    annotate("text", x = 2017 - 0.5, y = y_min + 6, label = "Rosatellum (2017)", color = "red", angle = 90, size = 2.8) +
    
    # National Trend
    geom_line(aes(y = TURNOUT_NAZ, color = "National Average", linetype = "National Average"), linewidth = 1) +
    
    # Provincial Trend (Using a rich royal blue/purple theme)
    geom_area(aes(y = TURNOUT_PROV), fill = "slateblue", alpha = 0.2) + 
    geom_line(aes(y = TURNOUT_PROV, color = "Provincial Turnout", linetype = "Provincial Turnout"), linewidth = 1.2) +
    geom_point(aes(y = TURNOUT_PROV, color = "Provincial Turnout"), size = 3) +
    
    # Text labels above the PROVINCIAL points
    geom_text(aes(y = TURNOUT_PROV, label = paste0(round(TURNOUT_PROV, 1), "%")), 
              vjust = -1.2, size = 4, fontface = "bold", color = "darkblue") +
    
    # Styling the axes
    scale_x_continuous(breaks = plot_data$ANNO) +
    scale_y_continuous(limits = c(y_min, 100), labels = scales::label_percent(scale = 1)) +
    
    # Unify and style the legend
    scale_color_manual(
      name = NULL,
      breaks = c("Provincial Turnout", "National Average", "Electoral Law Change"),
      values = c(
        "Provincial Turnout" = "slateblue", 
        "National Average" = "grey40", 
        "Electoral Law Change" = "red"
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      breaks = c("Provincial Turnout", "National Average", "Electoral Law Change"),
      values = c(
        "Provincial Turnout" = "solid", 
        "National Average" = "dashed", 
        "Electoral Law Change" = "dashed"
      )
    ) +
    
    theme_minimal() +
    labs(
      title = paste("Voter Turnout Trend in ", str_to_title(province_name)),
      subtitle = "Camera dei Deputati (1987 - 2022)",
      x = "Election Year",
      y = "Turnout (%)",
      caption = "Source: Elaborazione su dati Ministero dell'Interno"
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key.width = unit(2, "cm")
    )
  
  return(p)
}

## LOOP TO GENERATE AND EXPORT MAJOR PROVINCIAL PLOTS
# The 10 most historically and demographically significant Italian provinces
major_provinces <- c(
  "ROMA", "MILANO", "NAPOLI", "TORINO", "PALERMO", 
  "BARI", "CATANIA", "FIRENZE", "BOLOGNA", "GENOVA"
)

# Loop through each major province
for (prov in major_provinces) {
  
  # Clean the name just in case
  safe_filename <- str_replace_all(prov, "[^A-Za-z0-9]", "_")
  
  tryCatch({
    
    # Generate the plot
    p <- plot_provincial_trend(prov)
    
    # Save it to your working directory
    ggsave(
      filename = paste0("Trend_Turnout_Prov_", safe_filename, ".png"), 
      plot = p, 
      width = 10,       
      height = 6,       
      dpi = 300,        
      bg = "white"      
    )
    
  }, error = function(e) {
    message(paste("Skipping", prov, "due to error:", e$message))
  })
}

print("All major provincial plots successfully exported!")

################################################################################
### 4. COVARIATE MAPS ###

# Define the function
generate_covariate_map <- function(cov_name) {
  
  print(paste("Processing map for:", cov_name))
  
  provincial_agg <- df_storico_master %>%
    filter(TIPO_ELEZIONE == "Camera") %>%
    # Use .data[[]] to dynamically evaluate the string as a column name
    distinct(ANNO, COD_PROV, PRO_COM, COMUNE, .data[[cov_name]], ELETTORI) %>%
    group_by(ANNO, COD_PROV) %>%
    summarise(
      # Calculate the weighted average dynamically
      Avg_Value = weighted.mean(.data[[cov_name]], w = ELETTORI, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(COD_PROV = as.numeric(COD_PROV))
  
  
  # Using your already loaded and cleaned conf_province_*_clean shapefiles
  map_87_96 <- conf_province_91_clean %>%
    left_join(provincial_agg %>% filter(ANNO == 1987), by = "COD_PROV")
  
  map_01 <- conf_province_01_clean %>%
    left_join(provincial_agg %>% filter(ANNO == 2001), by = "COD_PROV")
  
  map_13 <- conf_province_13_clean %>%
    left_join(provincial_agg %>% filter(ANNO == 2013), by = "COD_PROV")
  
  map_18 <- conf_province_18_clean %>%
    left_join(provincial_agg %>% filter(ANNO == 2018), by = "COD_PROV")
  
  map_22 <- conf_province_22_clean %>%
    left_join(provincial_agg %>% filter(ANNO == 2022), by = "COD_PROV")
  
  # Combine all eras into one master spatial object
  master_map <- bind_rows(
    map_87_96, map_01, map_13, map_18, map_22
  ) %>% 
    filter(!is.na(ANNO)) %>%
    # NEW STEP: Translate the Election Year to the Census Year for the facet labels
    mutate(
      ANNO_CENSUS = case_when(
        ANNO == 1987 ~ 1991,
        ANNO == 2001 ~ 2001,
        ANNO == 2013 ~ 2011,
        ANNO == 2018 ~ 2018,
        ANNO == 2022 ~ 2022,
        TRUE ~ as.numeric(ANNO)
      )
    )
  
  
  p <- ggplot(master_map) +
    geom_sf(aes(fill = Avg_Value), color = "grey30", linewidth = 0.1) +
    scale_fill_viridis_c(
      option = "magma", 
      direction = -1, 
      name = cov_name,
      na.value = "grey80"
    ) +
    # Use the newly created ANNO_CENSUS column for the facet titles
    facet_wrap(~ ANNO_CENSUS, ncol = 5) +
    theme_void() +
    labs(
      title = paste("Evolution of", cov_name, "in Italy"),
      # Adjusted the subtitle to match the displayed years
      subtitle = "Provincial population-weighted averages",
      caption = "Source: ISTAT | Mapping: Historical Borders"
    ) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(b = 20)),
      strip.text = element_text(size = 14, face = "bold"), # Slightly increased facet text size
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
  
  # Return the finished plot object
  return(p)
}


# Define the exact names of your covariates
covariate_list <- c("P13_Aging", "P7_Density", "SS4_Education", "L12_Employment")

# Apply the function to each covariate. 
# This creates a list named 'all_maps' containing your 4 plot objects.
all_maps <- setNames(lapply(covariate_list, generate_covariate_map), covariate_list)


# Loop to save each plot as a PNG on your Desktop / working directory
for (cov in names(all_maps)) {
  ggsave(
    filename = paste0("Map", cov, ".png"), 
    plot = all_maps[[cov]], 
    width = 16,    
    height = 6,    
    dpi = 300,      
    bg = "white"
  )
}


################################################################################
### 5. COVARIATE SLOPES OVER TIME (EMPLOYMENT) ###

# 1. Join Turnout with the Employment Covariate
slope_data <- provincial_turnout %>%
  left_join(
    df_storico_master %>%
      filter(TIPO_ELEZIONE == "Camera") %>%
      distinct(ANNO, COD_PROV, PRO_COM, L12_Employment, ELETTORI) %>%
      group_by(ANNO, COD_PROV) %>%
      summarise(Avg_Employment = weighted.mean(L12_Employment, w = ELETTORI, na.rm = TRUE), .groups = 'drop') %>%
      mutate(COD_PROV = as.numeric(COD_PROV)),
    by = c("ANNO", "COD_PROV")
  ) %>%
  drop_na(Avg_Employment)

# 2. Faceted Scatterplot
plot_slope <- ggplot(slope_data, aes(x = Avg_Employment, y = TURNOUT)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", fill = "red", alpha = 0.2) +
  facet_wrap(~ ANNO, ncol = 4) +
  theme_bw() +
  labs(
    title = "Employment Rate vs. Turnout (1987 - 2022)",
    subtitle = "Visualizing the changing strength of structural drivers over time",
    x = "Provincial Employment Rate (%)",
    y = "Provincial Turnout (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(b = 15)),
    # 1. Remove 'bg' from element_text
    strip.text = element_text(size = 12, face = "bold"),
    # 2. Add strip.background to color the box behind the text
    strip.background = element_rect(fill = "grey80", color = "grey80") 
  )

ggsave("EDA_Slope_Employment.png", plot = plot_slope, width = 12, height = 8, dpi = 300, bg = "white")
