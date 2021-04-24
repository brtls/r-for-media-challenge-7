# # B. Kießling
# Challenge 7: plotly und leaflet
# Zur Überprüfung dienen erneut die beiden Outputs (plot und map)

# plotly Verhältnis Arbeitlosigkeit und Migration -----------------------------

# 1. Lade den Datensatz stadtteil_profil_updated.rds in das Objekt stadtteile
# 2. Erstelle ein Scatter-Plot mit arbeitslosenanteil_in_percent_dez_2019 auf der y-Achse
# und anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent auf der x-Achse
# 3. Löse das Problem beim Column arbeitslosenanteil_in_percent_dez_2019, sodass sich dieser richtig visualisieren lässt
# Tipp: Die Funktion as.numeric() wird dir dabei weiterhelfen
# 4. Runde die beiden zu visualierenden Columns auf eine Nachkommastelle
# 5. Beschrifte die x-Achse und y-Achse und erstelle einen Titel für den Plot
# 6. Nutze die Argumente text und hoverinfo und stelle die Hoverinfo wie im Beispieldiagramm dar
# Tipp: Für die Gestaltung der Hoverinformationen findest du alle Informationen hier: https://plotly.com/r/text-and-annotations/ im Abschnitt Custom Hover Text
# 7. Speichere den Plot mit htmlwidgets::saveWidget(as_widget(plotobject), "myfirstplot.html")
# 8. Interpretiere die Daten

# leaflet: Arbeitslosigkeit pro Stadtteil ----------------------------------

# 1. Lade den Datensatz stadtteile_wsg84.RDS in das Objekt stadtteile_gps
# 2. Benenne die Columns im Objekt stadtteile_gps von Stadtteil und Bezirk und stadtteil und bezirk um
# 3. Joine beide Datensätze in das Objekt stadtteile und nutze die Funktion %>% st_as_sf() am Ende der Pipe, um das Objekt als sf zu konvertieren
# 4. Erstelle ein leaflet Objekt und wähle ein Design über addProviderTiles(), 
# 5. Setze setView() auf (9.993682, 53.551086 = und wähle eine Zoom-Stufe
# 6. Ergänze die Code-Fragmente
# 7. Speichere die Map mit htmlwidgets::saveWidget(as_widget(mapobject), "myfirstmap.html")

# Wir werden die Map in der nächsten Einheit weiter gestalten!

# load packages
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(dplyr)

# load data for plot
stadtteile <- readRDS("data/stadtteile_profil_updated.rds")

# mutating "arbeitslosenanteil_in_percent_dez_2019" from character into numeric
stadtteile <- stadtteile %>% 
  mutate(arbeitslosenanteil_in_percent_dez_2019 = as.numeric(arbeitslosenanteil_in_percent_dez_2019))

# plotly, creating scatter plot and rounding the values
scatterplot <- plot_ly(data = stadtteile, 
        x = ~anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent,
        y = ~arbeitslosenanteil_in_percent_dez_2019,
        type = "scatter", 
        mode = "markers",
        text = ~paste("</br> Migrations Anteil: ", round(anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent, digits = 1),
                      "</br> Arbeitslosen Anteil: ", round(arbeitslosenanteil_in_percent_dez_2019, digits = 1),
                      "</br> Stadtteil: ", stadtteil), 
        hoverinfo = "text") %>% 
  layout(title = "Verhältnis von Arbeitslosen und Menschen mit Migrationshintergrund in Hamburg",
         xaxis = list(title = "Anteil Migrationshintergrund in %"),
         yaxis = list(title = "Erwerbslosenanteil in %"))

#liniar model, checking coorelation between bevolkerung mit migrations hintergrund and arbeitslosenanteil
model <- lm(arbeitslosenanteil_in_percent_dez_2019~anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent, data = stadtteile)
summary(model)
# p-value: 2,2e-16 < 0.05 Null-Hyposthese gilt. Es besteht eine Korrelation zwischen dem Anteilmigrationshintergrund und dem Erwerbslosenanteil. 
# R²: 0.8097, damit ist recht gut, somit ist der Erklärungszusammenhang recht gut gesichert. 
# Steigt der Anteil der Bevolkerung mit Migrationshintergrund um eine Einheit, ändert sich die Erwerbslosigkeit um 0.136494.

# Saving plot in html format
htmlwidgets::saveWidget(as_widget(scatterplot), "myfirstplot.html")

# load data for map
stadtteile_gps <- readRDS("data/stadtteile_wsg84.RDS")

stadtteile_gps <- stadtteile_gps %>% 
  rename(stadtteil = Stadtteil,
         bezirk = Bezirk)


# join data
stadtteile <- stadtteile %>% 
  left_join(stadtteile_gps) %>% 
  st_as_sf()

# leaflet 
bins <- c(0, 2, 4, 6, 8, 10, Inf)
pal <- colorBin("YlOrRd", domain = stadtteile$arbeitslosenanteil_in_percent_dez_2019, bins = bins)

hamburg_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  setView(lng = 9.993682, lat = 53.551086, zoom = 10) %>% 
  addPolygons(data = stadtteile,
              fillColor = ~pal(arbeitslosenanteil_in_percent_dez_2019),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.75)

# Export to html format
htmlwidgets::saveWidget(as_widget(hamburg_map), "myfirstmap.html")


