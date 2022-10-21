setwd("E:/tjo_r/Projet/Project")

library(geosphere) # Pour les calculs avec les coordonnées
library(plotly)    # Pour ggplotly
library(rAmCharts) # Pour faire la Boxplot
library(leaflet)   # Pour faire la carte
library(sf)        # Pour faire la carte

df_in = read.csv('Data/input_test.csv', sep = ',')
df_out = read.csv('Data/output_test.csv', sep = ',')

df_all = df_in %>%
  mutate(weekday_index = case_when(weekday %in% c(5,6,7,1) ~ 0,
                                   weekday %in% c(2,3,4) ~ 1))
df_all = df_all %>%
  mutate(hour_index = case_when(hour %in% c(0,1,2,3,4,13,14,15,16,17,18,19,20,21,22,23) ~ 0,
                                hour %in% c(5,6,7,8,9,10,11,12) ~ 1))

df_all_final = df_all

df_all_final = subset(df_all, select = -c(ID,land_cover_class_HVC, land_cover_class_MAS,land_cover_class_CAU,land_cover_class_GOR,land_cover_class_HRI,land_cover_class_HAR))


# on compte avant traitement
sum(is.na.data.frame(df_all_final))
# RESULTAT : Il y en a 30

# Boucle qui balaye tous les points et si NA on prend la valeur de l'heure d'après
for(i in seq.int(nrow(df_all_final))){
  for (j in length(df_all_final):1){
    #print(i)
    if (is.na(df_all_final[i,j])){
      df_all_final[i,j] = (df_all_final[i+1,j])
      
    }
  }
}

# Verification des NA apres traitement
sum(is.na.data.frame(df_all_final))
# RESULTAT : Il y en a 0


df_all_final = df_all_final %>%
  mutate(distance_HRI = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_HRI, y_wgs84_HRI), ncol = 2))) %>%
  mutate(distance_HVH = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_HVH, y_wgs84_HVH), ncol = 2))) %>%
  mutate(distance_STA = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_STA, y_wgs84_STA), ncol = 2))) %>%
  mutate(distance_CAU = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_CAU, y_wgs84_CAU), ncol = 2))) %>%
  mutate(distance_GOR = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_GOR, y_wgs84_GOR), ncol = 2))) %>%
  mutate(distance_HAR = distGeo(matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2),matrix(c(x_wgs84_HAR, y_wgs84_HAR), ncol = 2)))

# Distance par rapport à MAS (de 0 à 360, comme a variable de vent)
df_all_final = df_all_final %>%
  mutate(direction_HRI = bearingRhumb(matrix(c(x_wgs84_HRI, y_wgs84_HRI), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2))) %>%
  mutate(direction_HVH = bearingRhumb(matrix(c(x_wgs84_HVH, y_wgs84_HVH), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2))) %>%
  mutate(direction_STA = bearingRhumb(matrix(c(x_wgs84_STA, y_wgs84_STA), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2))) %>%
  mutate(direction_CAU = bearingRhumb(matrix(c(x_wgs84_CAU, y_wgs84_CAU), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2))) %>%
  mutate(direction_GOR = bearingRhumb(matrix(c(x_wgs84_GOR, y_wgs84_GOR), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2))) %>%
  mutate(direction_HAR = bearingRhumb(matrix(c(x_wgs84_HAR, y_wgs84_HAR), ncol = 2),matrix(c(x_wgs84_MAS, y_wgs84_MAS), ncol = 2)))

df_all_final= df_all_final %>%
  mutate(indicateur_vent_HRI = abs((360-direction_HRI - windDirectionDegrees) / (direction_HRI - windDirectionDegrees))) %>% 
  mutate(indicateur_vent_HVH = abs((360-direction_HVH - windDirectionDegrees) / (direction_HVH - windDirectionDegrees))) %>% 
  mutate(indicateur_vent_STA = abs((360-direction_STA - windDirectionDegrees) / (direction_STA - windDirectionDegrees))) %>% 
  mutate(indicateur_vent_CAU = abs((360-direction_CAU - windDirectionDegrees) / (direction_CAU - windDirectionDegrees))) %>% 
  mutate(indicateur_vent_GOR = abs((360-direction_GOR - windDirectionDegrees) / (direction_GOR - windDirectionDegrees))) %>% 
  mutate(indicateur_vent_HAR = abs((360-direction_HAR - windDirectionDegrees) / (direction_HAR - windDirectionDegrees))) 

df_all_final = subset(df_all_final, select = -c(direction_HRI,direction_HVH,direction_STA,direction_CAU,direction_GOR,direction_HAR,distance_HRI,distance_HVH,distance_STA,distance_CAU,distance_GOR,distance_HAR))


df_all_final = df_all_final %>%
  mutate(windDirectionDegrees_sin = sin(windDirectionDegrees)) %>%
  mutate(windDirectionDegrees_cos = cos(windDirectionDegrees)) %>%
  mutate(indicateur_vent_HRI_sin = sin(indicateur_vent_HRI)) %>%
  mutate(indicateur_vent_HRI_cos = cos(indicateur_vent_HRI)) %>%
  mutate(indicateur_vent_HVH_sin = sin(indicateur_vent_HVH)) %>%
  mutate(indicateur_vent_HVH_cos = cos(indicateur_vent_HVH)) %>%
  mutate(indicateur_vent_STA_sin = sin(indicateur_vent_STA)) %>%
  mutate(indicateur_vent_STA_cos = cos(indicateur_vent_STA)) %>%
  mutate(indicateur_vent_CAU_sin = sin(indicateur_vent_CAU)) %>%
  mutate(indicateur_vent_CAU_cos = cos(indicateur_vent_CAU)) %>%
  mutate(indicateur_vent_GOR_sin = sin(indicateur_vent_GOR)) %>%
  mutate(indicateur_vent_GOR_cos = cos(indicateur_vent_GOR)) %>%
  mutate(indicateur_vent_HAR_sin = sin(indicateur_vent_HAR)) %>%
  mutate(indicateur_vent_HAR_cos = cos(indicateur_vent_HAR))


df_all_final = subset(df_all_final, select = -c(windDirectionDegrees,indicateur_vent_HRI,indicateur_vent_HVH,indicateur_vent_STA,indicateur_vent_CAU,indicateur_vent_GOR,indicateur_vent_HAR))

write.csv(df_all_final,"Data/input_test_processed.csv")
