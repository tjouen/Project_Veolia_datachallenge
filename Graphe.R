library(data.table)
library(tidyverse)
library('stringr')

library(geosphere) # Pour les calculs avec les coordonnées
library(plotly)    # Pour ggplotly
library(rAmCharts) # Pour faire la Boxplot
library(leaflet)   # Pour faire la carte
library(sf)        # Pour faire la carte

df_in = read.csv('Data/input_training.csv', sep = ',')
df_out = read.csv('Data/output_training.csv', sep = ',')

df_all = df_in %>% mutate(SO2_MAS = df_out$SO2_MAS)

df_all$weekday = as.character(df_all$weekday)
df_all$weekday = str_replace_all(df_all$weekday, "7", 'Dimanche')
df_all$weekday = str_replace_all(df_all$weekday, "6", 'Samedi')
df_all$weekday = str_replace_all(df_all$weekday, "5", 'Vendredi')
df_all$weekday = str_replace_all(df_all$weekday, "4", 'Jeudi')
df_all$weekday = str_replace_all(df_all$weekday, "3", 'Mercredi')
df_all$weekday = str_replace_all(df_all$weekday, "2", 'Mardi')
df_all$weekday = str_replace_all(df_all$weekday, "1", 'Lundi')

df_all$weekday

plot(x = df_all$ID, y = df_all$SO2_MAS)

df_day_mean = df_all %>% 
  group_by(weekday) %>% 
  summarize(mean(SO2_MAS),'stdp' = mean(SO2_MAS)+sd(SO2_MAS),'stdm' = mean(SO2_MAS)-sd(SO2_MAS))

plot(df_day_mean$weekday, df_day_mean$`mean(SO2_MAS)`)
plot(df_day_mean$weekday, df_day_mean$`mean(SO2_MAS) + sd(SO2_MAS)`)

df_day_mean %>%
ggplot() +
  geom_line(mapping = aes(x = weekday, y = `mean(SO2_MAS)`), size = 0.5, colour = "#112446") +
  geom_line(mapping = aes(x = weekday, y = stdm), size = 0.5, colour = "blue") +
  geom_line(mapping = aes(x = weekday, y = stdp), size = 0.5, colour = "blue") +
  # geom_line(aes(x = weekday,y = "stdp"), color = "blue") + 
  # geom_line(aes(x = weekday,y = "stdm"), color = "blue") + 
  labs(
    x = "Jour de la semaine",
    y = "CSO2 moyenne (Âµg / mÂ³)",
    title = "Evolution par jour moyenne de CSO2"
  ) +
  theme_minimal()

#

df_day_hour_mean = df_all %>% 
  group_by(weekday, hour) %>% 
  summarize("mean"=mean(SO2_MAS,na.rm=TRUE),
            'sd' = sd(SO2_MAS,na.rm=TRUE),
            "first_quart" = quantile(SO2_MAS, prob=c(.05,.5,.95),na.rm=TRUE)[1],
            "third_quart" = quantile(SO2_MAS, prob=c(.05,.5,.95),na.rm=TRUE)[3])

df_day_hour_mean %>%
  ggplot() +
  geom_line(mapping = aes(x = hour, y = mean), size = 0.5, colour = "#112446") +
  geom_line(mapping = aes(x = hour, y = first_quart), size = 0.5, colour = "blue") +
  geom_line(mapping = aes(x = hour, y = third_quart), size = 0.5, colour = "blue") +
  labs(
    x = "Jour de la semaine",
    y = "CSO2 moyenne (µg / m³)",
    title = "Evolution par jour de la moyenne de CSO2"
  ) +
  theme_minimal() +
  facet_wrap(~weekday)

plot(df_all$surfaceTemperatureCelsius)

esquisse::esquisser(viewer = "browser")


#df_all = df_all %>%
#  mutate(weekday_index = case_when(weekday %in% c("Vendredi","Samedi","Dimanche","Lundi") ~ 0,
#                                   weekday %in% c("Mardi","Mercredi","Jeudi") ~ 1))

df_all = df_in %>%
  mutate(weekday_index = case_when(weekday %in% c(5,6,7,1) ~ 0,
                                   weekday %in% c(2,3,4) ~ 1))
df_all = df_all %>%
  mutate(hour_index = case_when(hour %in% c(0,1,2,3,4,13,14,15,16,17,18,19,20,21,22,23) ~ 0,
                                hour %in% c(5,6,7,8,9,10,11,12) ~ 1))


unique(df_all$land_cover_class_HVC)
unique(df_all$land_cover_class_HVC)


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

write.csv(df_all_final,"Data/input_training_processed.csv")

