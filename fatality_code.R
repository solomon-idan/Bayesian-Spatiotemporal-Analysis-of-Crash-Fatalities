# set working directorate
setwd("/Volumes/BigMama/document/mphil")

#import dataset
kma_data <- rio:import(here:here("date","kma_data.xlsx") #import prepared data
kma_data <- as.data.frame(kma_data)

#load shape of kumasi metropolitan area
library(sf)
kma_shapefile <-st_read("SHAPEFILES/kumasi shapefile.shp") 

kma_shapefile <- kma_shapefile %>% 
  select(OBJECTID,REGION,DISTRICT,Shape_Leng,Shape_Area,geometry) %>%
  mutate(district=DISTRICT)

#create coordinates
coord <- st_coordinates(st_centroid(kma_shapefile))
kma_shapefile$long <- coord[,1]
kma_shapefile$lat <- coord[,2]
kma_shapefile$ID <- 1:dim(kma_shapefile)[1]

#selection of variables for shapefile
kma_shapefile <- kma_shapefile %>% 
  select(ID,district,geometry,
         Shape_Leng,Shape_Area,long,lat)

#create neighbour
library(spdep)
nb <- poly2nb(kma_shapefile)

#create weight matrix for INLA package
library(INLA)
nb2INLA("map.adj", nb)
g<- inla.read.graph(filename = "map.adj")

#spatiotemporal analysis with INLA [data=kma_data]
#monthly_fatal_injury
formula.fatal <-  monthly_fata_injury ~ avg_temp + avg_rainfall + 
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + f(idtime, model = "ar1")

res.fatal <- inla(formula.fatal,
                  family = "poisson", data = kma_data, E = E2,
                  control.predictor = list(compute = TRUE),
                  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE,
                                         residuals=TRUE))
summary(res.fatal)
kma_full_sf$r2 <- res.fatal$summary.fitted.values[, "mean"]
kma_full_sf$r2L <- res.fatal$summary.fitted.values[, "0.025quant"]
kma_full_sf$r2U <- res.fatal$summary.fitted.values[, "0.975quant"]


# Basic Model Diagnostics
# CPO Score
mean_cpo <- mean(res.fatal$cpo$cpo)
# DIC and WAIC
dic_value <- res.fatal$dic$dic
waic_value <- res.fatal$waic$waic

#Print Summary Statistics
cat("DIC:", dic_value, "\n")
cat("WAIC:", waic_value, "\n")
cat("Mean CPO:", mean_cpo, "\n")

#Residual Analysis
# Calculate Pearson residuals
pearson_residuals <- (kma_data$monthly_fata_injury - res.fatal$summary.fitted.values$mean) /
  sqrt(res.fatal$summary.fitted.values$mean)

##Plot Fitted vs Observed
plot(kma_data$date, kma_data$monthly_fata_injury,
     type = "l", col = "blue",
     xlab = "Time", ylab = "Fatal Injuries",
     main = "Observed vs Predicted Fatal Injuries Over Time")
lines(kma_data$date, 
      res.fatal$summary.fitted.values$mean, 
      col = "red")
# Add legend
legend("topright", 
       legend = c("Observed", "Predicted"),
       col = c("blue", "red"),
       lty = 1)

##Graphs
#selection of variables for shapefile
kma_shapefile <- kma_shapefile %>% 
  select(ID,district,geometry,
         Shape_Leng,Shape_Area,long,lat)

#join dataframe
kma_full <- kma_data %>%
  left_join(kma_shapefile, by = c("district"))

kma_full_sf <- st_as_sf(kma_full)


#plot-graph fatal injuries and climate variables
library(ggplot2)

ggplot(kma_full_sf, aes(x = date))+
  geom_bar(aes(y = monthly_fata_injury , fill = "Road Traffic Fatal Injuries"),
           stat = "identity", alpha = 0.9)+
  geom_line(aes(y = avg_temp, color = "Average Temperature"), size = 0.9) +
  geom_line(aes(y = avg_rainfall/0.55, color = "Average Rainfall"), size = 0.9) +
  scale_color_manual(values = c("Average Temperature" = "yellow",
                                "Average Rainfall" = "#2ca25f")) +
  scale_fill_manual(values = c("Road Traffic Fatal Injuries" = "#b5b8ff")) +
  labs(title = "TREND OF ROAD TRAFFIC FATAL INJURIES AND CLIMATE VARIABLES",
       x = "Date",
       y = "Injury Count / Temperature (Celsius)",
       color = " ",
       fill = " ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold",
                                  margin = margin(b = 20),
                                  family = "Montserrat SemiBold"), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6),legend.position = "bottom")+ 
  scale_x_date(date_breaks = "8 month", date_labels = "%b\n%Y")+
  scale_y_continuous(sec.axis = sec_axis(~.*0.5, name="Rainfall (mm)")) 

#Plot standardized incidence of fatal injuries
ggplot(kma_full_sf) +
  aes(fill = SIR2 ) +
  geom_sf(size = 1.2) +
  scale_fill_gradient(low ="white", high = "#b5b8ff") +
  theme_minimal() +
  facet_wrap(vars(year))+
  labs(title = "      SPATIAL TREND OF YEARLY STANDARDIZED INCIDENCE FOR MONTHLY      ",
       color = " ",
       fill = " ", subtitle ="ROAD TRAFFIC FATAL INJURIES",
       caption = "District:
       1.Asokore-Mampog     2.Asokwa     3.Kumasi     4.Kwadaso     5.Oforikrom     6.Old Tafo     7.Suame") + 
  theme(plot.title = element_text(size = 12L,
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 0.8,
                                  family = "Montserrat SemiBold"),
        axis.text.x = element_text(angle = 90),
        plot.subtitle = element_text(size = 11L,
                                     face = "bold",
                                     hjust = 0.5,
                                     vjust = 0.8,
                                     family = "Montserrat SemiBold"),
        plot.caption =element_text(size = 10L,
                                   hjust = 0.05,
                                   vjust = 0.8,
                                   family = "Montserrat Medium"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_sf_text(aes(label = as.factor(idarea)),size =2.5,check_overlap = TRUE)

#plot yearly trend of relative risk-fatal injury
ggplot(relative_risks2) +
  geom_sf(aes(fill = r2), size=2.5) +
  geom_sf_text(aes(label = paste0(idarea,"." ,"\n R.R","=",r2,"\n[", r2L, ", ", r2U, "]")),
               size =2, color = "black", check_overlap = TRUE, 
               alpha = 0.9, family="Montserrat ExtraBold") +
  theme_minimal()+
  labs(title = "TREND OF SPATIO-TEMPORAL RELATIVE RISK OF MONTHLY ROAD TRAFFIC FATAL INJURIES",
       color = " ",
       fill = " ",
       caption = "\n
       District:
       1.Asokore-Mampog     2.Asokwa     3.Kumasi     4.Kwadaso     5.Oforikrom     6.Old Tafo     7.Suame") + 
  theme(plot.title = element_text(size = 12L,
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 0.8,
                                  family = "Montserrat SemiBold"),
        plot.caption =element_text(size = 10L,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   family = "Montserrat Medium"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_gradient(low ="white", high = "#b5b8ff") +
  facet_wrap(vars(year))

#plot relative risk-fatal injury
ggplot(relative_risks) +
  geom_sf(aes(fill = r2), size=2.5) +
  geom_sf_text(aes(label = paste0(idarea,"." ,"\n R.R","=",r2,"\n[", r2L, ", ", r2U, "]")),
               size =2.8, color = "black", check_overlap = TRUE, 
               alpha = 0.9, family="Montserrat ExtraBold") +
  theme_minimal()+
  labs(title = "SPATIO-TEMPORAL RELATIVE RISK OF MONTHLY ROAD TRAFFIC FATAL INJURIES",
       color = " ",
       fill = " ",
       caption = "\n
       District:
       1.Asokore-Mampog     2.Asokwa     3.Kumasi     4.Kwadaso     5.Oforikrom     6.Old Tafo     7.Suame") + 
  theme(plot.title = element_text(size = 12L,
                                  face = "bold",
                                  hjust = 0.5,
                                  vjust = 0.8,
                                  family = "Montserrat SemiBold"),
        plot.caption =element_text(size = 10L,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   family = "Montserrat Medium"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_gradient(low ="white", high = "#b5b8ff") +
  facet_wrap(vars(season))
