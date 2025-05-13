#committed to github
library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(elevatr)
library(tidverse)
xrf<- xrf%>%
  mutate (lead_level= case_when(
    Pb < 1000 ~ "Low (Less than 1000 mg/kg)",
    Pb < 2000 ~ "Moderate (Less than 2000 mg/kg)",
    Pb> 2000 ~"High (greater than 2000 mg/kg)"
  ))
pal<- colorFactor(
  palette=c("green", "orange", "red"),
  levels= c("Low (Less than 1000 mg/kg)", "Moderate (Less than 2000 mg/kg)", "High (greater than 2000 mg/kg)")
)

#Composite Samples====
map<-leaflet(xrf %>% filter (Type == "composite"))%>%
  addTiles()%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    #stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  )
map



map1<-leaflet(xrf %>% filter (Type == "composite"))%>%
  addProviderTiles("OpenTopoMap")%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    #stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  ) 
map1


library(htmlwidgets)
saveWidget(map1, file = "composite_lead_mp.html")

map2<-leaflet(xrf %>% filter (Type == "composite"))%>%
  addProviderTiles("Esri.WorldImagery")%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    #stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  )
map2


#In Situ Samples====



map3<-leaflet(xrf %>% filter (Type == "insitu"))%>%
  addTiles()%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  )
map3



map4<-leaflet(xrf %>% filter (Type == "insitu"))%>%
  addProviderTiles("OpenTopoMap", group= "Topo Map")%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  )
map4



map5<-leaflet(xrf %>% filter (Type == "insitu"))%>%
  addProviderTiles("Esri.WorldImagery")%>%
  addCircleMarkers(
    lng=  ~Longitude,
    lat= ~Latitude,
    radius = 6,
    stroke= FALSE,
    fillColor = ~pal(lead_level),
    fillOpacity = 0.9,
    popup= ~paste("Lead", Pb, "ppm")
  ) %>%
  addLegend(
    "bottomright",
    pal=pal,
    values= ~lead_level,
    title= "Lead Level",
    opacity= 1
  )
map5

#Contaminants of Concern: Pb, Cu, Cd and Zn. Also Ni potentially 
#Look up the code to connect ArcGis Pro to R studio directly.







