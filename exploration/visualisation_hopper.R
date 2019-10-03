#Faire une seule MAP par ?quipe de vente pour permettre le bouton search qui ne peut fonctionner que sur un seul group ? la fois
#Cependant, on peut inclure les RTA ET les points des ILM 24+ dans la m?me map pour chaque ?quipe de vente
#install.packages('devtools')
library('devtools')
#install.packages('httpuv') #Car il manquait ce package pour installer leaflet

#install.packages("Rcpp")

#install.packages("leaflet")
#download.file("https://github.com/rstudio/leaflet/archive/master.zip",destfile = "leaflet-master.zip")
#unzip("leaflet-master.zip")
#devtools::install_local("leaflet-master")
#library('leaflet')

#download.file("https://github.com/bhaskarvk/leaflet.extras/archive/master.zip",destfile = "leaflet.extras-master.zip")
#unzip("leaflet.extras-master.zip")
#devtools::install_local("leaflet.extras-master")
#library('leaflet.extras')

#devtools::install_github('rstudio/leaflet')
#devtools::install_github('bhaskarvk/leaflet.extras')
#install.packages('OpenStreetMap')
#library('OpenStreetMap')
#install.packages('rgdal')
#library('rgdal')
#install.packages('raster')
library('raster')
#install.packages('dplyr')
#library('dplyr')
#install.packages('plyr')
#library('plyr')
library('leaflet')
library('leaflet.extras')
#install.packages('gplots')
library('gplots')
library('sp')
#library('alphahull')
#library('maptools')
library('htmltools')
#install.packages('htmlwidgets')
library('htmlwidgets')
library('mapview')
library('geosphere')

#install.packages("Rcpp")
#download.file("https://github.com/GIScience/openrouteservice-r/archive/master.zip", destfile = "master.zip")
#unzip("master.zip")
#devtools::install_local("openrouteservice-r-master")
#library('openrouteservice')

#ors_api_key("5b3ce3597851110001cf6248c6319394ebcd45dbb79f0273b2bc5b35")

#install.packages('plotly')
#library('plotly')

#install.packages("mapview")
#library('mapview')

options(viewer = NULL) #Pour aller directement dans un browser

library(data.table)
options("scipen"=100, "digits"=12)
options(datatable.integer64="character")

#Les logos de Google Cloud et de Hooper
#logo_googlecloud = paste("https://inthecloud.withgoogle.com/hopper-hackathon-19/static/img/svg-cloud-logo.svg",sep="")
#logo_hopper = paste("https://inthecloud.withgoogle.com/hopper-hackathon-19/static/img/partner-hopper.jpg",sep="")
#html_logo <- "<img src='https://inthecloud.withgoogle.com/hopper-hackathon-19/static/img/svg-cloud-logo.svg'
#style='width:300px;height:80px;'> rrr <br/> 
#<img src='https://inthecloud.withgoogle.com/hopper-hackathon-19/static/img/partner-hopper.jpg'  
#style='width:300px;height:80px;'>"

logo_hopper_googlecloud = paste("//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/image_GGcloud_hopper.jpg",sep="")

# x = les données qui définiront la grosseur des icons
# y = le nombre de quantiles à considérer (ou le nombre de grosseurs différentes qu'on veut au total)
echelle_rayon <- function(x,y) {
  
  cutx = as.integer(cut(x, 
              quantile(x, prob = seq(0, 1, length = y), type = 5) , 
              include.lowest=TRUE))
  return(cutx)
}


########
# TEST #
########

hopper_map = leaflet(data = quakes) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag), 
             icon =~makeIcon(
               iconUrl = "https://womenhack.com/wp-content/uploads/job-manager-uploads/company_avatar/2017/10/hopper_logo_small.png",
               iconWidth = 15+3*echelle_rayon(mag,11), 
               iconHeight = 15+3*echelle_rayon(mag,11)
             )
  )  %>%
              addMarkers(~long*1.0005, ~lat, popup = ~as.character(depth), label = ~as.character(depth), 
                         icon =~makeIcon(
                                         iconUrl = "https://image.flaticon.com/icons/svg/424/424076.svg",
                                         iconWidth = 15+3*echelle_rayon(depth,11), 
                                         iconHeight = 15+3*echelle_rayon(depth,11)
                                         )
                        ) %>%
  addLogo(logo_hopper_googlecloud ,src = "local",position = 'topleft', width = 300, height = 120)

#########################
# Aéroports de Wikidata #
#########################


library(jsonlite)
aeroport_wiki <- fromJSON("//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/WIKIDATA_AIRPORT_passagers.json",flatten=TRUE)

aeroport_wiki$longitude=as.numeric(aeroport_wiki$longitude)
aeroport_wiki$latitude=as.numeric(aeroport_wiki$latitude)
aeroport_wiki$MAX_patronage=as.numeric(aeroport_wiki$MAX_patronage)


#Pour mettre les drapeaux des pays
for (i in 1:nrow(aeroport_wiki)){
  
  aeroport_wiki$legendlabel[i] = paste("<img src=",aeroport_wiki$countryFlag[i]," height='20' width='30'>","  ",aeroport_wiki$paysLabel[i],sep="")
  
}
attach(aeroport_wiki)

aeroport_wiki <- aeroport_wiki[order(paysLabel),]

aeroport_wiki$echelle_rayon = echelle_rayon(aeroport_wiki$MAX_patronage,11)

attach(aeroport_wiki)


#On ajoute des lignes de vols d'un point à un autre
test=gcIntermediate(c(5,52), c(-120,37),breakAtDateLine=F,
                    addStartEnd=TRUE,
                    sp=T,n=100) 

test2= gcIntermediate(cbind(aeroport_wiki[110,]$longitude,aeroport_wiki[110,]$latitude),cbind(aeroport_wiki[1:100,]$longitude,aeroport_wiki[1:100,]$latitude),breakAtDateLine=F,
                      addStartEnd=TRUE,
                      sp=T,n=100)

map_aeroport_cluster = 

  leaflet(data = aeroport_wiki) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(MAX_patronage), label = ~as.character(itemLabel), group = ~legendlabel, 
             icon =~makeIcon(
               iconUrl = "https://image.flaticon.com/icons/svg/424/424076.svg",
               iconWidth = 15+3*echelle_rayon(MAX_patronage,11), 
               iconHeight = 15+3*echelle_rayon(MAX_patronage,11)),
             options = markerOptions(mag = ~MAX_patronage, mag2 = ~15+3*echelle_rayon(MAX_patronage,11)),
             clusterOptions =
               markerClusterOptions(iconCreateFunction =
                                      JS("
                                         function(cluster) {
                                                            var markers = cluster.getAllChildMarkers();
                                                            var sum = 0; 
                                                            var sum2 = 0;
                                                            for (i = 0; i < markers.length; i++) {
                                                            sum += Number(markers[i].options.mag);
                                                            //sum += 1;
                                                            }
                                                            for (i = 0; i < markers.length; i++) {
                                                            sum2 += Number(markers[i].options.mag2);
                                                            if (sum2 > 100){ break; }
                                                            }
                                         return new L.DivIcon({
                                         html: \"<img src='https://image.flaticon.com/icons/svg/424/424076.svg' />\" + sum,
                                         className: 'image-icon',
                                         iconSize: [sum2, sum2]
                                         });
                                         }"))
  ) %>%
  addPolylines(data=test2,weight = 1) %>%
  addLayersControl(
    overlayGroups = ~legendlabel,
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLogo(logo_hopper_googlecloud ,src = "local",position = 'topleft', width = 300, height = 120) %>%
  #addMiniMap() %>%
  addSearchFeatures(
    targetGroups = ~legendlabel,
    options = searchFeaturesOptions(
      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = FALSE )) %>%
  hideGroup(~legendlabel) 

library(htmlwidgets)
saveWidget(map_aeroport_cluster, selfcontained = TRUE, file="//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/map_aeroport_cluster.html")



map_aeroport = 
  
  leaflet(data = aeroport_wiki) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(MAX_patronage), label = ~as.character(itemLabel), group = ~legendlabel, 
             icon =~makeIcon(
               iconUrl = "https://image.flaticon.com/icons/svg/424/424076.svg",
               iconWidth = 15+3*echelle_rayon(MAX_patronage,11), 
               iconHeight = 15+3*echelle_rayon(MAX_patronage,11)),
             
  ) %>%
  addPolylines(data=test2,weight = 1) %>%
  addLayersControl(
    overlayGroups = ~legendlabel,
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLogo(logo_hopper_googlecloud ,src = "local",position = 'topleft', width = 300, height = 120) %>%
  #addMiniMap() %>%
  addSearchFeatures(
    targetGroups = ~legendlabel,
    options = searchFeaturesOptions(
      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = FALSE )) %>%
  hideGroup(~legendlabel) 

library(htmlwidgets)
saveWidget(map_aeroport, selfcontained = TRUE, file="//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/map_aeroport.html")




leaflet(quakes[1:20,]) %>% addTiles() %>%
  addMarkers(
    options = markerOptions(mag = ~mag),
    clusterOptions =
               markerClusterOptions(iconCreateFunction =
                                      JS("
                                          function(cluster) {
var markers = cluster.getAllChildMarkers();
var sum = 0; 
for (i = 0; i < markers.length; i++) {
  sum += Number(markers[i].options.mag);
//sum += 1;
}
                                             return new L.DivIcon({
                                              html: '<div><span>' + sum + '</span></div>',
                                              className: 'marker-cluster',
                                               iconSize: new L.Point(40,40)});
                                           }")))

#var markers = cluster.getAllChildMarkers();
#var sum = 0; 
#for (i = 0; i < markers.length; i++) {
  #sum += Number(markers[i].options.mag);
  #//      sum += 1;
#}






#install.packages("threejs")
library(threejs)
(earth <- system.file("images/world.jpg",  package="threejs"))


test_df <- data.frame(origin_lat = aeroport_wiki[110,]$latitude, origin_long = aeroport_wiki[110,]$longitude, dest_lat = aeroport_wiki[1:100,]$latitude, dest_long = aeroport_wiki[1:100,]$longitude)

test3=globejs(img=earth,
              lat=aeroport_wiki[110,]$latitude, long=aeroport_wiki[110,]$longitude, 
              arcs=test_df,
              arcsHeight=0.3, arcsLwd=2, arcsColor="red", arcsOpacity=0.15,
              atmosphere=TRUE, bg="black",height=800, width = 800)
library(htmlwidgets)
saveWidget(test3, selfcontained = TRUE, file="//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/test3.html")




#install.packages('digest')

#download.file("https://github.com/timelyportfolio/parsetR/archive/master.zip",destfile = "parsetR-master.zip")
#unzip("parsetR-master.zip")
#devtools::install_local("parsetR-master")
library(parsetR) # devtools::install_github("timelyportfolio/parsetR")


aeroport_wiki$echelle_rayon = echelle_rayon(MAX_patronage,11)
attach(aeroport_wiki)

parset(aeroport_wiki[aeroport_wiki$paysLabel %in% c(unique(aeroport_wiki$paysLabel)[1]),], dimensions = c('itemLabel', 'echelle_rayon','paysLabel'), 
       value = htmlwidgets::JS("function(d){return d.echelle_rayon}"), 
       tension = 0.5,height=400, width = 2000)


#install.packages("networkD3")
library(networkD3)
library(dplyr)
#Sera intéressant d'utiliser pour connecter les cultures d'hotels les plus proches et les plus loins entre pays.
#

name_vec <- c(unique(aeroport_wiki[aeroport_wiki$paysLabel %in% c(unique(aeroport_wiki$paysLabel)[5]),]$paysLabel), unique(aeroport_wiki[aeroport_wiki$paysLabel %in% c(unique(aeroport_wiki$paysLabel)[5]),]$itemLabel))

nodes <- data.frame(name = name_vec, id = 0:(length(name_vec)-1))

links <- aeroport_wiki[aeroport_wiki$paysLabel %in% c(unique(aeroport_wiki$paysLabel)[5]),] %>%
  left_join(nodes, by = c('paysLabel' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('itemLabel' = 'name')) %>%
  rename(dest_id = id)

forceNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
             Value = 'echelle_rayon', NodeID = 'name', Group = 'id', zoom = TRUE)

sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'echelle_rayon', NodeID = 'name', fontSize = 18)

