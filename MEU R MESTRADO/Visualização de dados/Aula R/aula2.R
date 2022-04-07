library(rgdal)
library(leaflet)
library(RColorBrewer)
library(classInt)
arquivo = "C:/Users/LucasZanolla/Desktop/Mestrado/Visualização de dados/Aula R"
shape_br <- readOGR(arquivo, "estados", GDAL1_integer64_policy = TRUE)
plot(shape_br)
shape_rs <- readOGR(arquivo, "municipiors", GDAL1_integer64_policy = TRUE)
plot(shape_rs)
shape_pm <- shape_rs[shape_rs$MUNICIPIO %in% c("Carazinho","Vila Maria", "Passo Fundo", "Casca", "Marau"),]
plot(shape_pm)
shape_viario_rs <- readOGR(arquivo, "sistemaviariors", GDAL1_integer64_policy = TRUE)

shape_brs <- shape_viario_rs[shape_viario_rs$NOME %in% c("BR 116", "BR 386", "BR 101"),]
plot(shape_brs)


leaflet() %>% addTiles() %>% setView(lng = -52.4, lat = -28,2, zoom = 4)

mapa = leaflet() %>% addTiles()

#exemplo mapa 1

mapa %>%
  addPolygons(data = shape_rs, weight = 1, color = "gray",
              highlightOptions = highlightOptions(color =
                                                    "blue", weight = 3, bringToFront = TRUE)) %>%
  addPolygons(data = shape_brs, weight = 2, color = "red") %>%
  addPolygons(data = shape_pm, weight = 2, color = "orange",
              highlightOptions = highlightOptions(color = "green", weight = 3, bringToFront = TRUE))

leaflet() %>%
  addTiles(group = "openStreetMap") %>%
  addProviderTiles("Stamen.Toner", group = "Toner By Stamen") %>%
  addProviderTiles("Esri.WordlTopoMap", group = "Topografia") %>%
  
  addPolygons(data = shape_rs, weight = 1,color = "gray") %>%
  addPolygons(data = shape_pm, weight = 2, color = "red", group = "Planalto Medio") %>%
  addPolygons(data = shape_brs, weight = 2, color = "orange", group = "Rodovias") %>%
  #Layers Control
  addLayersControl(
    baseGroups = c("openStreetMap", "Toner By Stamen", "Topografia"),
    overlayGroups = c("Planalto Medio", "Rodovias"),
    options = layersControlOptions(collapsed = FALSE)
  )

