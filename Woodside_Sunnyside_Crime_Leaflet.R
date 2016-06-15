


Chase_Map<-
  fin_df %>% 
  leaflet() %>% 
  setView(lng = location_lng, lat=location_lat, zoom = 12) %>% 
  addProviderTiles("Hydda.Base",options = providerTileOptions(noWrap = TRUE)) %>% 
  addProviderTiles("Stamen.TonerLines",
                   options = providerTileOptions(opacity = 0.35)
  ) %>% 
  addProviderTiles("Stamen.TonerLabels",options = providerTileOptions(opacity = 0.7)) %>% 
  addCircleMarkers(lng = ~lng
                   , lat = ~lat
                   , popup = ~paste0(formatted_address,"<br>",name,"<br>","Rating: ",rating)
                   , radius= 10
                   , color = "green"
                   , stroke = T
                   , opacity = 1
                   , fillOpacity = 0.5
                   # , clusterOptions  = markerClusterOptions(showCoverageOnHover = TRUE
                   #                                          , zoomToBoundsOnClick = TRUE
                   #                                          , spiderfyOnMaxZoom = TRUE
                   #                                          , removeOutsideVisibleBounds = TRUE
                   #                                          , disableClusteringAtZoom = 18
                   #                                          , maxClusterRadius = 20 #20 stable
                   #                                          ,iconCreateFunction=JS(readLines("custom.cluster.js"))
  )
Chase_Map