
library(dplyr)
library(ggmap)
library(shiny)
library(leaflet)
#load ggmap maps
load("osm_map.rda")
#load election results
load("voting_results.rda")
#load polygons of precints
load("precinct_polygons.rda")

race <- "Ballot Measure 2 - 13PSUM"

yeses <- voting_results %>%
  filter(Race == race) %>%
  filter(Value == "YES") %>%
  select(n)

noes <- voting_results %>%
  filter(Race == race) %>%
  filter(Value == "NO") %>%
  select(n, DISTRICT)

registered <- voting_results %>%
  filter(Race == race) %>%
  filter(Value == "Registered Voters") %>%
  select(n, DISTRICT)  

summarized <- cbind(registered, var = yeses$n  / (noes$n + yeses$n))
summarized$DISTRICT <- factor(summarized$DISTRICT)
summarized <- arrange(summarized, var)
summarized$DISTRICT <- factor(summarized$DISTRICT,
                              levels = as.character(summarized$DISTRICT)) 




summarized <- cbind(summarized, passed = summarized$var > .5)

mapping_obj <- inner_join(summarized, anc.df, by="DISTRICT")
District_Centers <- mapping_obj %>% group_by(DISTRICT) %>% summarize(lat = mean(lat), lon = mean(long))



property_list <- rep(list(""), 118)
big_list <- list()
for(j in seq(levels(mapping_obj$DISTRICT))) {
  temp <- mapping_obj[mapping_obj$DISTRICT == levels(mapping_obj$DISTRICT)[j],]
  xyjson <- data.frame(temp$lat ,temp$long)
  json_filler <- list()
  
  for(i in seq(dim(xyjson)[1])) {
    json_filler[[i]] <- c(as.numeric(xyjson[i,2]),as.numeric(xyjson[i,1]))
  }
  
  
  property_list[[j]]$name       <- as.character(filter(mapping_obj, DISTRICT == as.character(levels(mapping_obj$DISTRICT)[j]))$DISTRICT[1])
  property_list[[j]]$registered <- filter(mapping_obj, DISTRICT == as.character(levels(mapping_obj$DISTRICT)[j]))$n[1]
  
  big_list[[j]] <- json_filler
  
}



district_polygons <- list(type = "FeatureCollection", 
                          features = x <- vector(mode = "list", length = length(levels(mapping_obj$DISTRICT)))
                       )

for(i in seq(levels(mapping_obj$DISTRICT))) {
  
    district_polygons$features[[i]] <- list(type = "Feature",
                                            geometry = list(type = "MultiPolygon",
                                                            coordinates = big_list[i]),
                                       properties = list(
                                         name = property_list[[i]]$name,
                                         population = property_list[[i]]$registered,
                                         style = list(
                                         fillColor = "yellow",
                                         weight = 2,
                                         color = "#000000"
                                         ),
                                         id = levels(mapping_obj$DISTRICT)[i]
                                       )
  )
}


save(big_list,  property_list, district_polygons, file = "test2/big_list.rda")
fileConn<-file("test2/district_polygons.json")
writeLines(toJSON(district_polygons, digits = 9, pretty = T), fileConn)
close(fileConn)


