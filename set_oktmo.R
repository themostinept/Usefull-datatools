true_oktmo <- function(x, y, full_dataset = TRUE, write_to_excel = FALSE){
require(tidyverse)
require(maptools)
require(openxlsx)
  crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  region <- suppressWarnings(readShapePoly(y, proj4string=crswgs84, verbose = TRUE, delete_null_obj = T))
  region@data$NAME <- unlist(lapply(region@data$NAME, function(x) iconv(x, 'UTF-8')))
  region_dataset <- read.xlsx(x, sheet= 1, colNames = T, skipEmptyRows = F, skipEmptyCols = F) %>% 
    separate(point, into = c("lon", "lat"), sep = "[[:space:]]", remove = F, convert = T) %>%  
    mutate(lon = as.numeric(gsub(",", ".", lon)), lat = as.numeric(gsub(",", ".", lat)))
  if (any(is.na(region_dataset$lon) == T) || any(is.na(region_dataset$lat) == T)) {
    return(region_dataset[unique(c(which(is.na(region_dataset$lon) == T), which(is.na(region_dataset$lat) == T))), ])
  }
  m <- matrix(c(region_dataset$lon, region_dataset$lat), ncol = 2)
  points_polygon <- SpatialPoints(m, proj4string = crswgs84)
  if (full_dataset == F) {
    compared <- cbind(region_dataset$point,(over(points_polygon, region)))
  } else {
    compared <- cbind(region_dataset,(over(points_polygon, region)))
  }
  if (write_to_excel == T) {
    write.xlsx(compared, "Comparison.xlsx", row.names = F)
  }
  return(compared)
}