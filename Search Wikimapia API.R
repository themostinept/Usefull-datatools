search_wikimapia <- function(x, api_key, output, page_n, search_point, place_name, req_dist, compute_area = T, write_to_excel = F) {
  require(tidyverse)
  require(jsonlite)
  require(geosphere)
  require(openxlsx)
  request <- paste(unlist(strsplit(x, split = " ")), collapse = "%20")
  coord <- unlist(strsplit(search_point, split = ", "))
  first_part <- "http://api.wikimapia.org/?function=search"
  second_part <- paste0("&key=", api_key)
  third_part <- "&q="
  fourth_part <- "&page="
  fifth_part <- "&count=100&language=ru"
  sixth_part <- "&format="
  lat <- "&lat="
  lon <- "&lon="
  url <- gsub(" ", "", paste(first_part, second_part, third_part, request, lat, coord[1], lon, coord[2], fourth_part, page_n, fifth_part, sixth_part, output, collapse = ""))
  site <- suppressWarnings(fromJSON(paste(readLines(url, encoding = "UTF-8"), collapse="")))
  df <- flatten(site$folder)
  df <- subset(df, df$distance <= req_dist & df$location.place %in% place_name)
  if (compute_area == T) {
    df$area <- lapply(df$polygon, areaPolygon)
  }
  if (write_to_excel == T) {
    write.xlsx(df[, -which(colnames(df)=="polygon")], "Request_result.xlsx")
  }
  cat("There are", site$found, "objects found. Meet criteria from them:", nrow(df))
  return(df[, -which(colnames(df)=="polygon")])
}