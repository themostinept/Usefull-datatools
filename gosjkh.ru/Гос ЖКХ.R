load('html_table_advanced.RData')
gosjkh(houses = 'http://gosjkh.ru/houses/kemerovskaya-oblast')
gosjkh <- function(houses){
  require(rvest)
  require(openxlsx)
  require(tidyverse)
houses_site <- read_html(houses)
info_nod <- html_nodes(houses_site, '.shadow-effect-2')
info_text <- html_text(info_nod, trim = T)
print(info_text)
houses_nodes <- html_nodes(houses_site, '.list-unstyled a')
houses_attr <- combine(html_attrs(houses_nodes))
all_links <- paste('http://gosjkh.ru', houses_attr, sep = "")
function_result <- lapply(all_links, function(x){
  page <- read_html(x)
  info_text_2 <- page %>% html_nodes('.shadow-effect-2') %>% html_text(trim = T)
  print(info_text_2)
  last_page_nod <- page %>% html_nodes("ul")
  if(length(xml_children(last_page_nod[[length(last_page_nod)]])) < 1){
    city_node <- html_nodes(page, 'body > div.wrapper > div.container.content')
    city_node <- xml_find_first(city_node, './/table')                                            
    city_data <- html_table(city_node, header = T, col_classes=list(`1`='integer',`2`='character',`3`='character',`4`='character',`5`='character',`6`='character',`7`='character'))[[1]]
    return(city_data)
  } else {
    last_page <- xml_attrs(xml_child(xml_child(last_page_nod[[length(last_page_nod)]], length(xml_children(last_page_nod[[length(last_page_nod)]]))), 1)) %>% 
      str_split('=') %>% unlist()
    city <- paste(x, "?page=", sep = "")
    pages_num <- seq(1,as.numeric(last_page[2]))
    city_pages <- paste(city, pages_num, sep = "")
    city_data <- lapply(city_pages, . %>%
                          read_html %>%
                          html_nodes('body > div.wrapper > div.container.content') %>% 
                          xml_find_first('.//table') %>% 
                          html_table(header = T, col_classes=list(`1`='integer',`2`='character',`3`='character',`4`='character',`5`='character',`6`='character',`7`='character')))
    city_data <- do.call(bind_rows, city_data)
    return(city_data)  
  }
})
out <- as.data.frame(do.call(rbind,function_result))
write.xlsx(out,'Ãîñ ÆÊÕ.xlsx')
}