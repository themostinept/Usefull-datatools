##„H„p„s„‚„…„w„p„u„} "„…„|„…„‰„Š„u„~„~„…„" „r„u„‚„ƒ„y„ „†„…„~„{„ˆ„y„y 'html_table' „y„x „„p„{„u„„„p 'rvest', „{„€„„„€„‚„p„‘ „„€„x„r„€„|„‘„u„„ „r„„q„y„‚„p„„„ „„„y„ „{„€„|„€„~„€„{ „r „ƒ„€„‡„‚„p„~„‘„u„}„€„z „„„p„q„|„y„ˆ„u
load('html_table_advanced.RData')

gosjkh <- function(houses) {
  require(rvest)
  require(openxlsx)
  require(tidyverse)
  
#„X„y„„„p„u„} „s„|„p„r„~„…„ „ƒ„„„‚„p„~„y„ˆ„…, „ƒ„€„q„y„‚„p„u„} „ƒ„„y„ƒ„€„{ „r„ƒ„u„‡ „ƒ„ƒ„„|„€„{, 
#„r„u„t„…„‹„y„‡ „~„p „ƒ„„„‚„p„~„y„ˆ„ „€„„„t„u„|„„~„„‡ „s„€„‚„€„t„€„r „‚„u„s„y„€„~„p, „s„t„u „ƒ„€„t„u„‚„w„y„„„ƒ„‘ „y„~„†„€„‚„}„p„ˆ„y„‘ „€ „~„p„‡„€„t„‘„‹„y„ƒ„‘ „r „~„y„‡ „M„K„D 
houses_site <- read_html(houses)
info_nod <- html_nodes(houses_site, '.shadow-effect-2')
info_text <- html_text(info_nod, trim = T)
##„B„„r„€„t „y„~„†„€„‚„}„p„ˆ„y„y „€„q „€„q„‹„u„z „„|„€„‹„p„t„y „y „‰„y„ƒ„|„u „t„€„}„€„r „‚„u„s„y„€„~„p, „€„„„‚„p„w„u„~„~„„‡ „r „q„p„x„u 
print(info_text)
houses_nodes <- html_nodes(houses_site, '.list-unstyled a')
houses_attr <- combine(html_attrs(houses_nodes))
all_links <- paste('http://gosjkh.ru', houses_attr, sep = "")

#„P„€„ƒ„|„u„t„€„r„p„„„u„|„„~„€ „„‚„€„‡„€„t„y„} „„€ „{„p„w„t„€„z „ƒ„ƒ„„|„{„u „y„x „„€„|„…„‰„y„r„Š„u„s„€„ƒ„‘ „ƒ„„y„ƒ„{„p
function_result <- lapply(all_links, function(x) {
  page <- read_html(x)
  info_text_2 <- page %>% html_nodes('.shadow-effect-2') %>% html_text(trim = T)
##„B„„r„€„t „y„~„†„€„‚„}„p„ˆ„y„y „€„q „€„q„‹„u„z „„|„€„‹„p„t„y „y „‰„y„ƒ„|„u „t„€„}„€„r „{„€„~„{„‚„u„„„~„€„s„€ „s„€„‚„€„t„p, „€„„„‚„p„w„u„~„~„„‡ „r „q„p„x„u
  print(info_text_2)
  last_page_nod <- page %>% html_nodes("ul")
##„B „x„p„r„ƒ„}„€„ƒ„„„y „€„„ „„„€„s„€, „ƒ„{„€„|„„{„€ „ƒ„„„‚„p„~„y„ˆ „x„p„~„y„}„p„u„„ „ƒ„„y„ƒ„€„{ „t„€„}„€„r „r „s„€„‚„€„t„u („€„t„~„… „y„|„y „q„€„|„„Š„u), „ƒ„‚„p„x„… „ƒ„{„p„‰„y„r„p„u„} „„„p„q„|„y„ˆ„…,
##„y„|„y „ƒ„€„x„t„p„u„} „u„‹„u „€„t„y„~ „ƒ„„y„ƒ„€„{, „s„t„u „q„…„t„…„„ „ƒ„€„t„u„‚„w„p„„„„ƒ„‘ „ƒ„ƒ„„|„{„y „~„p „r„ƒ„u „„€„t„ƒ„„„‚„p„~„y„ˆ„
  if (length(xml_children(last_page_nod[[length(last_page_nod)]])) < 1) {
    city_node <- html_nodes(page, 'body > div.wrapper > div.container.content')
    city_node <- xml_find_first(city_node, './/table')
##„H„t„u„ƒ„ „x„p„t„u„z„ƒ„„„r„…„u„„„ƒ„‘ „†„…„~„{„ˆ„y„‘ 'html_table' „y„x „†„p„z„|„p 'html_table_advanced'
    city_data <- html_table(city_node, header = T, col_classes = list(`1`='integer',
                                                                    `2`='character',
                                                                    `3`='character',
                                                                    `4`='character',
                                                                    `5`='character',
                                                                    `6`='character',
                                                                    `7`='character'))[[1]]
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
##„O„q„Œ„u„t„y„~„‘„u„} „ƒ„€„q„‚„p„~„~„„u „y„x „~„u„ƒ„{„€„|„„{„y„‡ „„€„t„ƒ„„„‚„p„~„y„ˆ „ƒ„„y„ƒ„{„y „r „€„t„y„~ „t„p„„„p„†„‚„u„z„}    
    city_data <- do.call(bind_rows, city_data)
    return(city_data)  
  }
})

#„O„q„Œ„u„t„y„~„‘„u„} „‚„u„x„…„|„„„„p„„ „r„„„€„|„~„u„~„y„‘ „†„…„~„{„ˆ„y„y „r „€„t„y„~ „t„p„„„p„†„‚„u„z„}
#„y „ƒ„€„‡„‚„p„~„‘„u„} „u„s„€ „{„p„{ „s„|„€„q„p„|„„~„…„ „„u„‚„u„}„u„~„~„…„ „t„|„‘ „„€„ƒ„|„u„t„…„„‹„u„s„€ „p„~„p„|„y„x„p + „x„p„„y„ƒ„„r„p„u„} „r xlsx
out <<- as.data.frame(do.call(rbind,function_result))
write.xlsx(out,'„C„€„ƒ „G„K„V.xlsx')
}