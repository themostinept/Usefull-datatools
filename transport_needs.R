##First transport##
years <- c(2019, 2023)
garbage_density_container <- 0.17284
garbage_sealing_coeff <- 4
truck_speed <- 14
truck_unload_time <- 12
container_load_time <- 1
truck_volume <- 12
working_time <- 10
container_volume <- 1.1
mean_filling_coef <- 0.9
##Second transport##
truck_2_speed <- 40
load_unload_time_2 <- 1
working_time_2 <- 12
truck_capacity <- NULL
truck_volume_2 <- 30
garbage_density_truck <- 0.415

transport_needs <- function(cls, direct, years) {
require(openxlsx)
require(tidyverse)
  
years <- sort(years)  
  ##First transport##
cls <- list.files()[grep('clusters', list.files())]
cls <- read.csv(cls, sep = ";",  dec = ",", stringsAsFactors = F)

unchange_object_load_reload_time <- round((truck_unload_time / 60) + 
                                          (round(((truck_volume * garbage_sealing_coeff) /
                                                  (container_volume*mean_filling_coef)) * container_load_time) / 60), digits = 2)
unchange_yearly_truck_capacity <- (truck_volume * garbage_sealing_coeff * garbage_density_container * 365) / 1000

cls_comp <- cls %>% 
  filter(Год %in% years) %>% 
  group_by(Год, Название.района, Наименование.ОИ) %>% 
  summarise(mass = sum(Масса.образованных.отходов..тыс..тонн),
            distance = weighted.mean(Пробег.первого.звена..км, Масса.образованных.отходов..тыс..тонн)) %>% 
  mutate(trip_duartion = (distance / truck_speed) + unchange_object_load_reload_time, 
         trips_per_day = if_else(floor(working_time / trip_duartion) < 1, 1, floor(working_time / trip_duartion)), 
         mass_transported = trips_per_day * unchange_yearly_truck_capacity,
         transport_needs = ceiling(mass / mass_transported))

cls_final <- cls_comp %>% 
  group_by(Название.района, Год) %>% 
  summarise(mass = sum(mass),
           transport = sum(transport_needs))

cls_final_region <- unique(cls_final[ , 1])
cls_final_tm <- split(cls_final[ , -1], as.factor(cls_final$Год))
cls_final_tm <- do.call(cbind, cls_final_tm)

cls_final <- bind_cols(cls_final_region, cls_final_tm)
rem <- grep("Год", colnames(cls_final))
cls_final <- cls_final[ , -rem]

c_names <- colnames(cls_final)
c_names_mass <- grep("mass", c_names)
c_names_transport <- grep("transport", c_names)
c_names[c_names_mass] <- paste0("Масса ", years, "., тыс. т.")
c_names[c_names_transport] <- paste0("Необходимо мусоровозов ", years, "., шт.")
c_names[1] <- "Название района"
colnames(cls_final) <- c_names

##Second transport##
if (is.null(truck_capacity) == T) {
  truck_capacity <- truck_volume_2 * garbage_density_truck
  }
  
direct <- list.files()[grep('directions', list.files())]
direct <- read.csv(direct, sep = ";",  dec = ",", stringsAsFactors = F)
  
direct_comp <- direct %>% 
  filter(Год %in% years & Тип.ОИ %in% c('Сортировка', 'recast', 'Перегрузка')) %>% 
  group_by(Год, Зона.РО.ОИ, Тип.ОИ, Наименование.ОИ, Тип.принимающего.ОИ, Наименование.принимающего.ОИ) %>% 
  summarise(mass = sum(Масса.отходов..отправленных.на.принимающий.ОИ..тыс..тонн),
            distance = sum(Расстояние.до.принимающего.ОИ..км)) %>% 
  filter(is.na(mass) == F) %>% 
  mutate(distance = if_else(distance < 0.01, 0.1, distance),
         trip_duration = (distance * (2 / truck_2_speed)) + load_unload_time_2,
         trips_per_day = if_else((floor(working_time_2 / trip_duration)) < 1, 1, floor(working_time_2 / trip_duration)),
         trips_per_year = ceiling((mass * 1000) / truck_capacity),
         transport_needs = ceiling((trips_per_year / 365) / trips_per_day))
  
direct_final <- select(direct_comp, c(1:7, 12))
  
colnames(direct_final) <- c("Год", "Зона РО", "Тип ОИ", "Наименование ОИ", "Тип принимающего ОИ", "Наименование принимающего ОИ", "Масса отходов, отправленных на принимающий ОИ", "Требуется машин")
  
write.xlsx(list("I" = cls_final, "II" = direct_final), "Потребность в транспорте.xlsx")
}
