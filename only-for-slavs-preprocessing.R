library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(data.table)
library(rio)
library(ggplot2)
library(ggmap)
library(tmap)
library(rgeos)
library(geosphere)
library(tidyr)
library(osmdata)
library(OpenStreetMap)
library(maps)
library(RColorBrewer)
library(spatstat)
library(spdep)
library(igraph)
library(janitor)
library(spatialEco)
library(RJSONIO)
library(RCurl)
library(jsonlite)
library(rangeMapper)
library(geojsonio)
library(descr)
library(BaylorEdPsych)
library(mvnmle)
library(mice)
library(Amelia)
library(stringr)
library(spatialreg)
library(xlsx)

# require necessary packages
library(MASS)
library(lattice)
library(utils)
library(sf)
library(reshape)
library(geodist)
library(tidyr)
library(haven)
library(spdep)
library(sp)
library(spatialreg)
library(DescTools)
library(dplyr)

library(splm)
library(spaMM)
library(spatialprobit)
library(geoR)
library(gridExtra)
library(NLMR)
library(DHARMa)
library(ROI.plugin.glpk)
library(lme4)
library(caret)
library(performance)
library(INLA)
library(bigmemory)
library(lmerTest)
library(MuMIn)
library(lavaan)
library(multcomp)
library(plm) # time series 
library(car)

Sys.setlocale(locale = "Russian")
setwd("/data")

#################################################################################################################### 
############################# 1. Avito base preprocessing
####################################################################################################################

###### ------------------------- 1.0 Data loading

# Load the dataframe
df_avito <- bind_rows(rio::import("df_avito_1.xlsx", "xlsx"),
                      rio::import("df_avito_2.xlsx", "xlsx"),
                      rio::import("df_avito_3.xlsx", "xlsx"))

###### ------------------------- 1.1 Intial preprocessing

# Rename columns
df_avito <- df_avito %>% dplyr::rename("avito_id"="uID", "metro"="Метро", "address"="Адрес",
                                       "text_description"="Описание", "user_type"="Тип пользователя",
                                       "owner_gender"="Пол продавца", "pictures"="Ссылки на картинки",
                                       "name"="Название", "parameters"="Параметры", "price"="Цена",
                                       "date"="Дата", "lng"="Широта", "lat"="Долгота")
# Drop redundant columns
df_avito <- df_avito[,str_detect(colnames(df_avito), '[a-z]')]
# Convert variable types
df_avito$lat <- as.numeric(df_avito$lat)
df_avito$lng <- as.numeric(df_avito$lng)
df_avito$price <- as.numeric(df_avito$price)
# Remove NA's in desription
df_avito <- df_avito[!is.na(df_avito$text_description),]
# Remove duplicates based on the ID
df_avito <- df_avito[!duplicated(df_avito$avito_id),]
# Remove duplicates based on the pictures
df_avito <- df_avito[!duplicated(df_avito$pictures) | is.na(df_avito$pictures),]
# Remove duplicates based on the text description
df_avito <- df_avito[!duplicated(df_avito$text_description),]
# Remove flat for sales and ads from the people who are looking for flats
df_avito <- df_avito[grepl("объявления=Сдам|объявления=Снять", df_avito$parameters),]

###### ------------------------- 1.2. Variables extraction and preprocessing

### --- 1. Extract rooms count
df_avito$rooms_count <- df_avito$name %>% strsplit( "," ) %>% sapply( "[", 1 )
df_avito$rooms_count[grepl("1", df_avito$rooms_count)] <- "1_room"
df_avito$rooms_count[grepl("2", df_avito$rooms_count)] <- "2_rooms"
df_avito$rooms_count[grepl("3", df_avito$rooms_count)] <- "3_rooms"
df_avito$rooms_count[grepl("4", df_avito$rooms_count)] <- "4_rooms"
df_avito$rooms_count[grepl("5", df_avito$rooms_count)] <- "5_rooms"
df_avito$rooms_count[grepl("6", df_avito$rooms_count)] <- "6_rooms"
df_avito$rooms_count[grepl("7", df_avito$rooms_count)] <- "7_rooms"
df_avito$rooms_count[grepl("8", df_avito$rooms_count)] <- "8_rooms"
df_avito$rooms_count[grepl("9", df_avito$rooms_count)] <- "9_rooms"
df_avito$rooms_count[!grepl("room", df_avito$rooms_count, fixed = TRUE)] <- "studio"
# Recode 4+ rooms in one category
df_avito$rooms_count[grepl(paste(c("4_rooms", "5_rooms", "6_rooms", "7_rooms", "8_rooms", "9_rooms"),
            collapse="|"), df_avito$rooms_count)] <- "4_rooms_plus"
### --- 2. Extract area in m2
df_avito$area_m2 <- df_avito$name %>% strsplit( "," ) %>% sapply( "[", 2 )
df_avito$area_m2 <- as.numeric(substr(df_avito$area_m2, 1, nchar(df_avito$area_m2) - 2))
### --- 3. Extract floor number, floor number total, fist and last floor indicators
temp_vec <- df_avito$name %>% strsplit( "," ) %>% sapply( "[", 3)
temp_vec <- substr(temp_vec, 1, nchar(temp_vec) - 4)
df_avito$floor_number <- temp_vec %>% strsplit( "/" ) %>% sapply("[", 1) %>% as.numeric()
df_avito$floor_number_total <- temp_vec %>% strsplit( "/" ) %>% sapply("[", 2) %>% as.numeric()
df_avito$floor_first <- as.numeric(df_avito$floor_number == 1)
df_avito$floor_last <- as.numeric(df_avito$floor_number == df_avito$floor_number_total)
rm(temp_vec)
### --- 4. Extract lease term
df_avito$parameters <- df_avito$parameters %>%
  str_replace_all("[^А-Яа-яA-Za-z0-9 ,=]", ';')
df_avito$lease_term <- (df_avito$parameters %>%
                          str_match("Срок аренды=\\s*(.*?)\\s*;"))[,2]
### --- 5. Extract presence of commision
df_avito$commision <- (df_avito$parameters %>%
                          str_match("Комиссия=\\s*(.*?)\\s*;"))[,2]
df_avito$commision <- ifelse(df_avito$commision == "Есть", 1, 0)
### --- 6. Extract commision percent
df_avito$commision_percent <- (df_avito$parameters %>%
                               str_match("Размер комиссии, =\\s*(.*?)\\s*;"))[,2] %>% as.numeric()
### --- 7. Extract deposit value
df_avito$deposit_months <- (df_avito$parameters %>%
                            str_match("Залог=\\s*(.*?)\\s*;"))[,2]
df_avito$deposit_months <- df_avito$deposit_months %>% 
  dplyr::recode("Без залога"=0, "0,5 месяца"=0.5, "1 месяц"=1,
         "1,5 месяца"=1.5, "2 месяца"=2, "2,5 месяца"=2.5, "3 месяца"=3)
### --- 8. Extract building type
df_avito$building_type <- (df_avito$parameters %>%
                              str_match("Тип дома=\\s*(.*?)\\s*$"))[,2]
df_avito$building_type <- df_avito$building_type %>% strsplit( ";" ) %>% sapply("[", 1)
### --- 9. Datetime
# Convert date to date format
df_avito$date <- as.POSIXct(df_avito$date,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
# Create date in YYYY-MM format
df_avito$date_y_m <- paste(strftime(df_avito$date, "%Y"), strftime(df_avito$date, "%m"), sep="_")
# Create date in YYYY format
df_avito$date_y <- strftime(df_avito$date, "%Y")
# Recode date_y
df_avito$date_y <- as.factor(df_avito$date_y)
### --- 10. Price
# Defliate prices by years based on the 2018
df_avito[df_avito$date_y == "2016",]$price <- df_avito[df_avito$date_y == "2016",]$price*1.07
df_avito[df_avito$date_y == "2017",]$price <- df_avito[df_avito$date_y == "2017",]$price*1.03
df_avito[df_avito$date_y == "2018",]$price <- df_avito[df_avito$date_y == "2018",]$price*1.
df_avito[df_avito$date_y == "2019",]$price <- df_avito[df_avito$date_y == "2019",]$price*0.97
# Create Logged and Scaled version of price variable
df_avito$price_norm <- scale(log(df_avito$price))
df_avito$price_norm <- as.numeric(df_avito$price_norm)

###### ------------------------- 1.3 Filtering

# Remove daily rent
df_avito <- df_avito %>% filter((lease_term == "На длительный срок") %>% replace_na(TRUE))
# Remove outliers by price and flats with 0 price
df_avito <- df_avito %>% filter(price <= quantile(df_avito$price, 0.99))
df_avito <- df_avito %>% filter(price > 0)
# Remove flats with missing coordinates
df_avito <- df_avito[!is.na(df_avito$lat),]
# Remove flats with impossible coordinates
df_avito <- df_avito %>% filter(lat >= -90 & lat <= 90)
df_avito <- df_avito %>% filter(lng >= -180 & lng <= 180)

###### ------------------------- 1.4  Extract types of discriminations

### --- 1. No Children
children_patterns <- c("без детей", "без детей и животных", "без животных и детей",
                       "без маленьких детей", "без маленьких детей и животных", "без животных и маленьких детей",
                       "без питомцев и детей",
                       "без вредных привычек, животных и детей")
df_avito$D_nochildren <- grepl(paste(children_patterns, collapse="|"), tolower(df_avito$text_description))
sum(df_avito$D_nochildren)
### --- 2. No Animals
animals_patterns <- c("без животных", "без домашних животных", "без кошек", "без собак",
                      "без детей и животных", "без животных и детей", "домашним питомцем нельзя",
                      "без домашних питомцев", "без маленьких детей и животных", "без животных и маленьких детей",
                      "без питомцев", "без детей и питомцев", "с животными нельзя",
                      "без вредных привычек, животных и детей", "нельзя с животными",
                      "бeз животных")
df_avito$D_noanimals <- grepl(paste(animals_patterns, collapse="|"), tolower(df_avito$text_description))
sum(df_avito$D_noanimals)
### --- 3. Russian Citizens
citizens_patterns <- c("росси", "\\bрф\\b", "российской федерации")
df_avito$D_citizens <- grepl(paste(citizens_patterns, collapse="|"), tolower(df_avito$text_description))
sum(df_avito$D_citizens)
### --- 4. Bad habbits
badhabbits_patterns <- c("некурящи", "не курящи", "непьющи", "не пьющи", "без вредных привычек",
                         "не курить", "курить в квартире запрещено",
                         "без вредных привычек, животных и детей")
df_avito$D_nobadhabbits <- grepl(paste(badhabbits_patterns, collapse="|"), tolower(df_avito$text_description))
sum(df_avito$D_nobadhabbits)
### --- 5. Slavs
slav_patterns <- c("славян\\b", "славяне\\b", "славянам\\b", "славянской\\b", "славянскую\\b",
                   "славянку\\b",
                   "русск", "кавказ", "средняя азия", "средней азии", "кроме азии", "cлaвянaм\\b",
                   "cлавянам\\b", "cлавянaм\\b", "слaвян\\b")
df_avito$D_slavs <- grepl(paste(slav_patterns, collapse="|"), tolower(df_avito$text_description))
sum(df_avito$D_slavs)
### Add total count of different discriminations characteristics in ad
df_avito$D_totalcount <- rowSums(df_avito %>% dplyr::select(starts_with("D_")))
table(df_avito$D_totalcount)
### Create new category - Only for slavs without any other indicators of discrimination
df_avito$D_slavs_alone <- df_avito$D_slavs & df_avito$D_totalcount == 1
### Create new category based on the No children, No animals, No bad habbits - D_propertydamage
df_avito$D_propertydamage <- df_avito$D_nochildren | df_avito$D_noanimals | df_avito$D_nobadhabbits
### Only property damage
df_avito$D_propertydamage_alone <- df_avito$D_totalcount == as.numeric(rowSums(df_avito %>% data.frame() %>%
                                                                                 dplyr::select(D_nochildren, D_noanimals, D_nobadhabbits)))
### Create new category - presence of any type of discrimination
df_avito$D_present <- df_avito$D_totalcount > 0

###### ------------------------- 1.5 Districts information

### --- 1. Loading
# Load district polygons from https://gis-lab.info/qa/moscow-atd.html
df_districts <- geojson_sf("moscow.geojson")
colnames(df_districts)[1] <- "district_name"
### --- 2. Add data from Rosstat
# Add number of migrants from Rosstat
# Load Rosstat data
df_rosstat <- rio::import("df_districts.xlsx", "xlsx")
# Calculate cumulative number of migrants from 2013 to 2018
df_rosstat$migrants_2013_2018 <- rowSums(df_rosstat %>%
                                           dplyr::select(migrants_2013, migrants_2014, migrants_2015, migrants_2016, migrants_2017, migrants_2018), na.rm=T)
# Match names
names_borders <- data.frame(df_districts$district_name)
# Load matched names
district_names <- rio::import("district_names.xlsx", "xlsx")
# Add json district names
df_rosstat <- df_rosstat %>%
  data.frame() %>%
  full_join(district_names, by = c("district" = "rosstat_district_names"))
df_rosstat <- df_rosstat %>%
  dplyr::select(-c(district, district_fullname, lng, lat, miscp_district_names)) %>%
  dplyr::rename(district_name=json_district_names)
df_districts <- df_districts %>% left_join(df_rosstat)
### Add data from MISCP
# Load MISCP data
df_miscp <- rio::import("miscp_data.xlsx", "xlsx")
df_names <- rio::import("district_names.xlsx", "xlsx")
df_names <- df_names %>% dplyr::select(json_district_names, miscp_district_names)
df_miscp <- df_miscp %>% left_join(df_names, by = c("district_name"="miscp_district_names"))
# Add MISCP data to df_districts
df_districts <- df_districts %>%
  left_join(df_miscp, by=c("district_name"="json_district_names"))
# District names with aggregated ones in New Moscow
df_districts$district_name_agg <- df_districts$district_name
df_districts[is.na(df_districts$many_migrants) &
               df_districts$ABBREV_AO == "Новомосковский",]$district_name_agg <- "Малые населенные пункты НАО"
df_districts[is.na(df_districts$many_migrants) &
               df_districts$ABBREV_AO == "Троицкий",]$district_name_agg <- "Малые населенные пункты ТАО"
# Add missing values to variable from MISCP
df_districts[is.na(df_districts$many_migrants) &
               df_districts$ABBREV_AO == "Новомосковский",]$many_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты НАО",]$many_migrants
df_districts[is.na(df_districts$aggressive_migrants) &
               df_districts$ABBREV_AO == "Новомосковский",]$aggressive_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты НАО",]$aggressive_migrants
df_districts[is.na(df_districts$illegal_migrants) &
               df_districts$ABBREV_AO == "Новомосковский",]$illegal_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты НАО",]$illegal_migrants
df_districts[is.na(df_districts$many_migrants) &
               df_districts$ABBREV_AO == "Троицкий",]$many_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты ТАО",]$many_migrants
df_districts[is.na(df_districts$aggressive_migrants) &
               df_districts$ABBREV_AO == "Троицкий",]$aggressive_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты ТАО",]$aggressive_migrants
df_districts[is.na(df_districts$illegal_migrants) &
               df_districts$ABBREV_AO == "Троицкий",]$illegal_migrants <- 
  df_miscp[df_miscp$district_name == "Малые населенные пункты ТАО",]$illegal_migrants
# Remove used objects
rm(df_miscp, df_names, df_rosstat, names_borders, district_names)
# Remove redundant columns
df_districts <- df_districts %>%
  dplyr::select(district_name, district_name_agg, NAME_AO, TYPE_MO,
                many_migrants, aggressive_migrants, illegal_migrants,
                migrants_2016, migrants_2017, migrants_2018, migrants_2019,
                mean_salary_2016, population_2016)
# Add missing values to Lefortovo
df_districts[df_districts$district_name == "Лефортово",]$population_2016 <- 93311
df_districts[df_districts$district_name == "Лефортово",]$mean_salary_2016 <- 75347
df_districts[df_districts$district_name == "Лефортово",]$migrants_2019 <- 1
### --- 3 CFA
df_districts$aggressive_migrants.scale <- scale(df_districts$aggressive_migrants)
df_districts$illegal_migrants.scale <- scale(df_districts$illegal_migrants)
df_districts$many_migrants.scale <- scale(df_districts$many_migrants)
m1a  <- 'f  =~ aggressive_migrants.scale + illegal_migrants.scale + many_migrants.scale'
onefac3items_a <- cfa(m1a, data=df_districts) 
summary(onefac3items_a, fit.measures=TRUE, standardized=TRUE)
df_districts$factor_migrants <- as.numeric(lavPredict(onefac3items_a))
df_districts$aggressive_migrants.scale <- NULL
df_districts$illegal_migrants.scale <- NULL
df_districts$many_migrants.scale <- NULL

### --- 4. Convert df_avito to sf and add coresponding district name
# Convert avito dataframe to sf object
df_avito <- df_avito %>% st_as_sf(coords=c("lng", "lat"))
# Add CRS to avito
st_crs(df_avito) <- st_crs(df_districts)
# Add name of district to flat
df_avito <- st_join(df_avito, df_districts["district_name"])
# Remove flats outside zone of districts
df_avito <- df_avito[!is.na(df_avito$district_name),]
# Append district information to Avito
df_avito <- df_avito %>%
  left_join(data.frame(df_districts)  %>% dplyr::select(-geometry),
            by = c("district_name"="district_name"))

###### ------------------------- 1.6 Buildings information

### --- 1. Preprocesing of building data
# Load buildings data
df_moscow_buildings <- rio::import("moscow_buildings.xlsx")
# Select only usefull columns
df_moscow_buildings <- df_moscow_buildings %>%
  dplyr::select(X.U.FEFF.id, address, built_year, exploitation_start_year, floor_count_max, quarters_count,
                area_total, area_residential, foundation_type, floor_type, wall_material, lng, lat)
# Remove buildings with impossible coordinates
df_moscow_buildings <- df_moscow_buildings %>% filter(lat >= -90 & lat <= 90)
df_moscow_buildings <- df_moscow_buildings %>% filter(lng >= -180 & lng <= 180)
# Convert to sf 
df_moscow_buildings <- df_moscow_buildings %>% st_as_sf(coords=c("lng", "lat"))
# Add CRS to building
st_crs(df_moscow_buildings) <- st_crs(df_districts)
# Add name of district to flat
df_moscow_buildings <- st_join(df_moscow_buildings, df_districts["district_name"])
# Remove flats outside zone of districts
df_moscow_buildings <- df_moscow_buildings[!is.na(df_moscow_buildings$district_name),]
# Rename columns in df_moscow_buildings
df_moscow_buildings <- df_moscow_buildings %>%
  dplyr::rename(district_name_buildings=district_name, address_buildings=address)

### --- 2. Match flats and buildings by coordinates
# Match by each district seperatly
df_avito <- df_avito[order(df_avito$district_name),]
vec_buildings_id <- c()
for (d_name in unique(df_avito$district_name)){
  df_avito_temp <- df_avito %>% filter(district_name == d_name)
  df_moscow_buildings_temp <- df_moscow_buildings %>% filter(district_name_buildings == d_name)
  
  flats_buildings_dist <- st_distance(df_avito_temp, df_moscow_buildings_temp,
                                      tolerance=200)
  vec_buildings_id <- c(vec_buildings_id,
                        df_moscow_buildings_temp[apply(flats_buildings_dist, 1, order)[1,],]$X.U.FEFF.id)
  print(d_name)
}
# Add building id to df avito
df_avito$building_id <- vec_buildings_id
# Add buildings variables to flats
df_moscow_buildings <- df_moscow_buildings %>% data.frame()
df_moscow_buildings$geometry <- NULL
df_avito <- df_avito %>%
  left_join(df_moscow_buildings, by = c("building_id"="X.U.FEFF.id"))

### --- 3. Preprocessing of buildings variables
# Remove outliers from exploitation_start_year
df_avito$exploitation_start_year[df_avito$exploitation_start_year < 1900] <- 1900
df_avito$exploitation_start_year[df_avito$exploitation_start_year > 2020] <- NA
# Create categories from exploitation_start_year
df_avito$exploitation_start_year_cat <- cut(df_avito$exploitation_start_year,
    breaks=c(1899, 1960, 1970, 1980, 1990, 2000, 2010, 2021) ,dig.lab=10,
    labels=c("built_1900_1960", "built_1961_1970", "built_1971_1980", "built_1981_1990",
             "built_1991_2000", "built_2001_2010", "built_2011_2020"))
# Change reference category for districts to the district with largest number of flats
df_avito$district_name <- 
  as.factor(as.character(df_avito$district_name))
df_avito$district_name <- relevel(as.factor(df_avito$district_name),
                                  names(which.max(table(df_avito$district_name))))
# Building type
df_avito$building_type <- dplyr::recode(as.character(df_avito$building_type),
                                        "Панельный"="panel", "Блочный"="block", "Деревянный"="wood",
                                        "Кирпичный"="brick", "Монолитный"="monolith")
# Change reference category for building_type
df_avito$building_type <- relevel(as.factor(df_avito$building_type),
                                  names(which.max(table(df_avito$building_type))))
# Remove observations with NA's in variables of interest
df_avito <- df_avito[(!is.na(df_avito$exploitation_start_year_cat) & !is.na(df_avito$rooms_count)),]

# Save FULL BASE to rds file
saveRDS(df_avito, "df_avito.rds")


