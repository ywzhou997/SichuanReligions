#============================
# Creator: Yuwei Zhou
# Description: This file deals with geospatial data cleaning and preliminart processinng
# Date created: April 12, 2023
# Date mofdified: April 12, 2023
# ============================


# loading library
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(tmap)
library(devtools)
library(showtext)
library(stringr)
library(rgdal)
library(shapefiles)
showtext_auto()

#Set up
setwd("/Users/zhouyuwei/Desktop/Elena")
prj_dd <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#read in data  and clean up
Qing_pref <- st_read("./dataverse_files/v6_1820_pref_pgn_gbk/v6_1820_pref_pgn_gbk.shp")
Sichuan_pref <- st_read("./1820_pref_pgn_Sichuan/1820_pref_pgn_Sichuan.shp")
Qing_Huiguan <- read_excel("./Qing_Huiguan_TempleName.xlsx")
Qing_Huiguan <- Qing_Huiguan[,-1]
nrow(Sichuan_pref) #n = 26

Qing_Huiguan <- st_as_sf(Qing_Huiguan, coords = c("Longitude", "Latitude"), crs = 4326)
Sichuan_pref <- st_transform(Sichuan_pref, crs = 4326)

# Join QIng_Huiguan and Sichuan_pref so that each Huiguan has its associated prefecture
Huiguan_Pref <- st_join(Qing_Huiguan, Sichuan_pref, join = st_within)
Huiguan_Pref <- table(Huiguan_Pref$NAME_PY, Huiguan_Pref$Temple_Name_simp) %>% as.data.frame.matrix()

# Predominant Huiguan in each Prefecture
# Huiguan_Pref_count <- data.frame(Predo_HG = colnames(Huiguan_Pref)[apply(Huiguan_Pref,1,which.max)], 
#                             Predo_num = apply(Huiguan_Pref,1,max), 
#                             Sum = apply(Huiguan_Pref,1,sum), 
#                             Prefecture = NA)
# Huiguan_Pref_count$Prefecture <- rownames(Huiguan_Pref_count)
Huiguan_Pref_count <- NULL
for(i in 1:nrow(Huiguan_Pref)){
  temp <- Huiguan_Pref[i,]
  list <- as.matrix(temp)
  names(list) <- colnames(temp)
  Huiguan_Pref_count <- rbind(Huiguan_Pref_count, c(Predo_HG_1 = names(sort(list, decreasing = TRUE)[1]), 
                              Predo_HG_2 = names(sort(list, decreasing = TRUE)[2]),
                              Predo_HG_3 = names(sort(list, decreasing = TRUE)[3]),
                              Predo_num_1 = as.numeric(sort(list, decreasing = TRUE)[1]),
                              Predo_num_2 = as.numeric(sort(list, decreasing = TRUE)[2]),
                              Predo_num_3 = as.numeric(sort(list, decreasing = TRUE)[3]),
                              Sum = sum(list), Prefecture = rownames(temp)))
}


Huiguan_Pref_count <- merge(Huiguan_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))


# Add pinyin to all Huiguan
unique(c(Huiguan_Pref_count[,2], Huiguan_Pref_count[,3], Huiguan_Pref_count[,4]))

Huiguan_Pref_count <- Huiguan_Pref_count %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "禹王宮", "Yuwanggong 禹王宮"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "萬壽宮", "Wanshougong 萬壽宮"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "三元宮", "Sanyuangong 三元宮"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "關帝廟", "Guandimiao 關帝廟"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "南華宮", "Nanhuagong 南華宮"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "川主廟", "Chuanzhumiao 川主廟"))) %>% 
  mutate_at(vars(matches("Predo_HG")), list(~str_replace(., "天后宮", "Tianhougong 天后宮")))

# Export the result
st_write(Huiguan_Pref_count, "/Qing_Huiguan_count_Prefecture/Qing_Huiguan_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")



#==========Producing shp files for individual temples=============
# Yuwanggong in each Prefecture 禹王宮
Yuwang_Pref_count <- data.frame(Yuwang_num = Huiguan_Pref %>% select("禹王宮"), 
                                Sum = apply(Huiguan_Pref,1,sum), 
                                Prefecture = NA)
Yuwang_Pref_count$Prefecture <- rownames(Yuwang_Pref_count)
Yuwang_Pref_count <- merge(Yuwang_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Yuwang_Pref_count, "Qing_Yuwanggong_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Wanshougong in each Prefecture 萬壽宮
Wanshou_Pref_count <- data.frame(Wanshou_num = Huiguan_Pref %>% select("萬壽宮"), 
                                Sum = apply(Huiguan_Pref,1,sum), 
                                Prefecture = NA)
Wanshou_Pref_count$Prefecture <- rownames(Wanshou_Pref_count)
Wanshou_Pref_count <- merge(Wanshou_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Wanshou_Pref_count, "Qing_Wanshougong_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Chuanzhumiao in each prefecture 川主廟
Chuanzhu_Pref_count <- data.frame(Chuanzhu_num = Huiguan_Pref %>% select("川主廟"), 
                                 Sum = apply(Huiguan_Pref,1,sum), 
                                 Prefecture = NA)
Chuanzhu_Pref_count$Prefecture <- rownames(Chuanzhu_Pref_count)
Chuanzhu_Pref_count <- merge(Chuanzhu_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Chuanzhu_Pref_count, "Qing_Chuanzhumiao_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Nanhuagong in each Prefecture 南華宮
Nanhua_Pref_count <- data.frame(Nanhua_num = Huiguan_Pref %>% select("南華宮"), 
                                 Sum = apply(Huiguan_Pref,1,sum), 
                                 Prefecture = NA)
Nanhua_Pref_count$Prefecture <- rownames(Nanhua_Pref_count)
Nanhua_Pref_count <- merge(Nanhua_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Nanhua_Pref_count, "Qing_Nanhuagong_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Tianhougong in each Prefecture 天后宮
Tianhou_Pref_count <- data.frame(Tianhou_num = Huiguan_Pref %>% select("天后宮"), 
                                Sum = apply(Huiguan_Pref,1,sum), 
                                Prefecture = NA)
Tianhou_Pref_count$Prefecture <- rownames(Tianhou_Pref_count)
Tianhou_Pref_count <- merge(Tianhou_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Tianhou_Pref_count, "Qing_Tianhougong_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Dizhuumiao in each prefecture 帝主宮
Dizhu_Pref_count <- data.frame(Dizhu_num = Huiguan_Pref %>% select("帝主宮"), 
                                  Sum = apply(Huiguan_Pref,1,sum), 
                                  Prefecture = NA)
Dizhu_Pref_count$Prefecture <- rownames(Dizhu_Pref_count)
Dizhu_Pref_count <- merge(Dizhu_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Dizhu_Pref_count, "Qing_Dizhugong_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")

# Guandiumiao in each prefecture 關帝廟
Guandi_Pref_count <- data.frame(Guandi_num = Huiguan_Pref %>% select("關帝廟"), 
                               Sum = apply(Huiguan_Pref,1,sum), 
                               Prefecture = NA)
Guandi_Pref_count$Prefecture <- rownames(Guandi_Pref_count)
Guandi_Pref_count <- merge(Guandi_Pref_count, Sichuan_pref %>% select("NAME_PY", "geometry"), by.x = c("Prefecture"), by.y = c("NAME_PY"))
st_write(Guandi_Pref_count, "Qing_Guandimiao_count_Prefecture.shp", 
         layer_options = "ENCODING=UTF-8", delete_layer = TRUE, 
         driver = "ESRI Shapefile")
