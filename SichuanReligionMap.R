#============================
# Creator: Yuwei Zhou
# Description: This file contains several methods that preform preliminary data manipulations for the 
# Sichuan Religion maps
# Date created: May 26th, 2022
# Date mofdified: Feb 24th, 2024
# ============================

library(readxl)
library(xlsx)
library(dplyr)
library(stringr)

# ================
# Set up: must run every time, this step stores the "Map Data" sheet in the SichuanMap variable
# ================
setwd(getwd())
filename <- file.choose()
SichuanMap <- read_excel(filename, sheet = "Map_Data", col_names = TRUE) # should > 177 (02/24/2024)
head(SichuanMap)


unique(SichuanMap$type_faith_simp)  #there should be 9 of them
type_name <- unique(SichuanMap$type_faith_simp)
View(SichuanMap)


# preliminary check
for(i in 1:nrow(SichuanMap)){
  temp <- SichuanMap[i,]
  if(as.numeric(temp$Longitude) < as.numeric(temp$Latitude)){
    print("Problem with longitude and latitude!")
  }
}

# ==================
# Making multiple excel sheet based on the type_faith_simp
# ================

for(type in type_name){
  df <- SichuanMap[0,]
  for(i in 1:nrow(SichuanMap)) {
    temp <- SichuanMap[i,]
    if(temp$type_faith_simp==type) {
      df <- rbind(df, temp)
    }
  }
  filename <- type
  if(length(grep("/", type)) > 0) {
    filename <- gsub("/", "+", filename)
  }
  print(paste(nrow(df), " entries in ", filename))
  write.xlsx2(df, showNA = FALSE, paste("./byType/type-", filename, ".xlsx", sep = ""))
}


# # ===============
# # This method takes the coordinates in the HTML code and put them into Latitude and Longtidue column
# # ================
# for(i in 1:nrow(SichuanMap)) {
#   temp <- SichuanMap[i,]
#   if(!is.na(temp$HTML)) {
#     lat <- temp$HTML %>% substr(unlist(gregexpr("\\[", temp$HTML))[1]+1, unlist(gregexpr(",", temp$HTML))[1]-1)
#     long <- temp$HTML %>% substr(unlist(gregexpr(",", temp$HTML))[1]+1, unlist(gregexpr("\\]", temp$HTML))[1]-1)
#     if(lat != 0 & long!= 0){
#       SichuanMap[i,]$Latitude <- lat
#       SichuanMap[i,]$Longitude <- long
#     }
#   }
# }
# 
# 
# # ================
# # This method transpose coordinates for all of Travagnin's site
# # datum point: corssroad at Neijiang, 
# # false location: (29.58900834124745, 105.06680698839386)
# # real location: (29.591769770754542, 105.06374347026255)
# # lat_off = real location - false location
# # ================
# lat_off <- 29.591769770754542 - 29.58900834124745
# long_off <- 105.06374347026255 - 105.06680698839386
# for(i in 1:nrow(SichuanMap)) {
#   temp <- SichuanMap[i,]
#   if(!is.na(temp$Transpose)) {
#     SichuanMap[i,]$Latitude <- as.character(as.numeric(SichuanMap[i,]$Latitude) + lat_off)
#     SichuanMap[i,]$Longitude <- as.character(as.numeric(SichuanMap[i,]$Longitude) + long_off)
#   }
# }
# 
# # ==============
# # Save above changes to a new file
# # =============
# write.xlsx2(SichuanMap, showNA = FALSE,"./Map_modified.xlsx")
# 
# 
