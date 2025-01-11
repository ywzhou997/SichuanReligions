#============================
# Creator: Yuwei Zhou
# Description: This file contains several methods that preform preliminary data manipulations for the 
# Huiguan Project of Elena Valussi
# Date created: Oct 15th, 2022
# Date modified: April 12, 2023
# ============================

#load library
library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)
library(devtools)
library(showtext)
library(stringr)
library(tidyverse)
showtext_auto()


#====[ONLY NEED TO DO IT ONCE, CAN SKIP] Set up: read in files and combine====
#setwd()

filename <- file.choose()
Qing_Huiguan_Sichuan <- read_excel(filename, col_names = TRUE, col_types = "text")
head(Qing_Huiguan_Sichuan)
nrow(Qing_Huiguan_Sichuan)  #n = 981

filename <- file.choose()
Qing_Huiguan_Chongqing <- read_excel(filename, col_names = TRUE, col_types = "text")
head(Qing_Huiguan_Chongqing)
nrow(Qing_Huiguan_Chongqing)  #n = 289

#create dataframe to hold the combined file
Qing_Huiguan <- data.frame(matrix(ncol = (ncol(Qing_Huiguan_Chongqing)+1), 
                                  nrow = (nrow(Qing_Huiguan_Sichuan) + 
                                            nrow(Qing_Huiguan_Chongqing))))
colnames(Qing_Huiguan) <- c(colnames(Qing_Huiguan_Sichuan), "Province")

k = 1
for(i in 1:nrow(Qing_Huiguan_Sichuan)){
  Qing_Huiguan[i,] <- Qing_Huiguan_Sichuan[i,]
  Qing_Huiguan[i,"Province"] <- "Sichuan"
  k = k+1
}

for(i in 1:nrow(Qing_Huiguan_Chongqing)){
  Qing_Huiguan[k,] <- Qing_Huiguan_Chongqing[i,]
  Qing_Huiguan[k,"Count"] <- k
  Qing_Huiguan[k,"Province"] <- "Chongqing"
  k = k+1
}

nrow(Qing_Huiguan) #n = 1270
write.xlsx2(Qing_Huiguan, showNA = FALSE,"./Huiguan_Qing_Sichuan_Chongqing.xlsx")



#====Part I: Qing_Huiguan Data cleaning====
# This section is for data cleaning
# This include: preliminary cleaning, 
#               reading in Elena_Huiguan_Name to clean up Temple and Huiguan names,
#               reading in Elena_Huiguan_date to clean up and set up start and end date
#               reading in Elena_Huiguan_sectionName to clean up the section name
# Finally, this section output files that can be directly visualized in ArcGIS
#====================================
Qing_Huiguan <- read_excel("./Huiguan_Qing_Sichuan_Chongqing.xlsx", col_names = TRUE)
Qing_Huiguan <- Qing_Huiguan[,-1]

sort(unique(Qing_Huiguan$會館))
for(i in 1:nrow(Qing_Huiguan)){
  if(grepl('\\(', Qing_Huiguan$會館[i])) {
    temp <- paste(substr(Qing_Huiguan$會館[i], 0, gregexpr('\\(', Qing_Huiguan$會館[i])[[1]][1]-1),
                 substr(Qing_Huiguan$會館[i], gregexpr('\\(', Qing_Huiguan$會館[i])[[1]][1]+1, nchar(Qing_Huiguan$會館[i])),
                 sep = "")
    temp <- paste(substr(temp, 0, gregexpr('\\)', temp)[[1]][1]-1),
                  substr(temp, gregexpr('\\)', temp)[[1]][1]+1, nchar(temp)),
                  sep = "")
    print(Qing_Huiguan$會館[i])
    print(temp)
    Qing_Huiguan$會館[i] <- temp
  }
  if(grepl('\\(', Qing_Huiguan$時間[i])) {
    temp <- paste(substr(Qing_Huiguan$時間[i], 0, gregexpr('\\(', Qing_Huiguan$時間[i])[[1]][1]-1),
                  substr(Qing_Huiguan$時間[i], gregexpr('\\(', Qing_Huiguan$時間[i])[[1]][1]+1, nchar(Qing_Huiguan$時間[i])),
                  sep = "")
    temp <- paste(substr(temp, 0, gregexpr('\\)', temp)[[1]][1]-1),
                  substr(temp, gregexpr('\\)', temp)[[1]][1]+1, nchar(temp)),
                  sep = "")
    print(Qing_Huiguan$時間[i])
    print(temp)
    Qing_Huiguan$時間[i] <- temp
  }
}

for(i in 1:nrow(Qing_Huiguan)){
  if(grepl('\\（', Qing_Huiguan$會館[i])) {
    temp <- paste(substr(Qing_Huiguan$會館[i], 0, gregexpr('\\（', Qing_Huiguan$會館[i])[[1]][1]-1),
                  substr(Qing_Huiguan$會館[i], gregexpr('\\（', Qing_Huiguan$會館[i])[[1]][1]+1, nchar(Qing_Huiguan$會館[i])),
                  sep = "")
    temp <- paste(substr(temp, 0, gregexpr('\\）', temp)[[1]][1]-1),
                  substr(temp, gregexpr('\\）', temp)[[1]][1]+1, nchar(temp)),
                  sep = "")
    print(Qing_Huiguan$會館[i])
    print(temp)
    Qing_Huiguan$會館[i] <- temp
  }
}

for(i in 1:nrow(Qing_Huiguan)){
  if(grepl('\\[', Qing_Huiguan$會館[i])) {
    temp <- paste(substr(Qing_Huiguan$會館[i], 0, gregexpr('\\[', Qing_Huiguan$會館[i])[[1]][1]-1),
                  substr(Qing_Huiguan$會館[i], gregexpr('\\[', Qing_Huiguan$會館[i])[[1]][1]+1, nchar(Qing_Huiguan$會館[i])),
                  sep = "")
    temp <- paste(substr(temp, 0, gregexpr('\\]', temp)[[1]][1]-1),
                  substr(temp, gregexpr('\\]', temp)[[1]][1]+1, nchar(temp)),
                  sep = "")
    print(Qing_Huiguan$會館[i])
    print(temp)
    Qing_Huiguan$會館[i] <- temp
  }
}

for(i in 1:nrow(Qing_Huiguan)){
  if(grepl('&nbsp;', Qing_Huiguan$會館[i])) {
    temp <- paste(substr(Qing_Huiguan$會館[i], 0, gregexpr('&nbsp;', Qing_Huiguan$會館[i])[[1]][1]-1),
                  substr(Qing_Huiguan$會館[i], gregexpr('&nbsp;', Qing_Huiguan$會館[i])[[1]][1]+7, nchar(Qing_Huiguan$會館[i])),
                  sep = "")
    print(Qing_Huiguan$會館[i])
    print(temp)
    Qing_Huiguan$會館[i] <- temp
  }
}

for(i in 1:nrow(Qing_Huiguan)){
  if(as.numeric(Qing_Huiguan$Longitude[i]) > 200) {
    temp <- paste(substr(Qing_Huiguan$Longitude[i], 0, 3),
                  substr(Qing_Huiguan$Longitude[i], 4, nchar(Qing_Huiguan$Longitude[i])),
                  sep = ".")
    print(Qing_Huiguan$Longitude[i])
    print(temp)
    Qing_Huiguan$Longitude[i] <- temp
  }
}

for(i in 1:nrow(Qing_Huiguan)){
  if(!is.na(Qing_Huiguan$`Book Year`[i])) {
    temp <- paste("1/1/", Qing_Huiguan$`Book Year`[i], sep = "")
    print(Qing_Huiguan$`Book Year`[i])
    print(temp)
    Qing_Huiguan$`Book Year`[i] <- temp
  }
  if(!is.na(Qing_Huiguan$`Edition Year`[i])) {
    temp <- paste("1/1/", Qing_Huiguan$`Edition Year`[i], sep = "")
    print(Qing_Huiguan$`Edition Year`[i])
    print(temp)
    Qing_Huiguan$`Edition Year`[i] <- temp
  }
}

print("Double check for abnomolies!")
sort(unique(Qing_Huiguan$會館))
#write.xlsx2(sort(unique(Qing_Huiguan$會館)),showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Elena_Huiguan_Name.xlsx")


#=====Read in Elena_Huiguan_Name.xlsx and clean up====
filename <- file.choose()
Huiguan_id <- read_excel(filename, col_names = TRUE)
head(Huiguan_id)

#check for uncovered Huiguan, i
unique_Huiguan <- sort(unique(Qing_Huiguan$會館))
for(i in unique_Huiguan){
  if(!i %in% Huiguan_id$Temple){
    print(paste("Uncovered Huiguan!", i, sep = " "))
  }
}

Temple_not_count <- Huiguan_id %>% filter(Do_not_count == "yes")
Temple_not_count <- Temple_not_count$Temple
Temple_name_list <- list()

#create list for every unique temple name
for(i in 1:nrow(Huiguan_id)){
  if(is.na(Huiguan_id$Do_not_count[i])){
    Temple <- Huiguan_id$Temple[i]
    Temple_name_list[[Temple]][1] <- Huiguan_id$Temple_simp[i]
    Temple_name_list[[Temple]][2] <- Huiguan_id$Huiguan_name_1[i]
    Temple_name_list[[Temple]][3] <- Huiguan_id$Huiguan_name_2[i]
  }
}


#Go through Huiguan take out the do not counts
for(i in 1:nrow(Qing_Huiguan)){
  if(Qing_Huiguan$會館[i] %in% Temple_not_count){
    Qing_Huiguan[i,] <- NA
  }
}
Qing_Huiguan <- Qing_Huiguan %>% filter(!is.na(Count)) #take out do-not-count Huiguan
Qing_Huiguan <- Qing_Huiguan %>% filter(!is.na(會館)) #take out unnamed Huiguan
nrow(Qing_Huiguan) # n = 1123


# fill in Temple_Name_simp and Huiguan_Name_simp
Qing_Huiguan$Temple_Name_simp <- NA
Qing_Huiguan$Huiguan_Name_simp_1 <- NA
Qing_Huiguan$Huiguan_Name_simp_2 <- NA
for(i in 1:nrow(Qing_Huiguan)){
  if(Qing_Huiguan$會館[i] %in% names(Temple_name_list)){
    Qing_Huiguan$Temple_Name_simp[i] <- Temple_name_list[[Qing_Huiguan$會館[i]]][1]
    Qing_Huiguan$Huiguan_Name_simp_1[i] <- Temple_name_list[[Qing_Huiguan$會館[i]]][2]
    Qing_Huiguan$Huiguan_Name_simp_2[i] <- Temple_name_list[[Qing_Huiguan$會館[i]]][3]
  }
}

nrow(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp),])  #n = 1100
nrow(Qing_Huiguan[!is.na(Qing_Huiguan$Huiguan_Name_simp_1),])  #n = 1119
nrow(Qing_Huiguan[!is.na(Qing_Huiguan$Huiguan_Name_simp_2),])  #n = 189


#===== Date modification====
sort(unique(Qing_Huiguan$時間))
#write.xlsx2(sort(unique(Qing_Huiguan$時間)), showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Date.xlsx")

# Read in Elena_Huiguan_Date
filename <- file.choose()
Date_id <- read_excel(filename, col_names = TRUE)
head(Date_id)

# check for uncovered Dates
unique_Date <- sort(unique(Qing_Huiguan$時間))
for(i in unique_Date){
  if(!i %in% Date_id$date){
    print(paste("Uncovered Date!", i, sep = " "))
  }
}
Date_list <- list()

#create list for every unique temple name
for(i in 1:nrow(Date_id)){
    Date <- Date_id$date[i]
    Date_list[[Date]][1] <- Date_id$date_built_simp[i]
    Date_list[[Date]][2] <- Date_id$date_built_western[i]
}


# fill in Date_established and Date_established_western
Qing_Huiguan$Date_established <- NA
Qing_Huiguan$Date_established_western <- NA
Qing_Huiguan$Date_established_western_numeric <- NA
Qing_Huiguan$Date_Qing_end_western <- NA #ONLY FOR QING!!!

for(i in 1:nrow(Qing_Huiguan)){
  if(!Qing_Huiguan$時間[i] %in% Date_id$date & !is.na(Qing_Huiguan$時間[i])){
    print(Qing_Huiguan$時間[i])
  }
}
for(i in 1:nrow(Qing_Huiguan)){
  if(!is.na(Qing_Huiguan$時間[i])){
    Qing_Huiguan$Date_established[i] <- Date_list[[Qing_Huiguan$時間[i]]][1]
    if(!is.na(Date_list[[Qing_Huiguan$時間[i]]][2])){
      Qing_Huiguan$Date_established_western_numeric[i] <- as.numeric(Date_list[[Qing_Huiguan$時間[i]]][2])
      Qing_Huiguan$Date_established_western[i] <- paste("1/1/", Date_list[[Qing_Huiguan$時間[i]]][2], sep = "")
      Qing_Huiguan$Date_Qing_end_western[i] <- "1/1/1911" #ONLY FOR QING!!!
    }
  }
}

nrow(Qing_Huiguan[!is.na(Qing_Huiguan$時間),])  #n = 349
nrow(Qing_Huiguan[!is.na(Qing_Huiguan$Date_established_western),])  #n = 349



#====Section in Gazatteer======
# write.xlsx2(unique(Qing_Huiguan %>% select("Book Name", "Section Name", "目次")), 
#             showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Elena_Huiguan_SectionName.xlsx")

# Read in Elena_Huiguan_SectionName
filename <- file.choose()
Section_id <- read_excel(filename, col_names = TRUE)
head(Section_id)
unique(Section_id$目次)


# check for uncovered Dates
unique_SectionName <- sort(unique(Qing_Huiguan$`Section Name`))
for(i in unique_SectionName){
  if(!i %in% Section_id$`Section Name`){
    print(paste("Uncovered Section Name!", i, sep = " "))
  }
}
Section_list <- list()

#create list for every unique section name
for(i in 1:nrow(Section_id)){
  Section_name <- Section_id$`Section Name`[i]
  Section_list[[Section_name]][1] <- Section_id$目次[i]
}

#check for anomalies
for(i in 1:nrow(Qing_Huiguan)){
  if(!Qing_Huiguan$`Section Name`[i] %in% Section_id$`Section Name`){
    print(Qing_Huiguan$`Section Name`[i])
  }
}

# fill in Section_name_simp for every row
for(i in 1:nrow(Qing_Huiguan)){
  if(!is.na(Qing_Huiguan$`Section Name`[i])){
    Qing_Huiguan$Section_name_simp[i] <- Section_list[[Qing_Huiguan$`Section Name`[i]]][1]
  }
}


nrow(Qing_Huiguan[!is.na(Qing_Huiguan$`Section Name`),])  #n = 1123
nrow(Qing_Huiguan[!is.na(Qing_Huiguan$Section_name_simp),])  #n = 1123



#===Exporting data=====

write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp),], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_TempleName.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Huiguan_Name_simp_1),], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_HuiguanName.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Date_established_western),], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Dated.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="萬壽宮",], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Wanshou.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="禹王宮",], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Yuwang.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="川主廟",], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Chuanzhu.xlsx")
write.xlsx2(Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="南華宮",], 
            showNA = FALSE,"/Users/zhouyuwei/Desktop/Elena/Qing_Huiguan_Nanhua.xlsx")






#==== Aprt II: Data analysis====
# This section performs data analysis on the processed data
#=======================

# Qing Huiguan 
#Temporal Distribution
Qing_Huiguan %>% filter(!is.na(Date_established_western), 
                        Date_established_western_numeric > 1500) %>%  
  arrange(Date_established_western_numeric) %>%
  dplyr::mutate(cs = row_number()) %>%
  ggplot(aes(x = Date_established_western_numeric)) + 
  geom_line(aes(y=cs, color = 'Cumulative Huiguan count'), linewidth = 1) +
  geom_line(aes(y = after_stat(count), color = 'Huiguan built per year'), stat = "count") +
  scale_color_manual(name='Huiguan Built in Qing Dynasty',
                     breaks=c('Cumulative Huiguan count', 'Huiguan built per year'),
                     values=c('Cumulative Huiguan count'='grey', 'Huiguan built per year'='black')) +
  xlab("Year") +
  ylab("Quantity") +
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14)) +
  theme_minimal()

# Huiguan built per year
Qing_Huiguan %>% filter(!is.na(Date_established_western), 
                        Date_established_western_numeric > 1500) %>%  
  arrange(Date_established_western_numeric) %>%
  dplyr::mutate(cs = row_number()) %>%
  ggplot(aes(x = Date_established_western_numeric)) + 
  #geom_density(aes(y = after_stat(count)), fill = 'grey') +
  geom_histogram(aes(y = after_stat(count), color = 'Huiguan built per year'), fill = '#0073C2FF', alpha = 0.5, binwidth = 10) +
  xlab("Year") +
  ylab("Quantity") +
  scale_color_manual(name = "", breaks=c('Huiguan built per year'),
                     values=c('Huiguan built per year'='#0073C2FF')) +
  theme(legend.title=element_text(size=14)) +
  theme_minimal()



#Occurance in Gazetteers
#method 1 - pie chart

Qing_Huiguan %>% group_by(Section_name_simp) %>% summarize(cnt = n()) %>% 
  ggplot(aes(x = 4, y = cnt, fill = reorder(Section_name_simp, -cnt))) +
  geom_col()+
  geom_bar(stat="identity") +
  coord_polar("y", start = 0)+
  theme_void() +
  scale_fill_brewer(palette = "Set3", name = "Section name") +
  xlim(c(0.2, 4.5))+
  ggtitle("Huiguan Section Distribution in Gazetteers") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))


# method 2 - pie chart, specified color for religous sites
sec_name_col <- c(rep(0, length(unique(Section_id$目次))))
names(sec_name_col) <- names(sort(table(Section_id$目次), decreasing = TRUE))
sec_name_col = c("寺觀" = "#2D4E40", "祠廟" = "#345B4B","壇廟" = "#3C6855", "祀典"  = "#437560", "典禮" = "#4A826B", 
                 "寺廟" = "#528E75", "祠祀" = "#599B80", 
                 "藝文" = "#FFFFAA", "人物" = "#FFFF55", "學校" = "#FF80FF", "古蹟" = "#FF8080", "建置" = "#FF8000",
                 "雜" = "#FF00FF", "山水" = "#FF0080", "倉儲" = "#FF0000", "陵墓" = "#80FFFF", "公署" = "#80FF80",
                 "風俗" = "#80FF00", "武功" = "#8080FF", "壇壝" = "#AAAAFF", "安撫" = "#AAFFAA", "忠義" = "#FF5500",
                 "戶口" = "#00FFFF", "星野" = "#0080FF", "物產" = "#FFAAFF", "瑣記" = "#55AAFF", "田賦" = "#0000FF",
                 "祥異" = "#FF5555", "私祀" = "#FFAA00", "節令" = "#FFAAAA", "節孝" = "#AAFF00", "義行" = "#AAFF55",
                 "諸祀" = "#FF55AA", "輿地" = "#FFFFC6")
DF <- Qing_Huiguan
DF$Section_name_simp <- factor(DF$Section_name_simp, levels = c("寺觀", "祠廟","壇廟", "祀典", "典禮", 
                                                           "寺廟" , "祠祀", 
                                                           "藝文", "人物", "學校", "古蹟", "建置",
                                                           "雜", "山水", "倉儲", "陵墓", "公署",
                                                           "風俗", "武功", "壇壝", "安撫", "忠義",
                                                           "戶口", "星野", "物產", "瑣記", "田賦",
                                                           "祥異", "私祀", "節令", "節孝", "義行",
                                                           "諸祀", "輿地"), ordered = TRUE)
DF %>% group_by(Section_name_simp) %>% summarize(cnt = n()) %>% 
  ggplot(aes(x = 4, y = cnt, fill = Section_name_simp)) +
  geom_col()+
  geom_bar(stat="identity") +
  coord_polar("y", start = 0)+
  theme_void() +
  scale_fill_manual(values = sec_name_col, name = "Section name") +
  xlim(c(0.2, 4.5))+
  ggtitle("Huiguan Section Distribution in Gazetteers") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#method 2 - pie donut chart
library(webr)
DF <- Qing_Huiguan
for(i in 1:nrow(DF)) {
  if(!is.null(DF$Section_name_simp[i])) {
    temp <- DF$Section_name_simp[i]
    if(temp == "寺觀" | temp == "祠廟" | temp == "壇廟" | temp == "祀典" | temp == "典禮" | temp == "寺廟" | temp == "祠祀"){
      DF$Section_name_simp_2[i] <- "Religious"
    }  else {
      DF$Section_name_simp_2[i] <- "Non-religious"
    }
  }
}
f <- DF %>% group_by(Section_name_simp, Section_name_simp_2) %>% summarize(n = n()) 
#%>% arrange(desc(n))
print(f)
colnames(f) <- c("Section_name", "Type", "n")
PieDonut(f, aes(Type, Section_name, count=n), title = "Huiguan Section Distribution in Gazetteers")


# Wanshougong 萬壽宮
# Geographical distribution
Wanshougong <- Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="萬壽宮",]
Wanshougong %>% 
  ggplot(aes(x = fct_infreq(`Place Name`))) +
  geom_bar(aes(y = after_stat(count))) +
  coord_cartesian(xlim=c(0,25)) +
  theme_minimal() +
  xlab("Place name") +
  ggtitle("Wanshougong Geographic Distribution") +
  theme(axis.text.x = element_text(angle = 45))

#Temporal Distribution
Wanshougong %>% filter(!is.na(Date_established_western), 
                       Date_established_western_numeric > 1500) %>%  
  arrange(Date_established_western_numeric) %>%
  dplyr::mutate(cs = row_number()) %>%
  ggplot(aes(x = Date_established_western_numeric)) + 
  geom_line(aes(y=cs, color = 'Cumulative temple count'), linewidth = 1) +
  geom_line(aes(y = ..count.., color = 'Temple built per year'), stat = "count") +
  scale_color_manual(name='Wanshougong Count',
                     breaks=c('Cumulative temple count', 'Temple built per year'),
                     values=c('Cumulative temple count'='grey', 'Temple built per year'='black')) +
  xlab("Year") +
  ylab("Quantity") +
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14)) +
  ylim(0, 100)+
  theme_minimal()


#Occurance in Gazetteer
Wanshougong %>% group_by(Section_name_simp) %>% summarize(cnt = n()) %>% 
  ggplot(aes(x = "", y = cnt, fill = reorder(Section_name_simp, -cnt))) +
  geom_col()+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0)+
  theme_void() +
  scale_fill_brewer(palette="Set3", name = "Section name") +
  ggtitle("Wanshougong Section Distribution in Gazetteers")


# Yu Wang gong 禹王宮
# Geographical distribution
Yuwanggong <- Qing_Huiguan[!is.na(Qing_Huiguan$Temple_Name_simp)&Qing_Huiguan$Temple_Name_simp =="禹王宮",]
Yuwanggong %>% 
  ggplot(aes(x = fct_infreq(`Place Name`))) +
  geom_bar(aes(y = ..count..)) +
  coord_cartesian(xlim=c(0,25)) +
  theme_minimal() +
  xlab("Place name") +
  ggtitle("Yuwanggong Geographic Distribution") +
  theme(axis.text.x = element_text(angle = 45))

# Temporal Distribution
Yuwanggong %>% filter(!is.na(Date_established_western), 
                       Date_established_western_numeric > 1500) %>%  
  arrange(Date_established_western_numeric) %>%
  dplyr::mutate(cs = row_number()) %>%
  ggplot(aes(x = Date_established_western_numeric)) + 
  geom_line(aes(y=cs, color = 'Cumulative temple count'), size = 1) +
  geom_line(aes(y = ..count.., color = 'Temple built per year'), stat = "count") +
  scale_color_manual(name='Yuwanggong Count',
                     breaks=c('Cumulative temple count', 'Temple built per year'),
                     values=c('Cumulative temple count'='grey', 'Temple built per year'='black')) +
  xlab("Year") +
  ylab("Quantity") +
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14)) +
  ylim(0, 100)+
  theme_minimal()
  transition_reveal(year)

