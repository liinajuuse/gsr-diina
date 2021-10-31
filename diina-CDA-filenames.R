# this script takes individual Ledalab export files from set directory
# adds full directory file name to each individual files
# adds variables "Participant", "Sex", "Age", and "Language" based on ParticipantId
# ParticipantId is the full individual code extracted from  

#adding file names as last variable

#require packages
require(tidyverse)
require(dplyr)
require(data.table)

#create directory
all_paths = list.files(path = "C:/Users/liina/Desktop/test/", pattern = ".txt", full.names = T)

#read file content
all_content = all_paths %>% lapply(read.table,
                                   header = TRUE,
                                   sep = "\t", #separator tab
                                   encoding = "UTF-8")

# read file name
all_filenames <- all_paths %>%
  basename() %>%
  as.list()

# combine file content list and file name list
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
all_result <- rbindlist(all_lists, fill = T)

# change column name
names(all_result)[16] <- "File.Name"

#separate "File.Name" by "_"
all_result = all_result %>% separate(File.Name, c("VideoId", "ParticipantId", "OverUnder"), sep = "_")

#grab txt file 5th digit, make new variable
CDAallCSV1 = all_result %>% mutate("Participant" = substr(ParticipantId, 1, 3),
                      "Sex" = substr(ParticipantId, 4, 4),
                      "Age" = substr(ParticipantId, 5, 6),
                      "Language" = substr(ParticipantId, 7, 8))

CDAallCSV1$Event.Name2 <- CDAallCSV1$Event.Name

#EmoId coding based on Event.Name
CDAallCSV1[,15] <- ifelse(CDAallCSV1[,15] == 1 | CDAallCSV1[,15] == 8 | CDAallCSV1[,15] == 15 | CDAallCSV1[,15] == 22 | CDAallCSV1[,15] == 29 | CDAallCSV1[,15] == 36, "anger", 
                          ifelse(CDAallCSV1[,15] == 2 | CDAallCSV1[,15] == 9 | CDAallCSV1[,15] == 16 | CDAallCSV1[,15] == 23 | CDAallCSV1[,15] == 30 | CDAallCSV1[,15] == 37, "happy", 
                                 ifelse(CDAallCSV1[,15] == 3 | CDAallCSV1[,15] == 10 | CDAallCSV1[,15] == 17 | CDAallCSV1[,15] == 24 | CDAallCSV1[,15] == 31 | CDAallCSV1[,15] == 38, "sad", 
                                        ifelse(CDAallCSV1[,15] == 4 | CDAallCSV1[,15] == 11 | CDAallCSV1[,15] == 18 | CDAallCSV1[,15] == 25 | CDAallCSV1[,15] == 32 | CDAallCSV1[,15] == 39, "neutral", 
                                               ifelse(CDAallCSV1[,15] == 5 | CDAallCSV1[,15] == 12 | CDAallCSV1[,15] == 19 | CDAallCSV1[,15] == 26 | CDAallCSV1[,15] == 33 | CDAallCSV1[,15] == 40, "disgust",
                                                      ifelse(CDAallCSV1[,15] == 6 | CDAallCSV1[,15] == 13 | CDAallCSV1[,15] == 20 | CDAallCSV1[,15] == 27 | CDAallCSV1[,15] == 34 | CDAallCSV1[,15] == 41, "fear", 
                                                             ifelse(CDAallCSV1[,15] == 7 | CDAallCSV1[,15] == 14 | CDAallCSV1[,15] == 21 | CDAallCSV1[,15] == 28 | CDAallCSV1[,15] == 35 | CDAallCSV1[,15] == 42, "surprise", NA)))))))

#changed file names
CDAallCSV1 = CDAallCSV1 %>% rename("EmoId" = "Event.Name",
                      "Event.Name" = "Event.NID",
                      "ExpId" = "Event.Name2")

#added condition ("ExpId")
CDAallCSV1[,23] <- ifelse(CDAallCSV1[,23] == 1 | CDAallCSV1[,23] == 2 | CDAallCSV1[,23] == 3 | CDAallCSV1[,23] == 4 | CDAallCSV1[,23] == 5 | CDAallCSV1[,23] == 6 | CDAallCSV1[,23] == 7, "Ekm", 
                          ifelse(CDAallCSV1[,23] == 8 | CDAallCSV1[,23] == 9 | CDAallCSV1[,23] == 10 | CDAallCSV1[,23] == 11 | CDAallCSV1[,23] == 12 | CDAallCSV1[,23] == 13 | CDAallCSV1[,23] == 14, "Ver", 
                                 ifelse(CDAallCSV1[,23] == 15 | CDAallCSV1[,23] == 16 | CDAallCSV1[,23] == 17 | CDAallCSV1[,23] == 18 | CDAallCSV1[,23] == 19 | CDAallCSV1[,23] == 20 | CDAallCSV1[,23] == 21, "Ov1", 
                                        ifelse(CDAallCSV1[,23] == 22 | CDAallCSV1[,23] == 23 | CDAallCSV1[,23] == 24 | CDAallCSV1[,23] == 25 | CDAallCSV1[,23] == 26 | CDAallCSV1[,23] == 27 | CDAallCSV1[,23] == 28, "Ov2", 
                                               ifelse(CDAallCSV1[,23] == 29 | CDAallCSV1[,23] == 30 | CDAallCSV1[,23] == 31 | CDAallCSV1[,23] == 32 | CDAallCSV1[,23] == 33 | CDAallCSV1[,23] == 34 | CDAallCSV1[,23] == 35, "Un1",
                                                      ifelse(CDAallCSV1[,23] == 36 | CDAallCSV1[,23] == 37 | CDAallCSV1[,23] == 38 | CDAallCSV1[,23] == 39 | CDAallCSV1[,23] == 40 | CDAallCSV1[,23] == 41 | CDAallCSV1[,23] == 42, "Un2", NA))))))


#rearranged columns
df2 = CDAallCSV1[,c(1:15, 23, 16:22)]

#changing variable class
df2 = df2 %>% mutate(Participant = as.factor(Participant),
                     Age = as.numeric(Age),
                     VideoId = as.factor(VideoId),
                     EmoId = as.factor(EmoId),
                     ExpId = as.factor(ExpId),
                     Language = as.factor(Language),
                     Sex = as.factor(Sex),
                     OverUnder = as.factor(OverUnder),
                     ParticipantId = as.factor(ParticipantId))

#checking for errors
summary(df2)

# didn't save script, but
# changed one Language "es" into "ee"
# one "OverUnder" from "3" into "o"
# one "Sex" "F" into "f"
# missing one Ekman Sad condition (3231 objects instead of 3232)

#trying to find missing Ekman Sad with frequency table
df3 = df2 %>% group_by(Event.Nr) 
table(df3$ParticipantId)

#one ekman sad missing for Participant 064

# expording and adding RepId by hand (too tired)
write_delim(df2, "CDA_all_2021.txt")

#---------------
#importing excel edited file for PARTICIPANTS 12 and 13
library(readr)
KI12_KI13 <- read_delim("KI12_KI13.txt", 
                          +     "\t", escape_double = FALSE, trim_ws = TRUE)

#EmoId coding based on Event.Name
KI12_KI13[,15] <- ifelse(KI12_KI13[,15] == 1 | KI12_KI13[,15] == 8 | KI12_KI13[,15] == 15 | KI12_KI13[,15] == 22 | KI12_KI13[,15] == 29 | KI12_KI13[,15] == 36, "anger", 
                         ifelse(KI12_KI13[,15] == 2 | KI12_KI13[,15] == 9 | KI12_KI13[,15] == 16 | KI12_KI13[,15] == 23 | KI12_KI13[,15] == 30 | KI12_KI13[,15] == 37, "happy", 
                                ifelse(KI12_KI13[,15] == 3 | KI12_KI13[,15] == 10 | KI12_KI13[,15] == 17 | KI12_KI13[,15] == 24 | KI12_KI13[,15] == 31 | KI12_KI13[,15] == 38, "sad", 
                                       ifelse(KI12_KI13[,15] == 4 | KI12_KI13[,15] == 11 | KI12_KI13[,15] == 18 | KI12_KI13[,15] == 25 | KI12_KI13[,15] == 32 | KI12_KI13[,15] == 39, "neutral", 
                                              ifelse(KI12_KI13[,15] == 5 | KI12_KI13[,15] == 12 | KI12_KI13[,15] == 19 | KI12_KI13[,15] == 26 | KI12_KI13[,15] == 33 | KI12_KI13[,15] == 40, "disgust",
                                                     ifelse(KI12_KI13[,15] == 6 | KI12_KI13[,15] == 13 | KI12_KI13[,15] == 20 | KI12_KI13[,15] == 27 | KI12_KI13[,15] == 34 | KI12_KI13[,15] == 41, "fear", 
                                                            ifelse(KI12_KI13[,15] == 7 | KI12_KI13[,15] == 14 | KI12_KI13[,15] == 21 | KI12_KI13[,15] == 28 | KI12_KI13[,15] == 35 | KI12_KI13[,15] == 42, "surprise", NA)))))))

#changing variable class
KI12_KI13 = KI12_KI13 %>% mutate(Participant = as.factor(Participant),
                     Age = as.numeric(Age),
                     VideoId = as.factor(VideoId),
                     EmoId = as.factor(EmoId),
                     ExpId = as.factor(ExpId),
                     Language = as.factor(Language),
                     Sex = as.factor(Sex),
                     OverUnder = as.factor(OverUnder),
                     ParticipantId = as.factor(ParticipantId))

#checking EmoId variables
summary(KI12_KI13)

#re-importing CDA large file (102 participants) with RepId attached
CDA_2021 <- read_delim("CDA_2021_wo_KI12_KI13.txt", 
                       +     "\t", escape_double = FALSE, trim_ws = TRUE)

#rbinding df2 and KI12_KI13
df3 = rbind(CDA_2021, KI12_KI13)

#changing variable class
df3 = df3 %>% mutate(Participant = as.factor(Participant),
                                 Age = as.numeric(Age),
                                 VideoId = as.factor(VideoId),
                                 EmoId = as.factor(EmoId),
                                 ExpId = as.factor(ExpId),
                                 Language = as.factor(Language),
                                 Sex = as.factor(Sex),
                                 OverUnder = as.factor(OverUnder),
                                 ParticipantId = as.factor(ParticipantId))

#rechecking new large file
summary(df3)

#everything checks out, exporting
write_delim(df3, "CDA_all_2021.txt")
