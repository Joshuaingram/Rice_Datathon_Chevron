library(readxl)
library(tidyverse)
library(dplyr)
# specifying the path for file
path <- "~/New_College/Rice_Datathon_2023/Congress Data"

# set the working directory 
setwd(path)

files <- list.files(path, pattern = "*.xlsx", full.names = T)

congress_data1 <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")

congress_data2 <- congress_data1

congress_data2$congressNumber <- sub("^", ".", congress_data2$congressNumber)     
congress_data2$congressNumber <- as.numeric(congress_data2$congressNumber)

duplicate_congress_data <- bind_rows(congress_data1, congress_data2)

#-------------------mutation----------------------------------
duplicate_congress_data <- duplicate_congress_data%>%
  mutate(Year = case_when(
    congressNumber == 106 ~ 1999,
    congressNumber == 107 ~ 2001,
    congressNumber == 108 ~ 2003,
    congressNumber == 109 ~ 2005,
    congressNumber == 110 ~ 2007,
    congressNumber == 111 ~ 2009,
    congressNumber == 112 ~ 2011,
    congressNumber == 113 ~ 2013,
    congressNumber == 114 ~ 2015,
    congressNumber == 115 ~ 2017,
    congressNumber == 116 ~ 2019,
    congressNumber == .106 ~ 1999+1,
    congressNumber == .107 ~ 2001+1,
    congressNumber == .108 ~ 2003+1,
    congressNumber == .109 ~ 2005+1,
    congressNumber == .110 ~ 2007+1,
    congressNumber == .111 ~ 2009+1,
    congressNumber == .112 ~ 2011+1,
    congressNumber == .113 ~ 2013+1,
    congressNumber == .114 ~ 2015+1,
    congressNumber == .115 ~ 2017+1,
    congressNumber == .116 ~ 2019+1
  ))

#View(duplicate_congress_data)

#---------------------------Cleaning-----------------------
duplicate_congress_data$stateNameFull <- state.name[match(duplicate_congress_data$stateName,state.abb)]

cleaned_congress_data <- duplicate_congress_data %>%
                          select(position, stateName, stateNameFull, Year, parties)

cleaned_congress_data_party_num <- cleaned_congress_data %>% 
                          mutate(partyNumeric = case_when(
                            parties == "Republican" ~ 1,
                            parties != "Republican" ~ 0
                          ))
                          
congress_data_props <- cleaned_congress_data_party_num %>% 
                          group_by(stateNameFull,
                                   Year,
                                   position) %>% 
                            summarise (partyProp = mean(partyNumeric))
#------------------Convert to Wide Format ----------------------------                         
congress_data_wide <- congress_data_props %>%
                        pivot_wider(names_from = position,
                                    values_from = partyProp)

final_congress_data <- congress_data_wide[1:4]

View(final_congress_data)

#------------------------------Write File as CSV ---------------------------
write.csv(final_congress_data, "Congress_Data_1999_2020.csv", row.names=FALSE)
