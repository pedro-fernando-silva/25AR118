#Carrego os pacotes
{
  require(readxl)
  require(dplyr)
  require(vtable)
  require(plotrix)
  require(openxlsx)
  require(ggplot2)
  require(gridExtra)
  require(ggpubr)
  require(showtext)
  require(RColorBrewer)
  require(scales)
  require(forcats)
  require(stringr)
  require(tidyr)
  require(tidyverse)
  require(exactRankTests)
  require(showtext)
  require(extrafont)
  options(scipen = 999)
}

#Compliance
{
  setwd("C:/Users/PedroFernandoSantosV")
  compliance = read_excel("Downloads/25AR118_raw_CRF.xlsx", skip = 1) %>% drop_na(`RANDOMISATION ID`)
  compliance = compliance %>% drop_na(`RANDOMISATION ID`) %>% 
    select(`RANDOMISATION ID`, STATUS, 
           contains("COMPLIANCE"))
  colnames(compliance) = gsub("COMPLIANCE|_|1APP", "", colnames(compliance))
  compliance = select(compliance, 
                      -D0D6)
  compliance <- compliance %>% mutate(STATUS = case_when(
    STATUS %in% c("IN_PROGRESS", "QUERIES", "VERIFICATIONS") ~ "In Study",
    str_starts(STATUS, "EXCLUDED") ~ "EXCLUDED",
    str_starts(STATUS, "COMPLETED") ~ "COMPLETED",
    str_starts(STATUS, "SIGNED") ~ "COMPLETED",
    TRUE ~ STATUS
  ))
  compliance = mutate(compliance, across(3:ncol(compliance), as.numeric)) %>% arrange(`RANDOMISATION ID`)
  compliance$`Total Applications` = rowSums(compliance[,3:ncol(compliance)], na.rm = T)
  compliance$NAs <- rowSums(is.na(compliance[, 3:(ncol(compliance)-1)]))
}

#Peso
{
  setwd("C:/Users/PedroFernandoSantosV")
  weight = read_excel("Downloads/25AR118_raw_CRF.xlsx", skip = 1) %>% 
    drop_na(`RANDOMISATION ID`) %>%
    select(`RANDOMISATION ID`, STATUS, SHADE_PRODUCTSECTION_D0POSTLASER, matches("WEIGHT")) %>% mutate(STATUS = case_when(
      STATUS %in% c("IN_PROGRESS", "QUERIES", "VERIFICATIONS") ~ "COMPLETED",
      str_starts(STATUS, "EXCLUDED") ~ "EXCLUDED",
      str_starts(STATUS, "COMPLETED") ~ "COMPLETED",
      str_starts(STATUS, "SIGNED") ~ "COMPLETED",
      TRUE ~ STATUS
    )) %>%
    select(`RANDOMISATION ID`, STATUS, SHADE_PRODUCTSECTION_D0POSTLASER, contains("INITIAL_WEIGHT"), contains("FINAL_WEIGHT")) %>%
    mutate("Total Use" = INITIAL_WEIGHT_1_PRODUCTWEIGHT - FINAL_WEIGHT_1_PRODUCTWEIGHT) %>%
    rename(ID = `RANDOMISATION ID`,
           Shade  = SHADE_PRODUCTSECTION_D0POSTLASER,
           "Initial Weight" = INITIAL_WEIGHT_1_PRODUCTWEIGHT,
           "Final Weight" = FINAL_WEIGHT_1_PRODUCTWEIGHT)
}

#Crio uma lista com o que preciso exportar pro excel e o faço
lista = list("Compliance" = compliance,
             "Weight" = weight)
write.xlsx(lista, "25AR118_Compliance.xlsx")
