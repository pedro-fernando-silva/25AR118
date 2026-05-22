#Carrego os pacotes
{
  require(readxl)
  require(dplyr)
  require(openxlsx)
  require(ggplot2)
  require(scales)
  require(forcats)
  require(stringr)
  require(tidyverse)
  require(extrafont)
  options(scipen = 999)
}

#Carrego a base e filtro
{
  setwd("C:/Users/PedroFernandoSantosV")
  demog = read_excel("Downloads/25AR118_raw_CRF.xlsx", skip = 1) %>% drop_na(`RANDOMISATION ID`)
  colnames(demog) = gsub("_|D0PRELASER", "", colnames(demog))
  demog = demog %>% drop_na(`RANDOMISATION ID`) %>% 
    select(`RANDOMISATION ID`, STATUS, 
           contains("DEMOG")) %>%
    rename("ID" = "RANDOMISATION ID")
  colnames(demog) = gsub("DEMOG", "", colnames(demog))
  demog$PHOTOTYPE = factor(demog$PHOTOTYPE, levels = c("Fototipo I = Pontuação Geral de 0 a 7", "Fototipo II = Pontuação Geral de 8 a 16", "Fototipo III = Pontuação Geral de 17 a 25", "Fototipo IV = Pontuação Geral de 26 a 30", "Fototipo V = Pontuação Geral de 31 a 35", "Fototipo VI = Pontuação Geral acima de 35"), labels = c("I", "II", "III", "IV", "V", "VI"))
  demog$GENDER = factor(demog$GENDER, levels = c("Feminino", "Masculino"), labels = c("Female", "Male"))
  demog$ETHNICITY = factor(demog$ETHNICITY, levels = c("Asiática", "Negra/Afro-americana", "Latina/Hispânica", "Branca/Caucasiana"), labels =  c("Asian", "Black/Afro-american", "Latina/Hispanic", "White/Caucasian"))
  demog$SENSITIVESKIN = factor(demog$SENSITIVESKIN, levels = c("Sim", "Não"), labels =  c("Yes", "No"))
  demog$SKINTYPE = factor(demog$SKINTYPE, levels = c("Seca", "Normal", "Mista", "Oleosa"), labels = c("Dry", "Normal", "Mixed", "Oily"))
  demog = demog %>% 
    mutate(`Age Group` = dplyr::case_when(
      AGE >= 18 & AGE <= 28 ~ "18-28",
      AGE >= 29 & AGE <= 39 ~ "29-39",
      AGE >= 39 & AGE <= 49 ~ "39-49",
      AGE >= 50 & AGE <= 60 ~ "50-60",
      TRUE ~ "ARRUMAR"))
  colnames(demog)[3:12] = c("AGE", "Gender", 
                            "Skin Type", "Ethnicity", 
                            "Sensitive Skin", 
                            "Contraceptive1", "Contraceptive2", "Contraceptive3", 
                            "Skin Care Routine", "Phototype")
  demog$`Age Group` = as.factor(demog$`Age Group`)
  demog <- demog %>% mutate(STATUS = case_when(
    STATUS %in% c("COMPLETED", "SIGNED") ~ "COMPLETED",
    STATUS %in% c("IN_PROGRESS", "QUERIES") ~ "In Study",
    str_starts(STATUS, "EXCLUDED") ~ "EXCLUDED",
    TRUE ~ STATUS
  )) %>%
    select(-contains("Contraceptive"), -`Skin Care Routine`)
}

#Carrego as funções
{
  demog_tab = function(data, column_position){
    demog_final = data.frame("Parameter" = NA,
                             "Variable" = NA,
                             "N" = NA,
                             "%" = NA)
    colnames(demog_final) = c("Parameter", "Variable", "N", "%")
    
    for(k in column_position){
      df_aux = data[,k]
      test_aux = data.frame()
      column_name = colnames(df_aux)
      colnames(df_aux) = "analysis_column"
      df_aux = filter(df_aux, is.na(df_aux$analysis_column) == F)
      df_helper = df_aux
      
      if(unlist(gregexpr("AGE", column_name)) != -1){
        df_aux = data.frame()
        df_aux[nrow(df_aux)+1, 1] = column_name
        df_aux[nrow(df_aux), 2] = round(mean(df_helper$analysis_column, na.rm = T), 2)
        df_aux[nrow(df_aux), 3] = round(median(df_helper$analysis_column, na.rm = TRUE),2)
        df_aux[nrow(df_aux), 4] = round(min(df_helper$analysis_column, na.rm = TRUE),2)
        df_aux[nrow(df_aux), 5] = round(max(df_helper$analysis_column, na.rm = TRUE),2)
        df_aux[nrow(df_aux), 6] = round(sd(df_helper$analysis_column, na.rm = TRUE),2)
        # df_aux[nrow(df_aux), 7] = round(std.error(df_helper$analysis_column, na.rm = TRUE),2)
        colnames(df_aux) = c("Parameter", "Mean", "Median", "Minimum", "Maximum", "SD")
        
        df_aux = as.data.frame(t(df_aux))
        df_aux$parameter = rownames(df_aux)
        df_aux$n = NA
        df_aux$freq = NA
        df_aux = select(df_aux, 3, 2, everything())
        df_aux = df_aux[2:nrow(df_aux),]
        df_aux[1,1] = column_name
        colnames(df_aux) = c("Parameter", "Variable", "N", "%")
      }else{
        df_aux = as.data.frame(table(df_aux[,1]))
        df_aux$Freq = ifelse(df_aux$analysis_column == "NA", NA, df_aux$Freq)
        df_aux$prop = percent(df_aux$Freq/sum(df_aux$Freq, na.rm = T), accuracy = 0.01)
        df_aux = filter(df_aux, analysis_column != "NA")
        df_aux[1,ncol(df_aux)+1] = column_name
        colnames(df_aux) = c("Variable", "N", "%", "Parameter")
        df_aux = select(df_aux, 4, everything())
      }
      demog_final = rbind(demog_final, df_aux)
    }
    demog_final = demog_final[2:nrow(demog_final),]
    demog_final$Parameter = str_to_title(demog_final$Parameter)
    demog_final[demog_final == "Age"] = NA
    demog_final[demog_final == "Age Group"] = "Age"
    return(demog_final)
  }
}

#Crio a tabela
{
  demog2 = demog %>% filter(STATUS != "EXCLUDED")
  demog_table = demog_tab(demog2, c(4, ncol(demog2), 3, 5:8))
}

#Crio uma lista com o que preciso exportar pro excel e o faço
lista = list("Data" = demog, "Summary" = demog_table)
write.xlsx(lista, "25AR118_Demog.xlsx")