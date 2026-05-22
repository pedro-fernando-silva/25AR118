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

#Carrego as bases, filtro e ajusto
{
  #Diretório - ajustar conforme necessário
  setwd("C:/Users/PedroFernandoSantosV")
  
  #Base - geralmente num caminho similar
  tolerance_all = read_excel("Downloads/25AR118_raw_CRF.xlsx", skip = 1) %>% 
    #Seleciono apenas as colunas necessárias para análise
    select(`RANDOMISATION ID`, STATUS,
           contains("TOLERANCE")) %>%
    #Crio duas colunas
    mutate(ERYTHEMA_D0POSTLASER = NA,
           ERYTHEMA_D0POSTIP = NA) %>%
    #Renomeio ID e ordeno
    arrange(`RANDOMISATION ID`) %>%
    rename("ID" = `RANDOMISATION ID`) %>% 
    #Renomeio os Status
    mutate(STATUS = case_when(
      STATUS %in% c("COMPLETED", "SIGNED") ~ "COMPLETED",
      STATUS %in% c("IN_PROGRESS", "QUERIES", "VERIFICATION") ~ "In Study",
      str_starts(STATUS, "EXCLUDED") ~ "EXCLUDED",
      TRUE ~ STATUS
    )) %>% 
    #Removo NA dos IDs
    drop_na(ID) %>% 
    #Ordeno por Timepoints
    select(ID, STATUS,
           matches("D0PRELASER$"),
           matches("D0POSTLASER$"),
           matches("D0POSTIP$"),
           matches("D7$"))  %>%
    #Removo o "_TOLERANCE"
    setNames(gsub("_TOLERANCE", "", colnames(.))) %>%
    #Renomeio as colunas
    rename_with(~ str_replace_all(., c(
      "D0PRELASER" = "D0 (Pre-Laser)",
      "D0POSTLASER" = "D0 (Post-Laser)",
      "D0POSTIP" = "D0 (Post-IP)",
      "BURNINGSENSATION" = "Burning Sensation",
      "BURNING_SENSATION" = "Burning Sensation",
      "ERYTHEMA" = "Erythema",
      "SCALINGSP" = "Scaling (Self-Perceived)", 
      "DRYNESSSP" = "Dryness (Self-Perceived)", 
      "DESQUAMATION" = "Peeling", 
      "SCALING" = "Scaling", 
      "DRYNESS" = "Dryness", 
      "STINGING" = "Stinging",
      "EDEMA" = "Edema",
      "TIGHTNESS" = "Tightness",
      "ITCHING" = "Itching",
      "TINGLING" = "Tingling"
    ))) %>%
    #Transformo em numeric
    mutate(across(everything(),
                  ~ recode(as.character(.),
                           "Severo" = "4",
                           "Intenso" = "4",
                           "Moderado" = "3",
                           "Leve" = "2",
                           "Muito Leve" = "1",
                           "Nenhum" = "0"
                  )))  %>%
    #Removo colunas não necessárias
    select(-contains("DIFFERENCE"), 
           -contains("EXTRA_COMMENT"), 
           -contains("STARTTIME")) 
  
  #Crio uma base apenas com os details
  tolerance_detail = tolerance_all %>%
    pivot_longer(
      cols = -c(ID, STATUS),
      names_to = "name",
      values_to = "value"
    ) %>%
    mutate(
      Grade_Detail = str_detect(name, "_DETAIL_"),
      Parameter = str_extract(name, "^[^_]+"),
      Timepoint = str_extract(name, "[^_]+$")
    ) %>%
    select(-name) %>%
    pivot_wider(
      names_from = Grade_Detail,
      values_from = value
    ) %>%
    rename(
      Grade = `FALSE`,
      Detail = `TRUE`
    ) %>%
    select(ID, STATUS, Parameter, Timepoint, Grade, Detail) %>%
    drop_na(Detail)
  
  #Crio uma apenas com os dados
  tolerance_all <- tolerance_all %>% 
    select(-contains("DETAIL")) %>%
    mutate(across(3:ncol(.), as.numeric))
}

#Chamo as funções
{
  stat_summary = function(data_og, columns_position, offset){
    # #Crio um dataframe pra armazenar os dados
    # stat_summary(efficacy, 3:65, 54)
    # data_og = tolerance_all
    # offset = 24
    # i = 11
    # columns_position = 3:34
    sumario = data.frame() 
    for (i in columns_position){
      #Crio uma base com uma coluna apenas, para cada coluna na lista providenciada antes
      data = select(data_og, i)
      
      #O timepoint é nomeado dependendo de como termina o nome da coluna
      {
        timepoint = str_extract(colnames(data)[1], "[^_]+$")
      }
      
      #Nomeio a única coluna do df para padronizar
      colnames(data) = "avaliacao"
      
      #Coloco média, DP, N, timepoint e a coluna
      sumario[nrow(sumario)+1, 1] = colnames(data_og)[i]
      sumario[nrow(sumario), 2] = timepoint
      
      if(dim(table(data$avaliacao)) != 0){
        sumario[nrow(sumario), 3] = sum(!(is.na(data$avaliacao)))
        sumario[nrow(sumario), 4] = mean(data$avaliacao, na.rm = T)
        sumario[nrow(sumario), 5] = "±"
        sumario[nrow(sumario), 6] = sd(data$avaliacao, na.rm = T)
        sumario[nrow(sumario), 7] = min(data$avaliacao, na.rm = T)
        sumario[nrow(sumario), 8] = max(data$avaliacao, na.rm = T)
        sumario[nrow(sumario), 9] = median(data$avaliacao, na.rm = T)
        #Se o timepoint não for D0, calculo a diferença e faço os testes estatísticos em cima dela
        if(!(timepoint %in% c("D0 (Pre-Laser)"))){
          
          df_aux = select(data_og, i+offset)
          colnames(df_aux) = "avaliacao"
          sumario[nrow(sumario), 10] = mean(df_aux$avaliacao, na.rm = T)
          sumario[nrow(sumario), 11] = "±"
          sumario[nrow(sumario), 12] = sd(df_aux$avaliacao, na.rm = T)
          
          media_D0 = sumario[nrow(sumario), 4] - sumario[nrow(sumario), 10]
          sumario[nrow(sumario), 13] = (sumario[nrow(sumario), 4]/media_D0) - 1
          
          if(dim(table(df_aux$avaliacao)) == 1){
            sumario[nrow(sumario), 14] = "Test was not performed (all values are equal)"
            sumario[nrow(sumario), 15] = "Test was not performed (all values are equal)"
            sumario[nrow(sumario), 16] = "Test was not performed (all values are equal)"
          }else{
            teste_normalidade = shapiro.test(df_aux$avaliacao)
            if(teste_normalidade$p.value >= 0.01){
              teste_media = t.test(df_aux$avaliacao, alternative = "two.sided")
              sumario[nrow(sumario), 14] = "T-Test"
            }
            if(teste_normalidade$p.value < 0.01){
              teste_media = exactRankTests::wilcox.exact(df_aux$avaliacao, alternative = "two.sided")
              sumario[nrow(sumario), 14] = "Wilcoxon Test"
            }
            
            if(teste_media$p.value > 0.001){
              sumario[nrow(sumario), 15] = as.character(round(teste_media$p.value, 5))
            }else{
              sumario[nrow(sumario), 15] = "< 0.001"
            }
            
            if(teste_media$p.value < 0.05){sumario[nrow(sumario), 16] = "Significant"}
            if(teste_media$p.value >= 0.05){sumario[nrow(sumario), 16] = "Non-Significant"}
          }
          
          sumario[nrow(sumario), 17] = sum(df_aux$avaliacao < 0, na.rm = T)/sum(!(is.na(df_aux$avaliacao)))
          sumario[nrow(sumario), 18] = sum(df_aux$avaliacao == 0, na.rm = T)/sum(!(is.na(df_aux$avaliacao)))
          sumario[nrow(sumario), 19] = sum(df_aux$avaliacao > 0, na.rm = T)/sum(!(is.na(df_aux$avaliacao)))
          
        }
        #Se for D0, tudo vira NA
        else{
          sumario[nrow(sumario), 10] = NA
          sumario[nrow(sumario), 11] = NA
          sumario[nrow(sumario), 12] = NA
          sumario[nrow(sumario), 13] = NA
          sumario[nrow(sumario), 14] = NA
          sumario[nrow(sumario), 15] = NA
          sumario[nrow(sumario), 16] = NA
          sumario[nrow(sumario), 17] = NA
          sumario[nrow(sumario), 18] = NA
          sumario[nrow(sumario), 19] = NA
        }
        
        #Serve caso haja interesse em calcular a frequência da escala
        # sumario[nrow(sumario), 20] = nrow(subset(data,data$avaliacao == 0))
        # sumario[nrow(sumario), 21] = nrow(subset(data,data$avaliacao == 1))
        # sumario[nrow(sumario), 22] = nrow(subset(data,data$avaliacao == 2))
        # sumario[nrow(sumario), 23] = nrow(subset(data,data$avaliacao == 3))
        # sumario[nrow(sumario), 24] = nrow(subset(data,data$avaliacao == 4))
        # sumario[nrow(sumario), 25] = nrow(subset(data,data$avaliacao == 5))
        # sumario[nrow(sumario), 26] = nrow(subset(data,data$avaliacao == 6))
        # sumario[nrow(sumario), 27] = nrow(subset(data,data$avaliacao == 7))
        # sumario[nrow(sumario), 28] = nrow(subset(data,data$avaliacao == 8))
        # sumario[nrow(sumario), 29] = nrow(subset(data,data$avaliacao == 9))
      }else{
        sumario[nrow(sumario), 3:19] = NA
      }
    }
    
    #Nomeio
    colnames(sumario) = c("Parameter", "Timepoint", "n", "Mean ± SD", ".1", ".2", "Min", "Max", "Median", 
                          "Mean Change from Baseline ± SD", ".3", ".4", "Mean % Change from D0", "Statistical Test", 
                          "p-value", "Significance", "% of subjects with improvement", 
                          "% of subjects with no change", "% of subjects with worsening")#, 
    #"0", "1", "2", "3", "4")#, "6", "7", "8", "9")
    
    # #Factor para timepoint
    sumario$Timepoint = factor(sumario$Timepoint, levels = c("D0 (Pre-Laser)", "D0 (Post-Laser)",  "D0 (Post-IP)", "D7"))
    
    # #Removo os timepoints dos parameters
    sumario$Parameter = gsub("D0 \\([^)]*\\)|D7|_", "", sumario$Parameter)
    
    #Ordeno por ordem alfabética
    sumario = sumario[order(sumario$Parameter, sumario$Timepoint),]
    
    return(sumario)
  }
  
  diferencas_new <- function(data){
    
    cols <- names(data)
    param_cols <- grep("_.+$", cols, value = TRUE)
    param <- sub("_[^_]+$", "", param_cols)
    tp <- sub(".*_", "", param_cols)
    
    info <- data.frame(
      col = param_cols,
      param = param,
      tp = tp,
      stringsAsFactors = FALSE
    )
    
    d0 <- info[info$tp == "D0 (Pre-Laser)", ]
    future <- info[info$tp != "D0 (Pre-Laser)", ]
    
    new_cols <- vector("list", nrow(future))
    
    for(i in seq_len(nrow(future))){
      p <- future$param[i]
      tp_col <- future$col[i]
      d0_col <- d0$col[d0$param == p]
      
      if(length(d0_col) == 1){
        new_cols[[i]] <- data[[tp_col]] - data[[d0_col]]
      }
    }
    
    # Keep track of which indices had a valid D0 match
    valid_idx <- which(!sapply(new_cols, is.null))
    new_cols  <- new_cols[valid_idx]
    
    # Build names using the same valid indices
    new_names <- paste0(
      future$col[valid_idx],
      "-",
      d0$col[match(future$param[valid_idx], d0$param)]
    )
    
    data[new_names] <- new_cols
    data
  }
}

#Transformo em numérico
{
  tolerance_all[ , 3:ncol(tolerance_all)] <- lapply(tolerance_all[ , 3:ncol(tolerance_all)], \(x) as.numeric(sub(",", ".", x)))
}

#Faço as diferenças
{
  #Sempre no formato: dataframe para aplicação da função, colunas D0, diferença entre colunas D0 e timepoints futuros
  tolerance_all = diferencas_new(tolerance_all)
}

#Organizo e faço os sumários
{
  tolerance = tolerance_all %>% filter(unlist(gregexpr("EXCLUDED", STATUS)) == -1) %>% ungroup()
  summary_tolerance = stat_summary(tolerance, 3:34, 24)
}

#Organização da base
{
  #Ordem alfabética
  tolerance_all = tolerance_all %>% select(ID, STATUS,
                                           contains("Burning Sensation"), 
                                           contains("Dryness"),
                                           contains("Edema"),
                                           contains("Erythema"),
                                           contains("Itching"),
                                           contains("Peeling"),
                                           contains("Stinging"),
                                           # contains("Tightness"),
                                           contains("Tingling")
  ) %>%
    arrange(ID)
}

sumarios = list("Data" = tolerance_all,
                "Data (Detail)" = tolerance_detail,
                "Summary" = summary_tolerance)
write.xlsx(sumarios, "25AR118_Tolerance.xlsx")