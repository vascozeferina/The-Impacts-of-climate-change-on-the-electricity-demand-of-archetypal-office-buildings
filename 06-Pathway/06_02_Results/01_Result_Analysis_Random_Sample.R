# Upload Results
rm(list=ls())
gc()

mypackages<-c("boot",
              "dplyr", 
              "devtools",
              "data.table",
              "eplusr",
              "fs",
              "gdata",
              "ggpubr",
              "grid",
              "gridExtra",
              "ggplot2", 
              "ncdf4",
              "plotly",
              "readxl",
              "sprof",
              "sensitivity",
              "sjstats",
              "stringr",
              "tidyr",
              "tidyverse",
              "tools",
              "TTR",
              "writexl",
              "xlsx",
              "zoo",
              "zonator")

#install.packages(mypackages) # rest omitted
lapply(mypackages, require, character.only = TRUE)

################### Functions ############

Upload_Results <- function(df_parameters){
  Out_Results <- df_parameters %>%
    mutate(ModelFile = case_when(
      ModelFile == "Large_Office_DOE_JP1_Sizing_File.imf" ~ "Large",
      ModelFile == "Medium_Office_DOE_JP1_Sizing_File.imf" ~ "Medium",
      ModelFile == "Small_Office_DOE_JP1_Sizing_File.imf" ~ "Small"))%>%
    rename(Model=ModelFile)%>%
    rename(Location=X..location..)%>%
    mutate(Case_n = as.numeric(str_extract(Job_ID,"(?<=_Case_)[:digit:]*")))%>%
    mutate(A1 = c0..A1) %>%
    mutate(A2 = c1..A2) %>%
    mutate(A3 = c2..A3) %>%
    mutate(A4 = c3..A4) %>%
    mutate(A5 = c4..A5) %>%
    mutate(A6 = c5..A6) %>%
    mutate(A7 = c6..A7) %>%
    mutate(A8 = c7..A8) %>%
    mutate(P1 = c8..P1) %>%
    mutate(P2 = c9..P2) %>%
    mutate(P3 = c10..P3) %>%
    mutate(P4 = c11..P4) %>%
    mutate(P5 = c12..P5) %>%
    mutate(P6 = c13..P6) %>%
    mutate(P7 = c14..P7) %>%
    mutate(P8 = c15..P8) %>%
    rename('P1_June'=str_subset(names(df_parameters),'P1_June'),
           'P2_June'=str_subset(names(df_parameters),'P2_June'),
           'P3_June'=str_subset(names(df_parameters),'P3_June'),
           'P4_June'=str_subset(names(df_parameters),'P4_June'),
           'P5_June'=str_subset(names(df_parameters),'P5_June'),
           'P6_June'=str_subset(names(df_parameters),'P6_June'),
           'P7_June'=str_subset(names(df_parameters),'P7_June'),
           'P8_June'=str_subset(names(df_parameters),'P8_June'),
           'P1_July'=str_subset(names(df_parameters),'P1_July'),
           'P2_July'=str_subset(names(df_parameters),'P2_July'),
           'P3_July'=str_subset(names(df_parameters),'P3_July'),
           'P4_July'=str_subset(names(df_parameters),'P4_July'),
           'P5_July'=str_subset(names(df_parameters),'P5_July'),
           'P6_July'=str_subset(names(df_parameters),'P6_July'),
           'P7_July'=str_subset(names(df_parameters),'P7_July'),
           'P8_July'=str_subset(names(df_parameters),'P8_July'),
           'P1_Aug'=str_subset(names(df_parameters),'P1_August'),
           'P2_Aug'=str_subset(names(df_parameters),'P2_August'),
           'P3_Aug'=str_subset(names(df_parameters),'P3_August'),
           'P4_Aug'=str_subset(names(df_parameters),'P4_August'),
           'P5_Aug'=str_subset(names(df_parameters),'P5_August'),
           'P6_Aug'=str_subset(names(df_parameters),'P6_August'),
           'P7_Aug'=str_subset(names(df_parameters),'P7_August'),
           'P8_Aug'=str_subset(names(df_parameters),'P8_August'))
  
  return(Out_Results)
}

Summer_Peaks <- function(Results_Matrix) {
  
  Out_Results<-Results_Matrix%>%
    mutate(P1=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P1_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P1_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P1_Aug, 
      TRUE ~ P1))%>%
    mutate(P2=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P2_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P2_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P2_Aug, 
      TRUE ~ P2))%>%
    mutate(P3=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P3_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P3_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P3_Aug, 
      TRUE ~ P3))%>%
    mutate(P4=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P4_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P4_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P4_Aug, 
      TRUE ~ P4))%>%
    mutate(P5=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P5_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P5_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P5_Aug, 
      TRUE ~ P5))%>%
    mutate(P6=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P6_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P6_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P6_Aug, 
      TRUE ~ P6))%>%
    mutate(P7=case_when(
      P8 > 0 & P1_June>P1_July& P1_June>P1_Aug ~ P7_June, 
      P8 > 0 & P1_July>P1_June& P1_July>P1_Aug ~ P7_July,
      P8 > 0 & P1_June<P1_Aug& P1_July<P1_Aug ~ P7_Aug, 
      TRUE ~ P7))%>%
    mutate(NP8=as.integer(0))%>%
    mutate(NP8=case_when(
      P8 > 1 & P1_June>P1_July& P1_June>P1_Aug ~ P8_June, 
      P8 > 1 & P1_July>P1_June& P1_July>P1_Aug ~ P8_July,
      P8 > 1 & P1_June<P1_Aug& P1_July<P1_Aug ~ P8_Aug, 
      TRUE ~ NP8))%>%
    select(-P8)%>%
    rename(P8=NP8)%>%
    select(Location,Model,Case_n,WeatherFile,P1:P8,A1:A8)
  
  return(Out_Results)
  
  
}

Results_per_Area <- function(Results_Matrix) {
  # Get Dataframe
  Large_A<-46320.38
  Medium_A<-4982.2
  Small_A<-511.16

  #Results_An[, "P1"] <- apply(Results_An[, 34:36], 1, max)         #@Check a way to do with name of columns
  Test<-Results_Matrix%>%
    mutate(PHVAC = P2+P3+P4+P5+P6+P7) %>%
    mutate(AHVAC = A2+A3+A4+A5+A6+A7) %>%
    mutate(A1 = A1-A8) %>%
    mutate(PNONHVAC = P1-PHVAC) %>%
    mutate(ANONHVAC = A1-AHVAC)
  
  Out_Results<-Test%>%
    mutate(A1= case_when(
      Model=="Large" ~ A1/Large_A,
      Model=="Medium" ~ A1/Medium_A,
      Model=="Small" ~ A1/Small_A))%>%
    mutate(A2= case_when(
      Model=="Large" ~ A2/Large_A,
      Model=="Medium" ~ A2/Medium_A,
      Model=="Small" ~ A2/Small_A))%>%
    mutate(A3= case_when(
      Model=="Large" ~ A3/Large_A,
      Model=="Medium" ~ A3/Medium_A,
      Model=="Small" ~ A3/Small_A))%>%
    mutate(A4= case_when(
      Model=="Large" ~ A4/Large_A,
      Model=="Medium" ~ A4/Medium_A,
      Model=="Small" ~ A4/Small_A))%>%
    mutate(A5= case_when(
      Model=="Large" ~ A5/Large_A,
      Model=="Medium" ~ A5/Medium_A,
      Model=="Small" ~ A5/Small_A))%>%
    mutate(A6= case_when(
      Model=="Large" ~ A6/Large_A,
      Model=="Medium" ~ A6/Medium_A,
      Model=="Small" ~ A6/Small_A))%>%
    mutate(A7= case_when(
      Model=="Large" ~ A7/Large_A,
      Model=="Medium" ~ A7/Medium_A,
      Model=="Small" ~ A7/Small_A))%>%
    mutate(A8= case_when(
      Model=="Large" ~ A8/Large_A,
      Model=="Medium" ~ A8/Medium_A,
      Model=="Small" ~ A8/Small_A))%>%
    mutate(P1= case_when(
      Model=="Large" ~ P1/Large_A,
      Model=="Medium" ~ P1/Medium_A,
      Model=="Small" ~ P1/Small_A))%>%
    mutate(P2= case_when(
      Model=="Large" ~ P2/Large_A,
      Model=="Medium" ~ P2/Medium_A,
      Model=="Small" ~ P2/Small_A))%>%
    mutate(P3= case_when(
      Model=="Large" ~ P3/Large_A,
      Model=="Medium" ~ P3/Medium_A,
      Model=="Small" ~ P3/Small_A))%>%
    mutate(P4= case_when(
      Model=="Large" ~ P4/Large_A,
      Model=="Medium" ~ P4/Medium_A,
      Model=="Small" ~ P4/Small_A))%>%
    mutate(P5= case_when(
      Model=="Large" ~ P5/Large_A,
      Model=="Medium" ~ P5/Medium_A,
      Model=="Small" ~ P5/Small_A))%>%
    mutate(P6= case_when(
      Model=="Large" ~ P6/Large_A,
      Model=="Medium" ~ P6/Medium_A,
      Model=="Small" ~ P6/Small_A))%>%
    mutate(P7= case_when(
      Model=="Large" ~ P7/Large_A,
      Model=="Medium" ~ P7/Medium_A,
      Model=="Small" ~ P7/Small_A))%>%
    mutate(P8= case_when(
      Model=="Large" ~ P8/Large_A,
      Model=="Medium" ~ P8/Medium_A,
      Model=="Small" ~ P8/Small_A))%>%
    mutate(PHVAC= case_when(
      Model=="Large" ~ PHVAC/Large_A,
      Model=="Medium" ~ PHVAC/Medium_A,
      Model=="Small" ~ PHVAC/Small_A))%>%
    mutate(PNONHVAC= case_when(
      Model=="Large" ~ PNONHVAC/Large_A,
      Model=="Medium" ~ PNONHVAC/Medium_A,
      Model=="Small" ~ PNONHVAC/Small_A))%>%
    mutate(AHVAC= case_when(
      Model=="Large" ~ AHVAC/Large_A,
      Model=="Medium" ~ AHVAC/Medium_A,
      Model=="Small" ~ AHVAC/Small_A))%>%
    mutate(ANONHVAC= case_when(
      Model=="Large" ~ ANONHVAC/Large_A,
      Model=="Medium" ~ ANONHVAC/Medium_A,
      Model=="Small" ~ ANONHVAC/Small_A))
  #mutate(P10= P1-(P2+P3+P4+P5+P6+P7+P8))
  
  return(Out_Results)
}
  
Normalisation_Function <- function(Results_Matrix, Base) {
  
  Short_Results <- rbind(Results_Matrix, Base)
  print(Short_Results)
  
  Out_Results <- Short_Results %>%
    group_by(Location, Model) %>%
    mutate(Case_n = replace_na(Case_n,"Unk")) %>%
    mutate(PTOT = P1) %>%
    mutate(ATOT = A1) %>%
    mutate(R_P_TOT = (PTOT/PTOT[Case_n == 'Base'] - 1) ) %>%
    mutate(R_P_HVAC = (PHVAC/PHVAC[Case_n == 'Base'] - 1) ) %>%
    mutate(R_P_NONHVAC = (PNONHVAC/PNONHVAC[Case_n == 'Base'] - 1) ) %>%
    mutate(R_A_TOT = (ATOT/ATOT[Case_n == 'Base'] - 1) ) %>%
    mutate(R_A_HVAC = (AHVAC/AHVAC[Case_n == 'Base'] - 1) ) %>%
    mutate(R_A_NONHVAC = (ANONHVAC/ANONHVAC[Case_n == 'Base'] - 1) ) %>%
    ungroup() %>%
    filter(is.na(Case_n) | Case_n != 'Base') %>%
    select(Location:WeatherFile,PTOT,ATOT,PHVAC,AHVAC,R_P_TOT:R_A_NONHVAC)
  
  return(Out_Results)
}  

Generating_Single_Norm_Charts <- function(Res_Matrix, Var_Anal){
  Results_Ordered <- Res_Matrix %>%
    arrange(Location, Model, get(Var_Anal))
  
  Plot_df <- data.frame(Order_Case = rep(0:200,18),Results_Ordered)
  
  out_plot <- ggplot(Plot_df,aes(x=Order_Case,y = get(Var_Anal), group = Model, col = Model))+
    geom_point() +
    facet_grid( ~ Location) +
    ggtitle(Var_Anal) +
    scale_y_continuous(labels=scales::percent) +
    ylab('Ration [%]') +
    theme(legend.position = "none",
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10))
return(out_plot)
}

Generating_Single_Norm_lm_Charts <- function(Res_Matrix, Var_Anal){
  Results_Ordered <- Res_Matrix %>%
    arrange(Location, Model, get(Var_Anal))
  
  Plot_df <- data.frame(Order_Case = rep(0:200,18),Results_Ordered)
  
  my.formula <- get(Var_Anal) ~ Order_Case
    
  out_plot <- ggplot(Plot_df,aes(x=Order_Case,y = get(Var_Anal) ))+
    geom_point() +
    facet_grid(Model ~ Location) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle(Var_Anal) +
    ylab('Ration [%]') +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10))
  return(out_plot)
}

Generating_Norm_Charts <- function(Res_Matrix,Measure_Text) {
  
  p1 <- Generating_Single_Norm_Charts(Res_Matrix, 'R_P_TOT') + ggtitle('Total Peak Demand')
  
  file<-paste('04_Charts/04_Random/',Measure_Text,'_Peak_Total.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm",p1)
  
  p2 <- Generating_Single_Norm_Charts(Res_Matrix, 'R_A_TOT') + ggtitle('Total Annual Demand')
  
  file<-paste('04_Charts/04_Random/',Measure_Text,'_Annual_Total.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm",p2)
  
  HVAC_Peak_Ordered <- Res_Matrix %>%
    arrange(Location,Model,R_P_HVAC)
  
  Plot_df <- data.frame(Order_Case=rep(0:200,18),HVAC_Peak_Ordered)
  
  p3 <- Generating_Single_Norm_Charts(Res_Matrix, 'R_P_HVAC') + ggtitle('HVAC Peak Demand')
    
  file<-paste('04_Charts/04_Random/',Measure_Text,'_Peak_HVAC.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm",p3)
  
  HVAC_AN_Ordered<-Res_Matrix %>%
    arrange(Location,Model,R_A_HVAC)
  
  Plot_df <- data.frame(Order_Case=rep(0:200,18),HVAC_AN_Ordered)
  
  p4 <- Generating_Single_Norm_Charts(Res_Matrix, 'R_A_HVAC') + ggtitle('HVAC Annual Demand')
  
  file<-paste('04_Charts/04_Random/',Measure_Text,'_Annual_HVAC.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm",p4)
  
  Chart_Global <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  annotate_figure(Chart_Global , top = textGrob(Measure_Text,gp=gpar(fontsize=20,font=3)))
  
  file<-paste('04_Charts/04_Random/', Measure_Text, '_All_Plots.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm")
  
}

Generating_Plots_Measures <- function(Res_Matrix,Var_Name){
  
  Ordered <- Res_Matrix %>%
    arrange(Location,Model,Sample,!!sym(Var_Name))
  
  
  
  Plot_df <- data.frame(Order_Case=rep(0:200,nrow(Res_Matrix)/201),Ordered)            ##@cHANGE TO UPLOAD NUMBER OF ROWS
  
  Out_Plot <- ggplot(Plot_df,aes(x=Order_Case,y=!!sym(Var_Name),group=Sample,col=Sample))+
    geom_point()+
    facet_grid( Model ~ Location) +
    ggtitle(Var_Name) +
    xlab('Pathway sample')+
    ylab('Ratio to the base case model for present day conditions') +
    scale_y_continuous(labels=scales::percent_format(accuracy = 5L))
  return(Out_Plot)
}

Statistica_Anal <- function(df_Short, Data_Var, sta_fun){
  
  Out <- df_Short %>%
    group_by(Location,Model) %>%
    select(Data_Var) %>%
    summarise_all(sta_fun) %>%
    spread(Location, Data_Var) %>%
    mutate(Stat = sta_fun) %>%
    select(Stat, Model, C1:C6)
  return(Out) 
}

Stat_Matrix_All <- function(df, Out_Var){
  
  df_out <- rbind(
    Statistica_Anal(df, Out_Var, 'max'),
    Statistica_Anal(df, Out_Var, 'median'),
    Statistica_Anal(df, Out_Var, 'mean'),
    Statistica_Anal(df, Out_Var, 'min'),
    Statistica_Anal(df, Out_Var, 'IQR'),
    Statistica_Anal(df, Out_Var, 'sd')
  )
  
  return(df_out)
}

Adaptation_Measure_Upload <- function(in_file, N_Measure = NULL){
  
  Table_Par <- read.csv(in_file, header = TRUE, sep = ",")
  Table_Par <- data.frame(Table_Par)
  #print('1')
  Original_Data <- Upload_Results(Table_Par)
  #print('2')
  
  if(is.null(N_Measure)){
    Original_Data_M <- Original_Data
    #print('Message to confirm no varaible')
  } else {
  Original_Data_M <- Original_Data %>% filter(str_detect(Job_ID, N_Measure))
  #print('Message to confirm Measure var')
  }
  
  Matrix_Peaks <- Summer_Peaks(Original_Data_M)
  Area_Cor_Mat <- Results_per_Area(Matrix_Peaks)
  
  #print(Area_Cor_Mat)
  
  #Normalised_Mat_M <- Normalisation_Function(Area_Cor_Mat, Base)
  return(Area_Cor_Mat)  
}

Generate_Base <- function(in_file){

  Table_Par <- read.csv(in_file, header = TRUE, sep = ",")
  Table_Par <- data.frame(Table_Par)
  Original_Data <- Upload_Results(Table_Par)
  
  Matrix_Peaks <- Summer_Peaks(Original_Data)
  Area_Cor_Mat <- Results_per_Area(Matrix_Peaks)

  Base <- Area_Cor_Mat %>% 
    filter(str_detect(WeatherFile,'EGY_Cairo.epw') | 
             str_detect(WeatherFile,'SGP_Singapore.epw') |
             str_detect(WeatherFile,'PRT_Lisbon.epw') |
             str_detect(WeatherFile,'GBR_London.epw') |
             str_detect(WeatherFile,'GRC_Athens.epw') |
             str_detect(WeatherFile,'CHN_Beijing.epw')          ) %>%
    mutate(Case_n = 'Base')
  
  
  return(Base)  
}

Order_All_WFiles <- function(Mat_Anal, Var_Anal){
  Base_WFiles <- c("CHN_Beijing.epw",
                   "EGY_Cairo.epw",
                   "GBR_London.epw",
                   "GRC_Athens.epw",
                   "PRT_Lisbon.epw",
                   "SGP_Singapore.epw")
  
  
  
  Mat_Out <- Mat_Anal  %>% 
    arrange(Location, Model, get(Var_Anal)) %>%
    #fill(Order_Case, .direction = "down") %>%
    #fill(Order_Case, .direction = "down") %>%
    mutate(WeatherFile = case_when(
      Case_n == "Unk" & str_detect(WeatherFile , paste(Base_WFiles, collapse = "|")) ~ "Base",
      str_detect(WeatherFile , 'Case_000_AN_T-') ~ "Extreme",
      str_detect(WeatherFile , 'Present') ~ "Present",
      str_detect(WeatherFile , 'HadCM3') ~ "CCWG",
      str_detect(WeatherFile , 'hour') ~ "Meteonorm",
      str_detect(WeatherFile,"(?<=rcp).*(?=.epw)") ~ 'WeatherShift',
      str_detect(WeatherFile , '.epw') ~ "Climate pathway",
      TRUE ~ WeatherFile))
  
  return(Mat_Out)
}

Plot_Order_WFiles <- function(Mat_Anal, Var_Anal){
  Res_Test <- Order_All_WFiles(Mat_Anal,Var_Anal) %>%
    group_by(Model, Location) %>%
    filter(!str_detect(WeatherFile , 'Present'),
           WeatherFile != "Base") %>%
    mutate(Order_Case = row_number())
  
  plot_out <- Res_Test %>%
    mutate(WeatherFile = factor(WeatherFile, levels = c("Climate pathway", "WeatherShift","Meteonorm","CCWG", "Extreme"))) %>%
    ggplot(aes(x=Order_Case,y=get(Var_Anal), group = WeatherFile)) +
    geom_point(aes(shape = WeatherFile, col = WeatherFile, size = WeatherFile)) + 
    scale_y_continuous(labels=scales::percent) +
    labs(group = "Weather dataset",col = "Weather dataset", size = "Weather dataset", shape = "Weather dataset",x = 'Weather data iteration in the climate pathway ordered by demand', y = 'Change relative to baseline') +
    facet_grid( Model ~ Location) +
    ggtitle(Var_Anal) +
    scale_shape_manual(values=c(19, 19, 19, 19, 4))+
    scale_size_manual(values=c(0.5,1.5,2.5,5,5))
  
  return(plot_out)
}

#Evaluate Effectiveness of Measures

Analys_Measures <- function(All_Samples,Var_Anal){
  Perc_List <- All_Samples %>%
    group_by(Sample,Location, Model) %>%
    summarise(Perc = mean(get(Var_Anal) > 0) *100) %>%
    ungroup() %>%
    spread(Location,Perc) %>%
    arrange(Model)
  
  Max_List <- All_Samples %>%
    group_by(Sample,Location, Model) %>%
    summarise(Max = max(get(Var_Anal))) %>%
    ungroup() %>%
    spread(Location,Max) %>%
    arrange(Model)
  
  Mean_List <- All_Samples %>%
    group_by(Sample,Location, Model) %>%
    summarise(Mean = mean(get(Var_Anal))) %>%
    ungroup() %>%
    spread(Location,Mean) %>%
    arrange(Model)
  
  Out_List <- list(Perc = Perc_List, Max = Max_List, Mean = Mean_List)
  return(Out_List)
  
}

Save_xlsx_Measure <- function(All_Samples,Var_Ana){
  
  file <- paste('04_Charts/05_Adap_Measures/',Var_Ana,'.xlsx',sep='')
  write.xlsx(Analys_Measures(All_Samples, Var_Ana)$Perc, 
             file, 
             sheetName = "Percentage", 
             col.names = TRUE, row.names = TRUE, append = FALSE)
  
  write.xlsx(Analys_Measures(All_Samples, Var_Ana)$Max, 
             file, 
             sheetName = "Max", 
             col.names = TRUE, row.names = TRUE, append = TRUE)
  
  write.xlsx(Analys_Measures(All_Samples, Var_Ana)$Mean, 
             file, 
             sheetName = 'Mean', 
             col.names = TRUE, row.names = TRUE, append = TRUE)
  
}

#Plots adaptation

Plot_Adapt_Normalised <- function(Mat_Anal_Samples,Sample_Name){
  Test <- Mat_Anal_Samples %>% 
    group_by(Location,Model,Case_n, ) %>% 
    mutate(Dif_R_P_TOT = case_when(
      Sample == 'Base' ~ R_P_TOT,
      TRUE ~ R_P_TOT[Sample == 'Base'] - R_P_TOT)
    ) %>% 
    mutate(Dif_R_P_HVAC = case_when(
      Sample == 'Base' ~ R_P_HVAC,
      TRUE ~ R_P_HVAC[Sample == 'Base'] - R_P_HVAC)
    ) %>%
    mutate(Dif_R_A_TOT = case_when(
      Sample == 'Base' ~ R_A_TOT,
      TRUE ~ R_A_TOT[Sample == 'Base'] - R_A_TOT)
    ) %>% 
    mutate(Dif_R_A_HVAC = case_when(
      Sample == 'Base' ~ R_A_HVAC,
      TRUE ~ R_A_HVAC[Sample == 'Base'] - R_A_HVAC)
    ) %>%
    ungroup()
  
  Results_Ordered <- Test %>%
    arrange(Model,Location,Sample,R_P_TOT) 
  
  Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)
  
  ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_P_TOT, col = Sample))+
    geom_point() +
    facet_grid(Model ~ Location) +
    ylab('Ration [%]') +
    ggtitle('Peak Total')
  
  file<-paste('04_Charts/05_Adap_Measures/','Diff_Total_Peak_',Sample_Name,'.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm")
  
  # Res_Test <- Results_Ordered %>% group_by(Location, Sample) %>%
  #   summarise(Base = mean(Dif_R_P_TOT[Case_n == 0]),
  #             Mean = mean(Dif_R_P_TOT),
  #             Max = max(Dif_R_P_TOT),
  #             Delta_Max = ((Max/Base) - 1) * 100,
  #             Delta_Mean = (Mean/Base -1) * 100)
  # 
  # Res_Test2 <- Results_Ordered %>% group_by(Model, Sample) %>%
  #   summarise(Base = mean(Dif_R_P_TOT[Case_n == 0]),
  #             Mean = mean(Dif_R_P_TOT),
  #             Max = max(Dif_R_P_TOT),
  #             Delta_Max = ((Max/Base) - 1) * 100,
  #             Delta_Mean = (Mean/Base -1) * 100)
  # 
  # Res_Test <- Results_Ordered %>% group_by(Location, Sample) %>%
  #   summarise(Base = mean(Dif_R_A_TOT[Case_n == 0]),
  #             Mean = mean(Dif_R_A_TOT),
  #             Max = max(Dif_R_A_TOT),
  #             Delta_Max = ((Max/Base) - 1) * 100,
  #             Delta_Mean = (Mean/Base -1) * 100)
  # 
  # Res_Test2 <- Results_Ordered %>% group_by(Model, Sample) %>%
  #   summarise(Base = mean(Dif_R_A_TOT[Case_n == 0]),
  #             Mean = mean(Dif_R_A_TOT),
  #             Max = max(Dif_R_A_TOT),
  #             Delta_Max = ((Max/Base) - 1) * 100,
  #             Delta_Mean = (Mean/Base -1) * 100)
  # 
  # Res_Test3 <- Results_Ordered %>% group_by(Sample) %>%
  #   summarise(Base = mean(Dif_R_A_TOT[Case_n == 0]),
  #             Mean = mean(Dif_R_A_TOT),
  #             Max = max(Dif_R_A_TOT),
  #             Delta_Max = ((Max/Base) - 1) * 100,
  #             Delta_Mean = (Mean/Base -1) * 100)
  
  
  
  Results_Ordered <- Test %>%
    arrange(Model,Location,Sample,R_A_TOT) 
  
  Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)
  
  ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_A_TOT, col = Sample))+
    geom_point() +
    facet_grid(Model ~ Location) +
    ylab('Ration [%]') +
    ggtitle('Annual Total')
  
  file<-paste('04_Charts/05_Adap_Measures/','Diff_Total_Annual_',Sample_Name,'.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm")
  
  
  Results_Ordered <- Test %>%
    arrange(Model,Location,Sample,R_A_HVAC) 
  
  Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)
  
  ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_A_HVAC, col = Sample))+
    geom_point() +
    facet_grid(Model ~ Location) +
    ylab('Ration [%]') +
    ggtitle('Annual HVAC')
  
  file<-paste('04_Charts/05_Adap_Measures/','Diff_HVAC_Annual_',Sample_Name,'.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm")
  
  
  Results_Ordered <- Test %>%
    arrange(Model,Location,Sample,R_P_HVAC) 
  
  Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)
  
  
  ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_P_HVAC, col = Sample))+
    geom_point() +
    facet_grid(Model ~ Location) +
    ylab('Ratio [%]') +
    ggtitle('Peak HVAC')
  
  file<-paste('04_Charts/05_Adap_Measures/','Diff_HVAC_Peak_', Sample_Name,'.png',sep='')
  ggsave(file,width = 30, height = 15, units = "cm")
  
}

Function_Adapt_Normalise <- function(Mat_Anal_Samples, Var_Anal, Base_Name){
  Test <- Mat_Anal_Samples %>% 
    group_by(Location,Model,Case_n, ) %>% 
    mutate(Dif_R_P_TOT = case_when(
      Sample == Base_Name ~ R_P_TOT,
      TRUE ~ R_P_TOT[Sample == Base_Name] - R_P_TOT)
    ) %>% 
    mutate(Dif_R_P_HVAC = case_when(
      Sample == Base_Name ~ R_P_HVAC,
      TRUE ~ R_P_HVAC[Sample == Base_Name] - R_P_HVAC)
    ) %>%
    mutate(Dif_R_A_TOT = case_when(
      Sample == Base_Name ~ R_A_TOT,
      TRUE ~ R_A_TOT[Sample == Base_Name] - R_A_TOT)
    ) %>% 
    mutate(Dif_R_A_HVAC = case_when(
      Sample == Base_Name ~ R_A_HVAC,
      TRUE ~ R_A_HVAC[Sample == Base_Name] - R_A_HVAC)
    ) %>%
    ungroup()
  
  Results_Ordered <- Test %>%
    arrange(Model,Location,Sample,get(Var_Anal))
  
  return(Results_Ordered)
}

Function_Order_Ch <- function(Mat_Anal,Var_Anal){
  Res_Test <- Mat_Anal %>% group_by(Location,Model, Sample) %>%
    summarise(Base = mean(get(Var_Anal)[Case_n == 0]),
              Mean = mean(get(Var_Anal)),
              Max = max(get(Var_Anal)),
              Delta_Max = Max-Base,
              Delta_Mean = Mean-Base,
              Ratio_Delta_Max = ((Max/Base) - 1) * 100,
              Ratio_Delta_Mean = (Mean/Base -1) * 100)
  
  return(Res_Test)
}

Generate_Plot_Pathway_w_WGEN <- function(All_Files_In , Var_Anal , Model_In, Loc_In) {
  
  Res_Test <- Order_All_WFiles(All_Files_In,Var_Anal) %>%
    group_by(Model, Location) %>%
    filter(!str_detect(WeatherFile , 'Present'),
           WeatherFile != "Base") %>%
    mutate(Order_Case = row_number())
  
  Plot_Fig_A <- Res_Test %>%
    filter(Model == Model_In) %>%
    mutate(WeatherFile = factor(WeatherFile, levels = c("Climate pathway", "WeatherShift","Meteonorm","CCWG", "Extreme"))) %>%
    ggplot(aes(x = Order_Case,y = get(Var_Anal), group = WeatherFile)) +
    geom_point(aes(shape = WeatherFile, col = WeatherFile, size = WeatherFile)) + 
    facet_grid( Model ~ Location) +
    scale_shape_manual(values=c(19, 19, 19, 19, 4))+
    scale_size_manual(values=c(0.5,1.5,2.5,5,5)) +
    scale_y_continuous(labels=scales::percent) +
    labs(group = "Weather dataset",col = "Weather dataset", size = "Weather dataset", shape = "Weather dataset") +
    theme(legend.position="top", axis.title.y = element_blank() ,axis.title.x = element_blank())
  
  Plot_Fig_B <- Res_Test %>%
    filter(Model == Model_In, Location == Loc_In) %>%
    mutate(WeatherFile = factor(WeatherFile, levels = c("Climate pathway", "WeatherShift","Meteonorm","CCWG", "Extreme"))) %>%
    ggplot(aes(x = Order_Case,y = get(Var_Anal), group = WeatherFile)) +
    geom_point(aes(shape = WeatherFile, col = WeatherFile, size = WeatherFile)) + 
    facet_grid( Model ~ Location) +
    scale_shape_manual(values=c(19, 19, 19, 19, 4))+
    scale_size_manual(values=c(0.5,1.5,2.5,5,5)) +
    scale_y_continuous(labels=scales::percent) +
    labs(group = "Weather dataset") +
    xlab('Weather data iteration in the climate pathway ordered by demand') +
    theme(axis.title.y = element_blank(), legend.position = "none")
  
  Ex_Figure <- ggarrange(Plot_Fig_A, Plot_Fig_B,
                         labels = c("a) ", "b) "),
                         ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(Plot_Fig_A)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(color = guide_legend(nrow = 1))
  
  Ex_Figure <- annotate_figure(Ex_Figure , left = textGrob('Value to baseline', rot = 90))
  
  return(Ex_Figure)
}

Gen_Plot_WShift_Pathway <- function(Loc_Var, Model_Var){
  
  Plot_df <- rbind(df0,  Plot_Dataset) %>% filter(Location == Loc_Var, Model == Model_Var, Timeline == '2090') %>%
    mutate(Scenario = replace(Scenario, Scenario == '4.5', 'RCP 4.5 2090')) %>%
    mutate(Scenario = replace(Scenario, Scenario == '8.5', 'RCP 8.5 2090')) %>%
    mutate(Scenario = replace(Scenario, Scenario == 'Random', 'Climate pathway'))
  
  
  h_inter <- Normalised_Mat_Random %>%
    arrange(Location, Model, get('R_A_TOT')) %>%
    group_by(Location, Model) %>%
    slice(100) %>%
    filter(Model == Model_Var, Location == Loc_Var)%>%
    pull(R_A_TOT)
  
  v_inter <- Normalised_Mat_Random %>%
    arrange(Location, Model, get('R_P_TOT')) %>%
    group_by(Location, Model) %>%
    slice(100) %>%
    filter(Model == Model_Var, Location == Loc_Var) %>%
    pull(R_P_TOT)
  
  Plot_Out <- ggplot(Plot_df, aes(x=R_P_TOT,y= R_A_TOT, group = Scenario))+
    geom_point(aes(color = Scenario, size = Scenario, shape = Scenario)) +
    scale_size_manual(values = c(1,3, 3)) + 
    scale_shape_manual(values = c(20,15, 15)) + 
    geom_vline(xintercept = v_inter, linetype="dotted", 
               color = "blue", size=1.5, show.legend = TRUE)+
    geom_hline(yintercept = h_inter, linetype="dotted", 
               color = "red", size=1.5,show.legend = TRUE)+
    geom_text(size=3,aes(x = v_inter,
                         y = 0,
                         label = '50% pathway peak',
                         hjust = -0.1)) +
    geom_text(size=3,aes(x = 0,
                         y = h_inter,
                         label = '50% pathway annual',
                         vjust = -1,
                         hjust = +0.1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 5L)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
    ylab('Change relative to annual total electricity demand for baseline') +
    xlab('Change relative to peak total electricity demand for baseline') +
    labs(col= 'Weather data', size = 'Weather data', shape = 'Weather data') +
    ggtitle(paste(Loc_Var, ' ', Model_Var, ' office')) +
    theme_grey(base_size = 8) + 
    theme(legend.position="bottom")+ guides(colour = guide_legend(nrow = 1)) 
  
  return(Plot_Out)
}

################# End functions #############

setwd("C:/Users/mbgnwvz2/Dropbox (The University of Manchester)/01_PhD Research/14_Repository/06-Pathways/06_01-Result Analysis")

############## Uploading Results ###################

Base_Mat <- Generate_Base("01_Inputs/02_Simulation_Results/AllCombinedResults_Random_Sample_V1.csv")

Mat_Random <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_Random_Sample_V1.csv")
Mat_M1 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_1_2.csv", 'Measure_1')
Mat_M2 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_1_2.csv", 'Measure_2')
Mat_M3 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_3_4.csv", 'Measure_3')
Mat_M4 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_3_4.csv", 'Measure_4')
Mat_M5 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_5_6.csv", 'Measure_5')
Mat_M6 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_5_6.csv", 'Measure_6')
Mat_M7 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_7.csv", 'Measure_7')
Mat_M8 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_8.csv", 'Measure_8')
Mat_M9 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_M_9.csv", 'Measure_9')

Normalised_Mat_Random <- Normalisation_Function(Mat_Random, Base_Mat)
Normalised_Mat_M1 <- Normalisation_Function(Mat_M1, Base_Mat)
Normalised_Mat_M2 <- Normalisation_Function(Mat_M2, Base_Mat)
Normalised_Mat_M3 <- Normalisation_Function(Mat_M3, Base_Mat)
Normalised_Mat_M4 <- Normalisation_Function(Mat_M4, Base_Mat)
Normalised_Mat_M5 <- Normalisation_Function(Mat_M5, Base_Mat)
Normalised_Mat_M6 <- Normalisation_Function(Mat_M6, Base_Mat)
Normalised_Mat_M7 <- Normalisation_Function(Mat_M7, Base_Mat)
Normalised_Mat_M8 <- Normalisation_Function(Mat_M8, Base_Mat)
Normalised_Mat_M9 <- Normalisation_Function(Mat_M9, Base_Mat)

#### Other Projection files data 

Normalised_Other_WFiles <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_OtherWFiles.csv")
Normalised_Other_WFiles2 <- Adaptation_Measure_Upload("01_Inputs/02_Simulation_Results/AllCombinedResults_OtherWFiles_3.csv") %>%
  drop_na(P1)
Normalised_Other_WFiles2 <- Normalisation_Function(Normalised_Other_WFiles2, Base_Mat)

#### All results (Pathway + Other projections)

All_Weather_Files <- rbind(Normalised_Mat_Random, Normalised_Other_WFiles2)

@Is it possible to put the sample already

Normalised_Mat <- Normalised_Mat_Random %>% mutate(Sample = 'No adaptation')
Normalised_Mat_M1 <- Normalised_Mat_M1 %>% mutate(Sample = 'M1')
Normalised_Mat_M2 <- Normalised_Mat_M2 %>% mutate(Sample = 'M2')
Normalised_Mat_M3 <- Normalised_Mat_M3 %>% mutate(Sample = 'M3')
Normalised_Mat_M4 <- Normalised_Mat_M4 %>% mutate(Sample = 'M4')
Normalised_Mat_M5 <- Normalised_Mat_M5 %>% mutate(Sample = 'M5')
Normalised_Mat_M6 <- Normalised_Mat_M6 %>% mutate(Sample = 'M6')
Normalised_Mat_M7 <- Normalised_Mat_M7 %>% mutate(Sample = 'M7')
Normalised_Mat_M8 <- Normalised_Mat_M8 %>% mutate(Sample = 'M8')
Normalised_Mat_M9 <- Normalised_Mat_M9 %>% mutate(Sample = 'M9')

All_Samples_Measures <- rbind(Normalised_Mat,
                              Normalised_Mat_M1,
                              Normalised_Mat_M2,
                              Normalised_Mat_M3,
                              Normalised_Mat_M4,
                              Normalised_Mat_M5,
                              Normalised_Mat_M6,
                              Normalised_Mat_M7,
                              Normalised_Mat_M8,
                              Normalised_Mat_M9)

All_Samples_Single_Measures <- rbind(Normalised_Mat,
                                     Normalised_Mat_M1,
                                     Normalised_Mat_M2,
                                     Normalised_Mat_M3,
                                     Normalised_Mat_M4,
                                     Normalised_Mat_M5,
                                     Normalised_Mat_M6)

All_Samples_Comb_Measures <- rbind(Normalised_Mat,
                                   Normalised_Mat_M7,
                                   Normalised_Mat_M8,
                                   Normalised_Mat_M9)



# All_Samples_Measures <- All_Samples_Measures %>% mutate(Sample = rep(c('Base','M1','M2','M3','M4','M5','M6','M7','M8','M9'),each=201*3*6))
# All_Samples_Single_Measures <- All_Samples_Single_Measures %>% mutate(Sample = rep(c('Base','M1','M2','M3','M4','M5','M6'),each=201*3*6))
# All_Samples_Comb_Measures <- All_Samples_Comb_Measures %>% mutate(Sample = rep(c('Base','M7','M8','M9'),each=201*3*6))

####### Other Weather Analysis @ Plot ######
Base_WFiles <- c("CHN_Beijing.epw",
                 "EGY_Cairo.epw",
                 "GBR_London.epw",
                 "GRC_Athens.epw",
                 "PRT_Lisbon.epw",
                 "SGP_Singapore.epw")

OtherWFiles_Dataset <- Normalised_Other_WFiles2 %>% 
  mutate(WeatherSource = case_when(
    Case_n == "Unk" & str_detect(WeatherFile , paste(Base_WFiles, collapse = "|")) ~ "Base",
    str_detect(WeatherFile , 'Case_000_AN_T-') ~ "Extreme",
    str_detect(WeatherFile , 'Present') ~ "Present",
    str_detect(WeatherFile , 'HadCM3') ~ "CCWG",
    str_detect(WeatherFile , 'hour') ~ "Meteonorm",
    str_detect(WeatherFile,"(?<=rcp).*(?=.epw)") ~ 'WS_RCP_scenarios',
    TRUE ~ WeatherFile)) %>%
  arrange(Model,Location,R_A_TOT) %>%
  drop_na(R_P_TOT)

Normalised_RandomANDOtherWFiles <- rbind(
  Normalised_Mat_Random %>% mutate(WeatherSource = "Base_Random"),
  OtherWFiles_Dataset)


Results_Ordered <- Normalised_Mat_Random %>%
  arrange(Location, Model, R_P_TOT)


#Data for next figures

All_Files <- rbind(Normalised_Mat_Random, Normalised_Other_WFiles2)

Nam_Base_Scenario = 'No adaptation'   #define the scenario name for the base

RD_PTOT <- All_Samples_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PTOT[Sample == 'M1']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD2 = (PTOT[Sample == 'M2']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD3 = (PTOT[Sample == 'M3']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD4 = (PTOT[Sample == 'M4']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD5 = (PTOT[Sample == 'M5']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD6 = (PTOT[Sample == 'M6']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD7 = (PTOT[Sample == 'M7']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD8 = (PTOT[Sample == 'M8']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD9 = (PTOT[Sample == 'M9']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-1) %>%
  select(RD1:RD9) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(mean) %>%
  gather(key = Measure, value=value,RD1:RD9)

RD_ATOT <- All_Samples_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (ATOT[Sample == 'M1']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD2 = (ATOT[Sample == 'M2']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD3 = (ATOT[Sample == 'M3']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD4 = (ATOT[Sample == 'M4']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD5 = (ATOT[Sample == 'M5']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD6 = (ATOT[Sample == 'M6']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD7 = (ATOT[Sample == 'M7']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD8 = (ATOT[Sample == 'M8']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD9 = (ATOT[Sample == 'M9']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-1) %>%
  select(RD1:RD9) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(mean) %>%
  gather(key = Measure, value=value,RD1:RD9)

RD_AHVAC <- All_Samples_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (AHVAC[Sample == 'M1']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD2 = (AHVAC[Sample == 'M2']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD3 = (AHVAC[Sample == 'M3']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD4 = (AHVAC[Sample == 'M4']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD5 = (AHVAC[Sample == 'M5']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD6 = (AHVAC[Sample == 'M6']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD7 = (AHVAC[Sample == 'M7']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD8 = (AHVAC[Sample == 'M8']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD9 = (AHVAC[Sample == 'M9']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  select(RD1:RD9) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(mean) %>%
  gather(key = Measure, value=value,RD1:RD9)

RD_PHVAC <- All_Samples_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PHVAC[Sample == 'M1']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD2 = (PHVAC[Sample == 'M2']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD3 = (PHVAC[Sample == 'M3']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD4 = (PHVAC[Sample == 'M4']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD5 = (PHVAC[Sample == 'M5']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD6 = (PHVAC[Sample == 'M6']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD7 = (PHVAC[Sample == 'M7']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD8 = (PHVAC[Sample == 'M8']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  mutate(RD9 = (PHVAC[Sample == 'M9']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-1) %>%
  select(R_P_HVAC,RD1:RD9) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(mean) %>%
  gather(key = Measure, value=value,R_P_HVAC:RD9)

Res_Test <- All_Samples_Measures %>% group_by(Location,Model) %>%
  summarise(RD1 = (1- mean(PTOT[Sample == 'M1'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100,
            RD2 = (1- mean(PTOT[Sample == 'M2'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100,
            RD3 = (1- mean(PTOT[Sample == 'M3'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100,
            RD4 = (1- mean(PTOT[Sample == 'M4'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100,
            RD5 = (1- mean(PTOT[Sample == 'M5'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100,
            RD6 = (1- mean(PTOT[Sample == 'M6'])/mean(PTOT[Sample == Nam_Base_Scenario]))*100)


SD_PHVAC <- All_Samples_Single_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PHVAC[Sample == 'M1']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD2 = (PHVAC[Sample == 'M2']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD3 = (PHVAC[Sample == 'M3']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD4 = (PHVAC[Sample == 'M4']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD5 = (PHVAC[Sample == 'M5']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD6 = (PHVAC[Sample == 'M6']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  select(RD1:RD6) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(sd) %>%
  gather(key = Measure, value=value,RD1:RD6)

SD_AHVAC <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (AHVAC[Sample == 'M1']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD2 = (AHVAC[Sample == 'M2']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD3 = (AHVAC[Sample == 'M3']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD4 = (AHVAC[Sample == 'M4']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD5 = (AHVAC[Sample == 'M5']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD6 = (AHVAC[Sample == 'M6']-AHVAC[Sample == Nam_Base_Scenario])/AHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  select(RD1:RD6) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(sd) %>%
  gather(key = Measure, value=value,RD1:RD6)

SD_PTOT <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PTOT[Sample == 'M1']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD2 = (PTOT[Sample == 'M2']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD3 = (PTOT[Sample == 'M3']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD4 = (PTOT[Sample == 'M4']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD5 = (PTOT[Sample == 'M5']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD6 = (PTOT[Sample == 'M6']-PTOT[Sample == Nam_Base_Scenario])/PTOT[Sample == Nam_Base_Scenario]*-100) %>%
  select(RD1:RD6) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(sd) %>%
  gather(key = Measure, value=value,RD1:RD6)

SD_ATOT <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (ATOT[Sample == 'M1']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD2 = (ATOT[Sample == 'M2']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD3 = (ATOT[Sample == 'M3']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD4 = (ATOT[Sample == 'M4']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD5 = (ATOT[Sample == 'M5']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD6 = (ATOT[Sample == 'M6']-ATOT[Sample == Nam_Base_Scenario])/ATOT[Sample == Nam_Base_Scenario]*-100) %>%
  select(RD1:RD6) %>%
  ungroup() %>%
  select(-Case_n) %>%
  group_by(Location,Model) %>% 
  summarise_all(sd) %>%
  gather(key = Measure, value=value,RD1:RD6)

Base_Test_PHVAC <- All_Samples_Measures %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PHVAC[Sample == 'M1']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD2 = (PHVAC[Sample == 'M2']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD3 = (PHVAC[Sample == 'M3']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD4 = (PHVAC[Sample == 'M4']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD5 = (PHVAC[Sample == 'M5']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD6 = (PHVAC[Sample == 'M6']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD7 = (PHVAC[Sample == 'M7']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD8 = (PHVAC[Sample == 'M8']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  mutate(RD9 = (PHVAC[Sample == 'M9']-PHVAC[Sample == Nam_Base_Scenario])/PHVAC[Sample == Nam_Base_Scenario]*-100) %>%
  ungroup() %>%
  filter(Case_n == 0) %>%
  filter(Sample != 'Base') %>%
  group_by(Location,Model) %>% 
  select(RD1:RD9) %>%
  gather(key = Measure, value=value,RD1:RD9)

Base_Test_AHVAC <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (AHVAC[Sample == 'M1']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD2 = (AHVAC[Sample == 'M2']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD3 = (AHVAC[Sample == 'M3']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD4 = (AHVAC[Sample == 'M4']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD5 = (AHVAC[Sample == 'M5']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD6 = (AHVAC[Sample == 'M6']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD7 = (AHVAC[Sample == 'M7']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD8 = (AHVAC[Sample == 'M8']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  mutate(RD9 = (AHVAC[Sample == 'M9']-AHVAC[Sample == 'Base'])/AHVAC[Sample == 'Base']*-100) %>%
  ungroup() %>%
  filter(Case_n == 0) %>%
  filter(Sample != 'Base') %>%
  group_by(Location,Model) %>% 
  select(RD1:RD9) %>%
  gather(key = Measure, value=value,RD1:RD9)

Base_Test_PTOT <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (PTOT[Sample == 'M1']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD2 = (PTOT[Sample == 'M2']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD3 = (PTOT[Sample == 'M3']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD4 = (PTOT[Sample == 'M4']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD5 = (PTOT[Sample == 'M5']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD6 = (PTOT[Sample == 'M6']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD7 = (PTOT[Sample == 'M7']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD8 = (PTOT[Sample == 'M8']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  mutate(RD9 = (PTOT[Sample == 'M9']-PTOT[Sample == 'Base'])/PTOT[Sample == 'Base']*-100) %>%
  ungroup() %>%
  filter(Case_n == 0) %>%
  filter(Sample != 'Base') %>%
  group_by(Location,Model) %>% 
  select(RD1:RD9) %>%
  gather(key = Measure, value=value,RD1:RD9)

Base_Test_ATOT <- All_Samples %>% group_by(Location,Model,Case_n) %>%
  mutate(RD1 = (ATOT[Sample == 'M1']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD2 = (ATOT[Sample == 'M2']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD3 = (ATOT[Sample == 'M3']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD4 = (ATOT[Sample == 'M4']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD5 = (ATOT[Sample == 'M5']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD6 = (ATOT[Sample == 'M6']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD7 = (ATOT[Sample == 'M7']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD8 = (ATOT[Sample == 'M8']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  mutate(RD9 = (ATOT[Sample == 'M9']-ATOT[Sample == 'Base'])/ATOT[Sample == 'Base']*-100) %>%
  ungroup() %>%
  filter(Case_n == 0) %>%
  filter(Sample != 'Base') %>%
  group_by(Location,Model) %>% 
  select(RD1:RD9) %>%
  gather(key = Measure, value=value,RD1:RD9)


# Data to create plots on pathway versus projections
df0 <- Normalised_Mat_Random %>%   #Random
  mutate(Scenario = 'Random') %>%
  mutate(Probability = NA) %>%
  mutate(Timeline = '2090') %>%
  select(Location,Model,R_P_TOT:Timeline)

Plot_Dataset <- Normalised_Other_WFiles2 %>%   #Other files
  mutate(Scenario = case_when(
    str_detect(WeatherFile , 'rcp45') ~ "4.5",
    str_detect(WeatherFile , 'rcp85') ~ "8.5")) %>%
  mutate(Probability = case_when(
    str_detect(WeatherFile , '95%') ~ "95%",
    str_detect(WeatherFile , '90%') ~ "90%",
    str_detect(WeatherFile , '75%') ~ "75%",
    str_detect(WeatherFile , '50%') ~ "50%",
    str_detect(WeatherFile , '25%') ~ "25%",
    str_detect(WeatherFile , '10%') ~ "10%")) %>%
  mutate(Timeline = case_when(
    str_detect(WeatherFile , '2026-2045') ~ "2035",
    str_detect(WeatherFile , '2056-2075') ~ "2065",
    str_detect(WeatherFile , '2080-2099') ~ "2090")) %>%
  arrange(Model,Location,R_A_TOT) %>%
  drop_na(R_P_TOT, Timeline, Scenario) %>%
  select(Location,Model,R_P_TOT:Timeline)

#### Calculate sd based on base condition ####

sd_Peak <- Normalised_Mat%>%
  group_by(Model,Location)%>%
  summarise(sd_PK=sd(PTOT))%>%
  spread(key=Location,value=sd_PK)%>%
  ungroup()%>%
  select(-Model)

Base_Peak<-Normalised_Mat%>%
  filter(Case_n==0)%>%
  select(Location,Model,PTOT)%>%
  spread(key=Location,value=PTOT)%>%
  select(-Model)

sd_AN <- Normalised_Mat%>%
  group_by(Model,Location)%>%
  summarise(sd_AN=sd(ATOT))%>%
  spread(key=Location,value=sd_AN)%>%
  ungroup()%>%
  select(-Model)

Base_AN <- Normalised_Mat%>%
  filter(Case_n==0)%>%
  select(Location,Model,ATOT)%>%
  spread(key=Location,value=ATOT)%>%
  select(-Model)



##### Saving Table of Random results ########

file <- paste('04_Charts/04_Random/','Matrix_Results.xlsx',sep='')
write.xlsx(Stat_Matrix_All(Normalised_Mat_Random,'R_P_TOT'), 
           file, 
           sheetName = "R_P_TOT", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(Stat_Matrix_All(Normalised_Mat_Random,'R_A_TOT'), 
           file, 
           sheetName = "R_A_TOT", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx(Stat_Matrix_All(Normalised_Mat_Random,'R_P_HVAC'), 
           file, 
           sheetName = 'R_P_HVAC', 
           col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx(Stat_Matrix_All(Normalised_Mat_Random,'R_A_HVAC'), 
           file, 
           sheetName = "R_A_HVAC", 
           col.names = TRUE, row.names = TRUE, append = TRUE)



# Table of Peak versus Annual information 
write.xlsx(Normalised_Mat_Random %>% 
             group_by(Model,Location) %>%
             summarise(Tot_Share = mean(R_P_TOT > R_A_TOT),
                       HVAC_Share = mean(R_P_HVAC > R_A_HVAC),
                       AVG_PvsAN_Tot = mean(R_P_TOT) - mean(R_A_TOT),
                       AVG_PvsAN_HVAC = mean(R_P_HVAC) - mean(R_A_HVAC)) %>%
             ungroup(), 
           file, 
           sheetName = "PeakvsAn", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

file <- paste('04_Charts/05_Adap_Measures/02_RD/','RD_Mean_Tables.xlsx',sep='')
write.xlsx(RD_PTOT %>% spread(Measure,value) %>% arrange(Model,Location) %>% ungroup(), 
           file, 
           sheetName = "RD_PTOT", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(RD_ATOT %>% spread(Measure,value) %>% arrange(Model,Location) %>% ungroup(), 
           file, 
           sheetName = "RD_ATOT", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx(RD_PHVAC %>% spread(Measure,value) %>% arrange(Model,Location) %>% ungroup(), 
           file, 
           sheetName = "RD_PHVAC", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx(RD_AHVAC %>% spread(Measure,value) %>% arrange(Model,Location) %>% ungroup(), 
           file, 
           sheetName = "RD_AHVAC", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

# Creating Tables For Measures
Save_xlsx_Measure(All_Samples_Measures, 'R_P_HVAC')
Save_xlsx_Measure(All_Samples_Measures, 'R_A_TOT')
Save_xlsx_Measure(All_Samples_Measures, 'R_P_TOT')
Save_xlsx_Measure(All_Samples_Measures, 'R_A_HVAC')

############## Plotting ####################

############## Generating Plots Random Samples ############ 

#@Section 6.4 - Effects of the pathway

plot_out <- Generating_Single_Norm_Charts(Normalised_Mat_Random,'R_P_TOT') + 
  ggtitle('Effects on peak total electricity demand under climate pathway') +
  labs(col = "Office type", x = 'Weather data iteration in the climate pathway ordered by demand', y = 'Change relative to baseline') +
  theme(legend.position="top")

file <- paste('04_Charts/04_Random/','Random','_Peak_Total.png',sep='')
ggsave(file, width = 18, height = 10, units = "cm", plot_out)

plot_out <- Generating_Single_Norm_Charts(Normalised_Mat_Random,'R_A_TOT') + 
  ggtitle('Effects on annual total electricity demand under climate pathway') +
  labs(col = "Office type", x = 'Weather data iteration in the climate pathway ordered by demand', y = 'Change relative to baseline') +
  theme(legend.position="top")

file <- paste('04_Charts/04_Random/','Random','_Annual_Total.png',sep='')
ggsave(file, width = 18, height = 10, units = "cm", plot_out)

plot_out <- Generating_Single_Norm_Charts(Normalised_Mat_Random,'R_A_HVAC') + 
  ggtitle('Effects on annual HVAC end-use electricity demand under climate pathway') +
  labs(col = "Office type", x = 'Weather data iteration in the climate pathway ordered by demand', y = 'Change relative to baseline') +
  theme(legend.position="top")

file <- paste('04_Charts/04_Random/','Random','_Annual_HVAC.png',sep='')
ggsave(file, width = 18, height = 10, units = "cm", plot_out)

plot_out <- Generating_Single_Norm_Charts(Normalised_Mat_Random,'R_P_HVAC') + 
  ggtitle('Effects on peak HVAC end-use electricity demand under climate pathway') +
  labs(col = "Office type", x = 'Weather data iteration in the climate pathway ordered by demand', y = 'Change relative to baseline') +
  theme(legend.position="top")

file <- paste('04_Charts/04_Random/','Random','_Peak_HVAC.png',sep='')
ggsave(file, width = 18, height = 10, units = "cm", plot_out)

#@ Pathway with other generators

Plot_Order_WFiles(All_Files ,'R_P_TOT') + 
  ggtitle('Effects on peak total electricity demand - Compared with other weather datasets') +
  theme(legend.position="top")
  
file<-paste('04_Charts/04_Random/03_W_Gen/','Total_Peak_OtherWFiles.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Plot_Order_WFiles(All_Files,'R_A_TOT') + 
  ggtitle('Effects on annual total electricity demand - Compared with other weather datasets') +
  theme(legend.position="top")

file<-paste('04_Charts/04_Random/03_W_Gen/','Total_Annual_OtherWFiles.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Plot_Order_WFiles(All_Files,'R_A_HVAC') +
  ggtitle('Effects on annual HVAC electricity demand - Compared with other weather datasets') +
  theme(legend.position="top")

file<-paste('04_Charts/04_Random/03_W_Gen/','HVAC_Annual_OtherWFiles.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Plot_Order_WFiles(All_Files,'R_P_HVAC') +
  ggtitle('Effects on peak HVAC electricity demand - Compared with other weather datasets') +
  theme(legend.position="top")
  
file<-paste('04_Charts/04_Random/03_W_Gen/','HVAC_Peak_OtherWFiles.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Ex_Figure <- Generate_Plot_Pathway_w_WGEN(All_Files , 'R_P_TOT' , 'Medium' ,'C2')
Ex_Figure <- annotate_figure(Ex_Figure,top = text_grob("Effect to peak total electricity demand compared to weather generators data", face = "bold", size = 14))

file <- paste('04_Charts/04_Random/03_W_Gen/','Example_Total_Peak_OtherWFiles_Medium_C2.png',sep='')
ggsave(file,width = 25, height = 12, units = "cm",Ex_Figure)


Ex_Figure  <- Generate_Plot_Pathway_w_WGEN(All_Files , 'R_A_TOT' , 'Medium' ,'C5')
Ex_Figure  <- annotate_figure(Ex_Figure  , 
                  top = text_grob("Effect to annual total electricity demand compared to weather generators data", face = "bold", size = 14))

file <- paste('04_Charts/04_Random/03_W_Gen/','Example_Total_Annual_OtherWFiles_Medium_C5.png',sep='')
ggsave(file,width = 25, height = 12, units = "cm",Ex_Figure)

#@Improve labelling - Change Folder that are saved

Generating_Norm_Charts(Normalised_Mat_M1,'Measure 1')
Generating_Norm_Charts(Normalised_Mat_M2,'Measure 2')
Generating_Norm_Charts(Normalised_Mat_M3,'Measure 3')
Generating_Norm_Charts(Normalised_Mat_M4,'Measure 4')
Generating_Norm_Charts(Normalised_Mat_M5,'Measure 5')
Generating_Norm_Charts(Normalised_Mat_M6,'Measure 6')
Generating_Norm_Charts(Normalised_Mat_M7,'Measure 7')
Generating_Norm_Charts(Normalised_Mat_M8,'Measure 8')
Generating_Norm_Charts(Normalised_Mat_M9,'Measure 9')


Plot_Fig <- Generating_Plots_Measures(All_Samples_Measures,'R_A_HVAC') + ggtitle('Annual HVAC Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_HVAC_Measures_All.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Measures,'R_P_HVAC') + ggtitle('Peak HVAC Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_HVAC_Measures_All.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Measures,'R_A_TOT') + ggtitle('Annual Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_Total_Measures_All.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Measures,'R_P_TOT') + ggtitle('Peak Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_Total_Measures_All.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

# Single Measures Plots

Plot_Fig <- Generating_Plots_Measures(All_Samples_Single_Measures,'R_P_TOT') + ggtitle('Peak Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_Total_Single_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Single_Measures,'R_A_HVAC') + ggtitle('Annual HVAC Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_HVAC_Single_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Single_Measures,'R_P_HVAC') + ggtitle('Peak HVAC Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_HVAC__Single_Measures_Comb.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Single_Measures,'R_A_TOT') + ggtitle('Annual Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_Total_Single_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

# Combined Measures

Plot_Fig <- Generating_Plots_Measures(All_Samples_Comb_Measures,'R_P_HVAC') + ggtitle('Peak HVAC Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_HVAC_Measures_Combined.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Comb_Measures,'R_A_TOT') + ggtitle('Annual Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_Total_Measures_Combined.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Comb_Measures,'R_A_TOT') + ggtitle('Annual Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Annual_Total_Measures_Combined.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)

Plot_Fig <- Generating_Plots_Measures(All_Samples_Comb_Measures,'R_P_TOT') + ggtitle('Peak Total Demand') + labs(col='Scenario')
file<-paste('04_Charts/05_Adap_Measures/','Peak_Total_Measures_Combined.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm",Plot_Fig)


#Example for chapter

Plot_Fig_A <- Generating_Plots_Measures(All_Samples_Comb_Measures %>% 
                                          filter(Model == 'Large') %>%
                                          mutate(Sample = replace(Sample, Sample == "Base", "No adaptation")),
                                      'R_P_TOT') + 
  theme(
    plot.title = element_blank(),
    axis.title.y = element_blank()) +
  labs(x='Pathway sample', y='test' , col = 'Scenario')

Plot_Fig_B <- Generating_Plots_Measures(All_Samples_Comb_Measures %>% 
                                          filter(Model == 'Large', Location == 'C6') %>%
                                          mutate(Sample = replace(Sample, Sample == "Base", "No adaptation")),
                                        'R_P_TOT') + 
  theme(legend.position="bottom") +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_blank()) +
  labs(x='Pathway sample', col = 'Scenario')

Ex_Figure <- ggarrange(Plot_Fig_A, Plot_Fig_B,
          labels = c("a) ", "b) "),
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(Plot_Fig_B)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = guide_legend(nrow = 1))

Ex_Figure <- annotate_figure(Ex_Figure , 
                             top = text_grob("Effect to peak total electricity demand", face = "bold", size = 14),
                             left = textGrob('value to baseline', rot = 90))

file <- paste('04_Charts/05_Adap_Measures/','Example_Chapter_Measures_Combined.png',sep='')
ggsave(file,width = 20, height = 12, units = "cm",Ex_Figure)


###### Plots for RD Mean levels ###########

PlotA <- ggplot(RD_PTOT %>% filter(!Measure %in% c('RD7','RD8','RD9')), aes(x=Location,y=value,col=Measure))+
  geom_point() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
  facet_grid(~Model) +
  ggtitle('Peak total electricity demand') +
  labs(y = 'Mean demand reduction to baseline') +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  labs(col="Measure")

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Peak_Total_Measures_RD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

PlotB <- ggplot(RD_ATOT %>% filter(!Measure %in% c('RD7','RD8','RD9')), aes(x=Location,y=value,col=Measure))+
  geom_point() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
  facet_grid(~Model) +
  ggtitle('Annual total electricity demand') +
  labs(x = 'Location')

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Annual_Total_Measures_RD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Plot_fig <- ggarrange(PlotA, PlotB, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(PlotA))
Plot_fig <- annotate_figure(Plot_fig, left = text_grob('Mean demand reduction to baseline',  rot = 90))

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Measures_RD_Total_Electricity.png',sep='')
ggsave(file,width = 25, height = 15, units = "cm")


PlotA <- ggplot(RD_AHVAC %>% filter(!Measure %in% c('RD7','RD8','RD9')), aes(x=Location,y=value,col=Measure))+
  geom_point()+
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
  facet_grid(~Model)+
  ggtitle('Annual HVAC Demand')+
  labs(y = 'Mean % demand reduction to "No adaptation" scenario') +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  labs(col="Measure")

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Annual_HVAC_Measures_RD.png',sep='')
ggsave(file,width = 25, height = 15, units = "cm")

PlotB <- ggplot(RD_PHVAC %>% filter(!Measure %in% c('RD7','RD8','RD9')), aes(x=Location,y=value,col=Measure))+
  geom_point()+
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L)) +
  facet_grid(~Model)+
  ggtitle('Peak HVAC Demand')+
  theme(axis.title.y=element_blank()) +
  labs(x = 'Location')

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Peak_HVAC_Measures_RD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Plot_fig <- ggarrange(PlotA, PlotB, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(PlotA))
Plot_fig <- annotate_figure(Plot_fig, left = text_grob('Mean demand reduction to baseline',  rot = 90))

file<-paste('04_Charts/05_Adap_Measures/02_RD/','Measures_RD_HVAC_Electricity.png',sep='')
ggsave(file,width = 25, height = 15, units = "cm", Plot_fig)


# SD Plots Adaptation #

ggplot(SD_PHVAC, aes(x=Location,y=value,col=Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Peak Total Demand')+
  ylab('Mean % Reduction to Base buidling condition')

file<-paste('04_Charts/04_Random/','Peak_HVAC_Measures_SD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

ggplot(SD_AHVAC, aes(x=Location,y=value,col=Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Annual HVAC Demand')+
  ylab('Mean % Reduction to Base buidling condition')

file<-paste('04_Charts/04_Random/','Annual_HVAC_Measures_SD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

ggplot(SD_PTOT, aes(x=Location,y=value,col=Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Peak Total Demand')+
  ylab('Mean % Reduction to Base buidling condition')

file<-paste('04_Charts/04_Random/','Peak_Totak_Measures_SD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

ggplot(SD_ATOT, aes(x=Location,y=value,col=Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Annual Total Demand')+
  ylab('Mean % Reduction to Base buidling condition')

file<-paste('04_Charts/04_Random/','Annual_Total_Measures_SD.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")


#Pathway versus Projections with 50% Percentile

PlotA <- Gen_Plot_WShift_Pathway('C6', 'Small')
PlotB <- Gen_Plot_WShift_Pathway('C5', 'Large')

Ex_Figure <- ggarrange(PlotA, PlotB,
                       labels = c("a) ", "b) "),
                       ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(PlotA))

file <- paste('04_Charts/04_Random/','Ex_Wshift_vsPathway.png',sep='')
ggsave(file,width = 20, height = 12, units = "cm",Ex_Figure)

# Change in Base Cases Plots (@What is this for)

ggplot(Base_Test_PHVAC,aes(x=Location,y = value, col = Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Reduction on Base HVAC Peak Demand')+
  ylab('Reduction from Random results')

file<-paste('04_Charts/04_Random/','Mea_Reduction_Base_HVAC_Peak.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

ggplot(Base_Test_AHVAC,aes(x=Location,y = value, col = Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Reduction on Base HVAC Annual Demand')+
  ylab('Reduction from Random results')

file<-paste('04_Charts/04_Random/','Mea_Reduction_Base_HVAC_Annual.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")


ggplot(Base_Test_PTOT,aes(x=Location,y = value, col = Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Reduction on Base Total Peak Demand')+
  ylab('Average % Reduction (Location and Weather) to Base buidling condition')

file<-paste('04_Charts/04_Random/','Mea_Reduction_Base_Total_Peak.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")


ggplot(Base_Test_ATOT,aes(x=Location,y = value, col = Measure))+
  geom_point()+
  facet_grid(~Model)+
  ggtitle('Reduction on Base Total Annual Demand')+
  ylab('Average % Reduction (Location and Weather) to Base buidling condition')

file<-paste('04_Charts/04_Random/','Mea_Reduction_Base_Total_Annual.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

###### Plots for Adaptation in relationship to Random absoulte difference ###### 

Test_Res <- Function_Adapt_Normalise(All_Samples_Single_Measures,'R_P_TOT','No adaptation')

Res_Test_0 <- Function_Order_Ch(Test_Res,'Dif_R_A_HVAC')
Res_Test_1 <- Function_Order_Ch(Test_Res,'Dif_R_A_TOT')

Res_Test_1_Filtered <- Res_Test_1 %>% filter(Delta_Max >2, Sample != 'Base')

Res_Test_2 <- Function_Order_Ch(Test_Res,'Dif_R_P_TOT')
Res_Test_3 <- Function_Order_Ch(Test_Res,'Dif_R_P_HVAC')

Res_Test <- Results_Ordered %>% group_by(Location, Sample) %>%
  summarise(Base = mean(Dif_R_P_TOT[Case_n == 0]),
                              Mean = mean(Dif_R_P_TOT),
                              Max = max(Dif_R_P_TOT),
                              Delta_Max = ((Max/Base) - 1) * 100,
                              Delta_Mean = (Mean/Base -1) * 100)

Res_Test2 <- Results_Ordered %>% group_by(Model, Sample) %>%
  summarise(Base = mean(Dif_R_P_TOT[Case_n == 0]),
            Mean = mean(Dif_R_P_TOT),
            Max = max(Dif_R_P_TOT),
            Delta_Max = ((Max/Base) - 1) * 100,
            Delta_Mean = (Mean/Base -1) * 100)

Res_Test2 <- Results_Ordered %>% group_by(Model, Sample) %>%
  summarise(Base = mean(Dif_R_A_TOT[Case_n == 0]),
            Mean = mean(Dif_R_A_TOT),
            Max = max(Dif_R_A_TOT),
            Delta_Max = ((Max/Base) - 1) * 100,
            Delta_Mean = (Mean/Base -1) * 100)

Res_Test3 <- Results_Ordered %>% group_by(Sample) %>%
  summarise(Base = mean(Dif_R_A_TOT[Case_n == 0]),
            Mean = mean(Dif_R_A_TOT),
            Max = max(Dif_R_A_TOT),
            Delta_Max = ((Max/Base) - 1) * 100,
            Delta_Mean = (Mean/Base -1) * 100)


@6.5.4 example

Results_Ordered <- Res_Test2 %>%
  arrange(Model,Location,Sample,R_A_TOT) 

Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)

ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_A_TOT, col = Sample))+
  geom_point() +
  facet_grid(Model ~ Location) +
  ylab('Ration [%]') +
  ggtitle('Annual Total')

file<-paste('04_Charts/05_Adap_Measures/','Diff_Total_Annual_All_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")


Results_Ordered <- Test %>%
  arrange(Model,Location,Sample,R_A_HVAC) 

Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)

ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_A_HVAC, col = Sample))+
  geom_point() +
  facet_grid(Model ~ Location) +
  ylab('Ration [%]') +
  ggtitle('Annual HVAC')

file<-paste('04_Charts/05_Adap_Measures/','Diff_HVAC_Annual_All_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")

Results_Ordered <- Test %>%
  arrange(Model,Location,Sample,R_P_HVAC) 

Plot_df <- data.frame(Order_Case = rep(0:200,nrow(Results_Ordered)/201),Results_Ordered)

ggplot(Plot_df, aes(x=Order_Case, y = Dif_R_P_HVAC, col = Sample))+
  geom_point() +
  facet_grid(Model ~ Location) +
  ylab('Ratio [%]') +
  ggtitle('Peak HVAC')

file<-paste('04_Charts/05_Adap_Measures/','Diff_HVAC_Peak_All_Measures.png',sep='')
ggsave(file,width = 30, height = 15, units = "cm")



#### Final Experiemtns

Note <- 'The results for no adaptation scenario refer to an increase relative to baseline '

Ex_Figure <- ggplot(Plot_df %>% filter(Model =='Small', Location =='C2'), aes(x=Order_Case,y= Dif_R_A_TOT, col = Sample))+
  geom_point() +
  facet_grid(Model ~ Location) +
  scale_y_continuous(labels=scales::percent) +
  ylab('Reduction relative to baseline') +
  xlab('Weather data iteration in the climate pathway ordered by demand') +
  ggtitle('Reduction on annual total demand under the pathway') +
  theme(legend.position="bottom")+ guides(colour = guide_legend(nrow = 1)) + 
  labs(col= 'Scenario')+
  labs(caption = Note)

file <- paste('04_Charts/04_Random/','Diff_Plot_Annual_C2_Small.png',sep='')
ggsave(file,width = 20, height = 12, units = "cm",Ex_Figure)









