rm(list=ls())
gc()

#Packages
library(readxl)
library(sensitivity)
library(boot)
library(writexl)
library(boot)
library(gdata)
library(readxl)
library(sprof)
library(xlsx)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
library(stringr)
library(stringr)
#install.packages("ggpubr")
library(ggpubr)
library(data.table)
library(dplyr)
library(devtools)
library(sjstats)
library(tidyr)
library(tidyverse)
library(gtable)

#Folder Definition
setwd("C:/Users/mbgnwvz2/Dropbox (The University of Manchester)/01_PhD Research/14_Repository/04-SA/04_02_Results_analysis")

########### Functions ############

Upload_Morris_Data_File <- function(file_name){
  in_folder <- "01_Inputs/01_Simulation_Results/"
  in_file_Par <- paste(in_folder,file_name, sep="")              #Input file
  Table_Par <- read.csv(in_file_Par, header = TRUE, sep = ",")
  Table_Par <- data.frame(Table_Par)
  
  Table_Par <- Table_Par %>%
    rename(Loc = Job_ID) %>%
    arrange(Loc) %>%
    mutate(Loc = substr(Loc,1, 2))
  
  Old_Header <- paste('X..P',1:15,'..',sep='')
  New_Header <- paste('P',1:15,sep='')
  setnames(Table_Par, old = Old_Header, new = New_Header)
  
  return(Table_Par)
}

Morris_Results_DF_JP1 <- function(df_parameters){
  Morris_Res_AN <- df_parameters %>%
        mutate(Total = c0..A1_T/c33..O1) %>%
        mutate(Pumps = c8..A9_Pumps/c33..O1) %>%
        mutate(Heat_Rej = c9..A10_HR/c33..O1) %>%
        mutate(Fans = c7..A8_Fans/c33..O1) %>%
        mutate(Chiller = c2..A3_Cool/c33..O1) %>%
        mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
        mutate(Other_Total = Total - HVAC) %>%
        mutate(SPC1=-c34..SPC_A_01_SensAC/c33..O1*277.777) %>%
        mutate(SPC2=c35..SPC_A_02_CoolEN_Tran/c33..O1*277.777) %>%
        mutate(SPC3=c36..SPC_A_03_Cool_Coils/c33..O1*277.777) %>%
        mutate(Time='AN') %>%
        #mutate(Loc= substr(Job_ID,1, 2)) %>%
        mutate(Sen_Met= 'Morris') %>%
        select(Loc,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC1,SPC2,SPC3)
    
  Morris_Res_Peak <- df_parameters %>%
      mutate(Total = c15..P1_T/c33..O1) %>%
      mutate(Pumps = c23..P9_Pumps/c33..O1) %>%
      mutate(Heat_Rej = c24..P10_HR/c33..O1) %>%
      mutate(Fans = c22..P8_Fans/c33..O1) %>%
      mutate(Chiller = c17..P3_Cool/c33..O1) %>%
      mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
      mutate(Other_Total = Total - HVAC) %>%
      mutate(SPC1=-c39..SPC_P_01_SensAC/c33..O1) %>%
      mutate(SPC2=c40..SPC_P_02_CoolEN_Tran/c33..O1) %>%
      mutate(SPC3=c41..SPC_P_03_Cool_Coils/c33..O1) %>%
      mutate(Time='Peak') %>%
      #mutate(Loc= substr(Job_ID,1, 2)) %>%
      mutate(Sen_Met= 'Morris') %>%
      select(Loc,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC1,SPC2,SPC3)
      
    Out_Results <- rbind(Morris_Res_AN , Morris_Res_Peak)       #### Column with Time (Peak and Annual in the same Rows)
    
    return(Out_Results)
}

Gen_Plot_Morris <- function(Morris_Data , Var_in , Time_in){
  
  Morris_Data <- Morris_Data %>% 
    mutate(Var = str_replace(Var, 'SPC1', 'Space Cooling'))
  
  if(Var_in == 'SPC1'){
    Var_in = 'Space Cooling'
  }
  
  Fig_Out <- ggplot(Morris_Data %>% 
                      filter(Var == Var_in, 
                             Time == Time_in), 
                    aes(x = In_Par, 
                        y = U,
                        group = Location,
                        colour = Location)) + 
    geom_point(size = 1)+
    geom_line(size = 0.1) + 
    labs(x = 'Parameter',y = expression(paste(mu,'*')))+
    facet_wrap(~ Var, scale="free") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  return(Fig_Out)
}

Summer_Peaks <- function(Results_Matrix) {
  Col_Names <- c(paste('P',1:8,'_June', sep = ''),
                 paste('P', 1:8 ,'_July', sep = ''),
                 paste('P',1:8,'_Aug', sep = ''),
                 paste('A', 1:8, sep = ''),
                 paste('P', 1:8, sep = ''))
  
  Results_Matrix[Col_Names] <- sapply(Results_Matrix[Col_Names],as.numeric)
  
  Out_Results <- Results_Matrix%>%
      mutate(P1 = case_when(
        P8 > 0 & P1_June>P1_July & P1_June>P1_Aug ~ P1_June, 
        P8 > 0 & P1_July>P1_June & P1_July>P1_Aug ~ P1_July,
        P8 > 0 & P1_June<P1_Aug & P1_July<P1_Aug ~ P1_Aug, 
        TRUE ~ P1)) %>%
      mutate(P2 = case_when(
        P8 > 0 & P1_June >P1_July & P1_June>P1_Aug ~ P2_June, 
        P8 > 0 & P1_July >P1_June & P1_July>P1_Aug ~ P2_July,
        P8 > 0 & P1_June <P1_Aug & P1_July<P1_Aug ~ P2_Aug, 
        TRUE ~ P2)) %>%
      mutate(P3 = case_when(
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
      mutate(NP8 = as.numeric(0))%>%
      mutate(NP8 = case_when(
        P8 > 1 & P1_June>P1_July& P1_June>P1_Aug ~ P8_June, 
        P8 > 1 & P1_July>P1_June& P1_July>P1_Aug ~ P8_July,
        P8 > 1 & P1_June<P1_Aug& P1_July<P1_Aug ~ P8_Aug, 
        TRUE ~ NP8))%>%
      select(-P8) %>%
      rename(P8=NP8)

      return(Out_Results)
}

Summer_Peaks_2 <- function(Results_Matrix) {
  Col_Names <- c(paste('P',1:8,'_April', sep = ''),
                 paste('P',1:8,'_May', sep = ''),
                 paste('P',1:8,'_June', sep = ''),
                 paste('P', 1:8 ,'_July', sep = ''),
                 paste('P',1:8,'_Aug', sep = ''),
                 paste('P',1:8,'_Sep', sep = ''),
                 paste('A', 1:8, sep = ''),
                 paste('P', 1:8, sep = ''))
  
  Results_Matrix[Col_Names] <- sapply(Results_Matrix[Col_Names],as.numeric)
  
  Out_Results <- Results_Matrix %>%
    mutate(P1 = case_when(
      P8 > 0 & P1_April > pmax(P1_May, P1_June ,P1_July , P1_Aug, P1_Sep) ~ P1_April, 
      P8 > 0 & P1_May > pmax(P1_April, P1_June ,P1_July , P1_Aug, P1_Sep) ~ P1_May,
      P8 > 0 & P1_June > pmax(P1_April, P1_May ,P1_July , P1_Aug, P1_Sep) ~ P1_June,
      P8 > 0 & P1_July > pmax(P1_April, P1_June ,P1_May , P1_Aug, P1_Sep) ~ P1_July,
      P8 > 0 & P1_Aug > pmax(P1_April, P1_June ,P1_July , P1_May, P1_Sep) ~ P1_Aug,
      P8 > 0 & P1_Sep > pmax(P1_April, P1_June ,P1_July , P1_Aug, P1_May) ~ P1_Sep,
      TRUE ~ P1)) %>%
    mutate(P2 = case_when(
      P8 > 0 & P2_April > pmax(P2_May, P2_June ,P2_July , P2_Aug, P2_Sep) ~ P2_April, 
      P8 > 0 & P2_May > pmax(P2_April, P2_June ,P2_July , P2_Aug, P2_Sep) ~ P2_May,
      P8 > 0 & P2_June > pmax(P2_April, P2_May ,P2_July , P2_Aug, P2_Sep) ~ P2_June,
      P8 > 0 & P2_July > pmax(P2_April, P2_June ,P2_May , P2_Aug, P2_Sep) ~ P2_July,
      P8 > 0 & P2_Aug > pmax(P2_April, P2_June ,P2_July , P2_May, P2_Sep) ~ P2_Aug,
      P8 > 0 & P2_Sep > pmax(P2_April, P2_June ,P2_July , P2_Aug, P2_May) ~ P2_Sep,
      TRUE ~ P2)) %>%
    mutate(P3 = case_when(
      P8 > 0 & P3_April > pmax(P3_May, P3_June ,P3_July , P3_Aug, P3_Sep) ~ P3_April, 
      P8 > 0 & P3_May > pmax(P3_April, P3_June ,P3_July , P3_Aug, P3_Sep) ~ P3_May,
      P8 > 0 & P3_June > pmax(P3_April, P3_May ,P3_July , P3_Aug, P3_Sep) ~ P3_June,
      P8 > 0 & P3_July > pmax(P3_April, P3_June ,P3_May , P3_Aug, P3_Sep) ~ P3_July,
      P8 > 0 & P3_Aug > pmax(P3_April, P3_June ,P3_July , P3_May, P3_Sep) ~ P3_Aug,
      P8 > 0 & P3_Sep > pmax(P3_April, P3_June ,P3_July , P3_Aug, P3_May) ~ P3_Sep,
      TRUE ~ P3))%>%
    mutate(P4=case_when(
      P8 > 0 & P4_April > pmax(P4_May, P4_June ,P4_July , P4_Aug, P4_Sep) ~ P4_April, 
      P8 > 0 & P4_May > pmax(P4_April, P4_June ,P4_July , P4_Aug, P4_Sep) ~ P4_May,
      P8 > 0 & P4_June > pmax(P4_April, P4_May ,P4_July , P4_Aug, P4_Sep) ~ P4_June,
      P8 > 0 & P4_July > pmax(P4_April, P4_June ,P4_May , P4_Aug, P4_Sep) ~ P4_July,
      P8 > 0 & P4_Aug > pmax(P4_April, P4_June ,P4_July , P4_May, P4_Sep) ~ P4_Aug,
      P8 > 0 & P4_Sep > pmax(P4_April, P4_June ,P4_July , P4_Aug, P4_May) ~ P4_Sep,
      TRUE ~ P4))%>%
    mutate(P5=case_when(
      P8 > 0 & P5_April > pmax(P5_May, P5_June ,P5_July , P5_Aug, P5_Sep) ~ P5_April, 
      P8 > 0 & P5_May > pmax(P5_April, P5_June ,P5_July , P5_Aug, P5_Sep) ~ P5_May,
      P8 > 0 & P5_June > pmax(P5_April, P5_May ,P5_July , P5_Aug, P5_Sep) ~ P5_June,
      P8 > 0 & P5_July > pmax(P5_April, P5_June ,P5_May , P5_Aug, P5_Sep) ~ P5_July,
      P8 > 0 & P5_Aug > pmax(P5_April, P5_June ,P5_July , P5_May, P5_Sep) ~ P5_Aug,
      P8 > 0 & P5_Sep > pmax(P5_April, P5_June ,P5_July , P5_Aug, P5_May) ~ P5_Sep,
      TRUE ~ P5))%>%
    mutate(P6=case_when(
      P8 > 0 & P6_April > pmax(P6_May, P6_June ,P6_July , P6_Aug, P6_Sep) ~ P6_April, 
      P8 > 0 & P6_May > pmax(P6_April, P6_June ,P6_July , P6_Aug, P6_Sep) ~ P6_May,
      P8 > 0 & P6_June > pmax(P6_April, P6_May ,P6_July , P6_Aug, P6_Sep) ~ P6_June,
      P8 > 0 & P6_July > pmax(P6_April, P6_June ,P6_May , P6_Aug, P6_Sep) ~ P6_July,
      P8 > 0 & P6_Aug > pmax(P6_April, P6_June ,P6_July , P6_May, P6_Sep) ~ P6_Aug,
      P8 > 0 & P6_Sep > pmax(P6_April, P6_June ,P6_July , P6_Aug, P6_May) ~ P6_Sep,
      TRUE ~ P6))%>%
    mutate(P7=case_when(
      P8 > 0 & P7_April > pmax(P7_May, P7_June ,P7_July , P7_Aug, P7_Sep) ~ P7_April, 
      P8 > 0 & P7_May > pmax(P7_April, P7_June ,P7_July , P7_Aug, P7_Sep) ~ P7_May,
      P8 > 0 & P7_June > pmax(P7_April, P7_May ,P7_July , P7_Aug, P7_Sep) ~ P7_June,
      P8 > 0 & P7_July > pmax(P7_April, P7_June ,P7_May , P7_Aug, P7_Sep) ~ P7_July,
      P8 > 0 & P7_Aug > pmax(P7_April, P7_June ,P7_July , P7_May, P7_Sep) ~ P7_Aug,
      P8 > 0 & P7_Sep > pmax(P7_April, P7_June ,P7_July , P7_Aug, P7_May) ~ P7_Sep,
      TRUE ~ P7))%>%
    mutate(NP8 = as.numeric(0))%>%
    mutate(NP8 = case_when(
      P8 > 1 & P1_June>P1_July& P1_June>P1_Aug ~ P8_June, 
      P8 > 1 & P1_July>P1_June& P1_July>P1_Aug ~ P8_July,
      P8 > 1 & P1_June<P1_Aug& P1_July<P1_Aug ~ P8_Aug, 
      TRUE ~ NP8))%>%
    select(-P8) %>%
    rename(P8=NP8)
  
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

Upload_Morris_Data_File_840 <- function(file_name){
  in_folder <- "01_Inputs/01_Simulation_Results/"
  in_file_Par <- paste(in_folder,file_name, sep="")              #Input file
  Table_Par <- read.csv(in_file_Par, header = TRUE, sep = ",")
  Table_Par <- data.frame(Table_Par)
  
  Old_Header <- paste('X..P',1:15,'..',sep='')
  New_Header <- paste('P',1:15,sep='')
  setnames(Table_Par, old = Old_Header, new = New_Header)
  
  return(Table_Par)
}

Orig_Results_Choice_840 <- function(df_parameters){
  Out_Results <- df_parameters %>%
    mutate(ModelFile = case_when(
      ModelFile == "Large_Office_DOE_JP1_Sizing_File.imf" ~ "Large",
      ModelFile == "Medium_Office_DOE_JP1_Sizing_File.imf" ~ "Medium",
      ModelFile == "Small_Office_DOE_JP1_Sizing_File.imf" ~ "Small"))%>%
    rename(Model=ModelFile)%>%
   # rename(Location=X..location..)%>%
    mutate(Case_n = as.numeric(str_extract(Job_ID,"(?<=_Large_)[:digit:]*")))%>%
    rename(A1 = c0..A1) %>%
    rename(A2 = c1..A2) %>%
    rename(A3 = c2..A3) %>%
    rename(A4 = c3..A4) %>%
    rename(A5 = c4..A5) %>%
    rename(A6 = c5..A6) %>%
    rename(A7 = c6..A7) %>%
    rename(A8 = c7..A8) %>%
    rename(P1 = c8..P1) %>%
    rename(P2 = c9..P2) %>%
    rename(P3 = c10..P3) %>%
    rename(P4 = c11..P4) %>%
    rename(P5 = c12..P5) %>%
    rename(P6 = c13..P6) %>%
    rename(P7 = c14..P7) %>%
    rename(P8 = c15..P8) %>%
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
           'P8_Aug'=str_subset(names(df_parameters),'P8_August'),
           'SPC_June' = str_subset(names(df_parameters),'SPC_ET_June'),
           'SPC_July' = str_subset(names(df_parameters),'SPC_ET_July'),
           'SPC_Aug' = str_subset(names(df_parameters),'SPC_ET_August')) %>%
    group_by(Location) %>%
    arrange(Case_n) %>%
    ungroup()
  
  return(Out_Results)
}

Orig_Results_Choice_840_2 <- function(df_parameters){
  Out_Results <- df_parameters %>%
    mutate(ModelFile = case_when(
      ModelFile == "Large_Office_DOE_JP1_Sizing_File.imf" ~ "Large",
      ModelFile == "Medium_Office_DOE_JP1_Sizing_File.imf" ~ "Medium",
      ModelFile == "Small_Office_DOE_JP1_Sizing_File.imf" ~ "Small"))%>%
    rename(Model=ModelFile)%>%
    # rename(Location=X..location..)%>%
    mutate(Case_n = as.numeric(str_extract(Job_ID,"(?<=_Case_)[:digit:]+$")))%>%
    rename(A1 = c0..A1) %>%
    rename(A2 = c1..A2) %>%
    rename(A3 = c2..A3) %>%
    rename(A4 = c3..A4) %>%
    rename(A5 = c4..A5) %>%
    rename(A6 = c5..A6) %>%
    rename(A7 = c6..A7) %>%
    rename(A8 = c7..A8) %>%
    rename(P1 = c8..P1) %>%
    rename(P2 = c9..P2) %>%
    rename(P3 = c10..P3) %>%
    rename(P4 = c11..P4) %>%
    rename(P5 = c12..P5) %>%
    rename(P6 = c13..P6) %>%
    rename(P7 = c14..P7) %>%
    rename(P8 = c15..P8) %>%
    rename('P1_April'=str_subset(names(df_parameters),'P1_April'),
           'P2_April'=str_subset(names(df_parameters),'P2_April'),
           'P3_April'=str_subset(names(df_parameters),'P3_April'),
           'P4_April'=str_subset(names(df_parameters),'P4_April'),
           'P5_April'=str_subset(names(df_parameters),'P5_April'),
           'P6_April'=str_subset(names(df_parameters),'P6_April'),
           'P7_April'=str_subset(names(df_parameters),'P7_April'),
           'P8_April'=str_subset(names(df_parameters),'P8_April'),
           'P1_May'=str_subset(names(df_parameters),'P1_May'),
           'P2_May'=str_subset(names(df_parameters),'P2_May'),
           'P3_May'=str_subset(names(df_parameters),'P3_May'),
           'P4_May'=str_subset(names(df_parameters),'P4_May'),
           'P5_May'=str_subset(names(df_parameters),'P5_May'),
           'P6_May'=str_subset(names(df_parameters),'P6_May'),
           'P7_May'=str_subset(names(df_parameters),'P7_May'),
           'P8_May'=str_subset(names(df_parameters),'P8_May'),
           'P1_June'=str_subset(names(df_parameters),'P1_June'),
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
           'P8_Aug'=str_subset(names(df_parameters),'P8_August'),
           'P1_Sep'=str_subset(names(df_parameters),'P1_September'),
           'P2_Sep'=str_subset(names(df_parameters),'P2_September'),
           'P3_Sep'=str_subset(names(df_parameters),'P3_September'),
           'P4_Sep'=str_subset(names(df_parameters),'P4_September'),
           'P5_Sep'=str_subset(names(df_parameters),'P5_September'),
           'P6_Sep'=str_subset(names(df_parameters),'P6_September'),
           'P7_Sep'=str_subset(names(df_parameters),'P7_September'),
           'P8_Sep'=str_subset(names(df_parameters),'P8_September'),
           'SPC_April' = str_subset(names(df_parameters),'SPC2_April'),
           'SPC_May' = str_subset(names(df_parameters),'SPC2_May'),
           'SPC_June' = str_subset(names(df_parameters),'SPC2_June'),
           'SPC_July' = str_subset(names(df_parameters),'SPC2_July'),
           'SPC_Aug' = str_subset(names(df_parameters),'SPC2_August'),
           'SPC_Sep' = str_subset(names(df_parameters),'SPC2_September')) %>%
    group_by(Location) %>%
    arrange(Case_n) %>%
    ungroup()
  
  return(Out_Results)
}

Morris_Results_DF_840 <- function(df_parameters){
  if(unique(df_parameters$Model) == 'Large'){
    Area <- 46320.38
  } else if (unique(df_parameters$Model) == 'Medium'){
    Area <- 4982.2
  } else if (unique(df_parameters$Model) == 'Small'){
    Area <- 511.16
  }
    
  Morris_Res_Peak <- df_parameters %>%
    mutate(Total = P1 / Area) %>%
    mutate(Pumps = P4 / Area) %>%
    mutate(Heat_Rej = P5 / Area) %>%
    mutate(Fans = P3 / Area) %>%
    mutate(Chiller = P2 / Area) %>%
    mutate(HVAC = Pumps + Heat_Rej + Fans + Chiller) %>%
    mutate(Other_Total = Total - HVAC) %>%
    mutate(SPC = case_when(
      P1_June > P1_July & P1_June > P1_Aug ~ SPC_June / Area, 
      P1_July > P1_June & P1_July > P1_Aug ~ SPC_July / Area,
      P1_June < P1_Aug & P1_July < P1_Aug ~ SPC_Aug /Area)) %>%
    mutate(Time='Peak') %>%
    #mutate(Loc= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Morris') %>%
    select(Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)
  
  Morris_Res_AN <- df_parameters %>%
    mutate(Total = (A1 - A8) / Area) %>%
    mutate(Pumps = A4 / Area) %>%
    mutate(Heat_Rej = A5 / Area) %>%
    mutate(Fans = A3 / Area) %>%
    mutate(Chiller = A2 / Area) %>%
    mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
    mutate(Other_Total = Total - HVAC) %>%
    mutate(SPC = 0) %>%
    mutate(Time='AN') %>%
    #mutate(Loc= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Morris') %>%
    select(Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)
  
  Out_Results <- rbind(Morris_Res_AN , Morris_Res_Peak)       #### Column with Time (Peak and Annual in the same Rows)
  
  return(Out_Results)
}

Calculate_SA_MorrisIndex <- function(Morris_Res , Morris_Data){
  
  # Range Limits of the Input Parameters
  P1L = 0.1;    P1U = 2;        # %Wall Conductivity
  P2L=0.005;  P2U=0.08;     # %Roof Conductivity
  P3L=0.075;  P3U=0.5;      # %SGHC
  P4L=1;      P4U=7;        #%Window U-Value
  P5L=5;      P5U=20;        #%Lighting
  P6L=6;      P6U=22;        #%Equiment
  P7L=5;      P7U=20;        #%People
  P8L=21;     P8U=26;        #%Ambient SP
  P9L=5.5;    P9U=8;         #%CW SP
  P10L=0.0002;P10U=0.005;    #%Ventilation
  P11L=0.0001;P11U=0.003;   #%Infiltration
  P12L=4;     P12U=7;        #%Ref COP
  P13L=0.1;   P13U=0.3;      #%Unload Factor
  P14L=0;   P14U=1;         #%Schedule Strectch
  #P15L=-0.2;   P15U=0;      #%Schedule Peak
  
  
  
  #%All Parameter in a column
  LBO <- c(P1L,P2L,P3L,P4L,P5L,P6L,P7L,P8L, P9L, P10L, P11L, P12L, P13L,P14L)#,P15L)   #%P1-P15
  UBO <- c(P1U,P2U,P3U,P4U,P5U,P6U,P7U,P8U,P9U,P10U,P11U,P12U,P13U,P14U)#,P15U)   #%P1-P15
  
  P_n=14
  r_n=80
  
  a <- morris(model = NULL, factors = P_n, r = r_n, design = list(type = "oat", levels = 8, grid.jump = 4),binf = LBO, bsup = UBO)  
  
  #Uploading to Morris method the sampling conditions simulated
  a$X <- Morris_Data
  
  #Creating the Morris EE method for each climate
  x_C1 <- a
  x_C2 <- a
  x_C3 <- a
  x_C4 <- a
  x_C5 <- a
  x_C6 <- a
  
  ### Results in columns for Peak and Annual ... No time column to calculate Sensitivity indices
  Morris_Res_For_SesIndex <- cbind(Morris_Res %>% filter(Time=='AN') %>% select(Location,Total:SPC),
                                   Morris_Res %>% filter(Time=='Peak') %>% select(Total:SPC))
  
  Header <- c('Location','Total','Other_Total','Pumps','Heat_Rej','Fans','Chiller','HVAC','SPC','Total_Peak','Other_Total_Peak','Pumps_Peak','Heat_Rej_Peak','Fans_Peak','Chiller_Peak','HVAC_Peak','SPC_Peak')
  colnames(Morris_Res_For_SesIndex) <- Header
  
  #Atributting the results of each climate to each method
  tell(x_C1, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C1')%>%select(-Location)))
  tell(x_C2, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C2')%>%select(-Location)))
  tell(x_C3, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C3')%>%select(-Location)))
  tell(x_C4, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C4')%>%select(-Location)))
  tell(x_C5, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C5')%>%select(-Location)))
  tell(x_C6, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C6')%>%select(-Location)))
  
  #Calculate the Morris indexes for the four Climates
  mu.star_C1 <- as.data.frame(apply(abs(x_C1$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C1 <- as.data.frame(apply(x_C1$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C2 <- as.data.frame(apply(abs(x_C2$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C2 <- as.data.frame(apply(x_C2$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C3 <- as.data.frame(apply(abs(x_C3$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C3 <- as.data.frame(apply(x_C3$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C4 <- as.data.frame(apply(abs(x_C4$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C4 <- as.data.frame(apply(x_C4$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C5 <- as.data.frame(apply(abs(x_C5$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C5 <- as.data.frame(apply(x_C5$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C6 <- as.data.frame(apply(abs(x_C6$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C6 <- as.data.frame(apply(x_C6$ee, 3, function(M){apply(M, 2, sd)}))
  
  #Create Varaibles for climate Pair mu # sigma
  
  Par_List <- paste('P',sprintf("%02d", 1:14),sep='')

  
  C1_EE <- cbind(Par_List , Location = 'C1', mu.star_C1 , sigma_C1)
  C2_EE <- cbind(Par_List ,Location ='C2', mu.star_C2 , sigma_C2)
  C3_EE <- cbind(Par_List ,Location ='C3', mu.star_C3 , sigma_C3)
  C4_EE <- cbind(Par_List ,Location ='C4', mu.star_C4 , sigma_C4)
  C5_EE <- cbind(Par_List ,Location ='C5', mu.star_C5 , sigma_C5)
  C6_EE <- cbind(Par_List ,Location ='C6', mu.star_C6 , sigma_C6)
  
  #Check the ratio between sigma and U 
  #Give name to columns
  
  C_EE <- rbind(C1_EE,C2_EE,C3_EE,C4_EE,C5_EE,C6_EE)
  C_EE <- data.frame(C_EE)
  return(C_EE)
}

Calculate_SA_MorrisIndex_Small <- function(Morris_Res , Morris_Data){
  #%All Parameter in a column Limits of ranges
  LBO <- c(min(Morris_Data$P01),
           min(Morris_Data$P02),
           min(Morris_Data$P03),
           min(Morris_Data$P04),
           min(Morris_Data$P05),
           min(Morris_Data$P06),
           min(Morris_Data$P07),
           min(Morris_Data$P08), 
           min(Morris_Data$P10), 
           min(Morris_Data$P11), 
           min(Morris_Data$P12), 
           min(Morris_Data$P14)) # Low end
  UBO <- c(max(Morris_Data$P01),
           max(Morris_Data$P02),
           max(Morris_Data$P03),
           max(Morris_Data$P04),
           max(Morris_Data$P05),
           max(Morris_Data$P06),
           max(Morris_Data$P07),
           max(Morris_Data$P08), 
           max(Morris_Data$P10), 
           max(Morris_Data$P11), 
           max(Morris_Data$P12), 
           max(Morris_Data$P14))   # Upper Limit
  
  P_n=12
  r_n=80
  
  a <- morris(model = NULL, factors = P_n, r = r_n, design = list(type = "oat", levels = 8, grid.jump = 4),binf = LBO, bsup = UBO)  
  
  #Uploading to Morris method the sampling conditions simulated
  a$X <- Morris_Data
  
  #Creating the Morris EE method for each climate
  x_C1 <- a
  x_C2 <- a
  x_C3 <- a
  x_C4 <- a
  x_C5 <- a
  x_C6 <- a
  
  ### Results in columns for Peak and Annual ... No time column to calculate Sensitivity indices
  Morris_Res_For_SesIndex <- cbind(Morris_Res %>% filter(Time=='AN') %>% select(Location,Total:SPC),
                                   Morris_Res %>% filter(Time=='Peak') %>% select(Total:SPC))
  
  Header <- c('Location','Total','Other_Total','Pumps','Heat_Rej','Fans','Chiller','HVAC','SPC','Total_Peak','Other_Total_Peak','Pumps_Peak','Heat_Rej_Peak','Fans_Peak','Chiller_Peak','HVAC_Peak','SPC_Peak')
  colnames(Morris_Res_For_SesIndex) <- Header
  
  #Atributting the results of each climate to each method
  tell(x_C1, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C1')%>%select(-Location)))
  tell(x_C2, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C2')%>%select(-Location)))
  tell(x_C3, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C3')%>%select(-Location)))
  tell(x_C4, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C4')%>%select(-Location)))
  tell(x_C5, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C5')%>%select(-Location)))
  tell(x_C6, y = as.matrix(Morris_Res_For_SesIndex%>%filter(Location == 'C6')%>%select(-Location)))
  
  #Calculate the Morris indexes for the four Climates
  mu.star_C1 <- as.data.frame(apply(abs(x_C1$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C1 <- as.data.frame(apply(x_C1$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C2 <- as.data.frame(apply(abs(x_C2$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C2 <- as.data.frame(apply(x_C2$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C3 <- as.data.frame(apply(abs(x_C3$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C3 <- as.data.frame(apply(x_C3$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C4 <- as.data.frame(apply(abs(x_C4$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C4 <- as.data.frame(apply(x_C4$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C5 <- as.data.frame(apply(abs(x_C5$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C5 <- as.data.frame(apply(x_C5$ee, 3, function(M){apply(M, 2, sd)}))
  
  mu.star_C6 <- as.data.frame(apply(abs(x_C6$ee), 3, function(M){apply(M, 2, mean)}))
  sigma_C6 <- as.data.frame(apply(x_C6$ee, 3, function(M){apply(M, 2, sd)}))
  
  #Create Varaibles for climate Pair mu # sigma
  
  Par_List <- paste('P',sprintf("%02d", c(1:8,10:12,14)),sep='')
  
  C1_EE <- cbind(Par_List , Location = 'C1', mu.star_C1 , sigma_C1)
  C2_EE <- cbind(Par_List ,Location ='C2', mu.star_C2 , sigma_C2)
  C3_EE <- cbind(Par_List ,Location ='C3', mu.star_C3 , sigma_C3)
  C4_EE <- cbind(Par_List ,Location ='C4', mu.star_C4 , sigma_C4)
  C5_EE <- cbind(Par_List ,Location ='C5', mu.star_C5 , sigma_C5)
  C6_EE <- cbind(Par_List ,Location ='C6', mu.star_C6 , sigma_C6)
  
  #Check the ratio between sigma and U 
  #Give name to columns
  
  C_EE <- rbind(C1_EE,C2_EE,C3_EE,C4_EE,C5_EE,C6_EE)
  C_EE <- data.frame(C_EE)
  return(C_EE)
}

CV_data_generator <- function(df_data,Build_Type){
  Out_df <- df_data %>%
    select(case_when(
      Build_Type == 'Large' ~ -('SPC'),
      Build_Type == 'Medium' ~ -(c('SPC','Heat_Rej')),
      Build_Type == 'Small' ~ -(c('SPC','Heat_Rej','Pumps')))) %>%
    group_by(Location , Time) %>%
    summarise_all(cv) %>%
    ungroup()
  
  return(Out_df)  
}

CV_Plot_Generator <- function(df_data){
  
  df_Stack <- df_data %>% 
    gather(key = "Var", value = "Value", Total:HVAC) #Including SPC1,SPC2, SPC3
  
  
  Plot_Out <- ggplot(df_Stack, aes(x = Location,y=Value,group=Time,colour=Time)) +
    geom_line(size=1) +
    facet_wrap(~ Var, scale="free") +
    ggtitle('Morris CV by variable')+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="top",
          legend.title=element_blank())
  
  return(Plot_Out)
  
}

Morris_Results_DF_840_2 <- function(df_parameters){
  df_parameters <- df_parameters %>%
    mutate(Area = case_when(
      str_detect(Model, 'Large') ~ 46320.38,
      str_detect(Model, 'Medium') ~ 4982.2,
      str_detect(Model, 'Small') ~ 511.16,
      TRUE ~ 1))
  
  Morris_Res_Peak <- df_parameters %>%
    mutate(Total = P1 / Area) %>%
    mutate(Pumps = P4 / Area) %>%
    mutate(Heat_Rej = P5 / Area) %>%
    mutate(Fans = P3 / Area) %>%
    mutate(Chiller = P2 / Area) %>%
    mutate(HVAC = Pumps + Heat_Rej + Fans + Chiller) %>%
    mutate(Other_Total = Total - HVAC) %>%
    mutate(SPC = c90..SPC2_P_CoolEN_Tran / Area) %>%
    mutate(Time='Peak') %>%
    #mutate(Loc= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Morris') %>%
    select(Model,Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)
  
  Morris_Res_AN <- df_parameters %>%
    mutate(Total = (A1 - A8) / Area) %>%
    mutate(Pumps = A4 / Area) %>%
    mutate(Heat_Rej = A5 / Area) %>%
    mutate(Fans = A3 / Area) %>%
    mutate(Chiller = A2 / Area) %>%
    mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
    mutate(Other_Total = Total - HVAC) %>%
    mutate(SPC = c88..SPC2_A_CoolEN_Tran / Area) %>%
    mutate(Time='AN') %>%
    #mutate(Loc= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Morris') %>%
    select(Model,Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)
  
  Out_Results <- rbind(Morris_Res_AN , Morris_Res_Peak)       #### Column with Time (Peak and Annual in the same Rows)
  
  return(Out_Results)
}
Normalised_Res <- function(Morris_Res, Model_Type,Base_Results_Stack){
  Base_Results <- Base_Results_Stack %>% spread(Var,value)
  
  Out_Res <- Morris_Res %>%
    filter(Model == Model_Type) %>% 
    mutate(Total = case_when(
      Location == 'C1' & Time == 'AN' ~ Total / Base_Results$Total[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C2' & Time == 'AN' ~ Total / Base_Results$Total[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C3' & Time == 'AN'~  Total / Base_Results$Total[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C4' & Time == 'AN'~ Total / Base_Results$Total[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C5' & Time == 'AN'~ Total / Base_Results$Total[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C6' & Time == 'AN'~ Total / Base_Results$Total[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
      Location == 'C1' & Time == 'Peak' ~ Total / Base_Results$Total[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
      Location == 'C2' & Time == 'Peak' ~ Total / Base_Results$Total[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
      Location == 'C3' & Time == 'Peak'~  Total / Base_Results$Total[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
      Location == 'C4' & Time == 'Peak'~ Total / Base_Results$Total[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
      Location == 'C5' & Time == 'Peak'~ Total / Base_Results$Total[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
      Location == 'C6' & Time == 'Peak'~ Total / Base_Results$Total[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      Other_Total = case_when(
        Location == 'C1' & Time == 'AN' ~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ Other_Total / Base_Results$Other_Total[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      HVAC = case_when(
        Location == 'C1' & Time == 'AN' ~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  HVAC / Base_Results$HVAC[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  HVAC / Base_Results$HVAC[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ HVAC / Base_Results$HVAC[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      SPC = case_when(
        Location == 'C1' & Time == 'AN' ~ SPC / Base_Results$SPC[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ SPC / Base_Results$SPC[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  SPC / Base_Results$SPC[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ SPC / Base_Results$SPC[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ SPC / Base_Results$SPC[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ SPC / Base_Results$SPC[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ SPC / Base_Results$SPC[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ SPC / Base_Results$SPC[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  SPC / Base_Results$SPC[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ SPC / Base_Results$SPC[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ SPC / Base_Results$SPC[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ SPC / Base_Results$SPC[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      Pumps  = case_when(
        Location == 'C1' & Time == 'AN' ~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  Pumps / Base_Results$Pumps[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  Pumps / Base_Results$Pumps[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ Pumps / Base_Results$Pumps[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      Heat_Rej = case_when(
        Location == 'C1' & Time == 'AN' ~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ Heat_Rej / Base_Results$Heat_Rej[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      Fans = case_when(
        Location == 'C1' & Time == 'AN' ~ Fans / Base_Results$Fans[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ Fans / Base_Results$Fans[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  Fans / Base_Results$Fans[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ Fans / Base_Results$Fans[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ Fans / Base_Results$Fans[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ Fans / Base_Results$Fans[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ Fans / Base_Results$Fans[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ Fans / Base_Results$Fans[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  Fans / Base_Results$Fans[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ Fans / Base_Results$Fans[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ Fans / Base_Results$Fans[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ Fans / Base_Results$Fans[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type]),
      Chiller = case_when(
        Location == 'C1' & Time == 'AN' ~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C1' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'AN' ~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C2' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'AN'~  Chiller / Base_Results$Chiller[Base_Results$Location == 'C3' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'AN'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C4' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'AN'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C5' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'AN'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C6' & Base_Results$Time == 'AN' & Base_Results$Model == Model_Type],
        Location == 'C1' & Time == 'Peak' ~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C1' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C2' & Time == 'Peak' ~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C2' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C3' & Time == 'Peak'~  Chiller / Base_Results$Chiller[Base_Results$Location == 'C3' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C4' & Time == 'Peak'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C4' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C5' & Time == 'Peak'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C5' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type],
        Location == 'C6' & Time == 'Peak'~ Chiller / Base_Results$Chiller[Base_Results$Location == 'C6' & Base_Results$Time == 'Peak' & Base_Results$Model == Model_Type])
    )
  return(Out_Res)
}

Data_Manipulation_Morris_SA_Index <- function(Morris_SA_Index){
  Out_Res <- Morris_SA_Index %>% gather(key = 'Var', value = 'U', -c(Par_List,Location)) %>% 
    filter(!str_detect(Var,'\\.1')) %>%
    mutate(Time = case_when(
      str_detect(Var,'_Peak') ~ 'Peak',
      TRUE ~ 'AN')) %>%
    mutate(Var = str_remove(Var, '_Peak')) %>%
    rename(In_Par = Par_List)
  
  return(Out_Res)
}

Create_Mu_Table <- function(Var_in){
  Out_Res <- cbind(C1 = mu.star_C1[,Var_in],
                   C2 = mu.star_C2[,Var_in],
                   C3 = mu.star_C3[,Var_in],
                   C4 = mu.star_C4[,Var_in],
                   C5 = mu.star_C5[,Var_in],
                   C6 = mu.star_C6[,Var_in])
  
  return(Out_Res)
}

SPC_Function <- function(df_data){
  Out_df <- df_data %>% select(c17..SPC_SenI_Fac:c20..SPC_Tot_Fac)
  
  return(Out_df)
}



###### All Create Database / Dataframes ######

#@Large Buildings

Table_Par_Large_840 <- Upload_Morris_Data_File_840("Morris_Large_840_Results.csv") %>% rename(Location = X..location..)  #Upload result dataframe

Morris_Res_840_Large <- Morris_Results_DF_840(Orig_Results_Choice_840(Table_Par_Large_840 %>% select(-(P1:P15)))
                                              )
Morris_Data_840_Large <- Table_Par_Large_840 %>% filter(Location=='C1') %>% select(P1:P14) %>% 
  setNames(paste('P',sprintf("%02d", 1:14),sep=''))

C_EE_840_Large <- Calculate_SA_MorrisIndex(Morris_Res_840_Large , 
                                           Morris_Data_840_Large)

#@Medium Buildings
Table_Par_Medium_840 <- Upload_Morris_Data_File_840("Morris_Medium_840_Results.csv") %>% rename(Location = X..location..) #Upload result dataframe

Morris_Res_840_Medium <- Morris_Results_DF_840( Summer_Peaks(Orig_Results_Choice_840(Table_Par_Medium_840 %>% select(-(P1:P15)))) )

Morris_Data_840_Medium <- Table_Par_Medium_840 %>% filter(Location=='C1') %>% select(P1:P14) %>% 
  setNames(paste('P',sprintf("%02d", 1:14),sep='')) %>%
  select(-P09,-P13)

C_EE_840_Medium <- Calculate_SA_MorrisIndex_Small(
  Morris_Res_840_Medium ,
  Morris_Data_840_Medium )

#Small Buildings

Table_Par_Small_840 <- Upload_Morris_Data_File_840("Morris_Small_840_Results.csv") %>% rename(Location = X..location..) #Upload result dataframe

Morris_Res_840_Small <- Morris_Results_DF_840(
  Orig_Results_Choice_840(Table_Par_Small_840 %>% select(-(P1:P15)))
)

Morris_Data_840_Small <- Table_Par_Small_840 %>% filter(Location=='C1') %>% select(P1:P14) %>%
  setNames(paste('P',sprintf("%02d", 1:14),sep='')) %>%
  select(-P09,-P13)

C_EE_840_Small <- Calculate_SA_MorrisIndex_Small(
  Morris_Res_840_Small,
  Morris_Data_840_Small
  )

#@All Buildings

Table_Par_All_840 <- Upload_Morris_Data_File_840('840_2021_Morris_Range.csv') %>% rename(Location = X..location..)

#@ Create Morris Parameters Matrix
Morris_Data_All <- Table_Par_All_840 %>% 
  filter(Location=='C1') %>% select(ModelFile,P1:P14) %>% rename(Model = ModelFile) %>%
  mutate(Model = case_when(
    str_detect(Model, 'Large_Office') ~ 'Large',
    str_detect(Model, 'Medium_Office') ~ 'Medium',
    str_detect(Model, 'Small_Office') ~ 'Small',
    TRUE ~ Model))

Morris_Res_840 <- Morris_Results_DF_840_2(
  Summer_Peaks_2( 
    Orig_Results_Choice_840_2(Table_Par_All_840 %>% select(-(P1:P15))) ))


#Base Values
Table_Par_840_Base_Cases <- Upload_Morris_Data_File_840("840_Base_Cases_2021.01.22.csv") %>% rename(Location = X..location..)

Test <- Summer_Peaks_2(Orig_Results_Choice_840_2(Table_Par_840_Base_Cases %>% select(-(P1:P15)))) %>% 
  mutate(
    P_SPC = c90..SPC2_P_CoolEN_Tran,
    A_SPC = c88..SPC2_A_CoolEN_Tran) %>%
  select(Location, Model, A1:A8, P1:P7, P8, P_SPC, A_SPC) %>% 
  mutate(Area = case_when(
    Model == 'Large' ~ 46320.38,
    Model == 'Medium' ~ 4982.2,
    Model == 'Small' ~ 511.16)) %>% 
  mutate_at(vars(A1:A_SPC), funs(. / Area))

Morris_Res_Peak <- Test %>%
  mutate(Total = P1) %>%
  mutate(Pumps = P4) %>%
  mutate(Heat_Rej = P5) %>%
  mutate(Fans = P3 ) %>%
  mutate(Chiller = P2 ) %>%
  mutate(HVAC = Pumps + Heat_Rej + Fans + Chiller) %>%
  mutate(Other_Total = Total - HVAC) %>%
  mutate(SPC = P_SPC) %>%
  mutate(Time='Peak') %>%
  select(Location,Model,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)

Morris_Res_AN <- Test %>%
  mutate(Total = (A1 - A8)) %>%
  mutate(Pumps = A4 ) %>%
  mutate(Heat_Rej = A5 ) %>%
  mutate(Fans = A3 ) %>%
  mutate(Chiller = A2) %>%
  mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
  mutate(Other_Total = Total - HVAC) %>%
  mutate(SPC = A_SPC) %>%
  mutate(Time='AN') %>%
  select(Location,Model,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC)

Base_Results_Stack <- rbind(Morris_Res_AN , Morris_Res_Peak) %>% gather(key = variable, value = 'value',Total:SPC) %>% rename(Var = variable)

Base_Results_Total <- Base_Results_Stack %>% filter(Var == 'Total') %>% spread(Model,value)
Base_Results_HVAC <- Base_Results_Stack %>% filter(Var == 'HVAC') %>% spread(Model,value)


Norm_Large <- Normalised_Res(Morris_Res_840, 'Large', Base_Results_Stack)
Norm_Medium <- Normalised_Res(Morris_Res_840, 'Medium', Base_Results_Stack) 
Norm_Small <- Normalised_Res(Morris_Res_840, 'Small', Base_Results_Stack)

### Range of Results

Min_Large <- Norm_Large %>% group_by(Model,Location, Time) %>% summarise_all(min)
Min_Medium <- Norm_Medium %>% group_by(Model,Location, Time) %>% summarise_all(min)
Min_Small <- Norm_Small %>% group_by(Model,Location, Time) %>% summarise_all(min)

Max_Large <- Norm_Large %>% group_by(Model,Location, Time) %>% summarise_all(max)
Max_Medium <- Norm_Medium %>% group_by(Model,Location, Time) %>% summarise_all(max)
Max_Small <- Norm_Small %>% group_by(Model,Location, Time) %>% summarise_all(max)

Max_Morris_840 <- rbind(Max_Large,Max_Medium,Max_Small)
Min_Morris_840 <- rbind(Min_Large,Min_Medium,Min_Small)

Range_840_Total <- data.frame(Max_Morris_840 %>% select(Time,Total) %>% rename(Max=Total),Min_Morris_840 %>% ungroup()%>%select(Total) %>% rename(Min=Total))
Range_840_HVAC <- data.frame(Max_Morris_840 %>% select(Time,HVAC) %>% rename(Max=HVAC),Min_Morris_840 %>% ungroup()%>%select(HVAC) %>% rename(Min=HVAC))
Range_840_SPC <- data.frame(Max_Morris_840 %>% select(Time,SPC) %>% rename(Max=SPC),Min_Morris_840 %>% ungroup()%>%select(SPC) %>% rename(Min=SPC))

#SA indexes normalised

C_EE_840_Large_Nor <- Calculate_SA_MorrisIndex(
  Norm_Large , 
  Morris_Data_840_Large)

C_EE_840_Medium_Nor <- Calculate_SA_MorrisIndex_Small(
  Norm_Medium , 
  Morris_Data_840_Medium)

C_EE_840_Small_Nor <- Calculate_SA_MorrisIndex_Small(
  Norm_Small , 
  Morris_Data_840_Small)

Large_Nor_SA_Index <- Data_Manipulation_Morris_SA_Index(C_EE_840_Large_Nor) %>% mutate(Model = 'Large')
Medium_Nor_SA_Index <- Data_Manipulation_Morris_SA_Index(C_EE_840_Medium_Nor) %>% mutate(Model = 'Medium')
Small_Nor_SA_Index <- Data_Manipulation_Morris_SA_Index(C_EE_840_Small_Nor) %>% mutate(Model = 'Small')

All_SA_Index_Norm <- rbind(Large_Nor_SA_Index, Medium_Nor_SA_Index, Small_Nor_SA_Index)

#CV values
CV_840 <- Morris_Res_840 %>% 
  group_by(Location , Time, Model) %>%
  select(-(Other_Total:Chiller)) %>%
  summarise_all(cv) %>%
  ungroup() %>%
  gather(key = Var, value ='value', Total:SPC) %>%
  mutate(Time = replace(Time, Time=='AN', 'Annual'))

CV_840_Locations_Total <- CV_840 %>% 
  filter(Var == 'Total') %>%
  spread(Location,value)

CV_840_Locations_HVAC <- CV_840 %>% 
  filter(Var == 'HVAC') %>%
  spread(Location,value)

CV_840_Locations_SPC <- CV_840 %>% 
  filter(Var == 'SPC') %>%
  spread(Location,value)

##### Create summary matrix #####

folder <- '02_Output_Files/'
Out_file <- paste(folder,'/CV_Values_Summary.xls', sep="")  #Output File

write.xlsx(CV_840_Locations_Total, Out_file, sheetName="Total",append = FALSE)
write.xlsx(CV_840_Locations_HVAC, Out_file, sheetName="HVAC",append = TRUE)
write.xlsx(CV_840_Locations_SPC, Out_file, sheetName="SPC",append = TRUE)


folder <- '02_Output_Files/'
Out_file <- paste(folder,'/Base_Case_Values.xls', sep="")  #Output File

write.xlsx(Base_Results_HVAC, Out_file, sheetName="HVAC",append = FALSE)
write.xlsx(Base_Results_Total, Out_file, sheetName="Total",append = TRUE)

folder <- '02_Output_Files/'
Out_file <- paste(folder,'/840_sample_Result_Limit_Ranges.xls', sep="")  #Output File

write.xlsx(Range_840_Total , Out_file , sheetName = "Total")
write.xlsx(Range_840_HVAC , Out_file , sheetName = "HVAC", append=TRUE)
write.xlsx(Range_840_SPC , Out_file , sheetName = "SPC", append=TRUE)

##### Plots #####

#@CV plots
ggplot(CV_840 %>% filter(Model =='Large', Var =='Total'), 
       aes(x= Location, y = value, col = Time)) +
  geom_point()+
  facet_grid(Var ~ Model) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  ylab(expression('Coefficient of variation '~italic('(CV)'))) +
  theme(legend.position="top") +
  labs(color='Temporal resolution')
  
folder <- '04_Charts/840_Morris_All/'
fig_num  <- 'All_'
file_name <- 'CV_per_TimeResolution_Large_Total.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 20, height = 12, units = "cm")


ggplot(CV_840 %>% filter(Model =='Large'), 
       aes(x= Location, y = value, col = Var)) +
  geom_point() +
  facet_grid(Time ~ Model) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  ggtitle('CV Type of loads') +
  theme(legend.position="top")

folder <- '04_Charts/840_Morris_All/'
fig_num  <- 'All_'
file_name <- 'CV_per_Demand.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 15, units = "cm")

ggplot(CV_840, 
       aes(x= Location, y = value, col = Model)) +
  geom_point() +
  facet_grid(Var ~ Time) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  ggtitle('CV by Model') +
  theme(legend.position="top")

folder <- '04_Charts/840_Morris_All/'
fig_num  <- 'All_'
file_name <- 'CV_per_Model.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 18, units = "cm")

ggplot(CV_840, 
       aes(x= Model, y = value, col = Location)) +
  geom_point() +
  facet_grid(Var ~ Time) +
  ggtitle('CV by location') +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  ylab(expression('Coefficient of variation '~italic('(CV)'))) +
  theme(legend.position="top") +
  guides(colour = guide_legend(nrow = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- 'All_'
file_name <- 'CV_per_Location.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 18, units = "cm")

### Boxplots 

a <- ggplot( Morris_Res_840 %>% 
          select(-(Other_Total:Chiller)) %>%
          gather(key = Var, value ='value', Total:SPC) %>%
          filter(Time == 'Peak')
  ,
        aes(x = Location, y = value, fill = factor(Model))) + 
  geom_boxplot() +
  ylab(expression('[' ~ W.m^-2 ~ ']')) +
  #geom_point(data = Base_Results %>% filter(Var %in% c('Total','HVAC','SPC')), aes(x = Location,y = value, colour = Model))+
  facet_grid( Time ~ Var) +
  scale_fill_discrete(name = "Office types")
  

b <- ggplot( Morris_Res_840 %>% 
          select(-(Other_Total:Chiller)) %>%
          gather(key = Var, value ='value', Total:SPC) %>%
          filter(Time == 'AN') %>%
          mutate(Time=replace(Time, Time=='AN', 'Annual'))
        ,
        aes(x = Location, y = value, fill = factor(Model))) + 
  geom_boxplot() +
  ylab(expression('[' ~ kWh.m^-2 ~ ']')) +
  #geom_point(data = Base_Results %>% filter(Var %in% c('Total','HVAC','SPC')), aes(x = Location,y = value, colour = Model))+
  facet_grid( Time ~ Var) +
  scale_fill_discrete(name = "Office types")

text <- "Boxplot of demand for all model cases in Morris EE sample"
# Create a text grob
tgrob <- text_grob(text,size = 20)
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

g_Morris_Boxplot_All <- ggarrange(a,b, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
g_Morris_Boxplot_All <- annotate_figure(g_Morris_Boxplot_All,
                                     top = text_grob(text, color = "Black", rot = 0,x=0,just='left'))
folder <- '04_Charts/840_Morris_All/'
fig_num  <- 'All_'
file_name <- 'Boxplot.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 25, height = 15, units = "cm",g_Morris_Boxplot_All)

#Basecases

a <- ggplot(Base_Results_Stack %>% filter(!Var %in% c('HVAC', 'Total','SPC'), Time == 'AN') %>% mutate(Time=replace(Time, Time=='AN', 'Annual')) ,aes(x=Location,y=value))+
  geom_col(aes(fill = Var), width = 0.7) + 
  ylab(expression('[' ~ kWh.m^-2 ~ ']')) +
  facet_grid(Time ~ Model) +
  scale_fill_discrete(name = "End-uses", labels = c("Chiller", "Fans", "Heat Rejection","Non-HVAC","Pumps"))


b <- ggplot(Base_Results_Stack %>% filter(!Var %in% c('HVAC', 'Total','SPC'), Time == 'Peak'),aes(x=Location,y=value))+
  geom_col(aes(fill = Var), width = 0.7) + 
  ylab(expression('[' ~ W.m^-2 ~ ']')) +
  facet_grid(Time ~ Model) +
  scale_fill_discrete(name = "End-uses", labels = c("Chiller", "Fans", "Heat Rejection","Non-HVAC","Pumps"))


g_Morris_Base_Case_All <- ggarrange(a,b, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
g_Morris_Base_Case_All <- annotate_figure(g_Morris_Base_Case_All,
                                          top = text_grob("Total electricity demand for base case conditions by end-Use", color = "Black", rot = 0,x=0,just='left'))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_Base_Cases.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 16, height = 12, units = "cm")

### @ Save Morris Index Plots Normalised

ggplot(All_SA_Index_Norm %>% filter(Var == 'Total') %>% mutate(Time=replace(Time, Time=='AN', 'Annual')) ,
       aes(x = In_Par , y = U, group = Model, color = Model))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Location ~ Time) +
  ggtitle('Morris SI for total electricity demand by office type') +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Office type")

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_Total_ByOffice.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 15, height = 18, units = "cm")


ggplot(All_SA_Index_Norm %>% filter(Var == 'HVAC') %>% mutate(Time = replace(Time, Time=='AN', 'Annual')), 
       aes(x = In_Par , y = U, group = Model, color = Model))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Location ~ Time) +
  ggtitle('Morris for HVAC Demand by Office Type') +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Office Type') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_HVAC_ByOffice.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 16, height = 24, units = "cm")

ggplot(All_SA_Index_Norm %>% filter(Var == 'SPC') %>% mutate(Time = replace(Time, Time=='AN', 'Annual')), 
       aes(x = In_Par , y = U, group = Model, color = Model)) +
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Location ~ Time) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Office Type') +
  ggtitle('Morris for SPC') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_SPC_ByOffice.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 16, height = 24, units = "cm")

ggplot(AAll_SA_Index_Norm %>% filter(Time == 'Peak', Var %in% c('Total','HVAC','SPC')), 
       aes(x = In_Par , y = U, group = Model, color = Model))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Var ~ Location) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Office Type') +
  ggtitle('Morris for Peak Demand by Location') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_Peak_ByLocation.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")


ggplot(All_SA_Index_Norm %>% filter(Time == 'AN', Var %in% c('Total','HVAC','SPC')), 
       aes(x = In_Par , y = U, group = Model, color = Model))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Var ~ Location) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Office Type') +
  ggtitle('Morris for Annual Demand by Location') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_Annual_ByLocation.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")

ggplot(All_SA_Index_Norm %>% filter(Time == 'AN', Var %in% c('Total','HVAC','SPC')), 
       aes(x = In_Par , y = U, group = Location, color = Location))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Var ~ Model) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  ggtitle('Morris for Annual Demand by Location') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(colour = guide_legend(nrow = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_By_Location_Annual.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")

ggplot(All_SA_Index_Norm %>% filter(Time == 'Peak', Var %in% c('Total','HVAC','SPC')), 
       aes(x = In_Par , y = U, group = Location, color = Location))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Var ~ Model) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  ggtitle('Morris for Peak Demand by Location') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(colour = guide_legend(nrow = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'All_Morris_SI_By_Location_Peak.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")



ggplot(All_SA_Index_Norm %>% filter(Time == 'Peak', Var %in% c('Total','HVAC','SPC')), 
       aes(x = In_Par , y = U, group = Var, color = Var))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Model ~ Location) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Demand Variable') +
  ggtitle('Morris for Peak Demand by Demand Type') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'Var_All_Morris_SI_Peak_ByDemand.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")

ggplot(All_SA_Index_Norm %>% filter(Time == 'AN', Var %in% c('Total','HVAC','SPC')) %>% mutate(Time = replace(Time, Time=='AN', 'Annual')), 
       aes(x = In_Par , y = U, group = Var, color = Var))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Model ~ Location) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Demand Variable') +
  ggtitle('Morris for Annual Demand by Demand Type') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'Var_All_Morris_SI_Annual_byDemand.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")

ggplot(All_SA_Index_Norm %>% filter( Var == 'Total') %>% mutate(Time = replace(Time, Time=='AN', 'Annual')), 
       aes(x = In_Par , y = U, group = Time, color = Time))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Model ~ Location) +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  labs(color = 'Temporal Resolution') +
  ggtitle('Annual Morris by Time -  Total Demand') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'Var_All_Morris_SI_Total_ByTime.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 24, height = 16, units = "cm")

#Example for presentation
ggplot(All_SA_Index_Norm %>% filter(Var == 'Total', Location =='C1') 
       %>% mutate(Time=replace(Time, Time=='AN', 'Annual')) ,
       aes(x = In_Par , y = U, group = Model, color = Model))+
  geom_point(size = 1)+
  geom_line(size = 0.1) + 
  facet_grid(Location ~ Time) +
  ggtitle('Morris SI for total electricity demand by office type') +
  ylab(expression(italic(paste(mu,"*")))) + 
  xlab('Parameter') +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Office type")

folder <- '04_Charts/840_Morris_All/'
fig_num  <- ''
file_name <- 'Ex_Presentation_Morris.png' #Output File
file <- paste(folder , fig_num , file_name , sep='')

ggsave(file ,width = 20, height = 6, units = "cm")

