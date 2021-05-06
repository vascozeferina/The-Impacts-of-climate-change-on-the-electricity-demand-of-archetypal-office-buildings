# Upload Results
rm(list=ls())
gc()

mypackages<-c("boot",
              "dplyr", 
              "devtools",
              "data.table",
              "eplusr",
              "fs",
              "grid",
              "gdata",
              "ggpubr",
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

########## FUnction ####################
Generate_Test_Plot <- function(Test,Var_Ratio){
  
  if(Var_Ratio=='R_P_TOT'){
    Title_Name='Peak - Total'
  } else if(Var_Ratio=='R_A_TOT'){
    Title_Name='Annual - Total'
  } else if(Var_Ratio=='R_A_HVAC'){
    Title_Name='Annual - HVAC'
  } else if(Var_Ratio=='R_P_HVAC'){
    Title_Name='Peak - HVAC'
  }
  
  # 
  # Plot_S<-ggplot(Test%>%filter(Model=='Small'),aes(x=factor(Test),y=!!sym(Var_Ratio),group=Location,col=Location))+
  #   geom_point()+
  #   scale_y_continuous(labels=scales::percent) +
  #   scale_y_continuous(name = "Ratio")+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #   ggtitle('Small')+
  #   xlab('Test')+
  #   theme(legend.position = "none") 
  # 
  # Plot_M<-ggplot(Test%>%filter(Model=='Medium'),aes(x=factor(Test),y=!!sym(Var_Ratio),group=Location,col=Location))+
  #   geom_point()+
  #   scale_y_continuous(name = "Ratio")+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #   ggtitle('Medium')+
  #   xlab('Test')+
  #   theme(legend.position = "none") 
  # 
  # Plot_L<-ggplot(Test%>%filter(Model=='Large'),aes(x=factor(Test),y=!!sym(Var_Ratio),group=Location,col=Location))+
  #   geom_point()+
  #   scale_y_continuous(name = "Ratio")+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #   ggtitle('Large')+
  #   xlab('Test')+
  #   theme(legend.position = "none") 
  # 
  # Plot_Save <- grid.arrange(Plot_L,Plot_M,Plot_S, ncol=3, nrow=1,
  #                         top = textGrob(Title_Name,gp=gpar(fontsize=20,font=3)))
  # 
  Plot_Gen <- ggplot(Test,aes(x=factor(Test),y=!!sym(Var_Ratio),group=Location,col=Location))+
    geom_point()+
    facet_grid(~Model)+
    scale_y_continuous(labels=scales::percent) +
    ylab('Change to baseline') + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(Title_Name)+
    xlab('Test')+
    theme(legend.position = "top") +
    guides(colour = guide_legend(nrow = 1))
  
   print(Plot_Gen)
   
   return(Plot_Gen)
}

Plot_LSA <- function(Test_Name,Var_X){
  
  
  ####@@@@ Make this double Plot (Peak and Annual)@@@@
  Table_Results <- Short_Results %>% 
    select(Location,Model,Test,AN_T:SR_Dif_T,R_P_TOT,R_A_TOT)%>%
    filter(Test==!!sym('Test_Name'))
  
  Plot_Save_PK <- ggplot(Table_Results,aes(x=!!sym(Var_X),y=R_P_TOT,group=Model,col=Model))+
    geom_point()+
    facet_grid(~ Location)+
    ggtitle('Peak')+
    xlab(Var_X) +
    ylab('Normalised to standard condition') +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    theme(legend.position="none")
  
  #file<-'04_Charts/03_LSA/Test_5_Peak.png'
  #ggsave(file,width = 30, height = 15, units = "cm",Plot_Save)
  
  print(Plot_Save_PK)
  
  Plot_Save_AN <- ggplot(Table_Results,aes(x=!!sym(Var_X),y=R_A_TOT,group=Model,col=Model))+
    geom_point() +
    facet_grid(~Location) +
    xlab(Var_X) +
    ylab('Normalised to standard condition') +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    ggtitle('Annual') +
    theme(legend.position="bottom")
  
  
  Plot_Save <- grid.arrange(Plot_Save_PK,Plot_Save_AN, ncol=1, nrow=2,
                          top = textGrob(Test_Name,gp=gpar(fontsize=20,font=3)))
  
  print(Plot_Save)
  
  file_path <- '04_Charts/'
  file_name <- paste('Progression',Test_Name, '.png',sep='_')
  ggsave(file_name,path = file_path,width = 30, height = 15, units = "cm",Plot_Save)
  
}


##################### End Function ############################

setwd("C:/Users/mbgnwvz2/Dropbox (The University of Manchester)/01_PhD Research/14_Repository/05-Weather_LSA/02_Results_Analysis")

######################    Data upload - Data frames ########################
in_file <- "01_Inputs/01_Simulation_Results/850_LSA_Results_V4.csv"
Table_Par <- read.csv(in_file, header = TRUE, sep = ",")
Table_Par <- data.frame(Table_Par)

Large_A <- 46320.38
Medium_A <- 4982.2
Small_A <- 511.16

Results_An <- Table_Par %>%
  mutate(ModelFile = case_when(
    Table_Par$ModelFile == "Large_Office_DOE_JP1_Sizing_File.imf" ~ "Large",
    Table_Par$ModelFile == "Medium_Office_DOE_JP1_Sizing_File.imf" ~ "Medium",
    Table_Par$ModelFile == "Small_Office_DOE_JP1_Sizing_File.imf" ~ "Small"))%>%
  rename(Model=ModelFile)%>%
  mutate(Location = case_when(
    str_detect(Table_Par$WeatherFile,"SGP_") ~ "C1",
    str_detect(Table_Par$WeatherFile,"EGY_") ~ "C2",
    str_detect(Table_Par$WeatherFile,"GRC_") ~ "C3",
    str_detect(Table_Par$WeatherFile,"CHN_") ~ "C4",
    str_detect(Table_Par$WeatherFile,"PRT_") ~ "C5",
    str_detect(Table_Par$WeatherFile,"GBR_") ~ "C6"))%>%
  mutate(Test =  case_when(
    str_detect(Job_ID,"Mean_T") ~ "01-Mean_T",
    str_detect(Job_ID,"02_R_") ~ "02-Rm",
    str_detect(Job_ID,"03_R_HW") ~ "03-HW",
    str_detect(Job_ID,"_WSpeed_") ~ "04-WS",
    str_detect(Job_ID,"_RH_Ch_NEG") ~ "05_1-RH",
    str_detect(Job_ID,"_RH_Ch_POS") ~ "05_2-RH",
    str_detect(Job_ID,"_RH_R_Ch_NEG") ~ "06_1-RH_R",
    str_detect(Job_ID,"_RH_R_Ch_POS") ~ "06_2-RH_R",
    str_detect(Job_ID,"_SR_HIR_") ~ "07-SR_HIR",
    str_detect(Job_ID,"_SR_Dir_Ch") ~ "08-SR_Dir",
    str_detect(Job_ID,"_SR_Dif_Ch") ~ "09-SR_Dif",
    str_detect(Job_ID,"_SR_Dir_TRU_") ~ "10-SR_Dir_T",
    str_detect(Job_ID,"_SR_Dif_TRU_") ~ "11-SR_Dif_T"))%>%
  mutate(AN_T = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=AN_T-)[:digit:]*.*[:digit:]*(?=_Rm)")))%>%
  mutate(Rm = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=Rm-)[:digit:]*.*[:digit:]*(?=_Rd)")))%>%
  mutate(Rd = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=Rd-)[:digit:]*.*[:digit:]*(?=.epw)")))%>%
  mutate(RH = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=_RH_Ch-).?[:digit:]*")))%>%
  mutate(RH_R = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=_R_Ch-)[:digit:]*.*[:digit:]*(?=.epw)")))%>%
  mutate(WS = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=WSpeed_Ch-)[:digit:]*.[:digit:]*")))%>%
  mutate(SR_HIR = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=SR_HIR_Ch-)[:digit:]*.*[:digit:]*(?=_R_Ch)")))%>%
  mutate(SR_Dir = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=SR_Dir_Ch-)[:digit:]*.*[:digit:]*(?=_R_Ch)")))%>%
  mutate(SR_Dif = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=SR_Dif_Ch-)[:digit:]*.*[:digit:]*(?=_R_Ch)")))%>%
  mutate(SR_Dir_T = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=_SR_Dir_TRUN_Ch-)[:digit:]*.*[:digit:]*(?=_R_Ch)")))%>%
  mutate(SR_Dif_T = as.numeric(str_extract(Table_Par$WeatherFile,"(?<=_SR_Dif_TRUN_Ch-)[:digit:]*.*[:digit:]*(?=_R_Ch)")))%>%
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
  #mutate(P1_Jun = c28..P1_June) %>%
  #mutate(P1_Jul = c37: P1_July) %>%
  #mutate(P1_Aug = c46: P1_August) %>%
  #mutate(PHVAC = P2+P3+P4+P5+P6+P7) %>%
  #mutate(AHVAC = A2+A3+A4+A5+A6+A7) %>%
  #mutate(A1 = A1-A8) %>%
  #mutate(PNONHVAC = P1-PHVAC) %>%
  #mutate(ANONHVAC = A1-AHVAC) %>%
  rename('P1_June'=str_subset(names(Table_Par),'P1_June'),
         'P2_June'=str_subset(names(Table_Par),'P2_June'),
         'P3_June'=str_subset(names(Table_Par),'P3_June'),
         'P4_June'=str_subset(names(Table_Par),'P4_June'),
         'P5_June'=str_subset(names(Table_Par),'P5_June'),
         'P6_June'=str_subset(names(Table_Par),'P6_June'),
         'P7_June'=str_subset(names(Table_Par),'P7_June'),
         'P8_June'=str_subset(names(Table_Par),'P8_June'),
         'P1_July'=str_subset(names(Table_Par),'P1_July'),
         'P2_July'=str_subset(names(Table_Par),'P2_July'),
         'P3_July'=str_subset(names(Table_Par),'P3_July'),
         'P4_July'=str_subset(names(Table_Par),'P4_July'),
         'P5_July'=str_subset(names(Table_Par),'P5_July'),
         'P6_July'=str_subset(names(Table_Par),'P6_July'),
         'P7_July'=str_subset(names(Table_Par),'P7_July'),
         'P8_July'=str_subset(names(Table_Par),'P8_July'),
         'P1_Aug'=str_subset(names(Table_Par),'P1_August'),
         'P2_Aug'=str_subset(names(Table_Par),'P2_August'),
         'P3_Aug'=str_subset(names(Table_Par),'P3_August'),
         'P4_Aug'=str_subset(names(Table_Par),'P4_August'),
         'P5_Aug'=str_subset(names(Table_Par),'P5_August'),
         'P6_Aug'=str_subset(names(Table_Par),'P6_August'),
         'P7_Aug'=str_subset(names(Table_Par),'P7_August'),
         'P8_Aug'=str_subset(names(Table_Par),'P8_August'))

Test <- Results_An%>%
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
  select(Location,Model,AN_T:SR_Dif_T,Test,P1:P8,A1:A8)
  
#Results_An[, "P1"] <- apply(Results_An[, 34:36], 1, max)         #@Check a way to do with name of columns
Test2 <- Test %>%
  mutate(PHVAC = P2+P3+P4+P5+P6+P7) %>%
  mutate(AHVAC = A2+A3+A4+A5+A6+A7) %>%
  mutate(A1 = A1-A8) %>%
  mutate(PNONHVAC = P1-PHVAC) %>%
  mutate(ANONHVAC = A1-AHVAC)

Results <- Test2 %>%
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

Short_Results <- Results%>%select(-A2,-A3,-A4,-A5,-A6,-A7,-A8,-P2,-P3,-P4,-P5,-P6,-P7,-P8)%>%
  relocate(WS, .after = Rd)%>%
  mutate(PTOT=P1)%>%
  mutate(ATOT=A1)

Short_Results <- Short_Results%>%arrange(Test,Location,Model,AN_T,Rm,Rd,WS,RH,RH_R,SR_HIR,SR_Dir,SR_Dif,SR_Dir_T,SR_Dif_T)

Short_Results <- Short_Results%>%mutate(Central = case_when(Test == '01-Mean_T' &  AN_T == 0 ~ "Central",
                                                          Test == '01-Mean_T' &  AN_T != 0 ~ "NCentral",
                                                          Test == '02-Rm' &  Rm == 1 ~ "Central",
                                                          Test == '02-Rm' &  Rm != 1 ~ "NCentral",
                                                          Test == '03-HW' &  Rd == 1 ~ "Central",
                                                          Test == '03-HW' &  Rd != 1 ~ "NCentral",
                                                          Test == '04-WS' &  WS == 1 ~ "Central",
                                                          Test == '04-WS' &  WS != 1 ~ "NCentral",
                                                          Test == '05_1-RH' &  RH == 0 ~ "Central",
                                                          Test == '05_1-RH' &  RH != 0 ~ "NCentral",
                                                          Test == '05_2-RH' &  RH == 0 ~ "Central",
                                                          Test == '05_2-RH' &  RH != 0 ~ "NCentral",
                                                          Test == '06_1-RH_R' &  RH_R == 1 ~ "Central",
                                                          Test == '06_1-RH_R' &  RH_R != 1 ~ "NCentral",
                                                          Test == '06_2-RH_R' &  RH_R == 1 ~ "Central",
                                                          Test == '06_2-RH_R' &  RH_R != 1 ~ "NCentral",
                                                          Test == '07-SR_HIR' &  SR_HIR == 1 ~ "Central",
                                                          Test == '07-SR_HIR' &  SR_HIR != 1 ~ "NCentral",
                                                          Test == '08-SR_Dir' &  SR_Dir == 1 ~ "Central",
                                                          Test == '08-SR_Dir' &  SR_Dir != 1 ~ "NCentral",
                                                          Test == '09-SR_Dif' &  SR_Dif == 1 ~ "Central",
                                                          Test == '09-SR_Dif' &  SR_Dif != 1 ~ "NCentral",
                                                          Test == '10-SR_Dir_T' &  SR_Dir_T == 1 ~ "Central",
                                                          Test == '10-SR_Dir_T' &  SR_Dir_T != 1 ~ "NCentral",
                                                          Test == '11-SR_Dif_T' &  SR_Dif_T == 1 ~ "Central",
                                                          Test == '11-SR_Dif_T' &  SR_Dif_T != 1 ~ "NCentral",))

levels(factor(Short_Results$Test))

Short_Results <- Short_Results %>%
  #filter(Test!='10-SR_Dir_T' & Test!='11-SR_Dif_T')%>%
  group_by(Location,Model,Test) %>%
  mutate(R_P_TOT = PTOT/PTOT[Central == "Central"]-1)%>%
  mutate(R_P_HVAC = PHVAC/PHVAC[Central == "Central"]-1)%>%
  mutate(R_P_NONHVAC = PNONHVAC/PNONHVAC[Central == "Central"]-1)%>%
  mutate(R_A_TOT = ATOT/ATOT[Central == "Central"]-1)%>%
  mutate(R_A_HVAC = AHVAC/AHVAC[Central == "Central"]-1)%>%
  mutate(R_A_NONHVAC = ANONHVAC/ANONHVAC[Central == "Central"]-1)
  
Sum_Tab <- Short_Results %>%
  group_by(Location,Model,Test) %>%
  summarise_all(max) %>%
  arrange(Model,Location,Test)

# Sum_Tab2<-Short_Results%>%
#   filter(Test %in% c('05_1-RH','06_1-RH_R'))%>%
#   group_by(Location,Model,Test) %>%
#   summarise_all(min)%>%
#   arrange(Model,Location,Test)


############# Create Data samples ################

Test_Res <- Sum_Tab %>%
  filter(!Test %in% c('05_2-RH','06_2-RH_R','10-SR_Dir_T','11-SR_Dif_T')) %>%
  select(Location,Model,Test,R_P_TOT,R_P_HVAC,R_A_TOT,R_A_HVAC) %>%
  mutate(Test = replace(Test, Test=='01-Mean_T', '01')) %>%
  mutate(Test = replace(Test, Test=='02-Rm', '02')) %>%
  mutate(Test = replace(Test, Test=='03-HW', '03')) %>%
  mutate(Test = replace(Test, Test=='04-WS', '04')) %>%
  mutate(Test = replace(Test, Test=='05_1-RH', '05')) %>%
  mutate(Test = replace(Test, Test=='06_1-RH_R', '06')) %>%
  mutate(Test = replace(Test, Test=='07-SR_HIR', '07')) %>%
  mutate(Test = replace(Test, Test=='08-SR_Dir', '08')) %>%
  mutate(Test = replace(Test, Test=='09-SR_Dif', '09'))


######## Save Plots ##############

#Automatic @Improve saving and titles

Plot_LSA('01-Mean_T','AN_T')
Plot_LSA('02-Rm','Rm')
Plot_LSA('03-HW','Rd')
Plot_LSA('04-WS','WS')
Plot_LSA('05_1-RH','RH')
Plot_LSA('05_2-RH','RH')
Plot_LSA('06_1-RH_R','RH_R')
Plot_LSA('06_2-RH_R','RH_R')
Plot_LSA('08-SR_Dir','SR_Dir')
Plot_LSA('09-SR_Dif','SR_Dif')
Plot_LSA('07-SR_HIR','SR_HIR')
Plot_LSA('10-SR_Dir_T','SR_Dir_T')
Plot_LSA('11-SR_Dif_T','SR_Dif_T')

#Other plots

Note <- '01 - Shift in mean annual DBT     02 - Seasonal summer ratio in DBT      03 - Heatwave stretch in DBT \n04 - Stretch of WS   05 - Shift of RH      06 - Seasonal summer ratio in RH \n07 - Stretch of HIR   08 - Stretch of DNR   09 - Stretch of DHR'

a <- Generate_Test_Plot(Test_Res,'R_A_TOT') + 
  ggtitle('Maximum effect of LSA tests on annual total electricity demand') + 
  labs(caption = Note ) +
  theme(plot.caption = element_text(hjust = 0, face = "italic" ,  size = 11))# move caption to the left

file_path <- '04_Charts/03_LSA'
file_name <- 'Annual_Total_LSA.png'
ggsave(file_name,path = file_path,width = 20, height = 12, units = "cm",a)

b <- Generate_Test_Plot(Test_Res,'R_P_TOT') + 
  ggtitle('Maximum effect of LSA tests on peak total electricity demand') + 
  labs(caption = Note ) +
  theme(plot.caption = element_text(hjust = 0, face = "italic" ,  size = 11))# move caption to the left

file_path<-'04_Charts/'
file_name<-'Peak_Total_LSA.png'
ggsave(file_name,path = file_path,width = 20, height = 12, units = "cm",b)

Generate_Test_Plot(Test_Res,'R_P_HVAC')
Generate_Test_Plot(Test_Res,'R_A_HVAC')

#Solar Radiation

Test_Res <- Sum_Tab %>%
  filter(Test %in% c('07-SR_HIR','08-SR_Dir','09-SR_Dif'))%>%
  select(Location,Model,Test,R_P_TOT,R_P_HVAC,R_A_TOT,R_A_HVAC) %>%
  mutate(Test = replace(Test, Test=='07-SR_HIR', '07 - HIR')) %>%
  mutate(Test = replace(Test, Test=='08-SR_Dir', '08 - DNR')) %>%
  mutate(Test = replace(Test, Test=='09-SR_Dif', '09 - DHR'))


b <- ggplot(Test_Res,aes(x=Model,y=R_P_TOT,col=Location))+
  geom_point() +
  xlab('Office type') +
  ggtitle('Effect to peak total electricity demand') +
  ylab('Change to baseline') + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(.~Test) +
  theme(legend.position="none")

c <- ggplot(Test_Res,aes(x=Model,y=R_A_TOT,col=Location))+
  geom_point() +
  ggtitle('Effect to annual total electricity demand') +
  xlab('Office type') +
  ylab('Change to baseline') + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(.~Test) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = guide_legend(nrow = 1))

Ex_Figure <- ggarrange(b,c,nrow=2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(c))

Plot_Ex <- annotate_figure(Ex_Figure , 
                           top = text_grob("Changes in solar irradiance variables", face = "bold", size = 14))

file_path<-'04_Charts/'
file_name<-'Total_Solar_LSA.png'
ggsave(file_name,path = file_path,width = 20, height = 12, units = "cm")

#Test 5 in detail for relative humidity

Table_Results <- Short_Results%>%select(Location,Model,RH,Test,R_P_TOT,R_A_TOT)%>%
  filter(RH!='NA')%>%
  filter(RH!=-5)%>%
  filter(RH!=5)%>%
  gather(key=Demand,value=value,R_P_TOT:R_A_TOT)%>%
  mutate(Demand=case_when(
    Demand=='R_P_TOT' ~ 'Peak',
    Demand=='R_A_TOT' ~ 'Annual'))

Plot_Save <- ggplot(Table_Results,aes(x=RH,y=value,group=Model,col=Model))+
  geom_point() +
  facet_grid(Demand~ Location) +
  ggtitle('Effect on total electricity demand for a wider range of shift values for relative humidity variable') +
  xlab('Shift change on relative humidity [%]') +
  ylab('Change of total elect. dem. to baseline') + 
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position = "top") +
  labs(col = 'Office type') +
  guides(colour = guide_legend(nrow = 1))

file<-'04_Charts/Test_5_Demand.png'
ggsave(file,width = 22, height = 12, units = "cm",Plot_Save)



######## End of automatic save of plots ##################



##### Summary Tables #######

Ratio_AN_Sum <- Sum_Tab%>%
       select(Location,Model,Test,R_A_TOT)%>%
       spread(Test,R_A_TOT)%>%
       arrange(Model,Location)

Ratio_AN_Sum2 <- Sum_Tab2%>%
  select(Location,Model,Test,R_A_TOT)%>%
  spread(Test,R_A_TOT)%>%
  arrange(Model,Location)

Ratio_AN_Sum$`05_1-RH` <- Ratio_AN_Sum2$`05_1-RH`
Ratio_AN_Sum$`06_1-RH_R` <- Ratio_AN_Sum2$`06_1-RH_R`

Ratio_AN_Sum-1

Ratio_PK_Sum<-Sum_Tab%>%
       select(Location,Model,Test,R_P_TOT)%>%
       spread(Test,R_P_TOT)%>%
       arrange(Model,Location)%>%
       mutate(DeltaDir=`08-SR_Dir`-`10-SR_Dir_T`)%>%
        mutate(DeltaDif=`09-SR_Dif`-`11-SR_Dif_T`)

Ratio_PK_Sum$`10-SR_Dir_T`

Ratio_PK_Sum

Ratio_PK_Sum2<-Sum_Tab2%>%
       select(Location,Model,Test,R_P_TOT)%>%
       spread(Test,R_P_TOT)%>%
        arrange(Model,Location)

Ratio_PK_Sum_3<-Ratio_PK_Sum

Ratio_PK_Sum_3$`05_1-RH`<-Ratio_PK_Sum2$`05_1-RH`
Ratio_PK_Sum_3$`06_1-RH_R`<-Ratio_PK_Sum2$`06_1-RH_R`

file_name <- '02_Output_Files/Summary_LSA_Ratios.xlsx'
Ratio_PK_Sum_3 <- as.data.table(Ratio_PK_Sum_3)
Ratio_AN_Sum <- as.data.table(Ratio_AN_Sum)
write.xlsx(Ratio_PK_Sum_3, file = file_name, sheetName = "Peak_Ratio", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx2(Ratio_AN_Sum, file = file_name, sheetName = "AN_Ratio", col.names = TRUE, row.names = TRUE, append = TRUE)









