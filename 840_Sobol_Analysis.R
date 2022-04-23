rm(list=ls())
gc()

library(readxl)
library(sensitivity)
library(boot)
library(writexl)
library(readxl)
library(sensitivity)
library(boot)
library(writexl)
library(boot)
library(gdata)
library(readxl)
library(xlsx)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
library(devtools)
library(sjstats)
library(tidyr)
library(tidyverse)
library(gtable)
library(ggpubr)
library(ggpubr)

setwd("C:/Users/mbgnwvz2/Dropbox (The University of Manchester)/01_PhD Research/01_Simulation/01_Simulation Files & Results/03.JP1/R-Files/JP1_R_Analysis")

#IMPORT FILES
In_Folder <- "01_Inputs/02_Simulation_Sampling/"

File_nameX1 <- paste(In_Folder,"Sobol_6P_n2500_X1_Sample",".csv",sep="")     #Input file
File_nameX2 <- paste(In_Folder,"Sobol_6P_n2500_X2_Sample",".csv",sep="")   #Input file
X1 <- read.csv(File_nameX1,header=FALSE)
X2 <- read.csv(File_nameX2,header=FALSE)

n=2500
p=6

List_of_Par<-c('P05','P06','P07','P08','P10','P12')
colnames(X1)<-List_of_Par
colnames(X2)<-List_of_Par

xSoboljansen <- soboljansen(model = NULL, X1, X2, nboot = n, conf = 0.95)
#xfast99 <- fast99(model = NULL, factors=6, n=250, M = 4, omega = NULL, q = NULL, q.arg = NULL)
xSobol2007 <- sobol2007(model = NULL, X1, X2, nboot = n, conf = 0.95)
xSobolmartinez <- sobolmartinez(model = NULL, X1, X2, nboot = n, conf = 0.95)

###### FUNCTIONS ############

Res_Gather <- function(Climate){
  in_folder <- "01_Inputs/01_Simulation_Results/"
  in_file_Par <- paste(in_folder,Climate,"_Sobol_AllCombinedResults.csv", sep="")     #Input file
  Table_Par <- read.csv(in_file_Par, header = TRUE, sep = ",")
  Table_Par <- Table_Par[order(Table_Par$Job_ID),]
  
  Out_AN<-Table_Par%>%
    mutate(Total = c0..A1_T/c33..O1) %>%
    mutate(Pumps = c8..A9_Pumps/c33..O1) %>%
    mutate(Heat_Rej = c9..A10_HR/c33..O1) %>%
    mutate(Fans = c7..A8_Fans/c33..O1) %>%
    mutate(Chiller = c2..A3_Cool/c33..O1) %>%
    mutate(HVAC = Pumps+Heat_Rej+Fans+Chiller) %>%
    mutate(Other_Total = Total - HVAC) %>%
    mutate(SPC1=-c34..SPC_A_01_SensAC/c33..O1*277.777) %>%
    mutate(SPC2=c35..SPC_A_02_CoolEN_Tran/c33..O1*277.777) %>%
    mutate(SPC3=c36..SPC_A_03_Cool_Coils/c33..O1*277.777)  %>%
    mutate(Time='AN') %>%
    mutate(Location= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Sobol') %>%
    select(Sen_Met,Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC1,SPC2,SPC3)
  
  Out_Peak<-Table_Par%>%
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
    mutate(Location= substr(Job_ID,1, 2)) %>%
    mutate(Sen_Met= 'Sobol') %>%
    select(Sen_Met,Location,Time,Total,Other_Total,Pumps,Heat_Rej,Fans,Chiller,HVAC,SPC1,SPC2,SPC3)
  
  Out<-rbind(Out_AN,Out_Peak)
  
  return(Out)
}

Sobol_Indice_Res<-function(Climate,Period,End_Use){
  Column <- as.matrix(Sobol_Res%>%
    filter(Location==Climate)%>%
    filter(Time==Period)%>%
    select(End_Use))
  
  Out<-tell(xSobolmartinez, Column[,End_Use])
  
  return(Out)
}

#######Function for Sobol Dataframe Indices ##############
Sobol_Indices_Fun <- function(End_Use){
  Out<-data.frame(Loc_Col,Sobol_In_Par_Header,rep(End_Use,each=6*n_Cli*n_Time),Time_Col,
                  c(get(paste('saC1_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC2_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC3_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC4_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC5_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC6_',End_Use,'_AN',sep=''))$T$original,
                    get(paste('saC1_',End_Use,'_Peak',sep=''))$T$original,
                    get(paste('saC2_',End_Use,'_Peak',sep=''))$T$original,
                    get(paste('saC3_',End_Use,'_Peak',sep=''))$T$original,
                    get(paste('saC4_',End_Use,'_Peak',sep=''))$T$original,
                    get(paste('saC5_',End_Use,'_Peak',sep=''))$T$original,
                    get(paste('saC6_',End_Use,'_Peak',sep=''))$T$original),
                  c(get(paste('saC1_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC2_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC3_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC4_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC5_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC6_',End_Use,'_AN',sep=''))$S$original,
                    get(paste('saC1_',End_Use,'_Peak',sep=''))$S$original,
                    get(paste('saC2_',End_Use,'_Peak',sep=''))$S$original,
                    get(paste('saC3_',End_Use,'_Peak',sep=''))$S$original,
                    get(paste('saC4_',End_Use,'_Peak',sep=''))$S$original,
                    get(paste('saC5_',End_Use,'_Peak',sep=''))$S$original,
                    get(paste('saC6_',End_Use,'_Peak',sep=''))$S$original))
  
  Header_Col<-c('Loc','In_Var','Var','Time','ST','SS')
  names(Out)<-Header_Col
  
  return(Out)
}

#Apply Functions to Upload Data from each location

Sobol_C1_Res <- Res_Gather('C1')
Sobol_C2_Res <- Res_Gather('C2')
Sobol_C3_Res <- Res_Gather('C3')
Sobol_C4_Res <- Res_Gather('C4')
Sobol_C5_Res <- Res_Gather('C5')
Sobol_C6_Res <- Res_Gather('C6')

Sobol_Res <- rbind(Sobol_C1_Res,Sobol_C2_Res,Sobol_C3_Res,Sobol_C4_Res,Sobol_C5_Res,Sobol_C6_Res)

Climate_List <- c("C1","C2","C3","C4","C5","C6")
Period_List <- c("AN","Peak")
End_Use_List <- c("Total","Other_Total","Pumps","Heat_Rej","Fans","Chiller","HVAC","SPC1","SPC2","SPC3")

########Apply Sobol calculations#########

#End_Use_List<-c("Total")
for (Cli in Climate_List)
{
  for (End_Use in End_Use_List)
  {
    for (Period in Period_List)
    {
      assign(paste('sa',Cli,'_',End_Use,'_',Period,sep=''),Sobol_Indice_Res(Cli,Period,End_Use)) 
    }
  }
}

#####################################

n_Cli <- 6
n_Time <- 2
Sobol_In_Par_Header <- rep(c('P05','P06','P07','P08','P10','P12'),n_Cli*n_Time)
Loc_Col <- rep(rep(c('C1','C2','C3','C4','C5','C6'),each=6),2)
Time_Col <- rep(c('AN','Peak'),each=6*n_Cli)

Sobol_Indices <- {}

for (End_Use in End_Use_List){
  Test <- Sobol_Indices_Fun(End_Use)
  Sobol_Indices <- rbind(Sobol_Indices,Test)
}

##################PLOT##############

Sobol_Indices <- Sobol_Indices %>% rename(Location = Loc)  
  
  
  
  #####@@@@ CREATE SOBOL_IND ############
  #####@@@@ Save new files
  #####@@@@ Delete Remaining
  
  
  a<-ggplot(Sobol_Indices%>%filter(Var %in% c('Total'),Time=='AN'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = TRUE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free") + 
    theme(legend.position="right")
  
  a_ord<-ggplot(Sobol_Indices%>%filter(Var %in% c('Total'),Time=='AN'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = TRUE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free") + 
    theme(legend.position="right")
    
  b<-ggplot(Sobol_Indices%>%filter(Var %in% c('HVAC'),Time=='AN'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
    
  
  b_ord<-ggplot(Sobol_Indices%>%filter(Var %in% c('HVAC'),Time=='AN'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  c<-ggplot(Sobol_Indices%>%filter(Var %in% c('SPC1'),Time=='AN')%>%mutate(Var='Space Cooling'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  c_ord<-ggplot(Sobol_Indices%>%filter(Var %in% c('SPC1'),Time=='AN')%>%mutate(Var='Space Cooling'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  plot_to_save<-grid.arrange(a+theme(legend.position='hidden'),
                             b+theme(legend.position='hidden'),
                             c+theme(legend.position='hidden'), 
                             top=text_grob(label = 'Annual',x=0,just="left"),
                             ncol=3,
                             right=g_legend(a))
  
  
  ########### Paper Graph - Sobol Indexes - Fig 9 ###########                             
  folder <- '04_Charts/01_Paper/'
  
  fig_num  <- 'Fig_9_'
  file_name <- 'ST_Annual_Demand.png'  #Output File
  
  file <- paste(folder , fig_num , file_name , sep='')
  ggsave(file,width = 20, height = 8, units = "cm",plot_to_save)
  
  plot_to_save<-grid.arrange(a_ord+theme(legend.position='hidden'),
                             b_ord+theme(legend.position='hidden'),
                             c_ord+theme(legend.position='hidden'), 
                             top=text_grob(label = 'Annual',x=0,just="left"),
                             ncol=3,
                             right=g_legend(a_ord))
  
  
  folder <- '04_Charts/'
  file <- 'ST_Annual_Demand_Ordered.png'   #Output File
  ggsave(paste(folder,file,sep=''),width = 20, height = 8, units = "cm",plot_to_save)
  
########## Peak PLOTS##################  
  a <- ggplot(Sobol_Indices%>%filter(Var %in% c('Total'),Time=='Peak'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    #geom_line(size=0.1,show.legend = TRUE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free") + 
    theme(legend.position="right")
  
  a_ord <- ggplot(Sobol_Indices%>%filter(Var %in% c('Total'),Time=='Peak'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = TRUE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free") + 
    theme(legend.position="right")
  
  b <- ggplot(Sobol_Indices%>%filter(Var %in% c('HVAC'),Time=='Peak'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    #geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  
  b_ord<-ggplot(Sobol_Indices%>%filter(Var %in% c('HVAC'),Time=='Peak'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  c<-ggplot(Sobol_Indices%>%filter(Var %in% c('SPC1'),Time=='Peak')%>%mutate(Var='Space Cooling'), aes(x = In_Var, ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    #geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  c_ord<-ggplot(Sobol_Indices%>%filter(Var %in% c('SPC1'),Time=='Peak')%>%mutate(Var='Space Cooling'), aes(x = reorder(In_Var,-ST), ST,group=Location,colour=Location)) + 
    geom_point(size=2)+
    geom_line(size=0.1,show.legend = FALSE) +
    ylab(expression(S[T]))+  
    xlab('Parameter')+
    facet_wrap(~Var, scale="free")
  
  plot_to_save <- grid.arrange(a+theme(legend.position='hidden'),
                             b+theme(legend.position='hidden'),
                             c+theme(legend.position='hidden'), 
                             top=text_grob(label = 'Peak',x=0,just="left"),
                             ncol=3,
                             right=g_legend(a))
  
########### Paper Graph - Sobol Indexes - Fig 10 ###########
  folder <- '04_Charts/01_Paper/'
  
  fig_num  <- 'Fig_10_'
  file_name <- 'ST_Peak_Demand.png'    #Output File
  
  file <- paste(folder , fig_num , file_name , sep='')
  
  ggsave(file ,width = 20, height = 8, units = "cm",plot_to_save)
  
  plot_to_save<-grid.arrange(a_ord+theme(legend.position='hidden'),
                             b_ord+theme(legend.position='hidden'),
                             c_ord+theme(legend.position='hidden'), 
                             top=text_grob(label = 'Peak',x=0,just="left"),
                             ncol=3,
                             right=g_legend(a_ord))
  
  
  folder <- '04_Charts/'
  file <- 'ST_Peak_Demand_Ordered.png'
  ggsave(paste(folder,file,sep=''),width = 20, height = 8, units = "cm",plot_to_save)
  








