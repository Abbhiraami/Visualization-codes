# Setting up the path
# Paths
setwd("D:/Abbhiraami/Cue")
path="D:/Abbhiraami/Cue"
input_path=paste0(path,"/Cue_Files")
output_path=paste0(path,"/Result_files")
#Date
From_date=as.Date("2019-01-01",format="%Y-%m-%d")
To_date=as.Date("2019-12-31",format="%Y-%m-%d")
# Loading Libraries
library(dplyr) # Data Manipulations
library(ggplot2) # For plots
library(tidyr)  
library(gridExtra) # Plots arrangements
library(grid)# odd plots arrangements
library(lubridate)# Date conversions
library(ggthemes)
library(ggpubr) # get_legend ---> For common legends
library(ggrepel)# Labelling texts
#library(patchwork)# grid arrangments
library(extrafont)# font types
library(directlabels)# to label lines
library(showtext) # Changing the fonts of graphs
# import fonts - only once
font_import() # loading fonts 
loadfonts(device = "win", quiet = TRUE)#connecting current device for different fonts
# Loading the dataset
# Input the Masterfile of Cue_FromTo_Return_2001
mf=read.csv(paste0(output_path,"/MasterFile.csv"))
### Data Preparation
# Date Conversion
mf$Date=as.Date(mf$Date,format="%Y-%m-%d")
# Date filters
mf <-mf %>%
  filter(Date>=From_date & Date<=To_date)#Closed calls from df
# Quarters--Categorizing months based on quarters
mf=mf %>% 
  mutate(Quarters=case_when(lubridate::month(Date)==1|lubridate::month(Date)==2|lubridate::month(Date)==3~"JAN-MAR",
                            lubridate::month(Date)==4|lubridate::month(Date)==5|lubridate::month(Date)==6~"APR-JUN",
                            lubridate::month(Date)==7|lubridate::month(Date)==8|lubridate::month(Date)==9 ~"JUL-SEP",
                            lubridate::month(Date)==10|lubridate::month(Date)==11|lubridate::month(Date)==12~"OCT-DEC"))

# Releveling the factors of variables--PnL & Quarters
mf$PnL=factor(mf$PnL,levels=c("Profit","Loss"))
mf$Quarters=factor(mf$Quarters,levels=c("JAN-MAR","APR-JUN","JUL-SEP","OCT-DEC"))
mf$Outcome=factor(mf$Outcome,levels=c("Buy","Sell","Alert","Tgt1","Tgt2","TSL","Profit Exit","Loss Exit","SL"))
# Filtering Status 3 (For Closed Calls)
msgr1<-mf %>% filter(Status==3)
#msgr1$Outcome=factor(msgr1$Outcome,levels=c("Tgt2","TSL","Profit Exit","Loss Exit","SL"))
colors=c("Blue","red4","yellow","lightgreen","darkgreen","orange","tan3","Maroon","red")
names(colors)=levels(mf$Outcome)
oc=msgr1 %>% group_by(Outcome) %>% summarise(n=n())
barc <- ggplot(oc, aes(Outcome))+geom_bar(aes(weight = n,fill=Outcome))+
  scale_fill_manual(values=colors)+
  geom_text(aes(y=n+0.25,label=n), position=position_dodge(width=1.0), vjust=1.5, color = "white",size=5)
barc+ theme(legend.title= element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
            legend.position = "none")



