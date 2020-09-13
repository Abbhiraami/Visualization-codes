rm(list=ls())
# To plot the Threshold and visual representation for T0 and T1 and T2 achvd

# Load the packages 
library(plyr)
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(reticulate)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(hms)
library(scales)
# Set the working directory to read the Ambalal files
setwd("D:/Abbhiraami/Cue/Visual Analysis/Futures")

# Listing down all the files present in that path and folder
filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE)

# storing the different filenames present in that folder
read_csv_filename <- function(filenames){
  ret <- read.csv(filenames)
  ret$Source <- filenames 
  ret
}

# Raeding all the files and making it into single file 
# Simultaneously adding the filenames as a column respectively
import.list <- ldply(filenames, read_csv_filename)

# From Source name dropping ".csv"
import.list$Source <- str_replace(import.list$Source, ".csv","")

import.list$Source <- gsub("(.*)_\\w+", "\\1", import.list$Source)

import.list$Source <- str_replace_all(import.list$Source, "_", ":")

import.list$Source <- as.character(import.list$Source)

import.list.1<- import.list
# import.list.1 <- import.list %>% filter(Source >'12:14:00' & Source <'15:18:00')


# To specify the SYMBOL
# result <- import.list.1 %>% filter(SYMBOL ==
#                                      c("INFRATEL","CANBK","JUSTDIAL","DLF","HAVELLS"))


import.list.1$time=as.POSIXct(import.list.1$Source,format="%H:%M:%S")
import.list.1$hr<-as.POSIXct(import.list.1$time,format="%H")
import.list.1=import.list.1%>%
  mutate(msg=word(MESSAGE,1))

import.list.1=import.list.1%>%
  mutate(Status_cat=ifelse(( msg=="Tgt2"),"T2",
                           ifelse(( msg =="TSL"),"Tsl",
                                  ifelse((msg== "SL"),"SL",
                                         ifelse((msg=="Alert"),"T0",
                                                ifelse((msg=="Tgt1"),"T1",
                                                       ifelse((msg %in% c("Buy","Sell")),
                                                              "R",NA)))))))


import.list.1[is.na(import.list.1)]<-""
data1=import.list.1%>%
  filter(SYMBOL=="AUROPHARMA")
data2 <- import.list.1[!(import.list.1$MESSAGE==""),]
data2=data2%>%
  filter(SYMBOL=="AUROPHARMA")
a=ggplot(data = import.list.1, 
         aes(hr, High,msg,color=SYMBOL)) + geom_line()+
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")+
  theme(legend.position = "none")

a+ geom_vline(xintercept = Stocks_with_msgs$hr)+
  geom_text(aes(label=Stocks_with_msgs$msg),size=5,group=as.factor(Stocks_with_msgs$msg), data=Stocks_with_msgs,angle=90, vjust = 1.2) 
ggsave("AUROPHARMA.png",width = 50.8,height = 28.575,path = "D:/Abbhiraami",units = "cm",dpi=300)
