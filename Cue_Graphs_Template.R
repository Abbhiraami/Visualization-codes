# Paths
path="D:/Abbhiraami/Cue"
input_path=paste0(path,"/Cue_Files")
output_path=paste0(path,"/Result_files")

#Date
From_date=as.Date("2019-01-01",format="%Y-%m-%d")
To_date=as.Date("2019-12-31",format="%Y-%m-%d")

# Load dataset# Loading packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(grid)# odd plots arrangements
library(plotly)
library(lubridate)
library(anytime)#oldfile date conversions
library(tibble)#rownames to column in df
library(data.table)#for writing multiple sheets in a excel
library(openxlsx)#for writing multiple sheets in a excel
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(easypackages)
library(patchwork)# grid arrangments

mf=read.csv(paste0(output_path,"/MasterFile2019.csv"))
# Date Conversion
mf$Date=as.Date(mf$Date,format="%m/%d/%Y")
mf$Month=lubridate::month(mf$Date,label=T)
# Quarters
mf=mf %>% 
  mutate(Quarters=case_when(lubridate::month(Date)==1|lubridate::month(Date)==2|lubridate::month(Date)==3~"Q1",
                            lubridate::month(Date)==4|lubridate::month(Date)==5|lubridate::month(Date)==6~"Q2",
                            lubridate::month(Date)==7|lubridate::month(Date)==8|lubridate::month(Date)==9 ~"Q3",
                            lubridate::month(Date)==10|lubridate::month(Date)==11|lubridate::month(Date)==12~"Q4"))
# Releveling the PnL
mf$PnL=factor(mf$PnL,levels=c("Profit","Loss"))

# Filtering only closed calls for P/L calculation
#msgr=mf %>% filter(Date>=From_date & Date<=To_date)#All calls and followups from the main df

msgr1<-mf %>% filter(Status==3)

########## Donuts and Pie Chart ##########
# 1-dimensional Graphs using Action, PnL, Outcome and Segment
### Donut ####
# Change the group_by to corresponding column  
group=msgr1$PnL
fill=oc$PnL
lab = "P/L"
oc=msgr1 %>%  group_by(PnL) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = cumsum(percent) - 0.5*percent)
bs=ggplot(oc, aes(x = 2, y = percent, fill = PnL)) + #Change fill acc to group
  geom_bar(stat = "identity", color = "white",show.legend = F) +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(60,20) , label = paste0(percent,"%")), color = "white",size=5)+
  theme_void()+scale_fill_manual(values=c("darkgreen","red"))+
  xlim(0.5, 2.5)+theme(legend.title= element_blank(),
                       legend.text=element_text(size=rel(1),face = "bold"),
                       legend.position = "bottom",
                       plot.title=element_text(hjust = 0.5))+annotate(geom = 'text', x = 0.5, y = 0,
                                                                      label =lab ,size=6,color="darkgreen")
bs
######## Pie Chart
# group=mf$PnL
# fill=oc$PnL
# lab = "P/L"
# oc=msgr1 %>%  group_by(PnL) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = cumsum(percent) - 0.5*percent)
piec=ggplot(oc, aes(x = 2, y = percent, fill = fill)) + #Change fill acc to group
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(60,20) , label = paste0(percent,"%")), color = "white",size=5)+theme_void()+
  theme(legend.title= element_blank())+scale_fill_manual(values=c("darkgreen","red"))

piec
### Bar Chart
# group=mf$PnL
# fill=oc$PnL
# lab = "P/L"
# oc=msgr1 %>%  group_by(group) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = cumsum(percent) - 0.5*percent)
barc <- ggplot(oc, aes(PnL))+geom_bar(aes(weight = percent,fill=PnL),show.legend = F)+
  scale_fill_manual(values=c("darkgreen","red"))+
  geom_text(aes(y=c(60,25),label = paste0(percent,"%")), position=position_dodge(width=1.0), vjust=1.5, color = "white",size=5)
barc+ theme(legend.title= element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
            legend.position = "none")
### Overall -- PnL VS Annual---- Two dimensional---------- Donut
Seg="Index"
act="Buy"
tit="P/L"
# %>% filter(Quarters==qua) 
# %>% filter(Segment==Seg
pc=msgr1  %>% group_by(Action) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = cumsum(percent) - 0.5*percent)
pt=ggplot(pc, aes(x = 2, y = percent, fill = Action)) +
  geom_bar(stat = "identity", color = "white", show.legend = F) +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(60,15) , label = paste0(percent,"%")), color = "white",size=5)+
  xlim(0.5, 2.5)+
  scale_fill_manual(values=c("blue","red"))+labs(title=paste("Total Calls","(JAN-DEC 2019)",sep="\n"))+
  theme_void( )+theme(legend.title= element_blank(),
                      plot.title = element_text(hjust = .5),
                      legend.text=element_text(size=rel(2),face = "bold"),
                      legend.position = "bottom")
# +annotate(geom = 'text', x = 0.5, y = 0,label = paste(tit,sep="\n"),size=6,color="blue")

pt
### Challenging part add it to library
#### Total Calls --- Segment 
df_legt <-  get_legend(ct)
grid.arrange(arrangeGrob(ct1,ft1,ot1,it1,nrow=2),
             arrangeGrob(nullGrob(), df_legt, nullGrob(), nrow = 1),ncol=1, heights = c(4,0.5),
             top=textGrob(paste("Total Calls - Segment Wise", "(JAN-DEC 2019)", sep="\n"),gp=gpar(fontsize=20,font=3)))

#### P/L --- Segment 
df_legP <-  get_legend(cp)
grid.arrange(arrangeGrob(cp1,fp1,op1,ip1,nrow=2),
             arrangeGrob(nullGrob(), df_legP, nullGrob(), nrow = 1),ncol=1, heights = c(2,0.5),
             top=textGrob(paste("Profit/Loss - Segment", "(JAN-DEC 2019)", sep="\n"),gp=gpar(fontsize=20,font=3))
             )


## Buy Sell --- P/L
grid.arrange(arrangeGrob(qb,qs,nrow=1),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1),ncol=1, heights = c(3,.5),top=textGrob(paste("Profit/Loss - Buy & Sell", "(JAN-DEC 2019)", sep="\n"),gp=gpar(fontsize=20,font=3)))

grid.arrange(arrangeGrob(cqb,cqs,fqb,fqs,nrow=2),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1), 
             ncol=1, heights = c(4,0.5),top=textGrob(paste("Quarterly Profit/ Loss", "(2019)", sep="\n"),gp=gpar(fontsize=20,font=3)) )

########## Quarterly ---- Three dimensions---- Segment VS Action VS Quarters---- Donut
msgr2=msgr1 %>% filter(PnL=="Profit")
df <- ftable(msgr2$Outcome,msgr2$Quarters)
cont <- stats:::format.ftable(df, quote = FALSE)# To save ftable as such 
write.table(cont, sep = ",", file = paste0(output_path,"/Quarters_TC.csv"))
# seg="Options"
qua="Q1"
tit=c("JAN-MAR")#,"APR-JUN","JUL-SEP","OCT-DEC")
cc=msgr1 %>% filter(Quarters==qua) %>% group_by(PnL) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2))
oq1=ggplot(cc, aes(x = 2, y = percent, fill = PnL)) +
  geom_bar(stat = "identity", color = "white",show.legend = F) +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(80,15) , label = paste0(100*round(n/sum(n),2),"%")), color = "white",size=6)+
  theme_void()+xlim(0.5, 2.5)+
  scale_fill_manual(values=c("darkgreen","red"))+theme(legend.title= element_blank(),legend.position = "bottom",legend.text=element_text(size=rel(2),face = "bold"))+annotate(geom = 'text', x = 0.5, y = 0,
                                                         label = tit,size=6,color="blue")
  # theme(legend.title= element_blank(),
  #                       legend.text=element_text(size=rel(2),face = "bold"),
  #                       legend.position = "bottom")+annotate(geom = 'text', x = 0.5, y = 0,
  #                                                            label = tit,size=6,color="blue")
  #+labs(title="Options-Total Number of Calls",subtitle = "(Quarter 1,2,3 & 4 2019)")
oq1
df_leg <-  get_legend(oq)
myplot=grid.arrange(arrangeGrob(oq1,oq2,oq3,oq4,nrow=2),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1), 
             ncol=1, heights = c(4,0.5),top=textGrob(paste("Quarterly Profit/ Loss", "(2019)", sep="\n"),gp=gpar(fontsize=20,font=3)) )
(bs+pt)/(oq+oq1)
(bs+pt+oq)/barc
(bs+pt+oq+oq1+bs)

pnl=ftable(msgr1$Month,msgr1$PnL)
