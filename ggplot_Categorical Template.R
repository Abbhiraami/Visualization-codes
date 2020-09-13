# Setting up the path
# Paths
setwd("D:/Abbhiraami/Cue")
path="D:/Abbhiraami/Cue"
input_path=paste0(path,"/Cue_Files")
output_path=paste0(path,"/Result_files")
#Date
From_date=as.Date("2020-03-23",format="%Y-%m-%d")
To_date=as.Date("2020-03-27",format="%Y-%m-%d")
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
colors=c("Blue","red2","yellow","lightgreen","darkgreen","orange","tan3","Maroon","red")
names(colors)=levels(mf$Outcome)
# Filtering Status 3 (For Closed Calls)
msgr1<-mf %>% filter(Status==3)

######################## GRAPHS- TEMPLATE #####################
### Donut ####
#######1-dimensional Graphs using Action, PnL, Outcome and Segment#########
# Change the group_by to corresponding column  
# annotate(geom = 'text', x = 0.5, y = 0,label =lab ,size=6,color="darkgreen") ----> name your plot in the desired directions
# Adjust x to align the labels in the graph
# legend.text=element_text(size=rel(1),face = "bold") -----> Adjusts the size of the text using size=rel(1)
# legend.position = "bottom" ----> position the legend - To avoid printing legend ---> use "none"in the place of "bottom"
# plot.title=element_text(hjust = 0.5)---> hjust=0.5 ----> aligns the title in the center
lab = "Buy/Sell"
oc=msgr1 %>%  group_by(Action) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = percent+ 0.5*percent)
bs=ggplot(oc, aes(x = 2, y = percent, fill = Action)) + #Change fill acc to group
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = percent-10 , label = paste0(percent,"%")), color = "white",size=5)+
  theme_void()+scale_fill_manual(values=colors)+
  xlim(0.5, 2.5)+theme(legend.title= element_blank(),
                       legend.text=element_text(size=rel(1),face = "bold"),
                       legend.position = "bottom",
                       plot.title=element_text(hjust = 0.5))+annotate(geom = 'text', x = 0.5, y = 0,label =lab ,size=6,color="darkgreen")

bs
##### Donut --- 2-dimennsional-- Quarters vs PnL 
# seg="Options" # set the variable based on reguirement and save them in different grob
# qua="JAN-MAR" # set the variable based on reguirement and save them in different grob
# tit=c("JAN-MAR")#JAN-MAR,"APR-JUN","JUL-SEP","OCT-DEC")
# set show.legend to true, to fetch common legends for combined graphs
tit="Options"
# Apply filters based on requirements--Types of graphs are mentioned in documentation
cc=msgr1 %>% filter(Quarters==qua) %>% group_by(PnL) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2))
cc=msgr1%>% filter(Segment==tit)  %>% group_by(Segment,PnL) %>% dplyr::summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2))
oq3=ggplot(cc, aes(x = 2, y = percent, fill = PnL)) +
  geom_bar(stat = "identity", color = "white",show.legend = F) +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(80,15) , label = paste0(100*round(n/sum(n),2),"%")), color = "white",size=3)+
  theme_void()+xlim(0.5, 2.5)+
  scale_fill_manual(values=c("darkgreen","red"))+theme(legend.title= element_blank(),legend.position = "top",legend.text=element_text(size=rel(1),face = "bold"),panel.background = element_rect(fill = "white",
                                                                                                                                                                                                    colour = "white"))+annotate(geom = 'text', x = 0.5, y = 0,
                                                                                                                                                                              label = tit,size=5,color="blue")
# To set themes of presenting  the graphs
# theme(legend.title= element_blank()-- to disable title 
# legend.text=element_text(size=rel(2),face = "bold")-- font size of graph's text
# legend.position = "bottom"-- customizing legends position)
# annotate(geom = 'text', x = 0.5, y = 0,label = tit,size=6,color="blue")-- Customizing text to graphs
# labs(title="Options-Total Number of Calls",subtitle = "(Quarter 1,2,3 & 4 2019)")
# plot.background = element_rect(fill="wheat1", color = NA)-- Graphs background color
oq #graph with legend
df_leg <-  get_legend(oq)
# Customizing font style of graphs
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")#only recognized fonts of google 
showtext_auto() # for retreiving fonts
windows() # To get the output in a separate to notice the changes in the graph.
# grid arragnment-- To bring four or more charts in one frame with common title and legends
myplot=grid.arrange(arrangeGrob(oq1,oq2,oq3,oq4,nrow=2),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1), 
                    ncol=1, heights = c(4,.5),top=textGrob(paste("Segment-wise Profit/ Loss", "(2019)", sep="\n"),gp=gpar(fontsize=15,font=3,family="Roboto Condensed")) )

g2 <- cowplot::ggdraw(myplot) + 
  theme(plot.background = element_rect(fill="lightblue", color = NA))

ggsave(g2,file = "QuaPL.jpeg",width=10,height =6.75 , units = "in",dpi=96)
#----------------------------------------------------------------------##
myplot=grid.arrange(arrangeGrob(oq1,oq2,oq3,oq4,nrow=2),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1), 
                    ncol=1, heights = c(4,.5))

## odd number of plots 
# grid.arrange(bsc,bsf,bso,bsi,bs)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(bsc, vp = vplayout(1, 1))
# print(bsf, vp = vplayout(1, 2))
# print(bso, vp = vplayout(3, 1))
# print(bsi, vp = vplayout(3, 2))
# print(bs, vp = vplayout(2, 1:2))
# grid.text(paste("TOTAL CALLS", "(JAN-DEC 2019)",sep="\n"), y=unit(1, "npc") - unit(1, "lines"),
#           gp=gpar(fontsize=15,font=3,face="bold"))

# Stacked bar ---- Quarters VS PnL
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
showtext_auto()
ccc=msgr1 %>% group_by(Segment,PnL) %>% dplyr::summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2))

# Enchancements - color, title of the plot, 
# labelling the categories and algining the axes
# Note: Adjust the labellings using vjust(vertical plots)
# For coord_flips of horizontal plots use hjust 
# plus and minus signs in the vjust or hjust adjusts the labels 
# upwards and downwards of the bars
windows()
TCA=ggplot(ccc, 
          aes(x = Segment,y=percent,fill=PnL
              ))+geom_bar(stat = "identity",position = "stack")+
  labs(title="Segment-wise Profit/Loss(2019)") +
  geom_text_repel(aes(label=paste0(percent,"%")), vjust=1.5,position=position_stack(vjust=0.5),color="white",size=3)+
  scale_fill_manual(values=c("darkgreen","red"))+theme_bw()+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=rel(1)),axis.title.y =element_blank(),axis.title.x =element_blank() ,legend.title= element_blank(),
        legend.position = "bottom",legend.text=element_text(size=rel(1),face = "bold"),
        plot.title = element_text(size=15,family="Roboto Condensed"),plot.background = element_rect(fill="white", color = NA))

TCA
ggsave(TCA,file = "SegmentPL.jpeg",width = 12.917,height = 5.21, units = "in",dpi=96)
# text=element_text(size=16,  family="Oxygen"),
# , vjust=-0.10, size=6, 
# position = position_stack(vjust=.25),colour="white"

# grouped bar plot--- Segment VS Action
ccd=msgr1 %>% group_by(Segment,Action) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2))

# Enchancements
# In the above graph g2 the zero counts are not plotted and the avialable
# categories are becoming wider to enhance the presentation of the graph
# use the position_dodge() option to peserve the size of the non-zero categories
windows()
g3=ggplot(ccd, 
          aes(x = Segment, y=percent,
              fill = Action))+geom_bar(stat="identity",position = position_dodge())+
  scale_fill_manual(values=c("blue","red"))+theme_void()+
  geom_text_repel(aes(y=percent+0.25, fill=Action, label=paste0(percent,"%"), hjust=0), vjust=0.25,position=position_dodge(width=0.9),color="white",size=3)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_blank(),axis.title.y =element_blank(),axis.title.x =element_blank(),legend.title= element_blank(),
        legend.position = "bottom",legend.text=element_text(size=rel(1),face = "bold"),
        plot.title = element_text(size=15,family = "Roboto Condensed",hjust=.5),
        plot.background = element_rect(fill="white", color = NA))
  
g3

# labs(title="TOTAL CALLS - SEGMENT WISE (2019) ")
d2=grid.arrange(arrangeGrob(g3,myplot,nrow=1),arrangeGrob(nullGrob(), df_leg, nullGrob(), nrow = 1), 
                ncol=1, heights = c(4,.5),
                top=textGrob(paste("Segment-wise Profit/ Loss", "(2019)", sep="\n"),gp=gpar(fontsize=15,font=3,family="Roboto Condensed")))
#d2+ggtitle("Annual performance")
ggsave(d2,file = "D:/Abbhiraami/Cue/Segment2.jpeg",width = 12.917,height = 5.21, units = "in",dpi=96)

#-----------------------------------------For equal widths preserve = "single"----- Important
#  
g4=ggplot(ccd,aes(Segment, percent, fill =Action,label=paste0(percent,"%"))) + 
       geom_col(position = position_dodge2(width = 0.9, preserve = "single"), show.legend = T) +
       geom_text(position = position_dodge2(width = 0.9, preserve = "single"), color="white",vjust=1, hjust=.5,size=5)+
  scale_fill_manual(values=c("blue","red")) + theme_void()+labs(title="TOTAL CALLS - SEGMENT WISE (2019) ")+
  theme(axis.text.x=element_text(size=20),axis.text.y=element_blank(),axis.title.y =element_blank(),axis.title.x =element_blank(),legend.title= element_blank(),
                                                    legend.position = "bottom",legend.text=element_text(size=rel(1),face = "bold"),
        plot.background = element_rect(fill="grey", color = NA))

# ggsave(g4,file = "BuySell.jpeg",width = 8,height = 4.75, units = "in",dpi=96)

######## Pie Chart
# group=mf$PnL
# fill=oc$PnL
# lab = "P/L"
oc=msgr1 %>%  group_by(PnL) %>% summarise(n=n()) %>% mutate(percent=100*round(n/sum(n),2),lab.ypos = cumsum(percent) - 0.5*percent)
piec=ggplot(oc, aes(x = 2, y = percent, fill = fill)) + #Change fill acc to group
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y =c(60,20) , label = paste0(percent,"%")), color = "white",size=5)+theme_void()+
  theme(legend.title= element_blank())+scale_fill_manual(values=c("darkgreen","red"))

piec
######## Bar Chart
# legend.title= element_blank()---> masks the title of the legend
# axis.title.y=element_blank()----> turns off the tick marks and axis label of the plot 
barc <- ggplot(oc, aes(PnL))+geom_bar(aes(weight = percent,fill=PnL))+
  scale_fill_manual(values=c("darkgreen","red"))+
  geom_text_repel(aes(y=percent+0.25,label = paste0(percent,"%")), position=position_dodge(width=1.0), vjust=1.5, color = "white",size=5)
barc+ theme(legend.title= element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
            legend.position = "none")

### Line Graphs --- Metric & Categorical ----- Return VS Segment ###
linegr=msgr1 %>% group_by(Segment,Quarters) %>% dplyr::summarise(Net_Return=round(sum(Return),0))
## geom_line(linetype=..,size=..)--for type of line represented in the graph.
lingr=ggplot(data=linegr, aes(x=Quarters, y=log(Net_Return), color=Segment,group=Segment)) +
  geom_line( linetype = 2,size=2)+
  geom_point()+labs(title="Return on Quarterly basis - 2019")+
  geom_dl(aes(label = linegr$Segment),size=8,method = list("last.qp",rot=90,cex=1),position = "identity") +
  geom_text_repel(aes(label=formattable::currency(linegr$Net_Return,"Rs")),size=5,
                  data=linegr)+theme_economist()+
  theme(axis.text.x=element_text(size=10),axis.text.y=element_blank(),axis.title.y =element_blank(),axis.title.x =element_blank(),legend.title= element_blank(),
        legend.position = "none",legend.text=element_text(size=rel(1),face = "bold"),
        plot.background = element_rect(fill="white", color = NA))
# To save the plot
ggsave(lingr,file = "D:/Abbhiraami/Cue/Returngr.jpeg",width = 12.917,height = 5.21, units = "in",dpi=96)

