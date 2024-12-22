#Graphing theil index
#note: the index was calculated in advance by TL


library(haven)
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)
library(forcats)


setwd("C:/Users/donghuiwang/SynologyDrive/MRP/data_mrp") #office desktop 
theil<-read_excel("Theil index_v2.xlsx",
                  sheet = "ideal_intend")%>%
  arrange(intend_theil)

ggplot(theil)+
  geom_segment(aes(x=ideal_theil, xend = intend_theil,
                   y=Eng_prov, yend = Eng_prov))+
  geom_point(aes(x=ideal_theil, y=Eng_prov), size= 3, color= "red")+
  geom_point(aes(x=intend_theil, y=Eng_prov ), size= 3, color= "black")+
  scale_y_discrete(limits = theil$Eng_prov)


#within province 

theil_within<-theil%>%
  mutate(Eng_prov = fct_reorder(Eng_prov, intend_withinprov))
  
ggplot(theil_within)+
  geom_segment(aes(x=ideal_withinprov, xend = intend_withinprov,
                   y=Eng_prov, yend = Eng_prov))+
  geom_point(aes(x=ideal_withinprov, y=Eng_prov), size= 3, color= "red")+
  geom_point(aes(x=intend_withinprov, y=Eng_prov ), size= 3, color= "black")+
  scale_y_discrete(limits = theil$Eng_prov)


Theil<-ggplot(theil_within)+
  geom_segment(aes(x=ideal_withinprov, xend = intend_withinprov,
                   y=Eng_prov, yend = Eng_prov))+
  geom_point(aes(x=intend_withinprov, y=Eng_prov),size= 3, color= "black")+
  geom_point(aes(x=ideal_withinprov, y=Eng_prov),size= 3, color= "red")+
  scale_color_manual(name = "Legend", values = c("intend" = "black", "ideal" = "red")) +
  theme_minimal()+
  labs(x="Within-provice Theil", y="Province")

ggsave("Theil.png", plot = Theil)