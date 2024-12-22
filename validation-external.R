#external validation 
#compare estimation with external survey 
install.packages("Metrics")
install.packages("openxlsx")
install.packages("patchwork")

library(haven)
library(dplyr)
library(ggplot2)
library(Metrics)
library(caret)
library(openxlsx)
#library(cowplot)
library(patchwork)


setwd("C:/Users/Donghui/SynologyDrive/MRP/data_mrp") #home desktop 
setwd("C:/Users/donghuiwang/SynologyDrive/MRP/data_mrp") #office desktop 

load("mrp_county.RData")
load("mrp_sub.RData")


mrp_city<-mrp_county%>%
  mutate(citycode=as.integer(county/100))%>%
  group_by(citycode)%>%
  summarise(ideal=weighted.mean(ideal, npop),
            intend=weighted.mean(intend, npop),
            ideal2=weighted.mean(ideal2,npop),
            intend2=weighted.mean(intend2,npop))


#dis-aggregation 

load("fer_ind.RData")

disag_city<-fer_ind%>%
  mutate(citycode=as.integer(county/100))%>%
  group_by(citycode)%>%
  summarise(idea_disag=mean(ideal),
            intend_disag=mean(intend))



#*Survey of Urban and Rural Residents' Lives in China:SURRL2022 
# external1<-read_dta("validation/urbanrurallife_2022.dta")%>%
#   group_by(citycode)%>%
#   summarise(intend_external=mean(intend),
#             samplesize=n())%>%
#   mutate(source="SURRL2022")
# 
# table(external1$samplesize)

#Family Fertility Decision-Making in China2016 ： FFDM2016
external2<-read_dta("validation/fertilitydecision2016.dta")%>%
  group_by(citycode)%>%
  summarise(intend_external=mean(intend_external2),
            ideal_external=mean(ideal_external2),
            samplesize=n())%>%
  mutate(source="FFDM2016")


# FS12CITES2016

external3<-read_dta("validation/wife&husband_city2017.dta")%>%
  group_by(citycode)%>%
  summarise(intend_external=mean(intend),
            ideal_external=mean(ideal),
            samplesize=n())%>%
  mutate(source= "FS12CITES2017")%>%
  filter(samplesize>100)


validation<-bind_rows(external2, external3)%>%
  inner_join(mrp_city, by="citycode")%>%
  inner_join(disag_city, by="citycode")%>%
  mutate(
        #performance 
        ae_intend_disag = abs(intend_external  - intend_disag),
        ae_ideal_disag=abs(ideal_external - idea_disag ),
        
        ae_intend_mrp = abs(intend_external  - intend),
        ae_ideal_mrp=abs(ideal_external - ideal),  
        
        ape_intend_disag = ae_intend_disag / intend_external,
        ape_intend_mrp = ae_intend_mrp / intend_external,
        
        ape_ideal_disag = ae_ideal_disag / ideal_external,
        ape_ideal_mrp = ae_ideal_mrp / ideal_external,        
        
        prov=as.integer(citycode/100))

#index
validation_index<-validation%>%
  group_by(source)%>%
  summarise(
        mae_intend_disag= MAE(intend_external, intend_disag),
        mae_intend_mrp= MAE(intend_external, intend),
        mae_ideal_disag= MAE(ideal_external, idea_disag),
        mae_ideal_mrp= MAE(ideal_external, ideal), 
        
        mape_intend_disag=mean(ape_intend_disag),
        mape_ideal_disag=mean(ape_ideal_disag),
        
        mape_intend_mrp=mean(ape_intend_mrp),
        mape_ideal_mrp=mean(ape_ideal_mrp))

write.xlsx(validation_index, "validation_index.xlsx")


  

cor_coef_ideal_mrp <- cor(validation$ideal, validation$ideal_external,method = "pearson") #0.53
cor_coef_ideal_disag <- cor(validation$idea_disag , validation$ideal_external)


cor_coef_intend_mrp <- cor(validation$intend, validation$intend_external ,method = "pearson") #0.43
cor_coef_intend_disag <- cor(validation$intend_disag , validation$intend_external)


ggplot(validation,aes(x= intend_external, y=intend)) +
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  geom_smooth(method='lm', formula= y~x)+
  labs(x = "Independent Prefecture-level Survey Means", y = "MRP Estimates", title= "Intention") +
  theme_minimal()

#calculate correlation   

intend_val_disag<-ggplot(validation, aes(y= intend_external, x=intend_disag))+
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(1, 2.5) +
  ylim(1, 2.5) +
  labs(y = "External Survey", x = "Disaggregated Mean Estimates", title= "Intended Number of Children") +
  scale_color_manual( name= "Survey", labels = c("FFDM2017", "FS12CITIES2016"), values = c("black", "red")) +
  scale_size_continuous(name = "Sample Size")+
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))+
  theme_minimal()



intend_val_whole<-ggplot(validation, aes(y= intend_external, x=intend))+
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(1, 2.5) +
  ylim(1, 2.5) +
  labs(y = "External Survey", x = "MRP Estimates", title= "Intended Number of Children") +
    scale_color_manual( name= "Survey", labels = c("FFDM2017", "FS12CITIES2016"), values = c("black", "red")) +
  scale_size_continuous(name = "Sample Size")+
  theme_minimal()+
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
  

ideal_val_disag<-ggplot(validation, aes(y= ideal_external, x=idea_disag ))+
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(1, 2.5) +
  ylim(1, 2.5) +
  labs(y = "External Survey", x = "Disaggregated Mean Estimates", title= "Ideal Number of Children") +
  theme_minimal()+
  scale_color_manual( name= "Survey", labels = c("FFDM2017", "FS12CITIES2016"), values = c("black", "red")) +
  scale_size_continuous(name = "Sample Size")+
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))



ideal_val_whole<-ggplot(validation, aes(y= ideal_external, x=ideal))+
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(1, 2.5) +
  ylim(1, 2.5) +
  labs(y = "External Survey", x = "MRP Estimates", title= "Ideal Number of Children") +
  theme_minimal()+
  scale_color_manual( name= "Survey", labels = c("FFDM2017", "FS12CITIES2016"), values = c("black", "red")) +
  scale_size_continuous(name = "Sample Size")+
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))



combined_plot <-  ideal_val_whole+ intend_val_whole  + plot_layout(guides = "collect")
ggsave(filename = "combined_plot_val_whole.png" , plot =combined_plot ) 

#=====================================================================================
#validation results by province : not presented in the paper 
intend_val<-ggplot(validation, aes(x=intend_external, y=intend))+
  geom_jitter(aes(size=samplesize, color=source), shape =21)+
  facet_wrap(~prov )+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(0, 2) +
  ylim(0, 2) +
  labs(x = "External Survey", y = "MRP Estimates", title= "Intention") +
  theme_minimal()

intend_val
ggsave(filename = "intend_val.png" , plot =intend_val ) 



ideal_validation <-validation%>%
  na.omit(ideal_external)

ideal_val<-ggplot(ideal_validation, aes(x=ideal_external, y=ideal))+
  geom_jitter(aes(size=samplesize, color = source), shape =21)+
  facet_wrap(~prov )+
  geom_abline(intercept = 0, slope = 1, size = 0.5,linetype = "dashed")+
  expand_limits(x = 0, y = 0)+
  xlim(0, 2) +
  ylim(0, 2) +
  labs(x = "External Survey", y = "MRP Estimates", title= "Ideal") +
  theme_minimal()  
ideal_val  
ggsave(filename = "ideal_val.png" , plot =ideal_val ) 



ggplot(external1, aes(x=intend_external, y=intend))+
  geom_jitter()+
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  expand_limits(x = 0, y = 0)+
  theme_minimal()
