#graphing result

library(tidyverse)
library(haven)
library(lme4)
library(arm)
library(jtools)
library(dplyr)
library(table1)
library(readxl)
library(rgeoda)
library(ggpubr)
library(REAT)


load("mrp_county.RData")
load("mrp_sub.RData")


# View the summaries
table1(~ideal+intend |agegp, data= mrp_sub, decimal=2)

#how many women of reproductive age are residing in below-replacement ideal counties ?
percentlow<-census_ind%>%
  left_join(mrp_county, by ="county")%>%
  dplyr::select(county, ideal, intend, npop)%>%
  unique()%>%
  mutate(lowideal=ifelse(ideal<2, 1,0))%>%
  group_by(lowideal)%>%
  summarise(lowidealpop=sum(npop))


#density plot by age and education subgroups 
age_colors <- c("lightgrey", "#a6d0c8", "#2c7c94")
edu_colors <- c( "lightgrey","#fbe45b", "#a65852")



ideal_agegp<-ggplot(data=mrp_sub)+
  stat_density(aes(ideal, color=agegp), geom="line", position="identity")+
  scale_color_manual(values=age_colors )+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = c(.8, .7),
        axis.title.x = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,0.5, 1, 1.5,2.0, 2.5, 3.0))+
  labs(color="Age")+
  xlab("Ideal number of children")


ideal_edugp<-ggplot(data=mrp_sub)+
  stat_density(aes(ideal, color=edugp), geom="line", position="identity")+
  scale_color_manual(values=edu_colors )+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = c(.8, .7),
        axis.title.x = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,0.5, 1, 1.5,2.0, 2.5, 3.0))+
  labs(color="Education") +
  xlab("Ideal number of children")


intend_agegp<-ggplot(data=mrp_sub)+
  stat_density(aes(intend, color=agegp), geom="line", position="identity")+
  scale_color_manual(values= age_colors )+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = c(.8, .7),
        axis.title.x = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,0.5, 1, 1.5,2.0, 2.5, 3.0))+
  labs(color="Age")+
  xlab("Intended number of children")


intend_edugp<-ggplot(data=mrp_sub)+
  stat_density(aes(intend, color=edugp), geom="line", position="identity")+
  scale_color_manual(values= edu_colors)+
  theme_bw()+
  theme(strip.background = element_rect(fill = "white"),
        legend.position = c(.8, .7),
        axis.title.x = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,0.5, 1, 1.5,2.0, 2.5, 3.0)) +
  labs(color="Education")+
  xlab("Intended number of children")

sub_combined <-cowplot:: plot_grid( ideal_edugp, ideal_agegp,  intend_edugp,intend_agegp,
                                    nrow=2)
print(sub_combined)

ggsave(filename = "sub_combined.png" , plot =sub_combined ) 


#maping 

county_map<-st_read("wholechina/export.shp")%>%
  dplyr::select(ename, gbcode,l700016_10,geometry)%>% #2872
  rename(county =gbcode, livebirth2010=l700016_10 )

province_map<-st_read("shapfile/中国行政区.shp")

length(unique(county_map$county)) #2872
length(unique(mrp_county$county)) #2473

merged<-merge(county_map , mrp_county, by.x="county" , by.y = "county" , all= FALSE ) 
#!150221 土默特右旗 is somehow not merged 


# mrpcounty<-merged%>%
#   dplyr::select(-livebirth2010,-npop)
# 
# st_write(mrpcounty, "mrpcounty.shp")
# 


summary(merged$ideal)
summary(merged$intend)

map_wholecounty<-merged%>%
  gather(ideal, intend, key = "measure" , value="value")%>%
  mutate(value_dis=cut(value, breaks = c(1.0, 1.5, 2, 2.5 , Inf),
                       labels = c("[1.0, 1.5)", "[1.5, 2)", "[2, 2.5)", "[2.5, 4.0] "), 
                       include.lowest = FALSE),
         valid=1)  #indicator for places with observation 


#construct a map of China 
prov<-province_map%>%
  filter(ID !="Xianggang" &  ID !="Aomen")%>%
  mutate(provname=if_else(ID=="Taiwan" , " ", ID) ,
         provname=if_else(ID=="Xizang", "Tibet", provname),
         provname=if_else(ID=="NeiMongol", "Inner Mongolia", provname))

mchina<-ggplot(data=prov)+
  geom_sf(fill=NA)+
  geom_text(aes(label = provname, x=X, y=Y))+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank())+
  geom_sf(data=map_wholecounty, aes(fill=valid), alpha=0.4, color="transparent", fill="lightgrey")

print(mchina)

ggsave(filename = "mchina.png" , plot =mchina ) 



#graph result 

table(map_wholecounty$value_dis[map_wholecounty$measure == "ideal"])

# [1.3, 1.5)    [1.5, 2)    [2, 2.5) [2.5, 3.8]  
# 10        1343        1002         117


table(map_wholecounty$value_dis[map_wholecounty$measure == "intend"])

#[1.0, 1.5)    [1.5, 2)    [2, 2.5) [2.5, 4.0]  
# 402        1179         752         139  


# #top 10 countie with largest / smallest estimates 
# countyname <- read_excel("2020中国人口普查分县资料.xlsx",  sheet = "livebirth2020")
# 
# ideal_top10_lowest<-mrp_county%>%
#   arrange(ideal)%>%
#   head(10)%>%
#   left_join(countyname, by="county")
# 
# ideal_top10_highest<-mrp_county%>%
#   arrange(desc(ideal))%>%
#   head(10)%>%
#   left_join(countyname, by="county")
# 
# 
# intend_top10_lowest<-mrp_county%>%
#   arrange(intend)%>%
#   head(10)%>%
#   left_join(countyname, by="county")
# 
# intend_top10_highest<-mrp_county%>%
#   arrange(desc(intend))%>%
#   head(10)%>%
#   left_join(countyname, by="county")


#----------------

mrpgp_colors <- c("#2c7c94", "#a6d0c8", "#fbe45b", "#a65852" )

new_labels<-c("ideal" = "Ideal number of children" , "intend" = "Intended number of children")

mrp_graph<-ggplot(data = map_wholecounty) +
  geom_sf(aes(fill=value_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  facet_wrap(~ measure , labeller=labeller(measure = new_labels))+
  scale_fill_manual(values = mrpgp_colors, drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="")


print(mrp_graph)
ggsave(filename = "mrp_graph.png" , plot =mrp_graph ) 


#---------
#single map 

mrpgp_colors <- c("#2c7c94", "#a6d0c8", "#fbe45b", "#a65852" )

ideal<-map_wholecounty%>%
  filter(measure=="ideal")

mrp_ideal<-ggplot(data = ideal) +
  geom_sf(aes(fill=value_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  scale_fill_manual(values = mrpgp_colors, drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="")

print(mrp_ideal)
ggsave(filename = "mrp_ideal.png" , plot =mrp_ideal ) 




intend<-map_wholecounty%>%
  filter(measure=="intend")

mrp_intend<-ggplot(data = intend) +
  geom_sf(aes(fill=value_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  scale_fill_manual(values = mrpgp_colors, drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="")

print(mrp_intend)
ggsave(filename = "mrp_intend.png" , plot =mrp_intend) 




#--------------------------------
#ideal to intend ratio 

ratio<-merged%>%
  mutate(ratio=intend/ideal,
         ratio_dis=cut(ratio, breaks = c(0.6, 0.8, 1.00001, Inf),
                       labels = c("[60%, 80%)", "[80%, 100%)", "[100%, 133%)"), 
                       include.lowest = FALSE))
#ratio_dis=cut(ratio, breaks = c(0.6, 0.7,0.8, 0.9, 1.0, 1.1, 1.2, 1.4),
#labels=c("[60%,70%)", "[70%,80%)", "[80%,90%)", "[90%,100%)",
#         "[100%,110%)", "[110%,120%)", "[120%,133%)")
# ,include.lowest = FALSE))

summary(ratio$ratio)
table(ratio$ratio_dis, useNA = "always")

#ratio_colors <- c("#DFDFEA" , "#8D9FD1" , "#D18CB8" )
#ratio_colors <- c("#e0ecf4" , "#9ebcda" , "#8856a7" )

ratio_colors <- c("#f2f0f7","#dadaeb", "#bcbddc", "#9e9ac8" ,
                  "#d9f0d3" , "#7fbf7b" , "#1b7837" )


ratio_graph<-ggplot(data = ratio) +
  geom_sf(aes(fill=ratio_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  scale_fill_manual(values = ratio_colors, drop= TRUE)+
  theme_map()+
  theme(plot.title = element_text(size = 12, face="plain" ,hjust = 0.5), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(title = "(Intended/Ideal)\u00D7100%",fill="")

print(ratio_graph) 

#ggsave(filename = "raio_graphv2.png" , plot =ratio_graph ) 


raio_combined <-cowplot:: plot_grid( mrp_graph, ratio_graph, 
                                     nrow=2)
print(raio_combined)

ggsave(filename = "raio_combined.png" , plot =raio_combined ) 


# #continuous version
# 
# new_labels<-c("ideal" = "Ideal number of children" , "intend" = "Intended number of children")
# 
# mrp_graph_continous<-ggplot(data = map_wholecounty) +
#   geom_sf(aes(fill=value) , color="transparent")+
#   geom_sf(data=province_map,color = "#514e4c", fill = NA)+
#   facet_wrap(~ measure , labeller=labeller(measure = new_labels))+
#   scale_fill_gradient2(low="#053061", mid = "#fddbc7", high= "#67001f", midpoint=2)+
#   theme_map()+
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         strip.background = element_blank(),
#         legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
#   labs(fill="")
# 
# print(mrp_graph_continous)
# 



summary(mrp_sub$ideal)
summary(mrp_sub$intend)

#by age and edu group 
map_gp<-county_map%>%
  mutate(county=as.integer(county)) %>%
  inner_join(mrp_sub, by="county")%>%
  mutate(ideal_dis=cut(ideal, breaks = c(1, 1.5, 2, 2.5 , Inf),
                       labels = c("[1, 1.5)", "[1.5,2)", "[2, 2.5)", "[2.5, 4]"), 
                       include.lowest = FALSE),
         intend_dis=cut(intend, breaks = c(0.8, 1, 1.5, 2, 2.5 , Inf),
                        labels = c("[0.8 ,1)", "[1, 1.5)", "[1.5,2)", "[2, 2.5)", "[2.5, 4]"), 
                        include.lowest = FALSE))

mrpgp_ideal <- c(  "#2c7c94", "#a6d0c8", "#fbe45b", "#a65852" )

sub_ideal<-ggplot(data = map_gp) +
  geom_sf(aes(fill=ideal_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  facet_wrap(~ agegp+edugp)+
  scale_fill_manual(values = mrpgp_ideal, drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="")

print(sub_ideal)

ggsave(filename = "sub_ideal.png" , plot =sub_ideal ) 


table(map_gp$intend_dis)

mrpgp_intend <- c( "#343c24" ,  "#2c7c94", "#a6d0c8", "#fbe45b", "#a65852" )

sub_intend<-ggplot(data = map_gp) +
  geom_sf(aes(fill=intend_dis) , color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  facet_wrap(~ agegp+edugp)+
  scale_fill_manual(values = mrpgp_intend, drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="")

print(sub_intend)

ggsave(filename = "sub_intend.png" , plot =sub_intend ) 

#------------------------
#LISA map 
#------------------------
queen_w<-queen_weights(merged)
summary(queen_w)
lisa_ideal<-local_moran(queen_w, merged['ideal'])
lisa_intend<-local_moran(queen_w, merged['intend'])


lisa_ideal_colors <- lisa_colors(lisa_ideal)
lisa_ideal_labels <- lisa_labels(lisa_ideal)
lisa_ideal_clusters <- lisa_clusters(lisa_ideal)

lisa_intend_clusters <- lisa_clusters(lisa_intend)


# 0 (not significant), 
#1 (high-high cluster), 
#2 (low-low cluster), 
#3 (low-high cluster), 
#4 (high-low cluster), 
#5 (neighborless/island), 
#6 (undefined)


merged$ideal_lisa_clusters<-lisa_ideal_clusters
merged$ideal_lisa_clusters <- as.factor(merged$ideal_lisa_clusters)

merged$intend_lisa_clusters<-lisa_intend_clusters
merged$intend_lisa_clusters <- as.factor(merged$intend_lisa_clusters)


table(merged$ideal_lisa_clusters)
table(merged$intend_lisa_clusters)



lisa_ideal_colors<-c ("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8" , "#999999" )
lisa_ideal_labels <- lisa_ideal_labels[-6]


ideal_lisa<-ggplot(data=merged)+
  geom_sf(aes(fill=ideal_lisa_clusters),color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  scale_fill_manual(values = lisa_ideal_colors, labels=lisa_ideal_labels,  drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="", title = "Ideal Number of Children")

print(ideal_lisa)



intend_lisa<-ggplot(data=merged)+
  geom_sf(aes(fill=intend_lisa_clusters),color="transparent")+
  geom_sf(data=province_map,color = "#514e4c", fill = NA)+
  scale_fill_manual(values = lisa_ideal_colors, labels=lisa_ideal_labels,  drop= TRUE)+
  theme_map()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",legend.justification = "center")+
  labs(fill="", title = "Intended Number of Children")

print(intend_lisa)

lisa<-ggarrange(ideal_lisa, intend_lisa, common.legend = TRUE,legend="bottom")
print(lisa)

ggsave(filename = "lisa.png" , plot =lisa ) 

