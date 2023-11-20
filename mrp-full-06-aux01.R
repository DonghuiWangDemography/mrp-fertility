#county level estimate using synthetic model 
#clean up mrp-full-06

#install.packages("parallel")
#install.packages("scales")
#install.packages("gtsummary")
#install.packages("rgeoda")
#install.packages("ggpubr")
library(tidyverse)
library(haven)
library(lme4)
library(arm)
library(jtools)
library(dplyr)
library(table1)
library(readxl)
library(sf)
library(caret)
library(merTools)
library(cowplot)
library(grid)
library(gridExtra)
library(flextable)
library(sf)
library(parallel)
library(scales)
library(sjPlot)
library(rgeoda)
library(ggpubr)


setwd("C:/Users/Donghui/SynologyDrive/MRP/data_mrp") #home desktop 

rm(list = ls())

load("X2017fertility_survey.RData")


#length(unique(X2017fertility_survey$c6)) #2737


# *work on 新疆建设兵团
# /*各师机关所在地:
# 第一师在阿拉尔市 659002
# 第二师在库尔勒市 652801
# 第三师在喀什市 653101
# 第四师在伊宁市 654002
# 第五师在博乐市 652701
# 第六师在五家渠市 659004
# 第七师在奎屯市 654003
# 第八师在石河子市  659001
# 第九师在额敏县 654221
# 第十师在北屯市 659005
# 第十一师(建工师)在乌鲁木齐市 650121
# 第十二师在乌鲁木齐市 650121
# 第十三师在哈密市 652201 
# 第十四师在和田市 653201
# */

#names(X2017fertility_survey)

fertility<-X2017fertility_survey%>%
  #work on xingjiang
  mutate(countycode = ifelse(c2 == "第一师", 659002, 
                      ifelse(c2 == "第二师", 652801, 
                      ifelse(c2 == "第三师", 653101, 
                      ifelse(c2 == "第四师", 654002, 
                      ifelse(c2 == "第五师", 652701, 
                      ifelse(c2 == "第六师", 659004, 
                      ifelse(c2 == "第七师", 654003, 
                      ifelse(c2 == "第八师", 659001, 
                      ifelse(c2 == "第九师", 654221, 
                      ifelse(c2 == "第十师", 659005, 
                      ifelse(c2 == "第十一师", 650121, 
                      ifelse(c2 == "第十二师", 650121, 
                      ifelse(c2 == "第十三师", 652201, 
                      ifelse(c2 == "第十四师", 653201, c6)))))))))))))))%>%
          mutate(prefecture=as.integer(c6/100),
                  prov=as.integer(prefecture/100),
                  bdate =  as.Date(paste0(q101m, "/01/", q101y), "%m/%d/%Y"),
                  cdate = as.Date("2017-07-01"),
                  age =as.integer((cdate-bdate)/365),
                  agegp = factor(case_when(age <=24 ~ "15-24",
                                           age >24 & age <= 34 ~ "25-34", 
                                           age >34 ~ "35-49")), 
                  edugp = factor(case_when(q105 <=2 ~  "primary and below",
                                           q105 == 3 ~  "secondary",
                                           q105 >= 4 ~ "hs and above")),
                  ideal2 = ifelse(q401>=2, 1,0),
                  intend2= ifelse(q402>=2, 1,0),
                  ideal= q401,
                  intend=q402,
                  edugp=factor(edugp, levels = c("primary and below", "secondary", "hs and above")))%>%
  #select, generate interactions 
  filter(age>=15 & age <=49, complete.cases(ideal, intend), ideal<10, intend<10, q102 !=5)%>%
  rename(prov_name = c1, county=countycode)%>%
  dplyr::select(prov_name, prov, prefecture, county, agegp, edugp, ideal2,intend2, ideal,intend,q102,w_2017)%>%
  mutate(age_edu = case_when 
         (agegp == "15-24" & edugp == "primary and below" ~ 1 ,
           agegp == "15-24" & edugp == "middle" ~ 2,
           agegp == "15-24" & edugp == "hs and above" ~3,
           agegp == "25-34" & edugp == "primary and below" ~ 4 ,
           agegp == "25-34" & edugp ==  "secondary" ~5  ,
           agegp == "25-34" & edugp == "hs and above"  ~ 6,
           agegp == "35-49" & edugp == "primary and below"  ~ 7 ,
           agegp == "35-49" & edugp == "middle" ~ 8,
           agegp == "35-49" & edugp == "hs and above" ~9 ,
           ))%>%
  na.omit()%>%
  arrange(prefecture, agegp, edugp)

table(fertility$edugp)
length(unique(fertility$prefecture)) #361
length(unique(fertility$county)) #2703
length(unique(fertility$prov)) 

summary(fertility$ideal)
summary(fertility$intend)



# merge with synthetic cells
load("margin_county.RData")
length(unique(margin$county)) #2821



#-----identify inconsisent geographic identifiers,no need to run this chunk-----------
fer_ind<-margin%>%
  dplyr::select(county, livebirth)%>%
  unique()%>%
  right_join(fertility, by ="county")

length(unique(fer_ind$county))  #2386


fer_ind_NA<-fer_ind%>%
  filter(is.na(livebirth))%>%
  dplyr::select(county,prov_name,prefecture)%>%
  unique()
#317 unmatched due to administrative unit change, export to excel and manually match with 2010 census 
#GB_T 2260-2007 XG1_2027
write_excel_csv(fer_ind_NA, file="fer_ind_NA.csv")

#------------------------------------------------------------------
#import imputed  gbcode 
fer_ind_NA_imputed <- read_excel("fer_ind_NA_imputed.xlsx")%>%
  dplyr::select(county, county2010)

livebirth2010<-margin%>%
  dplyr::select(county, livebirth )%>%
  unique()  #2821

#replace imputed gbcode with the previous ones and then merge with margin 
fer_ind<-fertility%>%
  left_join(fer_ind_NA_imputed, by="county")%>%
  mutate(county=if_else(!is.na(county2010), county2010, county))%>%
  dplyr::select(-county2010)%>%
  inner_join(livebirth2010, by="county")

length(unique(fer_ind$county))  #2473



#2473
census_ind<-fer_ind%>%
  dplyr::select(county)%>%
  unique()%>%
  inner_join(margin, by = "county")%>%
  na.omit()%>%
  mutate(edugp=factor(edugp, levels = c("primary and below", "secondary", "hs and above")))

length(unique(census_ind$county)) #2473



#------descriptive statistics-------
desc<-fer_ind%>%
  mutate(ideal3=if_else(ideal>3,3 ,ideal),
         intend3=if_else(intend>3,3 ,intend))

table1(~ideal+intend, data = desc,decimal=2,weights="w_2017" )


table1(~ideal+intend |agegp, data= desc,weights="w_2017" , decimal=2)
table1(~ideal+intend |edugp, data= desc,weights="w_2017" , decimal=2 )

table1(~as.factor(ideal3)+as.factor(intend3), data=desc,weights="w_2017" , decimal=3)

table1(~as.factor(ideal3)+as.factor(intend3) |agegp, data= desc, weights="w_2017")

table1(~as.factor(ideal3)+as.factor(intend3) |edugp, data= desc)




###################################
#model fit 
##################################

fit1<-lmer(ideal ~ 1+(1|county) , data = fer_ind)

fit2<-lmer(ideal ~ 1 + (1|agegp) + (1|edugp) +(1|county),
           data = fer_ind )

#no interaction, no contextual variable 
fit3<-lmer(ideal ~ 1 + (1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind )

#with interaction &  live birth
fit4<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind)

#two contetual variables 
fit5<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) + (1|age_edu)+(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind)


AIC(fit1, fit2, fit3, fit4, fit5)
BIC(fit1, fit2, fit3, fit4, fit5)
#logLik(fit1, fit2, fit3, fit4, fit5)
deviance(fit1, fit2, fit3, fit4, fit5)
df.residual(fit1, fit2, fit3, fit4, fit5)


summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)


#intention 

fit1<-lmer(intend ~ 1+(1|county) , data = fer_ind)

fit2<-lmer(intend ~ 1 + (1|agegp) + (1|edugp) +(1|county),
           data = fer_ind )

#no interaction, no contextual variable 
fit3<-lmer(intend ~ 1 + (1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind )

#with interaction &  live birth
fit4<-lmer(intend ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind,
           control = lmerControl(optimizer = "bobyqa"))

#two contetual variables 


fit5 <- lmer(intend ~ 1 + livebirth + (1 | agegp) + (1 | edugp) + (1 | age_edu) + (1 | county) + 
               (1 | prefecture) + (1 | prov),
             data = fer_ind, 
             control = lmerControl(optimizer = "bobyqa"))

#################
#final model : fit4
#################
ideal<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind,
           control = lmerControl(optimizer = "bobyqa"))

intend<-lmer(intend ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
            data = fer_ind,
            control = lmerControl(optimizer = "bobyqa"))

summ(ideal,  digits=4)
summ(intend,digits=4)


bin_model<-tab_model( ideal,intend)

print(bin_model)

# census_ind$est_ideal<-predict(ideal, newdata=census_ind,allow.new.levels = TRUE)
# census_ind$est_intend<-predict(intend, newdata=census_ind,allow.new.levels = TRUE)

#prediction with interval 
census_ind$ideal_ci<-predictInterval(ideal, newdata = census_ind, n.sims = 999, level= 0.95 )
census_ind$intend_ci<-predictInterval(intend, newdata = census_ind, n.sims = 999,level= 0.95)

summary(census_ind$ideal_ci$fit)
summary(census_ind$intend_ci$fit)
summary(census_ind$intend_ci$upr)


#----------------------
#scatter plot of ideal type & confidence interval 
#-------------------------
ideal_order<-census_ind%>%
  arrange(ideal_ci$fit)%>%
  mutate(order=row_number())


y_limits <- c(-0.5, 5.5)

scatter_ideal<-ggplot(data=ideal_order, aes(x=order, y=ideal_ci$fit))+
  geom_point(size=0.5)+
  geom_ribbon(aes(ymin=ideal_ci$lwr, ymax=ideal_ci$upr), fill="grey", alpha = 0.3)+
  scale_y_continuous(limits=y_limits)+
  theme_bw()+
  labs(x="county-age-education cells (sorted)" , y="Ideal number of children")

print(scatter_ideal)

intend_order<-census_ind%>%
  arrange(intend_ci$fit )%>%
  mutate(order=row_number())

scatter_intend<-ggplot(data=intend_order, aes(x=order, y=intend_ci$fit))+
  geom_point(size=0.5)+
  geom_ribbon(aes(ymin=intend_ci$lwr, ymax=intend_ci$upr), fill="grey", alpha = 0.3)+
  scale_y_continuous(limits=y_limits)+
  theme_bw()+
  labs(x="county-age-education cells (sorted)" , y="Intended number of children")

print(scatter_intend)

scatter_est<-cowplot::plot_grid(scatter_ideal,scatter_intend)
print(scatter_est)


ggsave(filename = "scatter_est.png" , plot =scatter_est ) 


#predict the ideal types 
# margin$est_ideal<-predict(ideal, newdata=margin,allow.new.levels = TRUE)
# margin$est_intend<-predict(intend, newdata=margin,allow.new.levels = TRUE)

# summary(margin$est_ideal)
# summary(margin$est_intend)

length(unique(margin$county))


#post stratification 
mrp_county<-census_ind%>%
  group_by(county)%>%
  summarise(ideal=weighted.mean(ideal_ci$fit, weight),
            intend=weighted.mean(intend_ci$fit, weight), 
            idealgt=if_else(ideal>=intend,1,0),
            npop=sum(weight)) 

table(mrp_county$idealgt)
summary(mrp_county$ideal)
summary(mrp_county$intend)


mrp_sub<-census_ind%>%
  group_by(county,agegp,edugp)%>%
  summarise(ideal=weighted.mean(ideal_ci$fit, weight),
            intend=weighted.mean(intend_ci$fit, weight), 
            npop=sum(weight)) 


summary(mrp_sub$ideal)
summary(mrp_sub$intend)



summaries_ideal <- mrp_sub %>%
  group_by(agegp, edugp) %>%
  summarize(
    min = min(ideal),
    q1 = quantile(ideal, 0.25),
    median = median(ideal),
    q3 = quantile(ideal, 0.75),
    max = max(ideal),
    ave=mean(ideal)
  )

summaries_intend <- mrp_sub %>%
  group_by(agegp, edugp) %>%
  summarize(
    min = min(intend),
    q1 = quantile(intend, 0.25),
    median = median(intend),
    q3 = quantile(intend, 0.75),
    max = max(intend),
    ave=mean(intend)
  )
view(summaries_intend)


# View the summaries
mrp_sub_gather<-mrp_sub%>%
  gather(ideal, intend, key = "attitude" , value ="value")


box_colors <- c("#fbe45b","#a6d0c8")

mrp_box<-ggplot(data = mrp_sub_gather, aes(x = attitude , y = value, fill=attitude))+
  geom_boxplot(outlier.shape = NA  )+
  stat_boxplot(geom = "errorbar", width = 0.5 )+
  scale_fill_manual(values = box_colors)+
  facet_wrap( ~edugp+agegp)+
  theme_bw()+
  labs(x ="", fill="")+
  theme(strip.background = element_rect(fill = "white"),
        text = element_text(size = 15))+
  scale_y_continuous(limits = c(0.5, 3))
print(mrp_box)  

ggsave(filename = "mrp_box.png" , plot =mrp_box ) 



# mrp_county_whole<-margin%>%
#   group_by(county)%>%
#   summarise(ideal=weighted.mean(est_ideal, weight),
#             intend=weighted.mean(est_intend, weight), 
#             npop=sum(weight)) 
# 
# summary(mrp_county_whole$ideal)
# summary(mrp_county_whole$intend)
# 
# mr_sub_whole<-margin%>%
#   group_by(county,agegp,edugp)%>%
#   summarise(ideal=weighted.mean(est_ideal, weight),
#             intend=weighted.mean(est_intend, weight), 
#             npop=sum(weight)) 

save(mrp, file="mrp.RData")
write.csv(mrp_county, file="mrp_county.csv")

#maping 
# county_map<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/shapfile/区县.shp")%>%
#   dplyr::select(省, 市, 县, 县代码,  geometry)%>%
#   rename(county = 县代码)


county_map<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/wholechina/export.shp")%>%
  dplyr::select(ename, gbcode,l700016_10,geometry)%>% #2872
  rename(county =gbcode, livebirth2010=l700016_10 )

length(unique(county_map$county))

province_map<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/shapfile/中国行政区.shp")

length(unique(county_map$county)) #2872
length(unique(mrp_county$county)) #2473

merged<-merge(county_map , mrp_county, by.x="county" , by.y = "county" , all= FALSE ) #2807

map_wholecounty<-merged%>%
  gather(ideal, intend, key = "measure" , value="value")%>%
  mutate(value_dis=cut(value, breaks = c(1.3, 1.5, 2, 2.5 , Inf),
                       labels = c("[1.3, 1.5)", "[1.5, 2)", "[2, 2.5)", "[2.5, 3.8] "), 
                       include.lowest = FALSE))

length(unique(map_wholecounty$county))  #2472


table(map_wholecounty$value_dis[map_wholecounty$measure == "ideal"])

# [1, 1.5)  [1.5, 2)  [2, 2.5) [2.5, 4]  
# 8      1307      1048       109 
# > ( 1048+ 109 )/2472
# [1] 0.4680421

table(map_wholecounty$value_dis[map_wholecounty$measure == "intend"])

# [1, 1.5)  [1.5, 2)  [2, 2.5) [2.5, 4]  
# 329      1271       730       142 
(730+142)/2472
countyname <- read_excel("2020中国人口普查分县资料.xlsx",  sheet = "livebirth2020")

ideal_top10_lowest<-mrp_county%>%
  arrange(ideal)%>%
  head(10)%>%
  left_join(countyname, by="county")

ideal_top10_highest<-mrp_county%>%
  arrange(desc(ideal))%>%
  head(10)%>%
  left_join(countyname, by="county")


intend_top10_lowest<-mrp_county%>%
  arrange(intend)%>%
  head(10)%>%
  left_join(countyname, by="county")

intend_top10_highest<-mrp_county%>%
  arrange(desc(intend))%>%
  head(10)%>%
  left_join(countyname, by="county")


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
         intend_dis=cut(intend, breaks = c(0.9, 1, 1.5, 2, 2.5 , Inf),
                       labels = c("[0.9,1)", "[1, 1.5)", "[1.5,2)", "[2, 2.5)", "[2.5, 4]"), 
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



#######################################
#correlation between estimates and county fertility 
livebirth2020 <- read_excel("2020中国人口普查分县资料.xlsx",  sheet = "livebirth2020")

cor2020<-mrp_county%>%
  inner_join(livebirth2020, by = "county")

cor_coef_ideal <- cor(cor2020$ideal, cor2020$livebirth2020)
cor_coef_intend <- cor(cor2020$intend, cor2020$livebirth2020)


corr_ideal<-ggplot(cor2020, aes(x = ideal, y = livebirth2020)) +
  geom_point(color = "grey") +
  labs(x = "Ideal number of children (MRP estimates)", y = "Live Birth 2020" ) +
  geom_smooth(method = "lm", color = "#a65852")+
  geom_text(aes(label = paste0("Correlation = ", round(cor_coef_ideal, 2))), x = Inf, y = Inf, 
            hjust = 2, vjust = 2, size =5)+
  theme_bw()+
  theme(axis.title = element_text(size = 14))
print(corr_ideal)

corr_intend<-ggplot(cor2020, aes(x = intend, y = livebirth2020)) +
  geom_point(color = "grey") +
  labs(x = "Intend number of children (MRP estimates)", y = "Live Birth 2020" ,size = 8) +
  geom_smooth(method = "lm",color = "#a65852")+
  geom_text(aes(label = paste0("Correlation = ", round(cor_coef_intend, 2))), x = Inf, y = Inf, 
            hjust = 2, vjust = 2, size =5)+
  theme_bw()+
  theme(axis.title = element_text(size = 14))  
print(corr_intend)

corr_combined<-cowplot::plot_grid(corr_ideal, corr_intend)

ggsave(filename = "corr_combined.png" , plot =corr_combined ) 

######################################
#validation 
#######################################
#random split sample into two 

# split the sample into two halves
set.seed(12345)
half<-createDataPartition(y=fer_ind$ideal, p=0.5, list = FALSE)
testing<-fer_ind[-half,]
otherhalf<-fer_ind[half,]

#calculating the ground truth 
groundtruth<-testing%>%
  group_by(county)%>%
  summarise(groundtruth=mean(ideal))


#function to perform analysis across samples 
perform_analysis <- function(sample) {
  # Fit the model

  ideal<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
              data = sample)
  
  # Predict ideal probabilities
  census_ind$est_ideal <- predict(ideal, newdata = census_ind, allow.new.levels = TRUE)
  
  # Compute ideal_mrp
  ideal_mrp <- census_ind %>%
    group_by(county) %>%
    summarise(ideal_mrp = weighted.mean(est_ideal, weight),
              npop = sum(weight))
  
  # Compute ideal_disag
  ideal_disag <- sample %>%
    group_by(county) %>%
    summarise(ideal_disag = mean(ideal))
  
  # Compute compare
  compare <- ideal_disag %>%
    left_join(groundtruth, by = "county") %>%
    left_join(ideal_mrp, by = "county") %>%
    mutate(ae_disag = abs(groundtruth - ideal_disag),
           ae_mrp = abs(groundtruth - ideal_mrp),
           ape_disag = ae_disag / groundtruth,
           ape_mrp = ae_mrp / groundtruth,
           mae_disag = MAE(ideal_disag, groundtruth),
           mae_mrp = MAE(ideal_mrp, groundtruth))
  
  return(compare)
}

#end of the function 
#-------------------

#append total female size 
npop<-census_ind%>%
  group_by(county)%>%
  summarise(totalfemale=sum(weight))

#loop for four sample sizes 

p_values <- c(0.1, 0.2, 0.5, 1)
#p_values <- seq(0.1, 1.0, by = 0.1)

combined<-list()

for (p in p_values) {
  
  # Create the sample data: sampling 200 times 
  sample_data <- replicate(200, createDataPartition(y = otherhalf$ideal, p = p, list = FALSE), simplify = FALSE)
  
  #sampled <- lapply(sample_data, function(indices) otherhalf[indices, ])
  #parallel processing to speed up simulation. 
  sampled <- mclapply(sample_data, function(indices) otherhalf[indices, ])
  
  # Perform the analysis
  compare <- mclapply(sampled, perform_analysis)
  sim_results <- do.call(rbind, compare) #compare has 200 subdatasets 
  
  combined[[as.character(p)]]<-sim_results 
}

combined
view(combined$`0.1`)


#evalauation statistics : MAPE AND SD 
#new variable as samplesize 
combined_id<-list()
for (p in p_values){
  combined_id[[as.character(p)]]<-combined[[as.character(p)]]%>%
    mutate(sample = as.character(p))
}


#MAPE
mape_ideal <- do.call(rbind, combined_id)%>%
  group_by(sample)%>%
  mutate(mape_mrp_combined=mean(ape_mrp ),
         mape_disag_combined=mean(ape_disag  ))%>%
  dplyr::select(sample, mape_mrp_combined, mape_disag_combined)%>%
  unique()%>%
  gather(mape_mrp_combined, mape_disag_combined, key = "method", value="mape")%>%
  mutate(method = ifelse(method == "mape_mrp_combined", "mrp", "disaggregation"))

mape_ideal




#standard deviation : consistency
sd_ideal <- do.call(rbind, combined_id)%>%
  group_by(sample)%>%
  mutate(std_mrp = sd(ideal_mrp),
         std_disag=sd(ideal_disag ))%>%
  dplyr::select(sample, std_mrp,std_disag )%>%
  unique()%>%
  gather(std_mrp, std_disag, key = "method", value = "sd")%>%
  mutate(method = ifelse(method == "std_mrp" , "mrp", "disaggregation"))



#=========================================
# do the same for intended number of children 
#=========================================

# split the sample into two halves
set.seed(12345)
half<-createDataPartition(y=fer_ind$intend, p=0.5, list = FALSE)
testing<-fer_ind[-half,]
otherhalf<-fer_ind[half,]

#calculating the ground truth 
groundtruth<-testing%>%
  group_by(county)%>%
  summarise(groundtruth=mean(intend))


#function to perform analysis across samples 
perform_analysis <- function(sample) {
  # Fit the model
  
  intend<-lmer(intend ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
              data = sample)
  
  # Predict intend probabilities
  census_ind$est_intend <- predict(intend, newdata = census_ind, allow.new.levels = TRUE)
  
  # Compute intend_mrp
  intend_mrp <- census_ind %>%
    group_by(county) %>%
    summarise(intend_mrp = weighted.mean(est_intend, weight),
              npop = sum(weight))
  
  # Compute intend_disag
  intend_disag <- sample %>%
    group_by(county) %>%
    summarise(intend_disag = mean(intend))
  
  # Compute compare
  compare <- intend_disag %>%
    left_join(groundtruth, by = "county") %>%
    left_join(intend_mrp, by = "county") %>%
    mutate(ae_disag = abs(groundtruth - intend_disag),
           ae_mrp = abs(groundtruth - intend_mrp),
           ape_disag = ae_disag / groundtruth,
           ape_mrp = ae_mrp / groundtruth,
           mae_disag = MAE(intend_disag, groundtruth),
           mae_mrp = MAE(intend_mrp, groundtruth))
  
  return(compare)
}

#end of the function 
#-------------------

#append total female size 
npop<-census_ind%>%
  group_by(county)%>%
  summarise(totalfemale=sum(weight))

#loop for four sample sizes 

p_values <- c(0.1, 0.2, 0.5, 1)
#p_values <- seq(0.1, 1.0, by = 0.1)

combined_intend<-list()

for (p in p_values) {
  
  # Create the sample data: sampling 200 times 
  sample_data <- replicate(200, createDataPartition(y = otherhalf$intend, p = p, list = FALSE), simplify = FALSE)
  
  #sampled <- lapply(sample_data, function(indices) otherhalf[indices, ])
  #parallel processing to speed up simulation. 
  sampled <- mclapply(sample_data, function(indices) otherhalf[indices, ])
  
  
  # Perform the analysis
  compare <- mclapply(sampled, perform_analysis)
  sim_results <- do.call(rbind, compare) #compare has 200 subdatasets 
  
  combined_intend[[as.character(p)]]<-sim_results 
}
combined_intend

#MALP for intend 
#new variable as samplesize 
combined_intend_id<-list()
for (p in p_values){
  combined_intend_id[[as.character(p)]]<-combined_intend[[as.character(p)]]%>%
    mutate(sample = as.character(p))
}


mape_intend <- do.call(rbind, combined_intend_id)%>%
  group_by(sample)%>%
  mutate(mape_mrp_combined=mean(ape_mrp ),
         mape_disag_combined=mean(ape_disag  ))%>%
  dplyr::select(sample, mape_mrp_combined, mape_disag_combined)%>%
  unique()%>%
  gather(mape_mrp_combined, mape_disag_combined, key = "method", value="mape")%>%
  mutate(method = ifelse(method == "mape_mrp_combined", "mrp", "disaggregation"))



#standard deviation 

sd_intend <- do.call(rbind, combined_intend_id)%>%
  group_by(sample)%>%
  mutate(std_mrp = sd(intend_mrp),
         std_disag=sd(intend_disag ))%>%
  dplyr::select(sample, std_mrp,std_disag )%>%
  unique()%>%
  gather(std_mrp, std_disag, key = "method", value = "sd")%>%
  mutate(method = ifelse(method == "std_mrp" , "mrp", "disaggregation"))

#----------------------------------
#graph results 

plot_mape_ideal<-ggplot(data=mape_ideal, aes(x=sample, y=mape, shape = method))+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16))+
  coord_flip()+
  scale_x_discrete(labels = c("5%", "10%", "25%", "50%"))+
  scale_y_continuous(breaks = seq(0.04, 0.2, by= 0.02), 
                     labels = seq(0.04, 0.2, by=0.02)) +  # Specify the desired breaks and labels
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = "grey", linetype = "dashed") ,
        legend.position="none")+
  ylab("MAPE:Ideal")+xlab("Sample")+
  labs(shape = "")

plot_mape_intend<-ggplot(data=mape_intend, aes(x=sample, y=mape, shape = method))+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16))+
  coord_flip()+
  scale_x_discrete(labels = c("5%", "10%", "25%", "50%"))+
  scale_y_continuous(breaks = seq(0.04, 0.2, by= 0.02), 
                     labels = seq(0.04, 0.2, by=0.02)) +  # Specify the desired breaks and labels
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = "grey", linetype = "dashed") ,
        legend.position="none")+
  ylab("MAPE: Intend")+xlab("Sample")+
  labs(shape = "")

plot_sd_ideal<-ggplot(data=sd_ideal, aes(x=sample, y=sd, shape = method))+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16))+
  coord_flip()+
  scale_x_discrete(labels = c("5%", "10%", "25%", "50%"))+
  scale_y_continuous(breaks = seq(0.2, 0.62, by= 0.05), 
                     labels = seq(0.2, 0.62, by= 0.05)) +  # Specify the desired breaks and labels
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        legend.position="none")+
  ylab("Standard Deviation: Ideal")+xlab("Sample")+
  labs(shape = "")

print(plot_sd_ideal)

plot_sd_intend<-ggplot(data=sd_intend, aes(x=sample, y=sd, shape = method))+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16) ,labels = c( "Disaggregation","MRP"))+
  coord_flip()+
  scale_x_discrete(labels = c("5%", "10%", "25%", "50%"))+
  scale_y_continuous(breaks = seq(0.2, 0.61, by= 0.05), 
                     labels = seq(0.2, 0.61, by=0.05)) +  # Specify the desired breaks and labels
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        legend.position = "bottom", legend.box = "horizontal", legend.justification = "center")+
  ylab("Standard Deviation: Intend")+xlab("Sample")+
  labs(shape = "")

legend<-get_legend(plot_sd_intend+
                   theme(legend.justification = "center",
                         legend.box.just = "bottom",
                         legend.text=element_text(size=12)))

eval<-cowplot::plot_grid(plot_mape_ideal+ theme(legend.position="none"),
                         plot_sd_ideal+ theme(legend.position="none"), 
                         plot_mape_intend+ theme(legend.position="none"),
                         plot_sd_intend+ theme(legend.position="none"),
                         nrow = 3 , ncol =2 , legend ,  rel_heights = c(1,1, .1))

print(eval)

ggsave(filename = "eval.png" , plot =eval ) 

#save the results to r datafile 
mape_ideal<-mape_ideal%>%
  mutate(model = "ideal")

mape_intend<-mape_intend%>%
  mutate(model = "intend")

sd_ideal<-sd_ideal%>%
  mutate(model= "ideal")

sd_intend<-sd_intend%>%
  mutate(model = "intend")

eval_stat<-rbind(mape_ideal,mape_intend,sd_ideal,sd_intend )

save(eval_stat, file="eval_stat.RData")