#county level estimate using synthetic model 
#clean up mrp-full-06

#install.packages("parallel")
install.packages("scales")
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

setwd("C:/Users/Donghui/SynologyDrive/MRP/data_mrp") #home desktop 

rm(list = ls())

load("X2017fertility_survey.RData")
fertility<-mutate(X2017fertility_survey,
                  prefecture=as.integer(c6/100),
                  prov=as.integer(prefecture/100),
                  bdate =  as.Date(paste0(q101m, "/01/", q101y), "%m/%d/%Y"),
                  cdate = as.Date("2017-07-01"),
                  age = as.numeric(format(cdate, "%Y")) - as.numeric(format(bdate, "%Y")),
                  agegp = factor(case_when(age <=24 ~ "15-24",
                                           age >24 & age <= 34 ~ "25-34", 
                                           age >34 ~ "35-49")),
                  edugp = factor(case_when(q105 <=2 ~  "primary and below",
                                           q105 == 3 ~  "middle",
                                           q105 >= 4 ~ "hs and above")),
                  ideal2 = ifelse(q401>=2, 1,0),
                  intend2= ifelse(q402>=2, 1,0),
                  ideal= q401,
                  intend=q402)%>% 
  #select, generate interactions 
  filter(age>=15 & age <=49, complete.cases(ideal, intend), ideal<8, intend<8, q102 !=5)%>%
  rename(prov_name = c1, county=c6)%>%
  dplyr::select(prov_name, prov, prefecture, county, agegp, edugp, ideal2,intend2, ideal,intend,q102)%>%
  mutate(age_edu = case_when 
         (agegp == "15-24" & edugp == "primary and below" ~ 1 ,
           agegp == "15-24" & edugp == "middle" ~ 2,
           agegp == "15-24" & edugp == "hs and above" ~3,
           agegp == "25-34" & edugp == "primary and below" ~ 4 ,
           agegp == "25-34" & edugp ==  "middle" ~5  ,
           agegp == "25-34" & edugp == "hs and above"  ~ 6,
           agegp == "35-49" & edugp == "primary and below"  ~ 7 ,
           agegp == "35-49" & edugp == "middle" ~ 8,
           agegp == "35-49" & edugp == "hs and above" ~9 ,
         ))%>%
  na.omit()%>%
  arrange(prefecture, agegp, edugp)

length(unique(fertility$prefecture)) #361
length(unique(fertility$county)) #2380
length(unique(fertility$prov)) 

table(fertility$ideal)



#descriptive graph

desc<-fertility%>%
  dplyr::select(agegp,edugp,ideal,intend)%>%
  gather(ideal, intend, key= "preferences", value= "values")


des_fig<-ggplot(data= desc) +
  geom_boxplot(mapping=aes(x=preferences, y=values, fill=preferences))+
  facet_wrap(~ edugp+ agegp, ncol = 3)


des_fig<-ggplot(data= desc) +
  geom_bar(mapping=aes(x=preferences, y=values, fill=preferences), stat = "summary")+
  facet_wrap(~ edugp+ agegp, ncol = 3)+
  scale_fill_manual(values = c("steelblue", "#D16103"), labels = c("Ideal", "Intend")) +
  theme_bw()

print(des_fig)



# merge with synthetic cells

load("margin_county.RData")
length(unique(margin$county)) #2821


census_ind<-fertility%>%
  dplyr::select(county)%>%
  unique()%>%
  inner_join(margin, by = "county")
  
length(unique(census_ind$county)) #2380
length(unique(margin$county)) #2380


fer_ind<-census_ind%>%
  dplyr::select(county, livebirth)%>%
  unique()%>%
  inner_join(fertility, by ="county") 

length(unique(fer_ind$county)) #2380




table1(~ ideal+intend+ideal2+intend2+agegp +edugp, data=fertility)
table1(~agegp +edugp,data=census_ind)

table1(~ ideal+intend+ideal2+intend2+agegp +edugp, data=fertility)


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

bin_model<-tab_model( intend, ideal)



census_ind$est_ideal<-predict(ideal, newdata=census_ind,allow.new.levels = TRUE)
census_ind$est_intend<-predict(intend, newdata=census_ind,allow.new.levels = TRUE)

# 
# margin$est_ideal<-predict(ideal, newdata=margin,allow.new.levels = TRUE)
# margin$est_intend<-predict(intend, newdata=margin,allow.new.levels = TRUE)
# 
# summary(margin$est_ideal)
# summary(margin$est_intend)


#post stratification 
mrp_county<-census_ind%>%
  group_by(county)%>%
  summarise(ideal=weighted.mean(est_ideal, weight),
            intend=weighted.mean(est_intend, weight), 
            npop=sum(weight)) 
summary(mrp_county$ideal)
summary(mrp_county$intend)


mrp_sub<-census_ind%>%
  group_by(county,agegp,edugp)%>%
  summarise(ideal=weighted.mean(est_ideal, weight),
            intend=weighted.mean(est_intend, weight), 
            npop=sum(weight)) 

summary(mrp_sub$ideal)
summary(mrp_sub$intend)

summaries <- mrp_sub %>%
  group_by(agegp, edugp) %>%
  summarize(
    min = min(intend),
    q1 = quantile(intend, 0.25),
    median = median(intend),
    q3 = quantile(intend, 0.75),
    max = max(intend),
    ave=mean(intend)
  )





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
  theme(strip.background = element_rect(fill = "white"))+
  scale_y_continuous(limits = c(0.5, 3))
  
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
write.csv(mrp, file="mrp.csv")

#maping 
county_map<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/shapfile/区县.shp") 
ninline<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/shapfile/九段线.shp")
province_map<-st_read("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/shapfile/中国行政区.shp")

map_wholecounty<-county_map%>%
  dplyr::select(省, 市, 县, 县代码,  geometry)%>%
  rename(county= 县代码)%>%
  inner_join(mrp_county, by="county")%>%
  gather(ideal, intend, key = "measure" , value="value")%>%
  mutate(value_dis=cut(value, breaks = c(1, 1.5, 2, 2.5 , Inf),
                       labels = c("[1, 1.5)", "[1.5,2)", "[2, 2.5)", "[2.5, 4] "), 
                       include.lowest = FALSE))

table(map_wholecounty$value_dis)

table_ideal<-map_wholecounty%>%
  filter(measure=="ideal")

table(table_ideal$value_dis)


table_intend<-map_wholecounty%>%
  filter(measure=="intend")

table(table_intend$value_dis)


# province<-ggplot(data=province_map)+
#   geom_sf( color = "black", fill = NA)+
#   geom_sf(data = county_map, color = "white", fill = NA, alpha=0.5)


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


#by age and edu group 
map_gp<-county_map%>%
  dplyr::select(县代码,  geometry)%>%
  rename(county= 县代码)%>%
  inner_join(mrp_sub, by="county")%>%
  mutate(ideal_dis=cut(ideal, breaks = c(0.9, 1, 1.5, 2, 2.5 , Inf),
                     labels = c("below 1", "1-1.5", "1.5-2", "2-2.5", "2.5 and above"), 
                     include.lowest = FALSE),
         intend_dis=cut(intend, breaks = c(0.9, 1, 1.5, 2, 2.5 , Inf),
                       labels = c("below 1", "1-1.5", "1.5-2", "2-2.5", "2.5 and above"), 
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