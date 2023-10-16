
#mrsp for county level estimates 
#prepare for county-level synct margin 

install.packages("reshape2")


library(tidyverse)
library(lme4)
library(haven)
library(readxl)
library(reshape2)

setwd("C:/Users/Donghui/SynologyDrive/MRP/data_mrp") #home desktop 


X2010年全国行政区划代码 <- read_excel("2010年全国行政区划代码.xlsx", 
                             sheet = "county")

county_gbcode<-X2010年全国行政区划代码%>%
  mutate(
    county=as.numeric(区县代码)/1000000,
    prefecture=as.numeric(市级代码)/100000000) %>% #3142 
  dplyr::select(-市级代码, -区县代码)

save(county_gbcode, file="county_gbcode.RData")


county2010 <- read_excel("C:/Users/Donghui/SynologyDrive/MRP/data_mrp/2010census/2010prfecture.xlsx", 
                         sheet = "county_2010")%>%
  na.omit()%>%
  unique() #3241 include province and prefecture aggreates 


county<-county_gbcode%>%
  inner_join(county2010, join_by(省级名称==prov_name,  区县名称==county_name , 重合 ==重合 ))   %>% #2827 obs
  mutate( female_14=female0+female1_4+female5_9+female10_14,  #below 14 
          female_15_24=female15_19+female20_24,
          female_25_34=female25_29+female30_34,
          female_35_49=female35_39 +female40_44 +female45_49,
          female_50=female50_54+female55_59 +female60_64+female65_69+female70_74 +
                   female75_79+female80_84+female85,
         
         # education starts to count as people age 6yr+. 
         #Those less than6, count as no primary so that to match with the individual data
         female_totaled=female_noed+female_primary+female_middle+female_hs+female_zhuanke+female_daxue,
         
         female_primblw=female_noed+female_primary+(totalfemale-female_totaled),
         female_hs=female_hs+female_zhuanke+female_daxue,
         
         #check consistency 
         female_totalage=female_14+female_15_24+female_25_34+female_35_49+female_50,
         difag=totalfemale-female_totalage)%>%
  filter(difag == 0) %>%  #six inconsistencies, remove 
  #calculate proportion at county level 
  mutate_at(vars(starts_with("female_")),
            list(p= ~ . / totalfemale))%>%
  rename(prov = 省级代码, prov_name = 省级名称, county_name= 区县名称 )%>%
  select(prov, prov_name,prefecture,county_name,county,livebirth,totalfemale ,
              matches("female_"))

#1.margin per county for edu and agegp 
#2.cross tabe to get cell percentages for edu and agegp combination 
#3.multiple county total count 
#4.remove out-of-target subgoups 

age_margin<-county %>%
 melt(id.vars = "county" , measure.vars = c("female_14_p", "female_15_24_p", "female_25_34_p","female_35_49_p", "female_50_p"))%>%
  arrange(county, variable)


margin<-county%>%
  #edu margin 
  melt(id.vars = c("county" , "totalfemale", "prov", "prefecture" , "livebirth"), measure.vars = c("female_primblw_p", "female_middle_p", "female_hs_p"))%>%
  arrange(county, variable)%>%
  inner_join(age_margin, by ="county" ,relationship = "many-to-many")%>%
  mutate(syc_count=value.x*value.y*totalfemale)%>%
  filter(variable.y !="female_14_p" , variable.y !="female_50_p")%>%
  mutate(edugp = if_else(variable.x == "female_primblw_p" , "primary and below",
                         if_else(variable.x=="female_middle_p" , "middle",
                                 if_else(variable.x == "female_hs_p" , "hs and above" , NA))),
         agegp = if_else(variable.y =="female_15_24_p" , "15-24",
                         if_else(variable.y=="female_25_34_p", "25-34",
                                 if_else(variable.y == "female_35_49_p", "35-49" , NA))))%>%
rename(weight = syc_count)%>%
select(county, weight, edugp ,agegp, prov, prefecture,livebirth )%>% 
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
  tibble()

save(margin, file="margin_county.RData")

