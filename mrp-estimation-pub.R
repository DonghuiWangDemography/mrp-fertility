#data cleaning & estimation 
#adapted 
#updated on 2024/12/11
library(tidyverse)
library(haven)
library(lme4)
library(arm)
library(jtools)
library(dplyr)
library(table1)
library(readxl)




#=======================================
#step 1: data cleaning, merging
#======================================
load("X2017fertility_survey.RData")

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
         age= as.integer((cdate - bdate) / 365.25),
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
  #filter age only : 180211
  filter(age>=15 & age <=49, complete.cases(ideal, intend), ideal<10, intend<10)%>%
  rename(prov_name = c1, county=countycode)%>%
  dplyr::select(prov_name, prov, prefecture, county, agegp, edugp, ideal2,intend2, ideal,intend,w_2017)%>%
  mutate(age_edu = case_when 
         (agegp == "15-24" & edugp == "primary and below" ~ 1 ,
           agegp == "15-24" & edugp == "secondary" ~ 2,
           agegp == "15-24" & edugp == "hs and above" ~3,
           agegp == "25-34" & edugp == "primary and below" ~ 4 ,
           agegp == "25-34" & edugp ==  "secondary" ~5  ,
           agegp == "25-34" & edugp == "hs and above"  ~ 6,
           agegp == "35-49" & edugp == "primary and below"  ~ 7 ,
           agegp == "35-49" & edugp == "secondary" ~ 8,
           agegp == "35-49" & edugp == "hs and above" ~9 ,
         ))%>%
  na.omit()  %>%
  arrange(prefecture, agegp, edugp)  #174394

length(unique(fertility$prefecture)) #361
length(unique(fertility$county)) #2703
length(unique(fertility$prov)) 


# merge with synthetic cells
load("margin_county.RData")
length(unique(margin$county)) #2821

#-----identify inconsistent geographic identifiers,no need to rerun this chunk ever time-----------
# fer_ind<-margin%>%
#   dplyr::select(county, livebirth)%>%
#   unique()%>%
#   right_join(fertility, by ="county")
# 
# length(unique(fer_ind$county))  #2386
# 
# 
# fer_ind_NA<-fer_ind%>%
#   filter(is.na(livebirth))%>%
#   dplyr::select(county,prov_name,prefecture)%>%
#   unique()
# #317 unmatched due to administrative unit change, export to excel and manually match with 2010 census 
# #GB_T 2260-2007 XG1_2027
# write_excel_csv(fer_ind_NA, file="fer_ind_NA.csv")

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
  dplyr::select(-prefecture)%>%
  mutate(county=if_else(!is.na(county2010), county2010, county),
         #update prefecture according to the imputed county id 
         prefecture=as.integer(county/100))%>%
  dplyr::select(-county2010)%>%
  inner_join(livebirth2010, by="county") #162426

length(unique(fer_ind$county))  #2473

#merge census with fertility survey to ensure consistencies between the two (removew those unmatched counties)
census_ind<-fer_ind%>%
  dplyr::select(county,prefecture)%>%
  unique()%>%
  inner_join(margin, by = c("county", "prefecture"))%>%
  na.omit()%>%
  mutate(edugp=factor(edugp, levels = c("primary and below", "secondary", "hs and above")))

length(unique(census_ind$county)) 


save(fer_ind, file="fer_ind.RData") 
save(census_ind, file="census_ind.RData")  #census (w weights), after merge with fertility survey 

#==========end of data cleaning procedure===============

#------descriptive statistics----------
load("fer_ind.RData")
load("census_ind.RData")

desc<-fer_ind%>%
  mutate(ideal3=if_else(ideal>3,3 ,ideal),
         intend3=if_else(intend>3,3 ,intend))

table1(~ideal+intend, data = desc,decimal=2,weights="w_2017" )

table1(~ideal+intend |agegp, data= desc,weights="w_2017" , decimal=2)
table1(~ideal+intend |edugp, data= desc,weights="w_2017" , decimal=2 )

table1(~as.factor(ideal3)+as.factor(intend3), data=desc,weights="w_2017" , decimal=3)
table1(~as.factor(ideal3)+as.factor(intend3) |agegp, data= desc, weights="w_2017")
table1(~as.factor(ideal3)+as.factor(intend3) |edugp, data= desc, weights="w_2017")



#==================================
# Step 2: model fit, post-stratification  
#===================================

fit1<-lmer(ideal ~ 1+(1|county) , data = fer_ind)

fit2<-lmer(ideal ~ 1 + (1|agegp) + (1|edugp) +(1|county),
           data = fer_ind, 
           control = lmerControl(optimizer = "bobyqa") )

#no interaction, no contextual variable 
fit3<-lmer(ideal ~ 1 + (1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind, 
           control = lmerControl(optimizer = "bobyqa") )

#fixed effect: live birth
fit4<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind)

#livebirth +interaction term 
fit5<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) + (1|age_edu)+(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind)


AIC(fit1, fit2, fit3, fit4, fit5)
BIC(fit1, fit2, fit3, fit4, fit5)

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
           data = fer_ind,
           control = lmerControl(optimizer = "bobyqa") )

#with interaction &  live birth
fit4<-lmer(intend ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
           data = fer_ind,
           control = lmerControl(optimizer = "bobyqa"))

#two contractual variables 

fit5 <- lmer(intend ~ 1 + livebirth + (1 | agegp) + (1 | edugp) + (1 | age_edu) + (1 | county) + 
               (1 | prefecture) + (1 | prov),
             data = fer_ind, 
             control = lmerControl(optimizer = "bobyqa"))
#boundary (singular) fit : no need to fit a complicated interaction model 


AIC(fit1, fit2, fit3, fit4, fit5)
BIC(fit1, fit2, fit3, fit4, fit5)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)



#------------------------
#final model : fit4

ideal<-lmer(ideal ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
            data = fer_ind,
            control = lmerControl(optimizer = "bobyqa"))

intend<-lmer(intend ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
             data = fer_ind,
             control = lmerControl(optimizer = "bobyqa"))

summary(ideal,  digits=4)
summary(intend,digits=4)

bin_model<-tab_model( ideal,intend)

print(bin_model)


#alternative binary estimates 

ideal2<-glmer(ideal2 ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
              data = fer_ind,
              family=binomial("probit"))


intend2<-glmer(intend2 ~ 1 +livebirth +(1|agegp) + (1|edugp) +(1|county)+(1|prefecture)+ (1|prov),
               data = fer_ind,
               family=binomial("probit"))

alt_model<-tab_model( ideal2,intend2)

print(alt_model)



# census_ind$est_ideal<-predict(ideal, newdata=census_ind,allow.new.levels = TRUE)
# census_ind$est_intend<-predict(intend, newdata=census_ind,allow.new.levels = TRUE)

#prediction with interval 
set.seed(12345)
census_ind$ideal_ci<-predictInterval(ideal, newdata = census_ind, n.sims = 999, level= 0.95 )
census_ind$intend_ci<-predictInterval(intend, newdata = census_ind, n.sims = 999,level= 0.95)

summary(census_ind$ideal_ci$fit)
summary(census_ind$intend_ci$fit)
summary(census_ind$intend_ci$upr)


census_ind$ideal2<-predict(ideal2, newdata=census_ind,allow.new.levels = TRUE)
census_ind$intend2<-predict(intend2, newdata=census_ind,allow.new.levels = TRUE)

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
            ideal2=weighted.mean(pnorm(ideal2) , weight),
            intend2=weighted.mean(pnorm(intend2) , weight),
            npop=sum(weight)) 

summary(mrp_county$ideal)
summary(mrp_county$intend)

summary(mrp_county$ideal2)
summary(mrp_county$intend2)


# > summary(mrp_county$ideal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.334   1.876   1.985   2.015   2.100   3.765 
# > summary(mrp_county$intend)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.113   1.608   1.880   1.888   2.102   3.936 


mrp_sub<-census_ind%>%
  group_by(county,agegp,edugp)%>%
  summarise(ideal=weighted.mean(ideal_ci$fit, weight),
            intend=weighted.mean(intend_ci$fit, weight), 
            npop=sum(weight)) 


summary(mrp_sub$ideal)
summary(mrp_sub$intend)


# > summary(mrp_sub$ideal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.203   1.834   1.958   1.987   2.095   3.824 
# > summary(mrp_sub$intend)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8824  1.5846  1.8282  1.8522  2.0852  4.0753 


save(mrp_county, file="mrp_county.RData")
save(mrp_sub, file="mrp_sub.RData")


write.csv(mrp_county, file="mrp_county.csv")
write.csv(mrp_sub, file="mrp_sub.csv")


#-end of model fitting & post stratification 
