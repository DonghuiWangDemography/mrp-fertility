#split sample validation 
#updated on 2024-12-22
library(tidyverse)
library(haven)
library(lme4)
library(arm)
library(jtools)
library(dplyr)
library(table1)
library(readxl)




setwd("C:/Users/donghuiwang/SynologyDrive/MRP/data_mrp") #office desktop 

load("census_ind.RData")
load("fer_ind.RData")


#=========================================
#split sample program for different sample size 
#===========================================

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
  census_ind$est_ideal <- predict(ideal, newdata = census_ind, allow.new.levels = TRUE)  #no confidence inverval to same computing time
  
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