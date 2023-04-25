
## @knitr setup2

library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(sjPlot)
library(car)
library(knitr)


absol_hist<-read.csv ("absol_hist.csv") # this dataset is used to test for assortative mating (model 1)
all_kitt<-read.csv("all_kitt_data3.csv") #this dataset is used for models 2, 3 and 5
next_season_repro<-read.csv("next_season_repro.csv") # this dataset is used for model 4
pers_colony<-read.csv("pers_scores_with_colony.csv") # this dataset and the one below are used to conduct the randomisations
absol_ran<-read.csv("absol_random.csv")

absol_hist$colony<-as.factor(absol_hist$colony)
absol_hist$male_ring<-as.factor(absol_hist$male_ring)
absol_hist$female_ring<-as.factor(absol_hist$female_ring)


all_kitt$colony<-as.factor(all_kitt$colony)
all_kitt$male_ring<-as.factor(all_kitt$male_ring)
all_kitt$female_ring<-as.factor(all_kitt$female_ring)
all_kitt$year<-as.factor(all_kitt$year)
all_kitt$nest<-as.factor(all_kitt$nest)
all_kitt$chick_sur<-as.factor(all_kitt$chick_sur)
all_kitt$season_outcome<-as.factor(all_kitt$season_outcome)
all_kitt$ulti_outcome<-as.factor(all_kitt$ulti_outcome)
all_kitt$male_next_season_sur<-as.factor(all_kitt$male_next_season_sur)
all_kitt$female_next_season_sur<-as.factor(all_kitt$female_next_season_sur)

## @knitr assort_mating_setup

#create transformed response variable and scale the female boldness scores

absol_hist$male_sqrt<-sqrt(max(absol_hist$male_pers.x)-absol_hist$male_pers.x)
absol_hist$female_pers_sc<-scale(absol_hist$female_pers.x)

#check for colinearity
colin_model <- glm(male_sqrt~  colony +female_pers_sc, data = absol_hist)
summary(colin_model)
vif(colin_model) # vif scores < 5 acceptable

## @knitr assort_mating_model

#linear mixed model testing relationship between male and female boldness scores, interacting with colony
mating_model<-lmer(male_sqrt ~ female_pers_sc+
                     colony+
                     (1|female_ring), #female id random effect
                   data = absol_hist, na.action=na.fail, REML = FALSE)
summary(mating_model)
r.squaredGLMM(mating_model)

## @knitr mating_dredge
mating_mod<-dredge(mating_model, extra = alist(deviance))
mat_sb<-subset(mating_mod, delta<2) #subset all models with delta < 2
mating_mod_sub<- subset(mat_sb, !nested(.))# remove nested models
mating_mod_coeffs<-coefTable(mating_mod_sub, full=TRUE) # create final table of coefficients
mating_mod_coeffs

## @knitr model_1_setup

# exclusions
repro_pers<-all_kitt[!is.na(all_kitt$absol),] # pairs where required pers data are missing removed
repro_pers<-repro_pers[!is.na(repro_pers$chick_sur),]# pairs where repro data not available

#scaling
repro_pers$male_pers_sc<-scale(repro_pers$male_pers) 
repro_pers$female_pers_sc<-scale(repro_pers$female_pers) 
repro_pers$absol_pers_sc<-scale(repro_pers$absol) 

## @knitr model_1_des

#males slightly bolder than females overall
repro_pers%>%summarise(average=mean(male_pers),med=median(male_pers), 
                       sd=sd(male_pers))

repro_pers%>%summarise(average=mean(female_pers),med=median(female_pers), 
                       sd=sd(female_pers))

#both males and females bolder at Grumant
repro_pers%>%group_by(colony)%>%summarise(average=mean(male_pers),med=median(male_pers), 
                                          sd=sd(male_pers))

repro_pers%>%group_by(colony)%>%summarise(average=mean(female_pers),med=median(female_pers), 
                                          sd=sd(female_pers))

## @knitr model_2_sample_sizes

unique(repro_pers$pair_ID)#124 pairs
unique(repro_pers$cycle_pair)#213 breeding attempts

unique(repro_pers$male_ring)#104 male birds
unique(repro_pers$female_ring)#107 female birds

unique(repro_pers$year) #11 breeding seasons
# sample size per year
with(repro_pers, tapply(pair_ID, year, FUN = function(x) length(unique(x)))) 
#pairs per colony
with(repro_pers, tapply(pair_ID, colony, FUN = function(x) length(unique(x))))

## @knitr model_1_colin

#assumption of multicolinearity

colin_model <- glm(chick_sur ~ male_pers_sc +female_pers_sc, data = repro_pers, family = "binomial")
summary(colin_model)
vif(colin_model)

colin_model <- glm(chick_sur ~ male_pers_sc +absol_pers_sc, data = repro_pers, family = "binomial")
summary(colin_model)
vif(colin_model)

colin_model <- glm(chick_sur ~ female_pers_sc +absol_pers_sc, data = repro_pers, family = "binomial")
summary(colin_model)
vif(colin_model)

## @knitr model_1_gm

repro_pers_mod<- glmer(chick_sur ~ female_pers_sc+
                         male_pers_sc+
                         absol_pers_sc+
                         colony+ 
                         absol_pers_sc:colony+
                         (1|year)+
                         (1|pair_ID)+
                         (1|male_ring)+
                         (1|female_ring), 
                       data = repro_pers, na.action=na.fail, family = binomial(link = "logit"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))

summary(repro_pers_mod)
r.squaredGLMM(repro_pers_mod)

## @knitr model_1_sel

sur_mol_ab<-dredge(repro_pers_mod, extra = alist(deviance)) # create all possible models
sur_mol_absb<-subset(sur_mol_ab, delta<2) ##subsets models delta <2
nest_repro_pers <- subset(sur_mol_absb, !nested(.)) # remove nested models
repro_pers_coeffs<-coefTable(nest_repro_pers, full=TRUE) # create final coefficient tables
repro_pers_coeffs

## @knitr model_1_sim_mod

repro_pers_mod_ab<- glmer(chick_sur ~ #female_pers_sc+
                         #male_pers_sc+
                         absol+ #changed to original variable so that the graph axis is not scaled i.e positive values only
                         #colony+ 
                         #absol_pers_sc:colony+
                         (1|year)+
                         (1|pair_ID)+
                         (1|male_ring)+
                         (1|female_ring), 
                       data = repro_pers, na.action=na.fail, family = binomial(link = "logit"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))

repro_pers_mod_fe<- glmer(chick_sur ~ female_pers_sc+ # does not matter is this is scaled
                         #male_pers_sc+
                         #absol_pers_sc+
                         #colony+ 
                         #absol_pers_sc:colony+
                         (1|year)+
                         (1|pair_ID)+
                         (1|male_ring)+
                         (1|female_ring), 
                       data = repro_pers, na.action=na.fail, family = binomial(link = "logit"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))
## @knitr model_1_sj

repro_pers$chick_sur_num <- as.numeric(repro_pers$chick_sur) - 1 #creates scale for chick survival probability

pers_chick_sur<-plot_model(repro_pers_mod_ab, type = "pred", terms=c ("absol[all]"))
repro_pers_plot1<-as.data.frame(pers_chick_sur[["data"]]) #data turned into data frame
repro_pers_plot1$absol<-repro_pers_plot1$x # variables are renamed so they can be used in the same graph 
repro_pers_plot1$chick_sur<-repro_pers_plot1$predicted

rep_in<-plot_model(repro_pers_mod_fe, type = "pred", terms=c ("female_pers_sc[all]"))
repro_pers_plot2<-as.data.frame(rep_in[["data"]]) #data turned into data frame
repro_pers_plot2$female_pers_sc<-repro_pers_plot2$x # variables are renamed so they can be used in the same graph 
repro_pers_plot2$chick_sur<-repro_pers_plot2$predicted



## @knitr model_1_g

repro_pers_plot1<-ggplot()+ geom_point(data= repro_pers, aes(x = absol, y = chick_sur_num), alpha = 0.2, size = 4)+
  geom_ribbon(data=repro_pers_plot1, aes(x = absol, ymin = conf.low, ymax = conf.high), fill = "lightgrey")+
 geom_line(data=repro_pers_plot1, aes(x = absol, y = chick_sur), linewidth = 1.5)+
 theme_classic()+
 xlab("Absolute difference in boldness score")+
 ylab("Breeding outcome in year t")+
theme(axis.text = element_text(size = 15))+
theme(axis.title= element_text(size=15))+
theme (axis.title.y = element_text (margin = margin (t = 0, r = 13, b = 0, l = 0)))+
theme (axis.title.x = element_text (margin = margin (t = 13, r = 0, b = 0, l = 0)))

repro_pers_plot1

#repro_pers_plot3

repro_pers_plot3<-ggplot()+ geom_point(data= repro_pers, aes(x = female_pers_sc, y = chick_sur_num), alpha = 0.2, size = 4)+
  geom_ribbon(data=repro_pers_plot2, aes(x = female_pers_sc, ymin = conf.low, ymax = conf.high), fill = "lightgrey")+
  geom_line(data=repro_pers_plot2, aes(x = female_pers_sc, y = chick_sur), linewidth = 1.5)+
  theme_classic()+
  xlab("Female boldness score")+
  ylab("Breeding outcome in year t")+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=15))+
  theme (axis.title.y = element_text (margin = margin (t = 0, r = 13, b = 0, l = 0)))+
  theme (axis.title.x = element_text (margin = margin (t = 13, r = 0, b = 0, l = 0)))

repro_pers_plot3

## @knitr model_2_setup

#exclusions
suc_rep<-all_kitt[!is.na(all_kitt$chick_sur),]# remove nests with no breeding success info
suc_rep<-suc_rep[!is.na(suc_rep$season_outcome),] # remove nests with no next season repairing outcome info

## @knitr model_2_des

unique(suc_rep$pair_ID)#139 pairs
unique(suc_rep$cycle_pair)#218 breeding attempts

unique(suc_rep$male_ring)#108 male birds
unique(suc_rep$female_ring)#111 female birds

unique(suc_rep$year) #11 breeding seasons
with(suc_rep, tapply(pair_ID, year, FUN = function(x) length(unique(x)))) # sample size per year

## @knitr model_2_gm

repro_repair  <- glmer(season_outcome ~ chick_sur+
                         colony+ 
                         (1|year)+
                         (1|pair_ID)+
                         (1|male_ring)+
                         (1|female_ring), 
                       data = suc_rep, na.action=na.fail, family = binomial(link = "logit"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))
summary(repro_repair)
r.squaredGLMM(repro_repair)

## @knitr model_2_sel

repair_repro<-dredge(repro_repair, extra = alist(deviance)) # create all possible models
rep_repro_sb<-subset(repair_repro, delta<2) ##subsets models delta <2
repair_repro_coeffs<-coefTable(rep_repro_sb, full=TRUE) # final coefficient table 
repair_repro_coeffs


## @knitr model3_g

repro_repair_g  <- glmer(season_outcome ~ chick_sur+
                         (1|year)+
                         (1|pair_ID)+
                         (1|male_ring)+
                         (1|female_ring), 
                       data = suc_rep, na.action=na.fail, family = binomial(link = "logit"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))
## @knitr model3_sj

rep_repair_chick_sur<-plot_model(repro_repair, type = "pred", terms=c ("chick_sur"))
repro_plot2<-as.data.frame(rep_repair_chick_sur[["data"]])
repro_plot2$season_outcome2<-repro_plot2$predicted # variables are renamed so they can be used in the same graph 

## @knitr model3_rename

for (a in 1:(nrow(repro_plot2))){
  
  if (repro_plot2$x[a] == "0") 
    
  {repro_plot2$chick_sur2[a]<-"Failure"} 
  else {repro_plot2$chick_sur2[a]<-"Success"}
}

for (a in 1:(nrow(suc_rep))){
  
  if (suc_rep$chick_sur[a] == "0") 
    
  {suc_rep$chick_sur2[a]<-"Failure"} 
  else {suc_rep$chick_sur2[a]<-"Success"}
}

#new scale created for season outcome to make graph more logical
suc_rep$season_outcome2<- as.numeric(suc_rep$season_outcome) - 1

## @knitr model3_graph

repro_plotg2<-ggplot()+ 
  geom_jitter(data = suc_rep, aes(x =chick_sur2, y = season_outcome2), width =0.35, height = 0.0, alpha = 0.2, size = 5)+ 
  geom_errorbar(data= repro_plot2, aes(x = chick_sur2, ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.8)+
  geom_point(data = repro_plot2, aes( x = chick_sur2, y = season_outcome2), colour ="black", fill = "lightgrey", pch=21, size = 6)+
  theme_classic()+
  xlab("Observed breeding outcome in year t")+
  ylab("Predicted probability of re-pairing in year t + 1")+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=15))+
  theme (axis.title.y = element_text (margin = margin (t = 0, r = 13, b = 0, l = 0)))+
  theme (axis.title.x = element_text (margin = margin (t = 13, r = 0, b = 0, l = 0)))

repro_plotg2  

## @knitr mod4_setup

ns_repro<-next_season_repro[!is.na(next_season_repro$season_outcome),]
ns_repro<-ns_repro[!is.na(ns_repro$next_season),]

ns_repro$colony<-as.factor(ns_repro$colony)
ns_repro$sex<-as.factor(ns_repro$sex)
ns_repro$ring<-as.factor(ns_repro$ring)
ns_repro$pair_ID<-as.factor(ns_repro$pair_ID)
ns_repro$year_t<-as.factor(ns_repro$year_t)
ns_repro$season_outcome<-as.factor(ns_repro$season_outcome)
ns_repro$next_season<-as.factor(ns_repro$next_season)
ns_repro$season_tru<-as.factor(ns_repro$season_tru)
ns_repro$cycle_pair<-as.factor(ns_repro$cycle_pair)


## @knitr mod4_ss

unique(ns_repro$cycle_pair) #175 breeding attempts
unique(ns_repro$pair_ID) # 107 pairs
with(ns_repro, tapply(cycle_pair, year_t, FUN = function(x) length(unique(x)))) # sample per year
with(ns_repro, tapply(ring, sex, FUN = function(x) length(unique(x)))) # 75 females, 70 males


## @knitr mod4_global

next_season_mol  <- glmer(next_season ~ colony+
                            season_outcome+
                            season_outcome:sex+
                            sex+
                            (1|year_t)+
                            (1|pair_ID)+
                            (1|ring), 
                          data = ns_repro,na.action=na.fail, family = binomial(link = "logit"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e5)))
summary(next_season_mol)
r.squaredGLMM(next_season_mol)

## @knitr model4_dredge

next_season_dre<-dredge(next_season_mol, extra = alist(deviance))
next_season_dre2<-subset(next_season_dre, delta<2) ##subsets models delta <2
next_season_dre3<- subset(next_season_dre2, !nested(.))# remove nested models
next_season_dre_coeff<-coefTable(next_season_dre3, full=TRUE)
next_season_dre_coeff


## @knitr model5_setup


pers_repair<-all_kitt[!is.na(all_kitt$ulti_outcome),]# where ultimate outcome of the pair is unknown- exclude
pers_repair<-pers_repair[!is.na(pers_repair$absol),]# exclude pairs where both personalities are not available

#scaling

pers_repair$male_pers_sc<-scale(pers_repair$male_pers) 
pers_repair$female_pers_sc<-scale(pers_repair$female_pers) 
pers_repair$absol_pers_sc<-scale(pers_repair$absol) 

## @knitr model5_des

pers_repair%>%summarise(average=mean(male_pers),med=median(male_pers), #males slightly bolder than females overall
                        sd=sd(male_pers))

pers_repair%>%summarise(average=mean(female_pers),med=median(female_pers), 
                        sd=sd(female_pers))

pers_repair%>%group_by(colony)%>%summarise(average=mean(male_pers),med=median(male_pers), #both males and females bolder at grumant
                                           sd=sd(male_pers))

pers_repair%>%group_by(colony)%>%summarise(average=mean(female_pers),med=median(female_pers), 
                                           sd=sd(female_pers))

## @knitr model5_ss

#sample sizes

unique(pers_repair$pair_ID)#98 pairs
unique(pers_repair$cycle_pair)#166 breeding attempts

unique(pers_repair$male_ring)#81 male birds
unique(pers_repair$female_ring)#87 female birds

unique(pers_repair$year) #11 breeding seasons
with(pers_repair, tapply(pair_ID, year, FUN = function(x) length(unique(x))))# sample size per year
with(pers_repair, tapply(pair_ID, colony, FUN = function(x) length(unique(x))))# sample size per colony


## @knitr model5_colin

#assumption multicolinearity

colin_model11 <- glm(ulti_outcome ~ male_pers_sc +female_pers_sc, data = pers_repair, family = "binomial")
summary(colin_model11)
vif(colin_model11)

colin_model22 <- glm(ulti_outcome ~ male_pers_sc +absol_pers_sc, data = pers_repair, family = "binomial")
summary(colin_model22)
vif(colin_model22)

colin_model33 <- glm(ulti_outcome ~ female_pers_sc +absol_pers_sc, data = pers_repair, family = "binomial")
summary(colin_model33)
vif(colin_model33)


## @knitr mod5_global

repair_mol_abs  <- glmer(ulti_outcome ~ female_pers_sc+
                           male_pers_sc+
                           absol_pers_sc+
                           colony+
                           missed_seasons+ # to account for pairs which had multiple opportunities to re-pair without being recorded
                           (1|year)+
                           (1|pair_ID)+
                           (1|male_ring)+
                           (1|female_ring), 
                         data = pers_repair,na.action=na.fail, family = binomial(link = "logit"),
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)))

summary(repair_mol_abs)
r.squaredGLMM(repair_mol_abs)


## @knitr model5_ms

repair_mol_ab<-dredge(repair_mol_abs,extra = alist(deviance))
repair_mol_absb<-subset(repair_mol_ab, delta<2) ##subsets models delta <2
repair_m1_nest<- subset(repair_mol_absb, !nested(.))# remove nested models
repair_pers_coeffs<-coefTable(repair_m1_nest, full=TRUE)
repair_pers_coeffs

## @knitr true_diff

absol_ran$trueDiff <- absol_ran$orig_absol - absol_ran$new_absol 

## @knitr perm_sep

#colonies separately 
absol_ran_g<-subset(absol_ran, colony.x == "grumant")
absol_ran_p<-subset(absol_ran, colony.x == "pyramiden")

pers_colony_g<-subset(pers_colony, colony == "Grumant")
pers_colony_p<-subset(pers_colony, colony == "Pyramiden")


## @knitr loop_per       

#randomise personality of new partner across the whole population
## sampling randomly from the true personality distribution

# grumant
n_perms <- 10000 # typical minimum for randomisation/permutation
perms_output <- list()
mean_trueDiff <- mean(absol_ran_g$trueDiff) # this is the observed mean difference
pers_cg<-pers_colony_g$pers_scores


## @knitr random_gru

for (i in 1:n_perms ) {
  
  # Randomise new partner personalities
  perm_dat <- data.frame(F_pers = absol_ran_g$F_pers, 
                         orig_absol = absol_ran_g$orig_absol,
                         shuffled_part_pers = sample(pers_cg, nrow(absol_ran_g))) 
  
  # Calculate differences
  perm_dat$shuffled_absol <- abs(perm_dat$F_pers - perm_dat$shuffled_part_pers)
  perm_dat$shuffled_orig_diff <- perm_dat$orig_absol - perm_dat$shuffled_absol
  perms_output[[i]] <- mean(perm_dat$shuffled_orig_diff)
  
}

## @knitr create_p_gru

# Null hypothesis: randomised personality values are equal or closer to focal birds'
# personality than true partners' personality

# Calculate proportion of cases that meet the null hypothesis
length(perms_output[perms_output >= mean_trueDiff])/length(perms_output) # less than #0.05

# Because we're using simulated data exact results will vary, but if p < 0.05, 
# we can infer that focal birds are ending up with partners who are closer to their
# personality than the previous partner than we would expect by chance 


## @knitr perm_g_gru

# visualise data
hist(as.numeric(perms_output)) # lots of negative differences = original partner is closer to Focal than randomised
abline(v = mean_trueDiff, col = "red", lty = "dotted")

## @knitr repeat_pyra

#pyramiden
n_perms <- 10000 # typical minimum for randomisation/permutation
perms_output <- list()
mean_trueDiff <- mean(absol_ran_p$trueDiff) # this is your observed mean difference
pers_cp<-pers_colony_p$pers_scores

for (i in 1:n_perms ) {
  
  # Randomise new partner personalities
  perm_dat <- data.frame(F_pers = absol_ran_p$F_pers, 
                         orig_absol = absol_ran_p$orig_absol,
                         shuffled_part_pers = sample(pers_cp, nrow(absol_ran_p))) 
  # x is the simulated personality distribution - you should sample
  # from the real personality distribution.
  
  # Calculate differences
  perm_dat$shuffled_absol <- abs(perm_dat$F_pers - perm_dat$shuffled_part_pers)
  perm_dat$shuffled_orig_diff <- perm_dat$orig_absol - perm_dat$shuffled_absol
  perms_output[[i]] <- mean(perm_dat$shuffled_orig_diff)
  
}

# Null hypothesis: randomised personality values are equal or closer to focal birds'
# personality than true partners' personality

# Calculate proportion of cases that meet the null hypothesis
length(perms_output[perms_output >= mean_trueDiff])/length(perms_output) # greater than 0.05

# visualise data
hist(as.numeric(perms_output)) # lots of negative differences = original partner is closer to Focal than randomised
abline(v = mean_trueDiff, col = "red", lty = "dotted")
