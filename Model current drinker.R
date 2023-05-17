##Model current drink~stratifiled by gender + agegr(15+,15-20)
rm(list=ls())
Sys.setlocale("LC_CTYPE", "thai")
#setwd("H:\\.shortcut-targets-by-id\\1iYwiyGEJj3X5PHgfF4yHlkvzLklpw7sZ\\Mixed LR")
setwd("C:/Users/User/Desktop")
#setwd("C:/Users/MDCRI/Google Drive/works/Polathep/Mixed LR")
library(openxlsx)
load('stack_data_2021-03-20.Rdata')
dt <- stack_data
str(dt)

dt$pre_of_current_drinker <- ifelse(dt$d_status=="Lifetime abstainer" | dt$d_status== "Former drinker",0,1)
dt$pre_of_regular_drinker <- ifelse(dt$d_status %in% c("1-2 days/week","3-4 days/week","5-7 days/week"),1,0)
dt$pre_of_binge_drinker <- ifelse(dt$d_binge %in% c("Abstainer"),0,1)

dt$age <- as.numeric(dt$age)
dt1 <- subset(dt,dt$age>=15)
dt1 <- dt1[,-26]    #delete d_finance
table(dt1$year)
# dt1 <- subset(dt1,dt1$pre_of_current_drinker==1)
dt1 <- subset(dt1,!(dt1$year%in%c('2557')))


# dt1$agegr <- ifelse(dt1$age<=20,'15-20','20+')
names(dt1)
str(dt1)
table(dt1$year)

ps <- read.xlsx('pop-shop.xlsx')
ps$PROVINCE <- as.character(ps$PROVINCE)
names(ps)[1] <- 'cwt'
ps <- ps[,c(1,2,3,7,13)]
cwt <- c(ps$cwt,ps$cwt,ps$cwt)
year <- c(rep(2550,77),rep(2554,77),rep(2560,77))
pshop <- c(ps$'Ле.2550',ps$'Ле.2554',ps$'Ле.2560')
ps1 <- cbind.data.frame(cwt,year,pshop)
ps1$psgr <- ifelse(is.na(ps1$pshop),NA,ifelse(ps1$pshop<=6,"6-",ifelse(ps1$pshop<=8,"6-8",ifelse(ps1$pshop<=10,"8-10","10+"))))



dt1$cwt_yr <- paste0(dt1$cwt,dt1$year)
ps1$cwt_yr <- paste0(ps1$cwt,ps1$year)
ps1 <- ps1[,-(1:2)]

dt2 <- merge(dt1,ps1,by='cwt_yr')

di <- read.xlsx('Table deprivation index.xlsx')
di <- di[,-2]
names(di)[1] <- 'cwt'

dt3 <- merge(dt2,di,by='cwt')
str(dt3)

library(epiDisplay)
dt3$member <- as.numeric(dt3$member)
dt3$income1 <- as.numeric(dt3$income1)
dt3$income2 <- as.numeric(dt3$income2)
dt3$s_onset <- as.numeric(dt3$s_onset)
dt3$s_consume <- as.numeric(dt3$s_consume)
dt3$s_buy <- as.numeric(dt3$s_buy)
dt3$d_onset <- as.numeric(dt3$d_onset)
dt3$d_buy <- as.numeric(dt3$d_buy)
dt3$alcohol.avg <- as.numeric(dt3$alcohol.avg)
dt3$pre_of_current_drinker <- as.character(dt3$pre_of_current_drinker)
dt3$pre_of_regular_drinker <- as.character(dt3$pre_of_regular_drinker)
dt3$pre_of_binge_drinker <- as.character(dt3$pre_of_binge_drinker)

dt3$h_material <- ifelse(dt3$h_material!='Concrete','Others','Concrete')
dt3$mem2 <- ifelse(dt3$mem2%in%c('Head of family','Husband/wife'),'Parent',ifelse(dt3$mem2%in%c('Child (married)','Child (single)'),'Child','Others'))
dt3$edu <- ifelse(dt3$edu%in%c('Below primary school','Primary school','No formal education','Etc','Unknown'),'Primary school or less',ifelse(dt3$edu%in%c('Junior high school','Senior high school'),'High school','Bachelor and above'))
dt3$mar <- ifelse(dt3$mar=='Single','Single',ifelse(dt3$mar%in%c('Married','Married with unknown status'),'Married','Divorced/separated/widowed'))
dt3$occ2 <- ifelse(dt3$occ2=='gr6','Employment',ifelse(dt3$occ2=='gr3','Business',ifelse(dt3$occ2%in%c('gr4','gr5'),'Government','Others')))
dt3$s_status <- ifelse(dt3$s_status %in% c('Current smoker (occasional)','Current smoker (regular)'),'Yes','No')
names(dt3)[44] <- 'depr_index.gr'
tableStack(data=dt3,vars = c(3:6,9:13,15,16,18:21,31,42,44),by=pre_of_current_drinker,total.column = T)->p1
p1
# write.csv(p1,"Characteristics_current_drinker15+.csv")
dt3$pre_of_current_drinker <- factor(dt3$pre_of_current_drinker)
dt3$mem2 <- factor(dt3$mem2)
dt3$agegr <- ifelse(dt3$age<=20,'15-20',ifelse(dt3$age<=40,'20-40',ifelse(dt3$age<=60,'40-60','60+')))
dt3$memgr <- ifelse(dt3$member<3,'1-2',ifelse(dt3$member<5,'3-4',ifelse(dt3$member<7,'5-6','7+')))
str(dt3)
dt4 <- dt3[,c(1,3,4,46,9:10,45,12,13,15,16,18,31,42,44,38)]
str(dt4)
tab1(dt4$year)

dt4 <- na.omit(dt4)
tab1(dt4$year)

dt4$income1 <- ifelse(dt4$income1<15000,'15000-',ifelse(dt4$income1<30000,'15001-30000',ifelse(dt4$income1<50000,'30001-50000','50000+')))
dt4[,2:16] <- lapply(dt4[,2:16] , factor)
# dt4 <- unique(dt4)

##Model current drinker
# Two model by sex 15+
dt.t <- dt4
dt.m <- subset(dt4,sex=="Male")  
dt.f <- subset(dt4,sex=="Female")
# Two model by sex 15-20
dt.t_age <- subset(dt.t,agegr=="15-20")
dt.m_age <- subset(dt.m,agegr=="15-20")
dt.f_age <- subset(dt.f,agegr=="15-20")

tableStack(data=dt.t,vars = c(2:5,7:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pt
pt
tableStack(data=dt.m,vars = c(2:5,7:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pm
pm
tableStack(data=dt.f,vars = c(2:5,7:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pf
pf
tableStack(data=dt.t_age,vars = c(2:5,8:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pt_age
pt_age
tableStack(data=dt.m_age,vars = c(2:5,8:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pm_age
pm_age
tableStack(data=dt.f_age,vars = c(2:5,8:15),by=pre_of_current_drinker,percent = "row",total.column = F)->pf_age
pf_age
# write.csv(pt,"Characteristics_current_drinker15+_Total.csv")   #read1
# write.csv(pm,"Characteristics_current_drinker15+_Male.csv")   #read2
# write.csv(pf,"Characteristics_current_drinker15+_Female.csv")   #read3
# write.csv(pt_age,"Characteristics_current_drinker15-20_Total.csv")   #read4
# write.csv(pm_age,"Characteristics_current_drinker15-20_Male.csv")   #read5
# write.csv(pf_age,"Characteristics_current_drinker15-20_Female.csv")   #read6


###################################################################################################################################################################################################################
##Logistic not selected
mod_m <- glm(family=binomial,data=dt.m,pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr)  #not selected
summary(mod_m)
mod_f <- glm(family=binomial,data=dt.f,pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr)  #not selected
summary(mod_f)

mod_m_age <- glm(family=binomial,data=dt.m_age,pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr)  #not selected
summary(mod_m_age)
mod_f_age <- glm(family=binomial,data=dt.f_age,pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr)  #not selected
summary(mod_f_age)

smod_m <-logistic.display(mod_m)    #not selected
smod_m
smod_f <-logistic.display(mod_f)    #not selected
smod_f
smod_m_age <-logistic.display(mod_m_age)    #not selected
smod_m_age
smod_f_age <-logistic.display(mod_f_age)    #not selected
smod_f_age

library(MASS)
library(lme4)
library(MCMCglmm)
library(epicalc)
library(plyr)


##glm Select Model
####Model By Male 15+####
###Step manual###
## Round 1 ####
glmm     <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_01  <- glm(pre_of_current_drinker~    area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_02  <- glm(pre_of_current_drinker~reg+     memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)   #selected
glmm_03  <- glm(pre_of_current_drinker~reg+area+      mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_04  <- glm(pre_of_current_drinker~reg+area+memgr+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_05  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_06  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_07  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_08  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_09  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_10  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_11  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.m)

AIC(glmm, glmm_01, glmm_02, glmm_03, glmm_04, glmm_05, glmm_06, glmm_07, glmm_08, glmm_09, glmm_10, glmm_11) # select glmm_02 AIC=150463.1

## Round 2 ####
glmm_20  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)   #selected
glmm_21  <- glm(pre_of_current_drinker~    memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_22  <- glm(pre_of_current_drinker~reg+      mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_23  <- glm(pre_of_current_drinker~reg+memgr+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_24  <- glm(pre_of_current_drinker~reg+memgr+mem2+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_25  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_26  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_27  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_28  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_29  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.m)
glmm_30  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.m)

AIC(glmm_20, glmm_21, glmm_22, glmm_23, glmm_24, glmm_25, glmm_26, glmm_27, glmm_28, glmm_29, glmm_30) # select glmm_20 AIC=150463.1
#Logistic selected By Male15+
glmm_final  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m)
summary(glmm_final)
smodm_final <- logistic.display(glmm_final)    ##finish  current male15+
smodm_final
# write.csv(smodm_final,"Logistic regression of current_drinker15+_Male.csv")   #read5


####Model By Female 15+####
## Round 1 ####
glmf     <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_01  <- glm(pre_of_current_drinker~    area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_02  <- glm(pre_of_current_drinker~reg+     memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)   #selected
glmf_03  <- glm(pre_of_current_drinker~reg+area+      mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_04  <- glm(pre_of_current_drinker~reg+area+memgr+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_05  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_06  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_07  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_08  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_09  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_10  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_11  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.f)

AIC(glmf, glmf_01, glmf_02, glmf_03, glmf_04, glmf_05, glmf_06, glmf_07, glmf_08, glmf_09, glmf_10, glmf_11) # select glmf_02 AIC=110286.9

## Round 2 ####
glmf_20  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)   #selected
glmf_21  <- glm(pre_of_current_drinker~    memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_22  <- glm(pre_of_current_drinker~reg+      mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_23  <- glm(pre_of_current_drinker~reg+memgr+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_24  <- glm(pre_of_current_drinker~reg+memgr+mem2+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_25  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_26  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_27  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_28  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_29  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.f)
glmf_30  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.f)

AIC(glmf_20, glmf_21, glmf_22, glmf_23, glmf_24, glmf_25, glmf_26, glmf_27, glmf_28, glmf_29, glmf_30) # select glmm_20 AIC=110286.9
#Logistic selected By Female15+
glmf_final  <- glm(pre_of_current_drinker~reg+memgr+mem2+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f)
summary(glmf_final)
smodf_final <- logistic.display(glmf_final)    ##finish  current female15+
smodf_final
# write.csv(smodf_final,"Logistic regression of current_drinker15+_Female.csv")   #read6


####Model By Male 15-20####
## Round 1 ####
glmm_age     <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_01  <- glm(pre_of_current_drinker~    area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_02  <- glm(pre_of_current_drinker~reg+     memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)  
glmm_age_03  <- glm(pre_of_current_drinker~reg+area+      mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_04  <- glm(pre_of_current_drinker~reg+area+memgr+     edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_06  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_07  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_08  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_09  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)    #selected
glmm_age_10  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_11  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.m_age)

AIC(glmm_age, glmm_age_01, glmm_age_02, glmm_age_03, glmm_age_04, glmm_age_06, glmm_age_07, glmm_age_08, glmm_age_09, glmm_age_10, glmm_age_11) # select glmm_age_09 AIC=6093.680

## Round 2 ####
glmm_age_20  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)   
glmm_age_21  <- glm(pre_of_current_drinker~    area+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_22  <- glm(pre_of_current_drinker~reg+     memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_23  <- glm(pre_of_current_drinker~reg+area+      mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_24  <- glm(pre_of_current_drinker~reg+area+memgr+     edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_25  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+    mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_26  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+    occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_27  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+     s_status+year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_28  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+         year+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_29  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+s_status+     psgr+depr_index.gr, family = binomial, data = dt.m_age)    #selected

AIC(glmm_age_20, glmm_age_21, glmm_age_22, glmm_age_23, glmm_age_24, glmm_age_25, glmm_age_26, glmm_age_27, glmm_age_28, glmm_age_29) # select glmm_age_29 AIC=6091.006

## Round 3 ####
glmm_age_30  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_31  <- glm(pre_of_current_drinker~    area+memgr+mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_32  <- glm(pre_of_current_drinker~reg+     memgr+mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)    #selected
glmm_age_33  <- glm(pre_of_current_drinker~reg+area+      mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_34  <- glm(pre_of_current_drinker~reg+area+memgr+     edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_35  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+    mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_36  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+    occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_37  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+     s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_38  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+         psgr+depr_index.gr, family = binomial, data = dt.m_age)

AIC(glmm_age_30, glmm_age_31, glmm_age_32, glmm_age_33, glmm_age_34, glmm_age_35, glmm_age_36, glmm_age_37, glmm_age_38) # select glmm_age_32 AIC=6089.552

## Round 4 ####
glmm_age_40  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_41  <- glm(pre_of_current_drinker~    memgr+mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_42  <- glm(pre_of_current_drinker~reg+      mem2+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_43  <- glm(pre_of_current_drinker~reg+memgr+     edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)    #selected
glmm_age_44  <- glm(pre_of_current_drinker~reg+memgr+mem2+    mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_45  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+    occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_46  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+     s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_47  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+         psgr+depr_index.gr, family = binomial, data = dt.m_age)

AIC(glmm_age_40, glmm_age_41, glmm_age_42, glmm_age_43, glmm_age_44, glmm_age_45, glmm_age_46, glmm_age_47) # select glmm_age_43 AIC=6088.565


## Round 5 ####
glmm_age_50  <- glm(pre_of_current_drinker~reg+memgr+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)    #selected
glmm_age_51  <- glm(pre_of_current_drinker~    memgr+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_52  <- glm(pre_of_current_drinker~reg+      edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_53  <- glm(pre_of_current_drinker~reg+memgr+    mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_54  <- glm(pre_of_current_drinker~reg+memgr+edu+    occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_55  <- glm(pre_of_current_drinker~reg+memgr+edu+mar+     s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
glmm_age_56  <- glm(pre_of_current_drinker~reg+memgr+edu+mar+occ2+         psgr+depr_index.gr, family = binomial, data = dt.m_age)

AIC(glmm_age_50, glmm_age_51, glmm_age_52, glmm_age_53, glmm_age_54, glmm_age_55, glmm_age_56) # select glmm_age_50 AIC=6088.565

#Logistic selected By Male15-20
glmm_age_final  <- glm(pre_of_current_drinker~reg+memgr+edu+mar+occ2+s_status+psgr+depr_index.gr, family = binomial, data = dt.m_age)
summary(glmm_age_final)
smodm_age_final <- logistic.display(glmm_age_final)    ##finish current male15-20
smodm_age_final
# write.csv(smodm_age_final,"Logistic regression of current_drinker15-20_Male.csv")   #read7



####Model By Female 15-20####
## Round 1 ####
glmf_age     <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_01  <- glm(pre_of_current_drinker~    area+memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_02  <- glm(pre_of_current_drinker~reg+     memgr+mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)  
glmf_age_03  <- glm(pre_of_current_drinker~reg+area+      mem2+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_04  <- glm(pre_of_current_drinker~reg+area+memgr+     edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_06  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+    mar+occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_07  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+    occ2+income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_08  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+     income1+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_09  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+        s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)    #selected
glmf_age_10  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+         year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_11  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+income1+s_status+     psgr+depr_index.gr, family = binomial, data = dt.f_age)


AIC(glmf_age, glmf_age_01, glmf_age_02, glmf_age_03, glmf_age_04, glmf_age_06, glmf_age_07, glmf_age_08, glmf_age_09, glmf_age_10, glmf_age_11) # select glmf_age_09 AIC=1876.364

## Round 2 ####
glmf_age_20  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)   
glmf_age_21  <- glm(pre_of_current_drinker~    area+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_22  <- glm(pre_of_current_drinker~reg+     memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)    #selected
glmf_age_23  <- glm(pre_of_current_drinker~reg+area+      mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_24  <- glm(pre_of_current_drinker~reg+area+memgr+     edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_25  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+    mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_26  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+    occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_27  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+     s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_28  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+         year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_29  <- glm(pre_of_current_drinker~reg+area+memgr+mem2+edu+mar+occ2+s_status+     psgr+depr_index.gr, family = binomial, data = dt.f_age)

AIC(glmf_age_20, glmf_age_21, glmf_age_22, glmf_age_23, glmf_age_24, glmf_age_25, glmf_age_26, glmf_age_27, glmf_age_28, glmf_age_29) # select glmf_age_22 AIC= 1875.413

## Round 3 ####
glmf_age_30  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)    #selected
glmf_age_31  <- glm(pre_of_current_drinker~    memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_32  <- glm(pre_of_current_drinker~reg+      mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_33  <- glm(pre_of_current_drinker~reg+memgr+     edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_34  <- glm(pre_of_current_drinker~reg+memgr+mem2+    mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_35  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+    occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_36  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+     s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_37  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+         year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
glmf_age_38  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+s_status+     psgr+depr_index.gr, family = binomial, data = dt.f_age)

AIC(glmf_age_30, glmf_age_31, glmf_age_32, glmf_age_33, glmf_age_34, glmf_age_35, glmf_age_36, glmf_age_37, glmf_age_38) # select glmf_age_30 AIC= 1875.413

#Logistic selected By Female15-20
glmf_age_final  <- glm(pre_of_current_drinker~reg+memgr+mem2+edu+mar+occ2+s_status+year+psgr+depr_index.gr, family = binomial, data = dt.f_age)
summary(glmf_age_final)
smodf_age_final <- logistic.display(glmf_age_final)    ##finish  current female15-20
smodf_age_final
# write.csv(smodf_age_final,"Logistic regression of current_drinker15-20_Female.csv")   #read8


###############################################################################################################################################################################################


###Mixed Model
dt4$ID <- 1:nrow(dt4)
dt.t$ID <- 1:nrow(dt.t)
dt.m$ID <- 1:nrow(dt.m)
dt.f$ID <- 1:nrow(dt.f)
dt.t_age$ID <- 1:nrow(dt.t_age)
dt.m_age$ID <- 1:nrow(dt.m_age)
dt.f_age$ID <- 1:nrow(dt.f_age)
dt.t$psgr <- relevel(dt.t$psgr, ref = "6-")
dt.m$psgr <- relevel(dt.m$psgr, ref = "6-")
dt.f$psgr <- relevel(dt.f$psgr, ref = "6-")
dt.t_age$psgr <- relevel(dt.t_age$psgr, ref = "6-")
dt.m_age$psgr <- relevel(dt.m_age$psgr, ref = "6-")
dt.f_age$psgr <- relevel(dt.f_age$psgr, ref = "6-")
library(MASS)
library(lme4)
library(MCMCglmm)
library(epicalc)
library(plyr)

#Full model not stratifiled
#glmm0 <- glmer(pre_of_current_drinker~reg+area+memgr+mem2+sex+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt4)
##Selected Model
#library(cAIC4)
#fm3_step <- stepcAIC(glmm0, direction = "backward",data = dt4)
#glmm0 <- glmer(pre_of_current_drinker~depr_index.gr+year+psgr+(1|cwt), family = binomial(link = "logit"), data = dt4)
#summary(glmm0)

##Select Model glmer
####glmer Model By Male 15+####
###Step manual###
## Round 1 ####
glmm_m    <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)   #selected final Mixed By Male 15+
glmm_01m  <- glmer(pre_of_current_drinker~    area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_02m  <- glmer(pre_of_current_drinker~reg+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_05m  <- glmer(pre_of_current_drinker~reg+area+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_06m  <- glmer(pre_of_current_drinker~reg+area+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_07m  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_08m  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_09m  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)
glmm_10m  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)


AIC(glmm_m, glmm_01m, glmm_02m, glmm_05m, glmm_06m, glmm_07m, glmm_08m, glmm_09m, glmm_10m) # select glmm_m AIC= 168734.6


####glmer Model By Female 15+####
## Round 1 ####
glmm_f    <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_01f  <- glmer(pre_of_current_drinker~    area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_02f  <- glmer(pre_of_current_drinker~reg+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   #selected
glmm_05f  <- glmer(pre_of_current_drinker~reg+area+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_06f  <- glmer(pre_of_current_drinker~reg+area+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_07f  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_08f  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_09f  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)
glmm_10f  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)

AIC(glmm_f, glmm_01f, glmm_02f, glmm_05f, glmm_06f, glmm_07f, glmm_08f, glmm_09f, glmm_10f) # select glmm_02f AIC=88089.40

## Round 2 ####
glmm_20f  <- glmer(pre_of_current_drinker~reg+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)      #selected final Mixed By Female 15+
glmm_21f  <- glmer(pre_of_current_drinker~    agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_24f  <- glmer(pre_of_current_drinker~reg+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_25f  <- glmer(pre_of_current_drinker~reg+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_26f  <- glmer(pre_of_current_drinker~reg+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_27f  <- glmer(pre_of_current_drinker~reg+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_28f  <- glmer(pre_of_current_drinker~reg+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   
glmm_29f  <- glmer(pre_of_current_drinker~reg+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   

AIC(glmm_20f, glmm_21f,glmm_24f, glmm_25f, glmm_26f, glmm_27f, glmm_28f, glmm_29f) # select glmm_20f AIC=88089.40


####glmer Total Model 15+####
## Round 1 ####
glmm_t    <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)      #selected final Mixed Total 15+
glmm_01t  <- glmer(pre_of_current_drinker~    area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_02t  <- glmer(pre_of_current_drinker~reg+     agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)      
glmm_05t  <- glmer(pre_of_current_drinker~reg+area+      edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_06t  <- glmer(pre_of_current_drinker~reg+area+agegr+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_07t  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_08t  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_09t  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)
glmm_10t  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)

AIC(glmm_t, glmm_01t, glmm_02t, glmm_05t, glmm_06t, glmm_07t, glmm_08t, glmm_09t, glmm_10t) # select glmm_0t AIC=




####glmer Model By Male 15-20####
## Round 1 ####
glmm_age_m    <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_01m  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_02m  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)  
glmm_age_06m  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_07m  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_08m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_09m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)   #selected
glmm_age_10m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)

AIC(glmm_age_m, glmm_age_01m, glmm_age_02m, glmm_age_06m, glmm_age_07m, glmm_age_08m, glmm_age_09m, glmm_age_10m) # select glmm_age_09m AIC=5902.622

## Round 2 ####
glmm_age_20m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_21m  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_22m  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)   #selected
glmm_age_23m  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_24m  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_25m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_26m  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)


AIC(glmm_age_20m, glmm_age_21m, glmm_age_22m, glmm_age_23m, glmm_age_24m, glmm_age_25m, glmm_age_26m) # select glmm_age_22m AIC=5900.704

## Round 3 ####
glmm_age_30m  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)      #selected final Mixed By Male 15-20
glmm_age_31m  <- glmer(pre_of_current_drinker~    edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_32m  <- glmer(pre_of_current_drinker~reg+    mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_33m  <- glmer(pre_of_current_drinker~reg+edu+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_34m  <- glmer(pre_of_current_drinker~reg+edu+mar+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)
glmm_age_35m  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)

AIC(glmm_age_30m, glmm_age_31m, glmm_age_32m, glmm_age_33m, glmm_age_34m, glmm_age_35m) # select glmm_age_30m AIC=5900.704



####glmer Model By Female 15-20####
## Round 1 ####
glmm_age_f    <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_01f  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_02f  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)  
glmm_age_06f  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_07f  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_08f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_09f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)    #selected
glmm_age_10f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)

AIC(glmm_age_f, glmm_age_01f, glmm_age_02f, glmm_age_06f, glmm_age_07f, glmm_age_08f, glmm_age_09f, glmm_age_10f) # select glmm_age_09f AIC=1597.213

## Round 2 ####
glmm_age_21f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_22f  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_23f  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_24f  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_25f  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)    #selected
glmm_age_26f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_27f  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)

AIC(glmm_age_21f, glmm_age_22f, glmm_age_23f, glmm_age_24f, glmm_age_25f, glmm_age_26f, glmm_age_27f) # select glmm_age_25f AIC=1596.801

## Round 3 ####
glmm_age_30f  <- glmer(pre_of_current_drinker~reg+area+edu+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)  #selected final Mixed By Female 15-20 
glmm_age_31f  <- glmer(pre_of_current_drinker~    area+edu+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_32f  <- glmer(pre_of_current_drinker~reg+     edu+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_33f  <- glmer(pre_of_current_drinker~reg+area+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_34f  <- glmer(pre_of_current_drinker~reg+area+edu+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)
glmm_age_35f  <- glmer(pre_of_current_drinker~reg+area+edu+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)

AIC(glmm_age_30f, glmm_age_31f, glmm_age_32f, glmm_age_33f, glmm_age_34f, glmm_age_35f) # select glmm_age_30f AIC=1596.801


####glmer Total Model 15-20####
## Round 1 ####
glmm_age_t    <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_01t  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_02t  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)  
glmm_age_06t  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_07t  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_08t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_09t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+        s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)   #selected
glmm_age_10t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+income1+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)

AIC(glmm_age_t, glmm_age_01t, glmm_age_02t, glmm_age_06t, glmm_age_07t, glmm_age_08t, glmm_age_09t, glmm_age_10t) # select glmm_age_09t AIC=8016.578

## Round 2 ####
glmm_age_20t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age) 
glmm_age_21t  <- glmer(pre_of_current_drinker~    area+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_22t  <- glmer(pre_of_current_drinker~reg+     edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)   #selected
glmm_age_23t  <- glmer(pre_of_current_drinker~reg+area+    mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_24t  <- glmer(pre_of_current_drinker~reg+area+edu+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_25t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_26t  <- glmer(pre_of_current_drinker~reg+area+edu+mar+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)

AIC(glmm_age_20t, glmm_age_21t, glmm_age_22t, glmm_age_23t, glmm_age_24t, glmm_age_25t, glmm_age_26t) # select glmm_age_22t AIC=8014.589

## Round 3 ####
glmm_age_30t  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age) #selected final Mixed Total 15-20 
glmm_age_31t  <- glmer(pre_of_current_drinker~    edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_32t  <- glmer(pre_of_current_drinker~reg+    mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_33t  <- glmer(pre_of_current_drinker~reg+edu+    occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_34t  <- glmer(pre_of_current_drinker~reg+edu+mar+     s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)
glmm_age_35t  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+         year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)

AIC(glmm_age_30t, glmm_age_31t, glmm_age_32t, glmm_age_33t, glmm_age_34t, glmm_age_35t) # select glmm_age_30t AIC=8014.589

#Six Mixed Model strtifield by sex and agegr
#15+
glmm_m  <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m)   #male15+ finish
summary(glmm_m)
glmm_20f  <- glmer(pre_of_current_drinker~reg+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f)   #female15+ finish
summary(glmm_20f)
glmm_t    <- glmer(pre_of_current_drinker~reg+area+agegr+edu+mar+occ2+income1+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t)   #total15+ finish
summary(glmm_t)

#15-20
glmm_age_30m  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.m_age)   #male15-20 finish
summary(glmm_age_30m)
glmm_age_30f  <- glmer(pre_of_current_drinker~reg+area+edu+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.f_age)    #female15-20 finish
summary(glmm_age_30f)
glmm_age_30t  <- glmer(pre_of_current_drinker~reg+edu+mar+occ2+s_status+year+psgr+depr_index.gr+(1|cwt), family = binomial(link = "logit"), data = dt.t_age)   #total15-20  finish
summary(glmm_age_30t)


#tab1(dt.m$psgr)
#tab1(dt.m$year)
#tab1(dt.f$psgr)
#tab1(dt.f$year)


##++ 95%CI ####
#By male15+
se1 <- sqrt(diag(vcov(glmm_m)))
tab1 <- cbind(Est = fixef(glmm_m), LL = fixef(glmm_m)-1.96*se1 , UL = fixef(glmm_m)+1.96*se1)
r1 <- round(exp(tab1),2); r1
# write.csv(r1,"Mixed model of current_drinker By male15+.csv")  #read7
#By female15+
se2 <- sqrt(diag(vcov(glmm_20f)))
tab2 <- cbind(Est = fixef(glmm_20f), LL = fixef(glmm_20f)-1.96*se2 , UL = fixef(glmm_20f)+1.96*se2)
r2 <- round(exp(tab2),2); r2
# write.csv(r2,"Mixed model of current_drinker By female15+.csv")  #read8
#Total 15+
se5 <- sqrt(diag(vcov(glmm_t)))
tab5 <- cbind(Est = fixef(glmm_t), LL = fixef(glmm_t)-1.96*se5 , UL = fixef(glmm_t)+1.96*se5)
r5 <- round(exp(tab5),2); r5
# write.csv(r5,"Mixed model of current_drinker Total15+.csv")  #read9

#By male15-20
se3 <- sqrt(diag(vcov(glmm_age_30m)))
tab3 <- cbind(Est = fixef(glmm_age_30m), LL = fixef(glmm_age_30m)-1.96*se3 , UL = fixef(glmm_age_30m)+1.96*se3)
r3 <- round(exp(tab3),2); r3
# write.csv(r3,"Mixed model of current_drinker By male15-20.csv")  #read10
#By female15-20
se4 <- sqrt(diag(vcov(glmm_age_30f)))
tab4 <- cbind(Est = fixef(glmm_age_30f), LL = fixef(glmm_age_30f)-1.96*se4 , UL = fixef(glmm_age_30f)+1.96*se4)
r4 <- round(exp(tab4),2); r4
# write.csv(r4,"Mixed model of current_drinker By female15-20.csv")  #read11
#Total 15-20
se6 <- sqrt(diag(vcov(glmm_age_30t)))
tab6 <- cbind(Est = fixef(glmm_age_30t), LL = fixef(glmm_age_30t)-1.96*se6 , UL = fixef(glmm_age_30t)+1.96*se6)
r6 <- round(exp(tab6),2); r6
# write.csv(r6,"Mixed model of current_drinker Total15-20.csv")  #read12



### (4) Write table (all 1, 2 and 3) #######
#r1 <- as.data.frame(r1)
#r2 <- as.data.frame(r2)
#r3 <- as.data.frame(r3)
#write.csv(r1, "r1.csv")
#write.csv(r2, "r2.csv")
#write.csv(r3, "r3.csv")

#15+
tabpct(dt.m$year, dt.m$psgr)
tabpct(dt.f$year, dt.f$psgr)
#15-20
tabpct(dt.m_age$year, dt.m_age$psgr)
tabpct(dt.f_age$year, dt.f_age$psgr)


####Check Assumption###
#Check Assumption Male15+
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_m),dt.m$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.m$glmm_m.Res <- residuals(glmm_m)
dt.m$Abs.glmm_m.Res <- abs(dt.m$glmm_m.Res)
dt.m$glmm_m.Res2 <- dt.m$Abs.glmm_m.Res^2
Levene.glmm_m <- lm(glmm_m.Res2~cwt,data = dt.m)
anova(Levene.glmm_m)

Plot.glmm_m <- plot(glmm_m)
Plot.glmm_m

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_m,id=0.05)


#Check Assumption Female15+
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_20f),dt.f$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.f$glmm_20f.Res <- residuals(glmm_20f)
dt.f$Abs.glmm_20f.Res <- abs(dt.f$glmm_20f.Res)
dt.f$glmm_20f.Res2 <- dt.f$Abs.glmm_20f.Res^2
Levene.glmm_20f <- lm(glmm_20f.Res2~cwt,data = dt.f)
anova(Levene.glmm_20f)

Plot.glmm_20f <- plot(glmm_20f)
Plot.glmm_20f

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_20f,id=0.05)


#Check Assumption Total15+
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_t),dt.t$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.t$glmm_t.Res <- residuals(glmm_t)
dt.t$Abs.glmm_t.Res <- abs(dt.t$glmm_t.Res)
dt.t$glmm_t.Res2 <- dt.t$Abs.glmm_t.Res^2
Levene.glmm_t <- lm(glmm_t.Res2~cwt,data = dt.t)
anova(Levene.glmm_t)

Plot.glmm_t <- plot(glmm_t)
Plot.glmm_t

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_t,id=0.05)




#Check Assumption Male15-20
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_age_30m),dt.m_age$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.m_age$glmm_age_30m.Res <- residuals(glmm_age_30m)
dt.m_age$Abs.glmm_age_30m.Res <- abs(dt.m_age$glmm_age_30m.Res)
dt.m_age$glmm_age_30m.Res2 <- dt.m_age$Abs.glmm_age_30m.Res^2
Levene.glmm_age_30m <- lm(glmm_age_30m.Res2~cwt,data = dt.m_age)
anova(Levene.glmm_age_30m)

Plot.glmm_age_30m <- plot(glmm_age_30m)
Plot.glmm_age_30m

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_age_30m,id=0.05)


#Check Assumption Female15-20
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_age_30f),dt.f_age$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.f_age$glmm_age_30f.Res <- residuals(glmm_age_30f)
dt.f_age$Abs.glmm_age_30f.Res <- abs(dt.f_age$glmm_age_30f.Res)
dt.f_age$glmm_age_30f.Res2 <- dt.f_age$Abs.glmm_age_30f.Res^2
Levene.glmm_age_30f <- lm(glmm_age_30f.Res2~cwt,data = dt.f_age)
anova(Levene.glmm_age_30f)

Plot.glmm_age_30f <- plot(glmm_age_30f)
Plot.glmm_age_30f

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_age_30f,id=0.05)


#Check Assumption Total15-20
##Linearity
Plot.Model.Linearity <- plot(resid(glmm_age_30t),dt.t_age$pre_of_current_drinker)

##Homogeneity of Variance ;if > 0.05 is pass 
dt.t_age$glmm_age_30t.Res <- residuals(glmm_age_30t)
dt.t_age$Abs.glmm_age_30t.Res <- abs(dt.t_age$glmm_age_30t.Res)
dt.t_age$glmm_age_30t.Res2 <- dt.t_age$Abs.glmm_age_30t.Res^2
Levene.glmm_age_30t <- lm(glmm_age_30t.Res2~cwt,data = dt.t_age)
anova(Levene.glmm_age_30t)

Plot.glmm_age_30t <- plot(glmm_age_30t)
Plot.glmm_age_30t

##The residual of the model are normal distribution
library(lattice)
library(boot)
qqmath(glmm_age_30t,id=0.05)


