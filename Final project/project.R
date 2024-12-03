library(haven)
library(tidyverse)
library(survey)
library(knitr)
library(kableExtra)

path='/Users/ymok/University of Michigan Dropbox/Yoonseo Mok/CAsToR-DAD/Yoonseo-working/phd/STATS 506/data/'
fempreg=read_sas(paste0(path,"fempreg1719.sas7bdat"))
femresp=read_sas(paste0(path,"femresp1719.sas7bdat"))

femalejoin=left_join(fempreg,femresp,by=c("CASEID","INTVWYEAR","HIEDUC","HISPRACE2","AGER","POVERTY","WGT2017_2019","SEST","SECU"))

tmp=femalejoin %>% select(CASEID,INTVWYEAR,HIEDUC,HISPRACE2,AGER,OUTCOME,AGEPREG,AGECON,POVERTY,
                          GEST_LB,GEST_OTHR,POT12,COC12,CRACK12,CRYSTMTH12,INJECT12,FMAROUT5,WGT2017_2019,SEST,SECU)

tmp=tmp[!(tmp$OUTCOME %in% 6),]

tmp$substance=NA
tmp$substance[tmp$POT12 %in% c(2:6) | tmp$COC12 %in% c(2:4) | tmp$CRACK12 %in% c(2:4) | 
                tmp$CRYSTMTH12 %in% c(2:4) | tmp$INJECT12 %in% c(2:4)]=1
tmp$substance[tmp$POT12 %in% 1 & tmp$COC12 %in% 1 & tmp$CRACK12 %in% 1 &
                tmp$CRYSTMTH12 %in% 1 & tmp$INJECT12 %in% 1]=0
table(tmp$substance,useNA = "always")
tmp=tmp[!is.na(tmp$substance),]

tmp$preterm=NA
tmp$preterm[tmp$GEST_LB %in% c(1,2) | tmp$GEST_OTHR %in% c(1,2,3)]=1
tmp$preterm[tmp$GEST_LB %in% c(3,4)]=0
table(tmp$preterm,useNA = "always")

tmp$educ=NA
tmp$educ[tmp$HIEDUC %in% c(5:8)]="Less than high school"
tmp$educ[tmp$HIEDUC %in% 9]="High school"
tmp$educ[tmp$HIEDUC %in% c(10,11)]="Some college"
tmp$educ[tmp$HIEDUC %in% c(12:15)]="College +"
table(tmp$educ,useNA = "always")
tmp$educ=factor(tmp$educ, levels = c("Less than high school","High school","Some college","College +"))

table(tmp$AGEPREG,useNA = "always")

a=tmp[tmp$AGER<tmp$AGEPREG,] # No people 

tmp$income=NA
tmp$income[tmp$POVERTY %in% c(50:99)]="<100% FPL"
tmp$income[tmp$POVERTY %in% c(100:199)]="100%-199% FPL"
tmp$income[tmp$POVERTY %in% c(200:299)]="200%-299% FPL"
tmp$income[tmp$POVERTY %in% c(300:399)]="300%-399% FPL"
tmp$income[tmp$POVERTY>=400]=">=400% FPL"
table(tmp$income,useNA = "always")
tmp$income=factor(tmp$income, levels = c("<100% FPL","100%-199% FPL","200%-299% FPL","300%-399% FPL",">=400% FPL"))

tmp$race=NA
tmp$race[tmp$HISPRACE2 %in% 1]="Hispanic"
tmp$race[tmp$HISPRACE2 %in% 2]="Non-Hispanic White"
tmp$race[tmp$HISPRACE2 %in% 3]="Non-Hispanic Black"
tmp$race[tmp$HISPRACE2 %in% 4]="Non-Hispanic Other"
table(tmp$race,useNA="always") 
tmp$race=factor(tmp$race, levels = c("Hispanic","Non-Hispanic White","Non-Hispanic Black","Non-Hispanic Other"))

nsfg_design=svydesign(id = ~1, strata = ~SEST, weights = ~WGT2017_2019, data = tmp,nest=TRUE)

prop.table(svytable(~preterm,design=nsfg_design))
table(tmp$preterm,useNA = "always")
prop.table(svytable(~substance,design=nsfg_design))
table(tmp$substance,useNA = "always")
svymean(~AGEPREG,design=nsfg_design)
prop.table(svytable(~educ,design=nsfg_design))
table(tmp$educ,useNA = "always")
prop.table(svytable(~race,design=nsfg_design))
table(tmp$race,useNA = "always")
prop.table(svytable(~income,design=nsfg_design))
table(tmp$income,useNA = "always")

model2=svyglm(preterm ~ substance + AGEPREG + educ + race + income, family = quasibinomial(link = "logit"), design = nsfg_design)
summary(model2)

length(unique(tmp$CASEID))
coef_summary <- coef(summary(model2))  # Extract coefficients' summary
odds_ratios <- exp(coef_summary[, "Estimate"])
conf_int_lower <- exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"])
conf_int_upper <- exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])

results_table <- data.frame(
  'Odds Ratio' = round(odds_ratios,1),
  'Lower CI (95%)' = round(conf_int_lower,1),
  'Upper CI (95%)' = round(conf_int_upper,1)
)