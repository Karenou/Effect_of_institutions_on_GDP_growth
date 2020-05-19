library(foreign)
library(stargazer)
library(tidyverse)

########## table 1 - summary statistics
data1=read.dta("data/maketable1.dta")
data2=read.dta("data/maketable2.dta")

# remove rows with missing data in short name
data1 = data1[data1$shortnam %in% as.matrix(data2$shortnam),]
rownames(data1)=NULL

# delete duplicated rows
nshortnames = data.frame(table(data1$shortnam))
nshortnames[nshortnames$Freq>1,]
data1[data1$shortnam == "DEU",] # remove row 38
data1[data1$shortnam == "YUG",] # remove row 160
data1[data1$shortnam == "ZWE",] # remove row 166
data1 = data1[-c(38,160,166),]

# divide settlement by 100
data1$euro1900 = data1$euro1900/100

# column 1: whole world
stargazer(data1, type="html",out="table1_world.html")

# column 2: base sample
base_sample = data1 %>% filter(baseco==1)
stargazer(base_sample, type="html",out="table1_base.html")

# column 3-6: by quantiles of mortality
# quantiles are for base sample of 64 obs
q=quantile(base_sample$extmort4, probs = c(0.25,0.5,0.75), na.rm=T)
bs_q1 = base_sample %>% filter(extmort4<=q[1])
bs_q2 = base_sample %>% filter(extmort4>q[1] & extmort4<=q[2])
bs_q3 = base_sample %>% filter(extmort4>q[2] & extmort4<=q[3])
bs_q4 = base_sample %>% filter(extmort4>q[3])
stargazer(bs_q1,bs_q2,bs_q3,bs_q4, type="html",out="table1_q.html")

########## table 2 - OLS regression
rm(list=ls())
world=read.dta("data/maketable2.dta")
base = world %>% filter(baseco==1)

m1 = lm(logpgp95~avexpr, data=world)
m2 = lm(logpgp95~avexpr, data=base)
m3 = lm(logpgp95~avexpr+lat_abst, data=world)
m4 = lm(logpgp95~avexpr+lat_abst+asia+africa+other, data=world)
m5 = lm(logpgp95~avexpr+lat_abst, data=base)
m6 = lm(logpgp95~avexpr+lat_abst+asia+africa+other, data=base)
m7 = lm(loghjypl~avexpr, data=world)
m8 = lm(loghjypl~avexpr, data=base)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8, type="html",out="table2.html")


########## table 3 - Determinants of Institutions
rm(list=ls())
world=read.dta("data/maketable3.dta")
# data preprocessing
world=world %>% filter(excolony==1) %>% drop_na(extmort4)
world$euro1900 = world$euro1900 / 100

# panel A
m1=lm(avexpr~cons00a, data=world)
m2=lm(avexpr~cons00a+lat_abst, data=world)
m3=lm(avexpr~democ00a, data=world)
m4=lm(avexpr~democ00a+lat_abst, data=world)
m5=lm(avexpr~cons1, data=world)
m6=lm(avexpr~cons1+lat_abst, data=world)
m7=lm(avexpr~euro1900, data=world)
m8=lm(avexpr~euro1900+lat_abst, data=world)
m9=lm(avexpr~logem4, data=world)
m10=lm(avexpr~logem4+lat_abst, data=world)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,type="html",out="table3_panelA.html")

# panel B
m1=lm(cons00a~euro1900, data=world)
m2=lm(cons00a~euro1900+lat_abst, data=world)
m3=lm(cons00a~logem4, data=world)
m4=lm(cons00a~logem4+lat_abst, data=world)
m5=lm(democ00a~euro1900, data=world)
m6=lm(democ00a~euro1900+lat_abst, data=world)
m7=lm(democ00a~logem4, data=world)
m8=lm(democ00a~logem4+lat_abst, data=world)
m9=lm(euro1900~logem4, data=world)
m10=lm(euro1900~logem4+lat_abst, data=world)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,type="html",out="table3_panelB.html")

