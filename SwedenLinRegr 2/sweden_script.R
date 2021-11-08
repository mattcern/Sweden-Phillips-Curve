# DO NOT CHANGE ANYTHING IN LINES 8-28
# RUN THIS LINE FIRST TO CLEAR YOUR ENVIRONMENT
rm(list=ls())

# you can ignore the code in lines 8-18 but make sure you run it
# first off, look into your lower-right portion of RStudio and click on Packages, click on Install and type in rstudioapi. Install it

library(rstudioapi)

# Getting the path of your current open file - I'm doing this to make this work on all our devices regardless of the path of our working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# print( getwd() )

# Loading the data
path_list = as.list(strsplit(current_path, '')[[1]])
clean_path_list = path_list[1:(length(path_list)-16)]
clean_path = paste(clean_path_list, collapse='')

inf = read.csv(paste(clean_path, '/inflation.csv', sep=''))
unemp = read.csv(paste(clean_path, '/unemployment.csv', sep=''))

st_int = read.csv(paste(clean_path, '/ST-int.csv', sep=''))
lt_int = read.csv(paste(clean_path, '/LT-int.csv', sep=''))

avg_wages = read.csv(paste(clean_path, '/avg_wages.csv', sep=''))

gdp = read.csv(paste(clean_path, '/gdp.csv', sep=''))

#####################################################################################

#####################################################################################
# BASIC PHILLIPS CURVE - inflation and unemployment data from Jan 1983 to Dec 2020  ---------> This one uses monthly data

inf_monthly = read.csv(paste(clean_path, '/inflation-monthly.csv', sep=''))
unemp_monthly = read.csv(paste(clean_path, '/unemployment-monthly.csv', sep=''))

pc_monthly = lm(inf_monthly$Value ~ unemp_monthly$Value)
summary(pc_monthly)
# plot(pc_monthly)
# plot(unemp$Value, inf$Value)
# abline(pc_monthly, col='red')

#####################################################################################


#####################################################################################
#####################################################################################
# FROM HERE WE SWITCH TO YEARLY DATA DUE TO DATA CONSTRAINTS AND MISSING MONTHLY DATA
#####################################################################################
#####################################################################################


#####################################################################################
# BASIC PHILLIPS CURVE WITH YEARLY DATA FROM 1983 to 2019  -------------------------------> This one uses yearly data and we will use it as control for our expanded models

pc_0 = lm(inf$Value ~ unemp$Value)
summary(pc_0)
# plot(pc_0)
# plot(unemp$Value, inf$Value)
# abline(pc_0, col='red')

#####################################################################################

#####################################################################################
# ADDING THE EFFECT OF SHORT-TERM & LONG-TERM INTEREST RATES - data adjusted to fit the period from 1987 to 2019

inf_adjusted = data.frame(TIME = inf[c(5:37), 'TIME'], Value = inf[c(5:37), 'Value'])
st_int_adjusted = data.frame(TIME = st_int[c(5:37), 'TIME'], Value = st_int[c(5:37), 'Value'])
unemp_adjusted = data.frame(TIME = unemp[c(5:37), 'TIME'], Value = unemp[c(5:37), 'Value'])

pc_1 = lm(inf_adjusted$Value ~ unemp_adjusted$Value + st_int_adjusted$Value + lt_int$Value)
summary(pc_1)
# plot(pc_1)

# building the basic Phillips Curve with data from this period to see if our model is an improvement

pc_control_1 = lm(inf_adjusted$Value ~ unemp_adjusted$Value)
summary(pc_control_1)
# plot(pc_control_1)

#####################################################################################

#####################################################################################
# ADDING THE EFFECT OF AVERAGE WAGES - data from 1990 to 2019
# here we just use average wages and unemployment as explanatory variables because adding more variables would make the model less precise because the data for
# for average wages is provided on a yearly basis from 1990 so the sample size if too small to safely add more explanatory variables

inf_adjusted_2 = data.frame(TIME = inf[c(8:37), 'TIME'], Value = inf[c(8:37), 'Value'])
unemp_adjusted_2 = data.frame(TIME = unemp[c(8:37), 'TIME'], Value = unemp[c(8:37), 'Value'])

pc_2 = lm(inf_adjusted_2$Value ~ unemp_adjusted_2$Value + avg_wages$Value)
summary(pc_2)
# plot(pc_2)

# building the basic Phillips Curve with data from this period to see if our model is an improvement

pc_control_2 = lm(inf_adjusted_2$Value ~ unemp_adjusted_2$Value)
summary(pc_control_2)
# plot(pc_control_2)

#####################################################################################

#####################################################################################
# ADDING THE EFFECT OF GDP - data from 1983 to 2019
# here we use unemployment and GDP for our explanatory variables

pc_3 = lm(inf$Value ~ unemp$Value + gdp$Value)
summary(pc_3)
# plot(pc_3)

# We already built a basic Phillips Curve for this period in 'pc_0' so we compare our model to pc_0 to see if it's an improvement
# What I mean is, if we build a 'pc_control_3' here it would be the same as pc_0

#####################################################################################

#####################################################################################
# ADDING THE EFFECT OF GDP, SHORT-TERM INTEREST RATES & LONG-TERM INTEREST RATES - data from 1987 to 2019

gdp_adjusted = data.frame(TIME = inf[c(5:37), 'TIME'], Value = inf[c(5:37), 'Value'])

pc_4 = lm(inf_adjusted$Value ~ unemp_adjusted$Value + st_int_adjusted$Value + gdp_adjusted$Value + lt_int$Value)
summary(pc_4)
# plot(pc_4)

# We already built a basic Phillips Curve for this period in 'pc_control_1' so we compare our model to pc_control_1 to see if it's an improvement
# What I mean is, if we build a 'pc_control_4' here it would be the same as pc_control_1









