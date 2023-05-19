#===============================================================================
#=============================== AGO ===================================
#===============================================================================


garch11e_AGO <- ugarchfit(g1e,data = e_AGO)

#Extract the VOLATILITIES 
vol_AGO <- ts(garch11e_AGO@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_AGO <- data.frame("date" = v_date, "vol" = vol_AGO)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_AGO02 <- dfvol_AGO %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_AGO20 <- dfvol_AGO %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_AGO0319 <- dfvol_AGO %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
AGO_vol_02avg <- mean(dfvol_AGO02$vol)
AGO_vol_20avg <- mean(dfvol_AGO20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_AGO02_df <- data.frame("year" = 2002, "avg_vol" = AGO_vol_02avg)
avg_AGO20_df <- data.frame("year" = 2020, "avg_vol" = AGO_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_AGO0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_AGO <- dfvol_AGO0319$vol
p=12 #Remove from subsequent codes
new_AGO=NULL
for (i in 1:length){
  ydfvol_AGO0319 <- e_series_AGO[((i-1)*p+1):(i*p)]
  yavg_AGO <- mean(ydfvol_AGO0319)
  new_AGO = rbind(new_AGO, yavg_AGO)
}

avg_AGO <- new_AGO
ts.plot(e_series_AGO)
ts.plot(avg_AGO)

#make the year_s manually
year_s <- rep(2002+1:length(avg_AGO))

#Make the dataframe for period average volatilities
avg_AGO0319 <- data.frame("year" = year_s, "avg_vol" = avg_AGO)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
AGO_vol_0220 <- rbind(avg_AGO02_df, avg_AGO0319, avg_AGO20_df)

#Remove the labels in the row number columns
AGO_vol_fix <- data.frame("year" = AGO_vol_0220$year, "avg_vol" = AGO_vol_0220$avg_vol)



#===============================================================================
#=============================== BDI ===================================
#===============================================================================


garch11e_BDI <- ugarchfit(g1e,data = e_BDI)

#Extract the VOLATILITIES 
vol_BDI <- ts(garch11e_BDI@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_BDI <- data.frame("date" = v_date, "vol" = vol_BDI)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_BDI02 <- dfvol_BDI %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_BDI20 <- dfvol_BDI %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_BDI0319 <- dfvol_BDI %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BDI_vol_02avg <- mean(dfvol_BDI02$vol)
BDI_vol_20avg <- mean(dfvol_BDI20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_BDI02_df <- data.frame("year" = 2002, "avg_vol" = BDI_vol_02avg)
avg_BDI20_df <- data.frame("year" = 2020, "avg_vol" = BDI_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_BDI0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_BDI <- dfvol_BDI0319$vol
p=12 #Remove from subsequent codes
new_BDI=NULL
for (i in 1:length){
  ydfvol_BDI0319 <- e_series_BDI[((i-1)*p+1):(i*p)]
  yavg_BDI <- mean(ydfvol_BDI0319)
  new_BDI = rbind(new_BDI, yavg_BDI)
}

avg_BDI <- new_BDI
ts.plot(e_series_BDI)
ts.plot(avg_BDI)

#make the year_s manually
year_s <- rep(2002+1:length(avg_BDI))

#Make the dataframe for period average volatilities
avg_BDI0319 <- data.frame("year" = year_s, "avg_vol" = avg_BDI)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BDI_vol_0220 <- rbind(avg_BDI02_df, avg_BDI0319, avg_BDI20_df)

#Remove the labels in the row number columns
BDI_vol_fix <- data.frame("year" = BDI_vol_0220$year, "avg_vol" = BDI_vol_0220$avg_vol)

#===============================================================================
#=============================== BEN ===================================
#===============================================================================


garch11e_BEN <- ugarchfit(g1e,data = e_BEN)

#Extract the VOLATILITIES 
vol_BEN <- ts(garch11e_BEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_BEN <- data.frame("date" = v_date, "vol" = vol_BEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_BEN02 <- dfvol_BEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_BEN20 <- dfvol_BEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_BEN0319 <- dfvol_BEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BEN_vol_02avg <- mean(dfvol_BEN02$vol)
BEN_vol_20avg <- mean(dfvol_BEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_BEN02_df <- data.frame("year" = 2002, "avg_vol" = BEN_vol_02avg)
avg_BEN20_df <- data.frame("year" = 2020, "avg_vol" = BEN_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_BEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_BEN <- dfvol_BEN0319$vol
p=12 #Remove from subsequent codes
new_BEN=NULL
for (i in 1:length){
  ydfvol_BEN0319 <- e_series_BEN[((i-1)*p+1):(i*p)]
  yavg_BEN <- mean(ydfvol_BEN0319)
  new_BEN = rbind(new_BEN, yavg_BEN)
}

avg_BEN <- new_BEN
ts.plot(e_series_BEN)
ts.plot(avg_BEN)

#make the year_s manually
year_s <- rep(2002+1:length(avg_BEN))

#Make the dataframe for period average volatilities
avg_BEN0319 <- data.frame("year" = year_s, "avg_vol" = avg_BEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BEN_vol_0220 <- rbind(avg_BEN02_df, avg_BEN0319, avg_BEN20_df)

#Remove the labels in the row number columns
BEN_vol_fix <- data.frame("year" = BEN_vol_0220$year, "avg_vol" = BEN_vol_0220$avg_vol)


#===============================================================================
#=============================== BWA ===================================
#===============================================================================


garch11e_BWA <- ugarchfit(g1e,data = e_BWA)

#Extract the VOLATILITIES 
vol_BWA <- ts(garch11e_BWA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_BWA <- data.frame("date" = v_date, "vol" = vol_BWA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_BWA02 <- dfvol_BWA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_BWA20 <- dfvol_BWA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_BWA0319 <- dfvol_BWA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BWA_vol_02avg <- mean(dfvol_BWA02$vol)
BWA_vol_20avg <- mean(dfvol_BWA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_BWA02_df <- data.frame("year" = 2002, "avg_vol" = BWA_vol_02avg)
avg_BWA20_df <- data.frame("year" = 2020, "avg_vol" = BWA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_BWA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_BWA <- dfvol_BWA0319$vol
p=12 #Remove from subsequent codes
new_BWA=NULL
for (i in 1:length){
  ydfvol_BWA0319 <- e_series_BWA[((i-1)*p+1):(i*p)]
  yavg_BWA <- mean(ydfvol_BWA0319)
  new_BWA = rbind(new_BWA, yavg_BWA)
}

avg_BWA <- new_BWA
ts.plot(e_series_BWA)
ts.plot(avg_BWA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_BWA))

#Make the dataframe for period average volatilities
avg_BWA0319 <- data.frame("year" = year_s, "avg_vol" = avg_BWA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BWA_vol_0220 <- rbind(avg_BWA02_df, avg_BWA0319, avg_BWA20_df)

#Remove the labels in the row number columns
BWA_vol_fix <- data.frame("year" = BWA_vol_0220$year, "avg_vol" = BWA_vol_0220$avg_vol)


#===============================================================================
#=============================== CAF ===================================
#===============================================================================


garch11e_CAF <- ugarchfit(g1e,data = e_CAF)

#Extract the VOLATILITIES 
vol_CAF <- ts(garch11e_CAF@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_CAF <- data.frame("date" = v_date, "vol" = vol_CAF)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_CAF02 <- dfvol_CAF %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_CAF20 <- dfvol_CAF %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_CAF0319 <- dfvol_CAF %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CAF_vol_02avg <- mean(dfvol_CAF02$vol)
CAF_vol_20avg <- mean(dfvol_CAF20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_CAF02_df <- data.frame("year" = 2002, "avg_vol" = CAF_vol_02avg)
avg_CAF20_df <- data.frame("year" = 2020, "avg_vol" = CAF_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_CAF0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_CAF <- dfvol_CAF0319$vol
p=12 #Remove from subsequent codes
new_CAF=NULL
for (i in 1:length){
  ydfvol_CAF0319 <- e_series_CAF[((i-1)*p+1):(i*p)]
  yavg_CAF <- mean(ydfvol_CAF0319)
  new_CAF = rbind(new_CAF, yavg_CAF)
}

avg_CAF <- new_CAF
ts.plot(e_series_CAF)
ts.plot(avg_CAF)

#make the year_s manually
year_s <- rep(2002+1:length(avg_CAF))

#Make the dataframe for period average volatilities
avg_CAF0319 <- data.frame("year" = year_s, "avg_vol" = avg_CAF)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CAF_vol_0220 <- rbind(avg_CAF02_df, avg_CAF0319, avg_CAF20_df)

#Remove the labels in the row number columns
CAF_vol_fix <- data.frame("year" = CAF_vol_0220$year, "avg_vol" = CAF_vol_0220$avg_vol)


#===============================================================================
#=============================== CIV ===================================
#===============================================================================


garch11e_CIV <- ugarchfit(g1e,data = e_CIV)

#Extract the VOLATILITIES 
vol_CIV <- ts(garch11e_CIV@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_CIV <- data.frame("date" = v_date, "vol" = vol_CIV)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_CIV02 <- dfvol_CIV %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_CIV20 <- dfvol_CIV %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_CIV0319 <- dfvol_CIV %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CIV_vol_02avg <- mean(dfvol_CIV02$vol)
CIV_vol_20avg <- mean(dfvol_CIV20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_CIV02_df <- data.frame("year" = 2002, "avg_vol" = CIV_vol_02avg)
avg_CIV20_df <- data.frame("year" = 2020, "avg_vol" = CIV_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_CIV0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_CIV <- dfvol_CIV0319$vol
p=12 #Remove from subsequent codes
new_CIV=NULL
for (i in 1:length){
  ydfvol_CIV0319 <- e_series_CIV[((i-1)*p+1):(i*p)]
  yavg_CIV <- mean(ydfvol_CIV0319)
  new_CIV = rbind(new_CIV, yavg_CIV)
}

avg_CIV <- new_CIV
ts.plot(e_series_CIV)
ts.plot(avg_CIV)

#make the year_s manually
year_s <- rep(2002+1:length(avg_CIV))

#Make the dataframe for period average volatilities
avg_CIV0319 <- data.frame("year" = year_s, "avg_vol" = avg_CIV)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CIV_vol_0220 <- rbind(avg_CIV02_df, avg_CIV0319, avg_CIV20_df)

#Remove the labels in the row number columns
CIV_vol_fix <- data.frame("year" = CIV_vol_0220$year, "avg_vol" = CIV_vol_0220$avg_vol)


#===============================================================================
#=============================== CMR ===================================
#===============================================================================

garch11e_CMR <- ugarchfit(g1e,data = e_CMR)

#Extract the VOLATILITIES 
vol_CMR <- ts(garch11e_CMR@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_CMR <- data.frame("date" = v_date, "vol" = vol_CMR)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_CMR02 <- dfvol_CMR %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_CMR20 <- dfvol_CMR %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_CMR0319 <- dfvol_CMR %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CMR_vol_02avg <- mean(dfvol_CMR02$vol)
CMR_vol_20avg <- mean(dfvol_CMR20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_CMR02_df <- data.frame("year" = 2002, "avg_vol" = CMR_vol_02avg)
avg_CMR20_df <- data.frame("year" = 2020, "avg_vol" = CMR_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_CMR0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_CMR <- dfvol_CMR0319$vol
p=12 #Remove from subsequent codes
new_CMR=NULL
for (i in 1:length){
  ydfvol_CMR0319 <- e_series_CMR[((i-1)*p+1):(i*p)]
  yavg_CMR <- mean(ydfvol_CMR0319)
  new_CMR = rbind(new_CMR, yavg_CMR)
}

avg_CMR <- new_CMR
ts.plot(e_series_CMR)
ts.plot(avg_CMR)

#make the year_s manually
year_s <- rep(2002+1:length(avg_CMR))

#Make the dataframe for period average volatilities
avg_CMR0319 <- data.frame("year" = year_s, "avg_vol" = avg_CMR)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CMR_vol_0220 <- rbind(avg_CMR02_df, avg_CMR0319, avg_CMR20_df)

#Remove the labels in the row number columns
CMR_vol_fix <- data.frame("year" = CMR_vol_0220$year, "avg_vol" = CMR_vol_0220$avg_vol)


#===============================================================================
#=============================== COG ===================================
#===============================================================================

#Extract
COG_ex <- filter(ex, iso3 == "COG") 

#Re-order 
COG_ex_ord <- COG_ex[order(COG_ex$year), ]

#Make a time series of the ordered series
E_COG <- ts(COG_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_COG,ylab="",xlab="")

#Plot log returns
e_COG = diff(log(E_COG))
plot(e_COG,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_COG <- ugarchfit(g1e,data = e_COG)

#Extract the VOLATILITIES 
vol_COG <- ts(garch11e_COG@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_COG <- data.frame("date" = v_date, "vol" = vol_COG)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_COG02 <- dfvol_COG %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_COG20 <- dfvol_COG %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_COG0319 <- dfvol_COG %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
COG_vol_02avg <- mean(dfvol_COG02$vol)
COG_vol_20avg <- mean(dfvol_COG20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_COG02_df <- data.frame("year" = 2002, "avg_vol" = COG_vol_02avg)
avg_COG20_df <- data.frame("year" = 2020, "avg_vol" = COG_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_COG0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_COG <- dfvol_COG0319$vol
p=12 #Remove from subsequent codes
new_COG=NULL
for (i in 1:length){
  ydfvol_COG0319 <- e_series_COG[((i-1)*p+1):(i*p)]
  yavg_COG <- mean(ydfvol_COG0319)
  new_COG = rbind(new_COG, yavg_COG)
}

avg_COG <- new_COG
ts.plot(e_series_COG)
ts.plot(avg_COG)

#make the year_s manually
year_s <- rep(2002+1:length(avg_COG))

#Make the dataframe for period average volatilities
avg_COG0319 <- data.frame("year" = year_s, "avg_vol" = avg_COG)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
COG_vol_0220 <- rbind(avg_COG02_df, avg_COG0319, avg_COG20_df)

#Remove the labels in the row number columns
COG_vol_fix <- data.frame("year" = COG_vol_0220$year, "avg_vol" = COG_vol_0220$avg_vol)


#===============================================================================
#=============================== CPV ===================================
#===============================================================================

#Extract
CPV_ex <- filter(ex, iso3 == "CPV") 

#Re-order 
CPV_ex_ord <- CPV_ex[order(CPV_ex$year), ]

#Make a time series of the ordered series
E_CPV <- ts(CPV_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_CPV,ylab="",xlab="")

#Plot log returns
e_CPV = diff(log(E_CPV))
plot(e_CPV,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_CPV <- ugarchfit(g1e,data = e_CPV)

#Extract the VOLATILITIES 
vol_CPV <- ts(garch11e_CPV@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_CPV <- data.frame("date" = v_date, "vol" = vol_CPV)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_CPV02 <- dfvol_CPV %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_CPV20 <- dfvol_CPV %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_CPV0319 <- dfvol_CPV %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CPV_vol_02avg <- mean(dfvol_CPV02$vol)
CPV_vol_20avg <- mean(dfvol_CPV20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_CPV02_df <- data.frame("year" = 2002, "avg_vol" = CPV_vol_02avg)
avg_CPV20_df <- data.frame("year" = 2020, "avg_vol" = CPV_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_CPV0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_CPV <- dfvol_CPV0319$vol
p=12 #Remove from subsequent codes
new_CPV=NULL
for (i in 1:length){
  ydfvol_CPV0319 <- e_series_CPV[((i-1)*p+1):(i*p)]
  yavg_CPV <- mean(ydfvol_CPV0319)
  new_CPV = rbind(new_CPV, yavg_CPV)
}

avg_CPV <- new_CPV
ts.plot(e_series_CPV)
ts.plot(avg_CPV)

#make the year_s manually
year_s <- rep(2002+1:length(avg_CPV))

#Make the dataframe for period average volatilities
avg_CPV0319 <- data.frame("year" = year_s, "avg_vol" = avg_CPV)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CPV_vol_0220 <- rbind(avg_CPV02_df, avg_CPV0319, avg_CPV20_df)

#Remove the labels in the row number columns
CPV_vol_fix <- data.frame("year" = CPV_vol_0220$year, "avg_vol" = CPV_vol_0220$avg_vol)


#===============================================================================
#=============================== GAB ===================================
#===============================================================================

#Extract
GAB_ex <- filter(ex, iso3 == "GAB") 

#Re-order 
GAB_ex_ord <- GAB_ex[order(GAB_ex$year), ]

#Make a time series of the ordered series
E_GAB <- ts(GAB_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_GAB,ylab="",xlab="")

#Plot log returns
e_GAB = diff(log(E_GAB))
plot(e_GAB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_GAB <- ugarchfit(g1e,data = e_GAB)

#Extract the VOLATILITIES 
vol_GAB <- ts(garch11e_GAB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_GAB <- data.frame("date" = v_date, "vol" = vol_GAB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_GAB02 <- dfvol_GAB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_GAB20 <- dfvol_GAB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_GAB0319 <- dfvol_GAB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GAB_vol_02avg <- mean(dfvol_GAB02$vol)
GAB_vol_20avg <- mean(dfvol_GAB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_GAB02_df <- data.frame("year" = 2002, "avg_vol" = GAB_vol_02avg)
avg_GAB20_df <- data.frame("year" = 2020, "avg_vol" = GAB_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_GAB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_GAB <- dfvol_GAB0319$vol
p=12 #Remove from subsequent codes
new_GAB=NULL
for (i in 1:length){
  ydfvol_GAB0319 <- e_series_GAB[((i-1)*p+1):(i*p)]
  yavg_GAB <- mean(ydfvol_GAB0319)
  new_GAB = rbind(new_GAB, yavg_GAB)
}

avg_GAB <- new_GAB
ts.plot(e_series_GAB)
ts.plot(avg_GAB)

#make the year_s manually
year_s <- rep(2002+1:length(avg_GAB))

#Make the dataframe for period average volatilities
avg_GAB0319 <- data.frame("year" = year_s, "avg_vol" = avg_GAB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GAB_vol_0220 <- rbind(avg_GAB02_df, avg_GAB0319, avg_GAB20_df)

#Remove the labels in the row number columns
GAB_vol_fix <- data.frame("year" = GAB_vol_0220$year, "avg_vol" = GAB_vol_0220$avg_vol)


#===============================================================================
#=============================== GHA ===================================
#===============================================================================

#Extract
GHA_ex <- filter(ex, iso3 == "GHA") 

#Re-order 
GHA_ex_ord <- GHA_ex[order(GHA_ex$year), ]

#Make a time series of the ordered series
E_GHA <- ts(GHA_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_GHA,ylab="",xlab="")

#Plot log returns
e_GHA = diff(log(E_GHA))
plot(e_GHA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_GHA <- ugarchfit(g1e,data = e_GHA)

#Extract the VOLATILITIES 
vol_GHA <- ts(garch11e_GHA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_GHA <- data.frame("date" = v_date, "vol" = vol_GHA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_GHA02 <- dfvol_GHA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_GHA20 <- dfvol_GHA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_GHA0319 <- dfvol_GHA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GHA_vol_02avg <- mean(dfvol_GHA02$vol)
GHA_vol_20avg <- mean(dfvol_GHA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_GHA02_df <- data.frame("year" = 2002, "avg_vol" = GHA_vol_02avg)
avg_GHA20_df <- data.frame("year" = 2020, "avg_vol" = GHA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_GHA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_GHA <- dfvol_GHA0319$vol
p=12 #Remove from subsequent codes
new_GHA=NULL
for (i in 1:length){
  ydfvol_GHA0319 <- e_series_GHA[((i-1)*p+1):(i*p)]
  yavg_GHA <- mean(ydfvol_GHA0319)
  new_GHA = rbind(new_GHA, yavg_GHA)
}

avg_GHA <- new_GHA
ts.plot(e_series_GHA)
ts.plot(avg_GHA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_GHA))

#Make the dataframe for period average volatilities
avg_GHA0319 <- data.frame("year" = year_s, "avg_vol" = avg_GHA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GHA_vol_0220 <- rbind(avg_GHA02_df, avg_GHA0319, avg_GHA20_df)

#Remove the labels in the row number columns
GHA_vol_fix <- data.frame("year" = GHA_vol_0220$year, "avg_vol" = GHA_vol_0220$avg_vol)


#===============================================================================
#=============================== GIN ===================================
#===============================================================================

#Extract
GIN_ex <- filter(ex, iso3 == "GIN") 

#Re-order 
GIN_ex_ord <- GIN_ex[order(GIN_ex$year), ]

#Make a time series of the ordered series
E_GIN <- ts(GIN_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_GIN,ylab="",xlab="")

#Plot log returns
e_GIN = diff(log(E_GIN))
plot(e_GIN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_GIN <- ugarchfit(g1e,data = e_GIN)

#Extract the VOLATILITIES 
vol_GIN <- ts(garch11e_GIN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_GIN <- data.frame("date" = v_date, "vol" = vol_GIN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_GIN02 <- dfvol_GIN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_GIN20 <- dfvol_GIN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_GIN0319 <- dfvol_GIN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GIN_vol_02avg <- mean(dfvol_GIN02$vol)
GIN_vol_20avg <- mean(dfvol_GIN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_GIN02_df <- data.frame("year" = 2002, "avg_vol" = GIN_vol_02avg)
avg_GIN20_df <- data.frame("year" = 2020, "avg_vol" = GIN_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_GIN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_GIN <- dfvol_GIN0319$vol
p=12 #Remove from subsequent codes
new_GIN=NULL
for (i in 1:length){
  ydfvol_GIN0319 <- e_series_GIN[((i-1)*p+1):(i*p)]
  yavg_GIN <- mean(ydfvol_GIN0319)
  new_GIN = rbind(new_GIN, yavg_GIN)
}

avg_GIN <- new_GIN
ts.plot(e_series_GIN)
ts.plot(avg_GIN)

#make the year_s manually
year_s <- rep(2002+1:length(avg_GIN))

#Make the dataframe for period average volatilities
avg_GIN0319 <- data.frame("year" = year_s, "avg_vol" = avg_GIN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GIN_vol_0220 <- rbind(avg_GIN02_df, avg_GIN0319, avg_GIN20_df)

#Remove the labels in the row number columns
GIN_vol_fix <- data.frame("year" = GIN_vol_0220$year, "avg_vol" = GIN_vol_0220$avg_vol)


#===============================================================================
#=============================== GMB ===================================
#===============================================================================

#Extract
GMB_ex <- filter(ex, iso3 == "GMB") 

#Re-order 
GMB_ex_ord <- GMB_ex[order(GMB_ex$year), ]

#Make a time series of the ordered series
E_GMB <- ts(GMB_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_GMB,ylab="",xlab="")

#Plot log returns
e_GMB = diff(log(E_GMB))
plot(e_GMB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_GMB <- ugarchfit(g1e,data = e_GMB)

#Extract the VOLATILITIES 
vol_GMB <- ts(garch11e_GMB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_GMB <- data.frame("date" = v_date, "vol" = vol_GMB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_GMB02 <- dfvol_GMB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_GMB20 <- dfvol_GMB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_GMB0319 <- dfvol_GMB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GMB_vol_02avg <- mean(dfvol_GMB02$vol)
GMB_vol_20avg <- mean(dfvol_GMB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_GMB02_df <- data.frame("year" = 2002, "avg_vol" = GMB_vol_02avg)
avg_GMB20_df <- data.frame("year" = 2020, "avg_vol" = GMB_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_GMB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_GMB <- dfvol_GMB0319$vol
p=12 #Remove from subsequent codes
new_GMB=NULL
for (i in 1:length){
  ydfvol_GMB0319 <- e_series_GMB[((i-1)*p+1):(i*p)]
  yavg_GMB <- mean(ydfvol_GMB0319)
  new_GMB = rbind(new_GMB, yavg_GMB)
}

avg_GMB <- new_GMB
ts.plot(e_series_GMB)
ts.plot(avg_GMB)

#make the year_s manually
year_s <- rep(2002+1:length(avg_GMB))

#Make the dataframe for period average volatilities
avg_GMB0319 <- data.frame("year" = year_s, "avg_vol" = avg_GMB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GMB_vol_0220 <- rbind(avg_GMB02_df, avg_GMB0319, avg_GMB20_df)

#Remove the labels in the row number columns
GMB_vol_fix <- data.frame("year" = GMB_vol_0220$year, "avg_vol" = GMB_vol_0220$avg_vol)


#===============================================================================
#=============================== GNB ===================================
#===============================================================================

#Extract
GNB_ex <- filter(ex, iso3 == "GNB") 

#Re-order 
GNB_ex_ord <- GNB_ex[order(GNB_ex$year), ]

#Make a time series of the ordered series
E_GNB <- ts(GNB_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_GNB,ylab="",xlab="")

#Plot log returns
e_GNB = diff(log(E_GNB))
plot(e_GNB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_GNB <- ugarchfit(g1e,data = e_GNB)

#Extract the VOLATILITIES 
vol_GNB <- ts(garch11e_GNB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_GNB <- data.frame("date" = v_date, "vol" = vol_GNB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_GNB02 <- dfvol_GNB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_GNB20 <- dfvol_GNB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_GNB0319 <- dfvol_GNB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GNB_vol_02avg <- mean(dfvol_GNB02$vol)
GNB_vol_20avg <- mean(dfvol_GNB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_GNB02_df <- data.frame("year" = 2002, "avg_vol" = GNB_vol_02avg)
avg_GNB20_df <- data.frame("year" = 2020, "avg_vol" = GNB_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_GNB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_GNB <- dfvol_GNB0319$vol
p=12 #Remove from subsequent codes
new_GNB=NULL
for (i in 1:length){
  ydfvol_GNB0319 <- e_series_GNB[((i-1)*p+1):(i*p)]
  yavg_GNB <- mean(ydfvol_GNB0319)
  new_GNB = rbind(new_GNB, yavg_GNB)
}

avg_GNB <- new_GNB
ts.plot(e_series_GNB)
ts.plot(avg_GNB)

#make the year_s manually
year_s <- rep(2002+1:length(avg_GNB))

#Make the dataframe for period average volatilities
avg_GNB0319 <- data.frame("year" = year_s, "avg_vol" = avg_GNB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GNB_vol_0220 <- rbind(avg_GNB02_df, avg_GNB0319, avg_GNB20_df)

#Remove the labels in the row number columns
GNB_vol_fix <- data.frame("year" = GNB_vol_0220$year, "avg_vol" = GNB_vol_0220$avg_vol)


#===============================================================================
#=============================== KEN ===================================
#===============================================================================

#Extract
KEN_ex <- filter(ex, iso3 == "KEN") 

#Re-order 
KEN_ex_ord <- KEN_ex[order(KEN_ex$year), ]

#Make a time series of the ordered series
E_KEN <- ts(KEN_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_KEN,ylab="",xlab="")

#Plot log returns
e_KEN = diff(log(E_KEN))
plot(e_KEN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_KEN <- ugarchfit(g1e,data = e_KEN)

#Extract the VOLATILITIES 
vol_KEN <- ts(garch11e_KEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_KEN <- data.frame("date" = v_date, "vol" = vol_KEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_KEN02 <- dfvol_KEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_KEN20 <- dfvol_KEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_KEN0319 <- dfvol_KEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
KEN_vol_02avg <- mean(dfvol_KEN02$vol)
KEN_vol_20avg <- mean(dfvol_KEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_KEN02_df <- data.frame("year" = 2002, "avg_vol" = KEN_vol_02avg)
avg_KEN20_df <- data.frame("year" = 2020, "avg_vol" = KEN_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_KEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_KEN <- dfvol_KEN0319$vol
p=12 #Remove from subsequent codes
new_KEN=NULL
for (i in 1:length){
  ydfvol_KEN0319 <- e_series_KEN[((i-1)*p+1):(i*p)]
  yavg_KEN <- mean(ydfvol_KEN0319)
  new_KEN = rbind(new_KEN, yavg_KEN)
}

avg_KEN <- new_KEN
ts.plot(e_series_KEN)
ts.plot(avg_KEN)

#make the year_s manually
year_s <- rep(2002+1:length(avg_KEN))

#Make the dataframe for period average volatilities
avg_KEN0319 <- data.frame("year" = year_s, "avg_vol" = avg_KEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
KEN_vol_0220 <- rbind(avg_KEN02_df, avg_KEN0319, avg_KEN20_df)

#Remove the labels in the row number columns
KEN_vol_fix <- data.frame("year" = KEN_vol_0220$year, "avg_vol" = KEN_vol_0220$avg_vol)


#===============================================================================
#=============================== MDG ===================================
#===============================================================================

#Extract
MDG_ex <- filter(ex, iso3 == "MDG") 

#Re-order 
MDG_ex_ord <- MDG_ex[order(MDG_ex$year), ]

#Make a time series of the ordered series
E_MDG <- ts(MDG_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_MDG,ylab="",xlab="")

#Plot log returns
e_MDG = diff(log(E_MDG))
plot(e_MDG,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_MDG <- ugarchfit(g1e,data = e_MDG)

#Extract the VOLATILITIES 
vol_MDG <- ts(garch11e_MDG@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_MDG <- data.frame("date" = v_date, "vol" = vol_MDG)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_MDG02 <- dfvol_MDG %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_MDG20 <- dfvol_MDG %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_MDG0319 <- dfvol_MDG %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MDG_vol_02avg <- mean(dfvol_MDG02$vol)
MDG_vol_20avg <- mean(dfvol_MDG20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_MDG02_df <- data.frame("year" = 2002, "avg_vol" = MDG_vol_02avg)
avg_MDG20_df <- data.frame("year" = 2020, "avg_vol" = MDG_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_MDG0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_MDG <- dfvol_MDG0319$vol
p=12 #Remove from subsequent codes
new_MDG=NULL
for (i in 1:length){
  ydfvol_MDG0319 <- e_series_MDG[((i-1)*p+1):(i*p)]
  yavg_MDG <- mean(ydfvol_MDG0319)
  new_MDG = rbind(new_MDG, yavg_MDG)
}

avg_MDG <- new_MDG
ts.plot(e_series_MDG)
ts.plot(avg_MDG)

#make the year_s manually
year_s <- rep(2002+1:length(avg_MDG))

#Make the dataframe for period average volatilities
avg_MDG0319 <- data.frame("year" = year_s, "avg_vol" = avg_MDG)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MDG_vol_0220 <- rbind(avg_MDG02_df, avg_MDG0319, avg_MDG20_df)

#Remove the labels in the row number columns
MDG_vol_fix <- data.frame("year" = MDG_vol_0220$year, "avg_vol" = MDG_vol_0220$avg_vol)


#===============================================================================
#=============================== MLI ===================================
#===============================================================================

#Extract
MLI_ex <- filter(ex, iso3 == "MLI") 

#Re-order 
MLI_ex_ord <- MLI_ex[order(MLI_ex$year), ]

#Make a time series of the ordered series
E_MLI <- ts(MLI_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_MLI,ylab="",xlab="")

#Plot log returns
e_MLI = diff(log(E_MLI))
plot(e_MLI,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_MLI <- ugarchfit(g1e,data = e_MLI)

#Extract the VOLATILITIES 
vol_MLI <- ts(garch11e_MLI@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_MLI <- data.frame("date" = v_date, "vol" = vol_MLI)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_MLI02 <- dfvol_MLI %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_MLI20 <- dfvol_MLI %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_MLI0319 <- dfvol_MLI %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MLI_vol_02avg <- mean(dfvol_MLI02$vol)
MLI_vol_20avg <- mean(dfvol_MLI20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_MLI02_df <- data.frame("year" = 2002, "avg_vol" = MLI_vol_02avg)
avg_MLI20_df <- data.frame("year" = 2020, "avg_vol" = MLI_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_MLI0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_MLI <- dfvol_MLI0319$vol
p=12 #Remove from subsequent codes
new_MLI=NULL
for (i in 1:length){
  ydfvol_MLI0319 <- e_series_MLI[((i-1)*p+1):(i*p)]
  yavg_MLI <- mean(ydfvol_MLI0319)
  new_MLI = rbind(new_MLI, yavg_MLI)
}

avg_MLI <- new_MLI
ts.plot(e_series_MLI)
ts.plot(avg_MLI)

#make the year_s manually
year_s <- rep(2002+1:length(avg_MLI))

#Make the dataframe for period average volatilities
avg_MLI0319 <- data.frame("year" = year_s, "avg_vol" = avg_MLI)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MLI_vol_0220 <- rbind(avg_MLI02_df, avg_MLI0319, avg_MLI20_df)

#Remove the labels in the row number columns
MLI_vol_fix <- data.frame("year" = MLI_vol_0220$year, "avg_vol" = MLI_vol_0220$avg_vol)


#===============================================================================
#=============================== MOZ ===================================
#===============================================================================

#Extract
MOZ_ex <- filter(ex, iso3 == "MOZ") 

#Re-order 
MOZ_ex_ord <- MOZ_ex[order(MOZ_ex$year), ]

#Make a time series of the ordered series
E_MOZ <- ts(MOZ_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_MOZ,ylab="",xlab="")

#Plot log returns
e_MOZ = diff(log(E_MOZ))
plot(e_MOZ,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_MOZ <- ugarchfit(g1e,data = e_MOZ)

#Extract the VOLATILITIES 
vol_MOZ <- ts(garch11e_MOZ@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_MOZ <- data.frame("date" = v_date, "vol" = vol_MOZ)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_MOZ02 <- dfvol_MOZ %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_MOZ20 <- dfvol_MOZ %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_MOZ0319 <- dfvol_MOZ %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MOZ_vol_02avg <- mean(dfvol_MOZ02$vol)
MOZ_vol_20avg <- mean(dfvol_MOZ20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_MOZ02_df <- data.frame("year" = 2002, "avg_vol" = MOZ_vol_02avg)
avg_MOZ20_df <- data.frame("year" = 2020, "avg_vol" = MOZ_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_MOZ0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_MOZ <- dfvol_MOZ0319$vol
p=12 #Remove from subsequent codes
new_MOZ=NULL
for (i in 1:length){
  ydfvol_MOZ0319 <- e_series_MOZ[((i-1)*p+1):(i*p)]
  yavg_MOZ <- mean(ydfvol_MOZ0319)
  new_MOZ = rbind(new_MOZ, yavg_MOZ)
}

avg_MOZ <- new_MOZ
ts.plot(e_series_MOZ)
ts.plot(avg_MOZ)

#make the year_s manually
year_s <- rep(2002+1:length(avg_MOZ))

#Make the dataframe for period average volatilities
avg_MOZ0319 <- data.frame("year" = year_s, "avg_vol" = avg_MOZ)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MOZ_vol_0220 <- rbind(avg_MOZ02_df, avg_MOZ0319, avg_MOZ20_df)

#Remove the labels in the row number columns
MOZ_vol_fix <- data.frame("year" = MOZ_vol_0220$year, "avg_vol" = MOZ_vol_0220$avg_vol)


#===============================================================================
#=============================== MRT ===================================
#===============================================================================

#Extract
MRT_ex <- filter(ex, iso3 == "MRT") 

#Re-order 
MRT_ex_ord <- MRT_ex[order(MRT_ex$year), ]

#Make a time series of the ordered series
E_MRT <- ts(MRT_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_MRT,ylab="",xlab="")

#Plot log returns
e_MRT = diff(log(E_MRT))
plot(e_MRT,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_MRT <- ugarchfit(g1e,data = e_MRT)

#Extract the VOLATILITIES 
vol_MRT <- ts(garch11e_MRT@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_MRT <- data.frame("date" = v_date, "vol" = vol_MRT)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_MRT02 <- dfvol_MRT %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_MRT20 <- dfvol_MRT %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_MRT0319 <- dfvol_MRT %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MRT_vol_02avg <- mean(dfvol_MRT02$vol)
MRT_vol_20avg <- mean(dfvol_MRT20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_MRT02_df <- data.frame("year" = 2002, "avg_vol" = MRT_vol_02avg)
avg_MRT20_df <- data.frame("year" = 2020, "avg_vol" = MRT_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_MRT0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_MRT <- dfvol_MRT0319$vol
p=12 #Remove from subsequent codes
new_MRT=NULL
for (i in 1:length){
  ydfvol_MRT0319 <- e_series_MRT[((i-1)*p+1):(i*p)]
  yavg_MRT <- mean(ydfvol_MRT0319)
  new_MRT = rbind(new_MRT, yavg_MRT)
}

avg_MRT <- new_MRT
ts.plot(e_series_MRT)
ts.plot(avg_MRT)

#make the year_s manually
year_s <- rep(2002+1:length(avg_MRT))

#Make the dataframe for period average volatilities
avg_MRT0319 <- data.frame("year" = year_s, "avg_vol" = avg_MRT)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MRT_vol_0220 <- rbind(avg_MRT02_df, avg_MRT0319, avg_MRT20_df)

#Remove the labels in the row number columns
MRT_vol_fix <- data.frame("year" = MRT_vol_0220$year, "avg_vol" = MRT_vol_0220$avg_vol)


#===============================================================================
#=============================== MUS ===================================
#===============================================================================

#Extract
MUS_ex <- filter(ex, iso3 == "MUS") 

#Re-order 
MUS_ex_ord <- MUS_ex[order(MUS_ex$year), ]

#Make a time series of the ordered series
E_MUS <- ts(MUS_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_MUS,ylab="",xlab="")

#Plot log returns
e_MUS = diff(log(E_MUS))
plot(e_MUS,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_MUS <- ugarchfit(g1e,data = e_MUS)

#Extract the VOLATILITIES 
vol_MUS <- ts(garch11e_MUS@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_MUS <- data.frame("date" = v_date, "vol" = vol_MUS)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_MUS02 <- dfvol_MUS %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_MUS20 <- dfvol_MUS %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_MUS0319 <- dfvol_MUS %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MUS_vol_02avg <- mean(dfvol_MUS02$vol)
MUS_vol_20avg <- mean(dfvol_MUS20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_MUS02_df <- data.frame("year" = 2002, "avg_vol" = MUS_vol_02avg)
avg_MUS20_df <- data.frame("year" = 2020, "avg_vol" = MUS_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_MUS0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_MUS <- dfvol_MUS0319$vol
p=12 #Remove from subsequent codes
new_MUS=NULL
for (i in 1:length){
  ydfvol_MUS0319 <- e_series_MUS[((i-1)*p+1):(i*p)]
  yavg_MUS <- mean(ydfvol_MUS0319)
  new_MUS = rbind(new_MUS, yavg_MUS)
}

avg_MUS <- new_MUS
ts.plot(e_series_MUS)
ts.plot(avg_MUS)

#make the year_s manually
year_s <- rep(2002+1:length(avg_MUS))

#Make the dataframe for period average volatilities
avg_MUS0319 <- data.frame("year" = year_s, "avg_vol" = avg_MUS)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MUS_vol_0220 <- rbind(avg_MUS02_df, avg_MUS0319, avg_MUS20_df)

#Remove the labels in the row number columns
MUS_vol_fix <- data.frame("year" = MUS_vol_0220$year, "avg_vol" = MUS_vol_0220$avg_vol)


#===============================================================================
#=============================== NAM ===================================
#===============================================================================

#Extract
NAM_ex <- filter(ex, iso3 == "NAM") 

#Re-order 
NAM_ex_ord <- NAM_ex[order(NAM_ex$year), ]

#Make a time series of the ordered series
E_NAM <- ts(NAM_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_NAM,ylab="",xlab="")

#Plot log returns
e_NAM = diff(log(E_NAM))
plot(e_NAM,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_NAM <- ugarchfit(g1e,data = e_NAM)

#Extract the VOLATILITIES 
vol_NAM <- ts(garch11e_NAM@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_NAM <- data.frame("date" = v_date, "vol" = vol_NAM)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_NAM02 <- dfvol_NAM %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_NAM20 <- dfvol_NAM %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_NAM0319 <- dfvol_NAM %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NAM_vol_02avg <- mean(dfvol_NAM02$vol)
NAM_vol_20avg <- mean(dfvol_NAM20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_NAM02_df <- data.frame("year" = 2002, "avg_vol" = NAM_vol_02avg)
avg_NAM20_df <- data.frame("year" = 2020, "avg_vol" = NAM_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_NAM0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_NAM <- dfvol_NAM0319$vol
p=12 #Remove from subsequent codes
new_NAM=NULL
for (i in 1:length){
  ydfvol_NAM0319 <- e_series_NAM[((i-1)*p+1):(i*p)]
  yavg_NAM <- mean(ydfvol_NAM0319)
  new_NAM = rbind(new_NAM, yavg_NAM)
}

avg_NAM <- new_NAM
ts.plot(e_series_NAM)
ts.plot(avg_NAM)

#make the year_s manually
year_s <- rep(2002+1:length(avg_NAM))

#Make the dataframe for period average volatilities
avg_NAM0319 <- data.frame("year" = year_s, "avg_vol" = avg_NAM)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NAM_vol_0220 <- rbind(avg_NAM02_df, avg_NAM0319, avg_NAM20_df)

#Remove the labels in the row number columns
NAM_vol_fix <- data.frame("year" = NAM_vol_0220$year, "avg_vol" = NAM_vol_0220$avg_vol)


#===============================================================================
#=============================== NER ===================================
#===============================================================================

#Extract
NER_ex <- filter(ex, iso3 == "NER") 

#Re-order 
NER_ex_ord <- NER_ex[order(NER_ex$year), ]

#Make a time series of the ordered series
E_NER <- ts(NER_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_NER,ylab="",xlab="")

#Plot log returns
e_NER = diff(log(E_NER))
plot(e_NER,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_NER <- ugarchfit(g1e,data = e_NER)

#Extract the VOLATILITIES 
vol_NER <- ts(garch11e_NER@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_NER <- data.frame("date" = v_date, "vol" = vol_NER)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_NER02 <- dfvol_NER %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_NER20 <- dfvol_NER %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_NER0319 <- dfvol_NER %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NER_vol_02avg <- mean(dfvol_NER02$vol)
NER_vol_20avg <- mean(dfvol_NER20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_NER02_df <- data.frame("year" = 2002, "avg_vol" = NER_vol_02avg)
avg_NER20_df <- data.frame("year" = 2020, "avg_vol" = NER_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_NER0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_NER <- dfvol_NER0319$vol
p=12 #Remove from subsequent codes
new_NER=NULL
for (i in 1:length){
  ydfvol_NER0319 <- e_series_NER[((i-1)*p+1):(i*p)]
  yavg_NER <- mean(ydfvol_NER0319)
  new_NER = rbind(new_NER, yavg_NER)
}

avg_NER <- new_NER
ts.plot(e_series_NER)
ts.plot(avg_NER)

#make the year_s manually
year_s <- rep(2002+1:length(avg_NER))

#Make the dataframe for period average volatilities
avg_NER0319 <- data.frame("year" = year_s, "avg_vol" = avg_NER)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NER_vol_0220 <- rbind(avg_NER02_df, avg_NER0319, avg_NER20_df)

#Remove the labels in the row number columns
NER_vol_fix <- data.frame("year" = NER_vol_0220$year, "avg_vol" = NER_vol_0220$avg_vol)


#===============================================================================
#=============================== NGA ===================================
#===============================================================================

#Extract
NGA_ex <- filter(ex, iso3 == "NGA") 

#Re-order 
NGA_ex_ord <- NGA_ex[order(NGA_ex$year), ]

#Make a time series of the ordered series
E_NGA <- ts(NGA_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_NGA,ylab="",xlab="")

#Plot log returns
e_NGA = diff(log(E_NGA))
plot(e_NGA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_NGA <- ugarchfit(g1e,data = e_NGA)

#Extract the VOLATILITIES 
vol_NGA <- ts(garch11e_NGA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_NGA <- data.frame("date" = v_date, "vol" = vol_NGA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_NGA02 <- dfvol_NGA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_NGA20 <- dfvol_NGA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_NGA0319 <- dfvol_NGA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NGA_vol_02avg <- mean(dfvol_NGA02$vol)
NGA_vol_20avg <- mean(dfvol_NGA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_NGA02_df <- data.frame("year" = 2002, "avg_vol" = NGA_vol_02avg)
avg_NGA20_df <- data.frame("year" = 2020, "avg_vol" = NGA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_NGA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_NGA <- dfvol_NGA0319$vol
p=12 #Remove from subsequent codes
new_NGA=NULL
for (i in 1:length){
  ydfvol_NGA0319 <- e_series_NGA[((i-1)*p+1):(i*p)]
  yavg_NGA <- mean(ydfvol_NGA0319)
  new_NGA = rbind(new_NGA, yavg_NGA)
}

avg_NGA <- new_NGA
ts.plot(e_series_NGA)
ts.plot(avg_NGA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_NGA))

#Make the dataframe for period average volatilities
avg_NGA0319 <- data.frame("year" = year_s, "avg_vol" = avg_NGA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NGA_vol_0220 <- rbind(avg_NGA02_df, avg_NGA0319, avg_NGA20_df)

#Remove the labels in the row number columns
NGA_vol_fix <- data.frame("year" = NGA_vol_0220$year, "avg_vol" = NGA_vol_0220$avg_vol)


#===============================================================================
#=============================== RWA ===================================
#===============================================================================

#Extract
RWA_ex <- filter(ex, iso3 == "RWA") 

#Re-order 
RWA_ex_ord <- RWA_ex[order(RWA_ex$year), ]

#Make a time series of the ordered series
E_RWA <- ts(RWA_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_RWA,ylab="",xlab="")

#Plot log returns
e_RWA = diff(log(E_RWA))
plot(e_RWA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_RWA <- ugarchfit(g1e,data = e_RWA)

#Extract the VOLATILITIES 
vol_RWA <- ts(garch11e_RWA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_RWA <- data.frame("date" = v_date, "vol" = vol_RWA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_RWA02 <- dfvol_RWA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_RWA20 <- dfvol_RWA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_RWA0319 <- dfvol_RWA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
RWA_vol_02avg <- mean(dfvol_RWA02$vol)
RWA_vol_20avg <- mean(dfvol_RWA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_RWA02_df <- data.frame("year" = 2002, "avg_vol" = RWA_vol_02avg)
avg_RWA20_df <- data.frame("year" = 2020, "avg_vol" = RWA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_RWA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_RWA <- dfvol_RWA0319$vol
p=12 #Remove from subsequent codes
new_RWA=NULL
for (i in 1:length){
  ydfvol_RWA0319 <- e_series_RWA[((i-1)*p+1):(i*p)]
  yavg_RWA <- mean(ydfvol_RWA0319)
  new_RWA = rbind(new_RWA, yavg_RWA)
}

avg_RWA <- new_RWA
ts.plot(e_series_RWA)
ts.plot(avg_RWA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_RWA))

#Make the dataframe for period average volatilities
avg_RWA0319 <- data.frame("year" = year_s, "avg_vol" = avg_RWA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
RWA_vol_0220 <- rbind(avg_RWA02_df, avg_RWA0319, avg_RWA20_df)

#Remove the labels in the row number columns
RWA_vol_fix <- data.frame("year" = RWA_vol_0220$year, "avg_vol" = RWA_vol_0220$avg_vol)


#===============================================================================
#=============================== SDN ===================================
#===============================================================================

#Extract
SDN_ex <- filter(ex, iso3 == "SDN") 

#Re-order 
SDN_ex_ord <- SDN_ex[order(SDN_ex$year), ]

#Make a time series of the ordered series
E_SDN <- ts(SDN_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_SDN,ylab="",xlab="")

#Plot log returns
e_SDN = diff(log(E_SDN))
plot(e_SDN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_SDN <- ugarchfit(g1e,data = e_SDN)

#Extract the VOLATILITIES 
vol_SDN <- ts(garch11e_SDN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_SDN <- data.frame("date" = v_date, "vol" = vol_SDN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_SDN02 <- dfvol_SDN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_SDN20 <- dfvol_SDN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_SDN0319 <- dfvol_SDN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SDN_vol_02avg <- mean(dfvol_SDN02$vol)
SDN_vol_20avg <- mean(dfvol_SDN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_SDN02_df <- data.frame("year" = 2002, "avg_vol" = SDN_vol_02avg)
avg_SDN20_df <- data.frame("year" = 2020, "avg_vol" = SDN_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_SDN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_SDN <- dfvol_SDN0319$vol
p=12 #Remove from subsequent codes
new_SDN=NULL
for (i in 1:length){
  ydfvol_SDN0319 <- e_series_SDN[((i-1)*p+1):(i*p)]
  yavg_SDN <- mean(ydfvol_SDN0319)
  new_SDN = rbind(new_SDN, yavg_SDN)
}

avg_SDN <- new_SDN
ts.plot(e_series_SDN)
ts.plot(avg_SDN)

#make the year_s manually
year_s <- rep(2002+1:length(avg_SDN))

#Make the dataframe for period average volatilities
avg_SDN0319 <- data.frame("year" = year_s, "avg_vol" = avg_SDN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SDN_vol_0220 <- rbind(avg_SDN02_df, avg_SDN0319, avg_SDN20_df)

#Remove the labels in the row number columns
SDN_vol_fix <- data.frame("year" = SDN_vol_0220$year, "avg_vol" = SDN_vol_0220$avg_vol)


#===============================================================================
#=============================== SEN ===================================
#===============================================================================

#Extract
SEN_ex <- filter(ex, iso3 == "SEN") 

#Re-order 
SEN_ex_ord <- SEN_ex[order(SEN_ex$year), ]

#Make a time series of the ordered series
E_SEN <- ts(SEN_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_SEN,ylab="",xlab="")

#Plot log returns
e_SEN = diff(log(E_SEN))
plot(e_SEN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_SEN <- ugarchfit(g1e,data = e_SEN)

#Extract the VOLATILITIES 
vol_SEN <- ts(garch11e_SEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_SEN <- data.frame("date" = v_date, "vol" = vol_SEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_SEN02 <- dfvol_SEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_SEN20 <- dfvol_SEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_SEN0319 <- dfvol_SEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SEN_vol_02avg <- mean(dfvol_SEN02$vol)
SEN_vol_20avg <- mean(dfvol_SEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_SEN02_df <- data.frame("year" = 2002, "avg_vol" = SEN_vol_02avg)
avg_SEN20_df <- data.frame("year" = 2020, "avg_vol" = SEN_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_SEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_SEN <- dfvol_SEN0319$vol
p=12 #Remove from subsequent codes
new_SEN=NULL
for (i in 1:length){
  ydfvol_SEN0319 <- e_series_SEN[((i-1)*p+1):(i*p)]
  yavg_SEN <- mean(ydfvol_SEN0319)
  new_SEN = rbind(new_SEN, yavg_SEN)
}

avg_SEN <- new_SEN
ts.plot(e_series_SEN)
ts.plot(avg_SEN)

#make the year_s manually
year_s <- rep(2002+1:length(avg_SEN))

#Make the dataframe for period average volatilities
avg_SEN0319 <- data.frame("year" = year_s, "avg_vol" = avg_SEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SEN_vol_0220 <- rbind(avg_SEN02_df, avg_SEN0319, avg_SEN20_df)

#Remove the labels in the row number columns
SEN_vol_fix <- data.frame("year" = SEN_vol_0220$year, "avg_vol" = SEN_vol_0220$avg_vol)


#===============================================================================
#=============================== SLE ===================================
#===============================================================================

#Extract
SLE_ex <- filter(ex, iso3 == "SLE") 

#Re-order 
SLE_ex_ord <- SLE_ex[order(SLE_ex$year), ]

#Make a time series of the ordered series
E_SLE <- ts(SLE_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_SLE,ylab="",xlab="")

#Plot log returns
e_SLE = diff(log(E_SLE))
plot(e_SLE,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_SLE <- ugarchfit(g1e,data = e_SLE)

#Extract the VOLATILITIES 
vol_SLE <- ts(garch11e_SLE@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_SLE <- data.frame("date" = v_date, "vol" = vol_SLE)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_SLE02 <- dfvol_SLE %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_SLE20 <- dfvol_SLE %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_SLE0319 <- dfvol_SLE %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SLE_vol_02avg <- mean(dfvol_SLE02$vol)
SLE_vol_20avg <- mean(dfvol_SLE20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_SLE02_df <- data.frame("year" = 2002, "avg_vol" = SLE_vol_02avg)
avg_SLE20_df <- data.frame("year" = 2020, "avg_vol" = SLE_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_SLE0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_SLE <- dfvol_SLE0319$vol
p=12 #Remove from subsequent codes
new_SLE=NULL
for (i in 1:length){
  ydfvol_SLE0319 <- e_series_SLE[((i-1)*p+1):(i*p)]
  yavg_SLE <- mean(ydfvol_SLE0319)
  new_SLE = rbind(new_SLE, yavg_SLE)
}

avg_SLE <- new_SLE
ts.plot(e_series_SLE)
ts.plot(avg_SLE)

#make the year_s manually
year_s <- rep(2002+1:length(avg_SLE))

#Make the dataframe for period average volatilities
avg_SLE0319 <- data.frame("year" = year_s, "avg_vol" = avg_SLE)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SLE_vol_0220 <- rbind(avg_SLE02_df, avg_SLE0319, avg_SLE20_df)

#Remove the labels in the row number columns
SLE_vol_fix <- data.frame("year" = SLE_vol_0220$year, "avg_vol" = SLE_vol_0220$avg_vol)


#===============================================================================
#=============================== SWZ ===================================
#===============================================================================

#Extract
SWZ_ex <- filter(ex, iso3 == "SWZ") 

#Re-order 
SWZ_ex_ord <- SWZ_ex[order(SWZ_ex$year), ]

#Make a time series of the ordered series
E_SWZ <- ts(SWZ_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_SWZ,ylab="",xlab="")

#Plot log returns
e_SWZ = diff(log(E_SWZ))
plot(e_SWZ,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_SWZ <- ugarchfit(g1e,data = e_SWZ)

#Extract the VOLATILITIES 
vol_SWZ <- ts(garch11e_SWZ@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_SWZ <- data.frame("date" = v_date, "vol" = vol_SWZ)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_SWZ02 <- dfvol_SWZ %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_SWZ20 <- dfvol_SWZ %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_SWZ0319 <- dfvol_SWZ %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SWZ_vol_02avg <- mean(dfvol_SWZ02$vol)
SWZ_vol_20avg <- mean(dfvol_SWZ20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_SWZ02_df <- data.frame("year" = 2002, "avg_vol" = SWZ_vol_02avg)
avg_SWZ20_df <- data.frame("year" = 2020, "avg_vol" = SWZ_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_SWZ0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_SWZ <- dfvol_SWZ0319$vol
p=12 #Remove from subsequent codes
new_SWZ=NULL
for (i in 1:length){
  ydfvol_SWZ0319 <- e_series_SWZ[((i-1)*p+1):(i*p)]
  yavg_SWZ <- mean(ydfvol_SWZ0319)
  new_SWZ = rbind(new_SWZ, yavg_SWZ)
}

avg_SWZ <- new_SWZ
ts.plot(e_series_SWZ)
ts.plot(avg_SWZ)

#make the year_s manually
year_s <- rep(2002+1:length(avg_SWZ))

#Make the dataframe for period average volatilities
avg_SWZ0319 <- data.frame("year" = year_s, "avg_vol" = avg_SWZ)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SWZ_vol_0220 <- rbind(avg_SWZ02_df, avg_SWZ0319, avg_SWZ20_df)

#Remove the labels in the row number columns
SWZ_vol_fix <- data.frame("year" = SWZ_vol_0220$year, "avg_vol" = SWZ_vol_0220$avg_vol)


#===============================================================================
#=============================== SYC ===================================
#===============================================================================

#Extract
SYC_ex <- filter(ex, iso3 == "SYC") 

#Re-order 
SYC_ex_ord <- SYC_ex[order(SYC_ex$year), ]

#Make a time series of the ordered series
E_SYC <- ts(SYC_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_SYC,ylab="",xlab="")

#Plot log returns
e_SYC = diff(log(E_SYC))
plot(e_SYC,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_SYC <- ugarchfit(g1e,data = e_SYC)

#Extract the VOLATILITIES 
vol_SYC <- ts(garch11e_SYC@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_SYC <- data.frame("date" = v_date, "vol" = vol_SYC)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_SYC02 <- dfvol_SYC %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_SYC20 <- dfvol_SYC %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_SYC0319 <- dfvol_SYC %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SYC_vol_02avg <- mean(dfvol_SYC02$vol)
SYC_vol_20avg <- mean(dfvol_SYC20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_SYC02_df <- data.frame("year" = 2002, "avg_vol" = SYC_vol_02avg)
avg_SYC20_df <- data.frame("year" = 2020, "avg_vol" = SYC_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_SYC0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_SYC <- dfvol_SYC0319$vol
p=12 #Remove from subsequent codes
new_SYC=NULL
for (i in 1:length){
  ydfvol_SYC0319 <- e_series_SYC[((i-1)*p+1):(i*p)]
  yavg_SYC <- mean(ydfvol_SYC0319)
  new_SYC = rbind(new_SYC, yavg_SYC)
}

avg_SYC <- new_SYC
ts.plot(e_series_SYC)
ts.plot(avg_SYC)

#make the year_s manually
year_s <- rep(2002+1:length(avg_SYC))

#Make the dataframe for period average volatilities
avg_SYC0319 <- data.frame("year" = year_s, "avg_vol" = avg_SYC)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SYC_vol_0220 <- rbind(avg_SYC02_df, avg_SYC0319, avg_SYC20_df)

#Remove the labels in the row number columns
SYC_vol_fix <- data.frame("year" = SYC_vol_0220$year, "avg_vol" = SYC_vol_0220$avg_vol)


#===============================================================================
#=============================== TCD ===================================
#===============================================================================

#Extract
TCD_ex <- filter(ex, iso3 == "TCD") 

#Re-order 
TCD_ex_ord <- TCD_ex[order(TCD_ex$year), ]

#Make a time series of the ordered series
E_TCD <- ts(TCD_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_TCD,ylab="",xlab="")

#Plot log returns
e_TCD = diff(log(E_TCD))
plot(e_TCD,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_TCD <- ugarchfit(g1e,data = e_TCD)

#Extract the VOLATILITIES 
vol_TCD <- ts(garch11e_TCD@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_TCD <- data.frame("date" = v_date, "vol" = vol_TCD)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_TCD02 <- dfvol_TCD %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_TCD20 <- dfvol_TCD %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_TCD0319 <- dfvol_TCD %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TCD_vol_02avg <- mean(dfvol_TCD02$vol)
TCD_vol_20avg <- mean(dfvol_TCD20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_TCD02_df <- data.frame("year" = 2002, "avg_vol" = TCD_vol_02avg)
avg_TCD20_df <- data.frame("year" = 2020, "avg_vol" = TCD_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_TCD0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_TCD <- dfvol_TCD0319$vol
p=12 #Remove from subsequent codes
new_TCD=NULL
for (i in 1:length){
  ydfvol_TCD0319 <- e_series_TCD[((i-1)*p+1):(i*p)]
  yavg_TCD <- mean(ydfvol_TCD0319)
  new_TCD = rbind(new_TCD, yavg_TCD)
}

avg_TCD <- new_TCD
ts.plot(e_series_TCD)
ts.plot(avg_TCD)

#make the year_s manually
year_s <- rep(2002+1:length(avg_TCD))

#Make the dataframe for period average volatilities
avg_TCD0319 <- data.frame("year" = year_s, "avg_vol" = avg_TCD)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TCD_vol_0220 <- rbind(avg_TCD02_df, avg_TCD0319, avg_TCD20_df)

#Remove the labels in the row number columns
TCD_vol_fix <- data.frame("year" = TCD_vol_0220$year, "avg_vol" = TCD_vol_0220$avg_vol)


#===============================================================================
#=============================== TGO ===================================
#===============================================================================

#Extract
TGO_ex <- filter(ex, iso3 == "TGO") 

#Re-order 
TGO_ex_ord <- TGO_ex[order(TGO_ex$year), ]

#Make a time series of the ordered series
E_TGO <- ts(TGO_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_TGO,ylab="",xlab="")

#Plot log returns
e_TGO = diff(log(E_TGO))
plot(e_TGO,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_TGO <- ugarchfit(g1e,data = e_TGO)

#Extract the VOLATILITIES 
vol_TGO <- ts(garch11e_TGO@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_TGO <- data.frame("date" = v_date, "vol" = vol_TGO)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_TGO02 <- dfvol_TGO %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_TGO20 <- dfvol_TGO %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_TGO0319 <- dfvol_TGO %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TGO_vol_02avg <- mean(dfvol_TGO02$vol)
TGO_vol_20avg <- mean(dfvol_TGO20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_TGO02_df <- data.frame("year" = 2002, "avg_vol" = TGO_vol_02avg)
avg_TGO20_df <- data.frame("year" = 2020, "avg_vol" = TGO_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_TGO0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_TGO <- dfvol_TGO0319$vol
p=12 #Remove from subsequent codes
new_TGO=NULL
for (i in 1:length){
  ydfvol_TGO0319 <- e_series_TGO[((i-1)*p+1):(i*p)]
  yavg_TGO <- mean(ydfvol_TGO0319)
  new_TGO = rbind(new_TGO, yavg_TGO)
}

avg_TGO <- new_TGO
ts.plot(e_series_TGO)
ts.plot(avg_TGO)

#make the year_s manually
year_s <- rep(2002+1:length(avg_TGO))

#Make the dataframe for period average volatilities
avg_TGO0319 <- data.frame("year" = year_s, "avg_vol" = avg_TGO)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TGO_vol_0220 <- rbind(avg_TGO02_df, avg_TGO0319, avg_TGO20_df)

#Remove the labels in the row number columns
TGO_vol_fix <- data.frame("year" = TGO_vol_0220$year, "avg_vol" = TGO_vol_0220$avg_vol)


#===============================================================================
#=============================== TZA ===================================
#===============================================================================

#Extract
TZA_ex <- filter(ex, iso3 == "TZA") 

#Re-order 
TZA_ex_ord <- TZA_ex[order(TZA_ex$year), ]

#Make a time series of the ordered series
E_TZA <- ts(TZA_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_TZA,ylab="",xlab="")

#Plot log returns
e_TZA = diff(log(E_TZA))
plot(e_TZA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_TZA <- ugarchfit(g1e,data = e_TZA)

#Extract the VOLATILITIES 
vol_TZA <- ts(garch11e_TZA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_TZA <- data.frame("date" = v_date, "vol" = vol_TZA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_TZA02 <- dfvol_TZA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_TZA20 <- dfvol_TZA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_TZA0319 <- dfvol_TZA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TZA_vol_02avg <- mean(dfvol_TZA02$vol)
TZA_vol_20avg <- mean(dfvol_TZA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_TZA02_df <- data.frame("year" = 2002, "avg_vol" = TZA_vol_02avg)
avg_TZA20_df <- data.frame("year" = 2020, "avg_vol" = TZA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_TZA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_TZA <- dfvol_TZA0319$vol
p=12 #Remove from subsequent codes
new_TZA=NULL
for (i in 1:length){
  ydfvol_TZA0319 <- e_series_TZA[((i-1)*p+1):(i*p)]
  yavg_TZA <- mean(ydfvol_TZA0319)
  new_TZA = rbind(new_TZA, yavg_TZA)
}

avg_TZA <- new_TZA
ts.plot(e_series_TZA)
ts.plot(avg_TZA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_TZA))

#Make the dataframe for period average volatilities
avg_TZA0319 <- data.frame("year" = year_s, "avg_vol" = avg_TZA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TZA_vol_0220 <- rbind(avg_TZA02_df, avg_TZA0319, avg_TZA20_df)

#Remove the labels in the row number columns
TZA_vol_fix <- data.frame("year" = TZA_vol_0220$year, "avg_vol" = TZA_vol_0220$avg_vol)


#===============================================================================
#=============================== UGA ===================================
#===============================================================================

#Extract
UGA_ex <- filter(ex, iso3 == "UGA") 

#Re-order 
UGA_ex_ord <- UGA_ex[order(UGA_ex$year), ]

#Make a time series of the ordered series
E_UGA <- ts(UGA_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_UGA,ylab="",xlab="")

#Plot log returns
e_UGA = diff(log(E_UGA))
plot(e_UGA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_UGA <- ugarchfit(g1e,data = e_UGA)

#Extract the VOLATILITIES 
vol_UGA <- ts(garch11e_UGA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_UGA <- data.frame("date" = v_date, "vol" = vol_UGA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_UGA02 <- dfvol_UGA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_UGA20 <- dfvol_UGA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_UGA0319 <- dfvol_UGA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
UGA_vol_02avg <- mean(dfvol_UGA02$vol)
UGA_vol_20avg <- mean(dfvol_UGA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_UGA02_df <- data.frame("year" = 2002, "avg_vol" = UGA_vol_02avg)
avg_UGA20_df <- data.frame("year" = 2020, "avg_vol" = UGA_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_UGA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_UGA <- dfvol_UGA0319$vol
p=12 #Remove from subsequent codes
new_UGA=NULL
for (i in 1:length){
  ydfvol_UGA0319 <- e_series_UGA[((i-1)*p+1):(i*p)]
  yavg_UGA <- mean(ydfvol_UGA0319)
  new_UGA = rbind(new_UGA, yavg_UGA)
}

avg_UGA <- new_UGA
ts.plot(e_series_UGA)
ts.plot(avg_UGA)

#make the year_s manually
year_s <- rep(2002+1:length(avg_UGA))

#Make the dataframe for period average volatilities
avg_UGA0319 <- data.frame("year" = year_s, "avg_vol" = avg_UGA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
UGA_vol_0220 <- rbind(avg_UGA02_df, avg_UGA0319, avg_UGA20_df)

#Remove the labels in the row number columns
UGA_vol_fix <- data.frame("year" = UGA_vol_0220$year, "avg_vol" = UGA_vol_0220$avg_vol)


#===============================================================================
#=============================== ZAF ===================================
#===============================================================================

#Extract
ZAF_ex <- filter(ex, iso3 == "ZAF") 

#Re-order 
ZAF_ex_ord <- ZAF_ex[order(ZAF_ex$year), ]

#Make a time series of the ordered series
E_ZAF <- ts(ZAF_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_ZAF,ylab="",xlab="")

#Plot log returns
e_ZAF = diff(log(E_ZAF))
plot(e_ZAF,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_ZAF <- ugarchfit(g1e,data = e_ZAF)

#Extract the VOLATILITIES 
vol_ZAF <- ts(garch11e_ZAF@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_ZAF <- data.frame("date" = v_date, "vol" = vol_ZAF)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_ZAF02 <- dfvol_ZAF %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_ZAF20 <- dfvol_ZAF %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_ZAF0319 <- dfvol_ZAF %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZAF_vol_02avg <- mean(dfvol_ZAF02$vol)
ZAF_vol_20avg <- mean(dfvol_ZAF20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_ZAF02_df <- data.frame("year" = 2002, "avg_vol" = ZAF_vol_02avg)
avg_ZAF20_df <- data.frame("year" = 2020, "avg_vol" = ZAF_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_ZAF0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_ZAF <- dfvol_ZAF0319$vol
p=12 #Remove from subsequent codes
new_ZAF=NULL
for (i in 1:length){
  ydfvol_ZAF0319 <- e_series_ZAF[((i-1)*p+1):(i*p)]
  yavg_ZAF <- mean(ydfvol_ZAF0319)
  new_ZAF = rbind(new_ZAF, yavg_ZAF)
}

avg_ZAF <- new_ZAF
ts.plot(e_series_ZAF)
ts.plot(avg_ZAF)

#make the year_s manually
year_s <- rep(2002+1:length(avg_ZAF))

#Make the dataframe for period average volatilities
avg_ZAF0319 <- data.frame("year" = year_s, "avg_vol" = avg_ZAF)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZAF_vol_0220 <- rbind(avg_ZAF02_df, avg_ZAF0319, avg_ZAF20_df)

#Remove the labels in the row number columns
ZAF_vol_fix <- data.frame("year" = ZAF_vol_0220$year, "avg_vol" = ZAF_vol_0220$avg_vol)


#===============================================================================
#=============================== ZMB ===================================
#===============================================================================

#Extract
ZMB_ex <- filter(ex, iso3 == "ZMB") 

#Re-order 
ZMB_ex_ord <- ZMB_ex[order(ZMB_ex$year), ]

#Make a time series of the ordered series
E_ZMB <- ts(ZMB_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_ZMB,ylab="",xlab="")

#Plot log returns
e_ZMB = diff(log(E_ZMB))
plot(e_ZMB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_ZMB <- ugarchfit(g1e,data = e_ZMB)

#Extract the VOLATILITIES 
vol_ZMB <- ts(garch11e_ZMB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_ZMB <- data.frame("date" = v_date, "vol" = vol_ZMB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_ZMB02 <- dfvol_ZMB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_ZMB20 <- dfvol_ZMB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_ZMB0319 <- dfvol_ZMB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZMB_vol_02avg <- mean(dfvol_ZMB02$vol)
ZMB_vol_20avg <- mean(dfvol_ZMB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_ZMB02_df <- data.frame("year" = 2002, "avg_vol" = ZMB_vol_02avg)
avg_ZMB20_df <- data.frame("year" = 2020, "avg_vol" = ZMB_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_ZMB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_ZMB <- dfvol_ZMB0319$vol
p=12 #Remove from subsequent codes
new_ZMB=NULL
for (i in 1:length){
  ydfvol_ZMB0319 <- e_series_ZMB[((i-1)*p+1):(i*p)]
  yavg_ZMB <- mean(ydfvol_ZMB0319)
  new_ZMB = rbind(new_ZMB, yavg_ZMB)
}

avg_ZMB <- new_ZMB
ts.plot(e_series_ZMB)
ts.plot(avg_ZMB)

#make the year_s manually
year_s <- rep(2002+1:length(avg_ZMB))

#Make the dataframe for period average volatilities
avg_ZMB0319 <- data.frame("year" = year_s, "avg_vol" = avg_ZMB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZMB_vol_0220 <- rbind(avg_ZMB02_df, avg_ZMB0319, avg_ZMB20_df)

#Remove the labels in the row number columns
ZMB_vol_fix <- data.frame("year" = ZMB_vol_0220$year, "avg_vol" = ZMB_vol_0220$avg_vol)


#===============================================================================
#=============================== ZWE ===================================
#===============================================================================

#Extract
ZWE_ex <- filter(ex, iso3 == "ZWE") 

#Re-order 
ZWE_ex_ord <- ZWE_ex[order(ZWE_ex$year), ]

#Make a time series of the ordered series
E_ZWE <- ts(ZWE_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_ZWE,ylab="",xlab="")

#Plot log returns
e_ZWE = diff(log(E_ZWE))
plot(e_ZWE,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_ZWE <- ugarchfit(g1e,data = e_ZWE)

#Extract the VOLATILITIES 
vol_ZWE <- ts(garch11e_ZWE@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol_ZWE <- data.frame("date" = v_date, "vol" = vol_ZWE)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol_ZWE02 <- dfvol_ZWE %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol_ZWE20 <- dfvol_ZWE %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol_ZWE0319 <- dfvol_ZWE %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZWE_vol_02avg <- mean(dfvol_ZWE02$vol)
ZWE_vol_20avg <- mean(dfvol_ZWE20$vol)

#Create a df to store the averages for 2002 and 2020.
avg_ZWE02_df <- data.frame("year" = 2002, "avg_vol" = ZWE_vol_02avg)
avg_ZWE20_df <- data.frame("year" = 2020, "avg_vol" = ZWE_vol_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol_ZWE0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_series_ZWE <- dfvol_ZWE0319$vol
p=12 #Remove from subsequent codes
new_ZWE=NULL
for (i in 1:length){
  ydfvol_ZWE0319 <- e_series_ZWE[((i-1)*p+1):(i*p)]
  yavg_ZWE <- mean(ydfvol_ZWE0319)
  new_ZWE = rbind(new_ZWE, yavg_ZWE)
}

avg_ZWE <- new_ZWE
ts.plot(e_series_ZWE)
ts.plot(avg_ZWE)

#make the year_s manually
year_s <- rep(2002+1:length(avg_ZWE))

#Make the dataframe for period average volatilities
avg_ZWE0319 <- data.frame("year" = year_s, "avg_vol" = avg_ZWE)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZWE_vol_0220 <- rbind(avg_ZWE02_df, avg_ZWE0319, avg_ZWE20_df)

#Remove the labels in the row number columns
ZWE_vol_fix <- data.frame("year" = ZWE_vol_0220$year, "avg_vol" = ZWE_vol_0220$avg_vol)

sGARCH11e_avg <- rbind(AGO_vol_fix, BDI_vol_fix, BEN_vol_fix, BWA_vol_fix, CAF_vol_fix,
                      CIV_vol_fix, CMR_vol_fix, COG_vol_fix, CPV_vol_fix, GAB_vol_fix, GHA_vol_fix,
                      GIN_vol_fix, GMB_vol_fix, GNB_vol_fix, KEN_vol_fix, MDG_vol_fix, MLI_vol_fix,
                      
                      MOZ_vol_fix, MRT_vol_fix, MUS_vol_fix, NAM_vol_fix,
                      NER_vol_fix, NGA_vol_fix, RWA_vol_fix, SDN_vol_fix, SEN_vol_fix, SLE_vol_fix,
                      SWZ_vol_fix, SYC_vol_fix, TCD_vol_fix,
                      TGO_vol_fix, TZA_vol_fix, UGA_vol_fix, ZAF_vol_fix, ZMB_vol_fix, ZWE_vol_fix)

