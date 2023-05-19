#===============================================================================
#=============================== AGO ===================================
#===============================================================================


#Extract
AGO_1ex <- filter(ex, iso3 == "AGO") 

#Re-order 
AGO_1ex_ord <- AGO_1ex[order(AGO_1ex$year), ]

#Make a time series of the ordered series
E_1AGO <- ts(AGO_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1AGO,ylab="",xlab="")

#Plot log returns
e_1AGO = diff(log(E_1AGO))
plot(e_1AGO,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document.

garch11e_1AGO <- ugarchfit(g1e, data = e_1AGO)

#Extract the VOLATILITIES 
vol1_AGO <- ts(garch11e_1AGO@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_AGO <- data.frame("date" = v_date, "vol" = vol1_AGO)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_AGO02 <- dfvol1_AGO %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_AGO20 <- dfvol1_AGO %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_AGO0319 <- dfvol1_AGO %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
AGO_vol1_02avg <- mean(dfvol1_AGO02$vol)
AGO_vol1_20avg <- mean(dfvol1_AGO20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_AGO02_df <- data.frame("year" = 2002, "avg1_vol" = AGO_vol1_02avg)
avg1_AGO20_df <- data.frame("year" = 2020, "avg1_vol" = AGO_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_AGO0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_AGO <- dfvol1_AGO0319$vol
p=12 #Remove from subsequent codes
new_AGO=NULL
for (i in 1:length){
  ydfvol1_AGO0319 <- e_1series_AGO[((i-1)*p+1):(i*p)]
  yavg1_AGO <- mean(ydfvol1_AGO0319)
  new_AGO = rbind(new_AGO, yavg1_AGO)
}

avg1_AGO <- new_AGO
ts.plot(e_1series_AGO)
ts.plot(avg1_AGO)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_AGO))

#Make the dataframe for period average volatilities
avg1_AGO0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_AGO)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
AGO_vol1_0220 <- rbind(avg1_AGO02_df, avg1_AGO0319, avg1_AGO20_df)

#Remove the labels in the row number columns
AGO_vol1_fix <- data.frame("year" = AGO_vol1_0220$year, "avg1_vol" = AGO_vol1_0220$avg1_vol)



#===============================================================================
#=============================== BDI ===================================
#===============================================================================

#Extract
BDI_1ex <- filter(ex, iso3 == "BDI") 

#Re-order 
BDI_1ex_ord <- BDI_1ex[order(BDI_1ex$year), ]

#Make a time series of the ordered series
E_1BDI <- ts(BDI_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1BDI,ylab="",xlab="")

#Plot log returns
e_1BDI = diff(log(E_1BDI))
plot(e_1BDI,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1BDI <- ugarchfit(g1e,data = e_1BDI)

#Extract the VOLATILITIES 
vol1_BDI <- ts(garch11e_1BDI@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_BDI <- data.frame("date" = v_date, "vol" = vol1_BDI)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_BDI02 <- dfvol1_BDI %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_BDI20 <- dfvol1_BDI %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_BDI0319 <- dfvol1_BDI %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BDI_vol1_02avg <- mean(dfvol1_BDI02$vol)
BDI_vol1_20avg <- mean(dfvol1_BDI20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_BDI02_df <- data.frame("year" = 2002, "avg1_vol" = BDI_vol1_02avg)
avg1_BDI20_df <- data.frame("year" = 2020, "avg1_vol" = BDI_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_BDI0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_BDI <- dfvol1_BDI0319$vol
p=12 #Remove from subsequent codes
new_BDI=NULL
for (i in 1:length){
  ydfvol1_BDI0319 <- e_1series_BDI[((i-1)*p+1):(i*p)]
  yavg1_BDI <- mean(ydfvol1_BDI0319)
  new_BDI = rbind(new_BDI, yavg1_BDI)
}

avg1_BDI <- new_BDI
ts.plot(e_1series_BDI)
ts.plot(avg1_BDI)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_BDI))

#Make the dataframe for period average volatilities
avg1_BDI0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_BDI)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BDI_vol1_0220 <- rbind(avg1_BDI02_df, avg1_BDI0319, avg1_BDI20_df)

#Remove the labels in the row number columns
BDI_vol1_fix <- data.frame("year" = BDI_vol1_0220$year, "avg1_vol" = BDI_vol1_0220$avg1_vol)

#===============================================================================
#=============================== BEN ===================================
#===============================================================================

#Extract
BEN_1ex <- filter(ex, iso3 == "BEN") 

#Re-order 
BEN_1ex_ord <- BEN_1ex[order(BEN_1ex$year), ]

#Make a time series of the ordered series
E_1BEN <- ts(BEN_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1BEN,ylab="",xlab="")

#Plot log returns
e_1BEN = diff(log(E_1BEN))
plot(e_1BEN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1BEN <- ugarchfit(g1e,data = e_1BEN)

#Extract the VOLATILITIES 
vol1_BEN <- ts(garch11e_1BEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_BEN <- data.frame("date" = v_date, "vol" = vol1_BEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_BEN02 <- dfvol1_BEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_BEN20 <- dfvol1_BEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_BEN0319 <- dfvol1_BEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BEN_vol1_02avg <- mean(dfvol1_BEN02$vol)
BEN_vol1_20avg <- mean(dfvol1_BEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_BEN02_df <- data.frame("year" = 2002, "avg1_vol" = BEN_vol1_02avg)
avg1_BEN20_df <- data.frame("year" = 2020, "avg1_vol" = BEN_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_BEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_BEN <- dfvol1_BEN0319$vol
p=12 #Remove from subsequent codes
new_BEN=NULL
for (i in 1:length){
  ydfvol1_BEN0319 <- e_1series_BEN[((i-1)*p+1):(i*p)]
  yavg1_BEN <- mean(ydfvol1_BEN0319)
  new_BEN = rbind(new_BEN, yavg1_BEN)
}

avg1_BEN <- new_BEN
ts.plot(e_1series_BEN)
ts.plot(avg1_BEN)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_BEN))

#Make the dataframe for period average volatilities
avg1_BEN0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_BEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BEN_vol1_0220 <- rbind(avg1_BEN02_df, avg1_BEN0319, avg1_BEN20_df)

#Remove the labels in the row number columns
BEN_vol1_fix <- data.frame("year" = BEN_vol1_0220$year, "avg1_vol" = BEN_vol1_0220$avg1_vol)


#===============================================================================
#=============================== BWA ===================================
#===============================================================================

#Extract
BWA_1ex <- filter(ex, iso3 == "BWA") 

#Re-order 
BWA_1ex_ord <- BWA_1ex[order(BWA_1ex$year), ]

#Make a time series of the ordered series
E_1BWA <- ts(BWA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1BWA,ylab="",xlab="")

#Plot log returns
e_1BWA = diff(log(E_1BWA))
plot(e_1BWA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1BWA <- ugarchfit(g1e,data = e_1BWA)

#Extract the VOLATILITIES 
vol1_BWA <- ts(garch11e_1BWA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_BWA <- data.frame("date" = v_date, "vol" = vol1_BWA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_BWA02 <- dfvol1_BWA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_BWA20 <- dfvol1_BWA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_BWA0319 <- dfvol1_BWA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
BWA_vol1_02avg <- mean(dfvol1_BWA02$vol)
BWA_vol1_20avg <- mean(dfvol1_BWA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_BWA02_df <- data.frame("year" = 2002, "avg1_vol" = BWA_vol1_02avg)
avg1_BWA20_df <- data.frame("year" = 2020, "avg1_vol" = BWA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_BWA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_BWA <- dfvol1_BWA0319$vol
p=12 #Remove from subsequent codes
new_BWA=NULL
for (i in 1:length){
  ydfvol1_BWA0319 <- e_1series_BWA[((i-1)*p+1):(i*p)]
  yavg1_BWA <- mean(ydfvol1_BWA0319)
  new_BWA = rbind(new_BWA, yavg1_BWA)
}

avg1_BWA <- new_BWA
ts.plot(e_1series_BWA)
ts.plot(avg1_BWA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_BWA))

#Make the dataframe for period average volatilities
avg1_BWA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_BWA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
BWA_vol1_0220 <- rbind(avg1_BWA02_df, avg1_BWA0319, avg1_BWA20_df)

#Remove the labels in the row number columns
BWA_vol1_fix <- data.frame("year" = BWA_vol1_0220$year, "avg1_vol" = BWA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== CAF ===================================
#===============================================================================

#Extract
CAF_1ex <- filter(ex, iso3 == "CAF") 

#Re-order 
CAF_1ex_ord <- CAF_1ex[order(CAF_1ex$year), ]

#Make a time series of the ordered series
E_1CAF <- ts(CAF_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1CAF,ylab="",xlab="")

#Plot log returns
e_1CAF = diff(log(E_1CAF))
plot(e_1CAF,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1CAF <- ugarchfit(g1e,data = e_1CAF)

#Extract the VOLATILITIES 
vol1_CAF <- ts(garch11e_1CAF@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_CAF <- data.frame("date" = v_date, "vol" = vol1_CAF)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_CAF02 <- dfvol1_CAF %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_CAF20 <- dfvol1_CAF %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_CAF0319 <- dfvol1_CAF %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CAF_vol1_02avg <- mean(dfvol1_CAF02$vol)
CAF_vol1_20avg <- mean(dfvol1_CAF20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_CAF02_df <- data.frame("year" = 2002, "avg1_vol" = CAF_vol1_02avg)
avg1_CAF20_df <- data.frame("year" = 2020, "avg1_vol" = CAF_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_CAF0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_CAF <- dfvol1_CAF0319$vol
p=12 #Remove from subsequent codes
new_CAF=NULL
for (i in 1:length){
  ydfvol1_CAF0319 <- e_1series_CAF[((i-1)*p+1):(i*p)]
  yavg1_CAF <- mean(ydfvol1_CAF0319)
  new_CAF = rbind(new_CAF, yavg1_CAF)
}

avg1_CAF <- new_CAF
ts.plot(e_1series_CAF)
ts.plot(avg1_CAF)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_CAF))

#Make the dataframe for period average volatilities
avg1_CAF0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_CAF)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CAF_vol1_0220 <- rbind(avg1_CAF02_df, avg1_CAF0319, avg1_CAF20_df)

#Remove the labels in the row number columns
CAF_vol1_fix <- data.frame("year" = CAF_vol1_0220$year, "avg1_vol" = CAF_vol1_0220$avg1_vol)


#===============================================================================
#=============================== CIV ===================================
#===============================================================================

#Extract
CIV_1ex <- filter(ex, iso3 == "CIV") 

#Re-order 
CIV_1ex_ord <- CIV_1ex[order(CIV_1ex$year), ]

#Make a time series of the ordered series
E_1CIV <- ts(CIV_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1CIV,ylab="",xlab="")

#Plot log returns
e_1CIV = diff(log(E_1CIV))
plot(e_1CIV,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1CIV <- ugarchfit(g1e,data = e_1CIV)

#Extract the VOLATILITIES 
vol1_CIV <- ts(garch11e_1CIV@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_CIV <- data.frame("date" = v_date, "vol" = vol1_CIV)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_CIV02 <- dfvol1_CIV %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_CIV20 <- dfvol1_CIV %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_CIV0319 <- dfvol1_CIV %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CIV_vol1_02avg <- mean(dfvol1_CIV02$vol)
CIV_vol1_20avg <- mean(dfvol1_CIV20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_CIV02_df <- data.frame("year" = 2002, "avg1_vol" = CIV_vol1_02avg)
avg1_CIV20_df <- data.frame("year" = 2020, "avg1_vol" = CIV_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_CIV0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_CIV <- dfvol1_CIV0319$vol
p=12 #Remove from subsequent codes
new_CIV=NULL
for (i in 1:length){
  ydfvol1_CIV0319 <- e_1series_CIV[((i-1)*p+1):(i*p)]
  yavg1_CIV <- mean(ydfvol1_CIV0319)
  new_CIV = rbind(new_CIV, yavg1_CIV)
}

avg1_CIV <- new_CIV
ts.plot(e_1series_CIV)
ts.plot(avg1_CIV)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_CIV))

#Make the dataframe for period average volatilities
avg1_CIV0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_CIV)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CIV_vol1_0220 <- rbind(avg1_CIV02_df, avg1_CIV0319, avg1_CIV20_df)

#Remove the labels in the row number columns
CIV_vol1_fix <- data.frame("year" = CIV_vol1_0220$year, "avg1_vol" = CIV_vol1_0220$avg1_vol)


#===============================================================================
#=============================== CMR ===================================
#===============================================================================

#Extract
CMR_1ex <- filter(ex, iso3 == "CMR") 

#Re-order 
CMR_1ex_ord <- CMR_1ex[order(CMR_1ex$year), ]

#Make a time series of the ordered series
E_1CMR <- ts(CMR_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1CMR,ylab="",xlab="")

#Plot log returns
e_1CMR = diff(log(E_1CMR))
plot(e_1CMR,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1CMR <- ugarchfit(g1e,data = e_1CMR)

#Extract the VOLATILITIES 
vol1_CMR <- ts(garch11e_1CMR@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_CMR <- data.frame("date" = v_date, "vol" = vol1_CMR)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_CMR02 <- dfvol1_CMR %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_CMR20 <- dfvol1_CMR %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_CMR0319 <- dfvol1_CMR %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CMR_vol1_02avg <- mean(dfvol1_CMR02$vol)
CMR_vol1_20avg <- mean(dfvol1_CMR20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_CMR02_df <- data.frame("year" = 2002, "avg1_vol" = CMR_vol1_02avg)
avg1_CMR20_df <- data.frame("year" = 2020, "avg1_vol" = CMR_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_CMR0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_CMR <- dfvol1_CMR0319$vol
p=12 #Remove from subsequent codes
new_CMR=NULL
for (i in 1:length){
  ydfvol1_CMR0319 <- e_1series_CMR[((i-1)*p+1):(i*p)]
  yavg1_CMR <- mean(ydfvol1_CMR0319)
  new_CMR = rbind(new_CMR, yavg1_CMR)
}

avg1_CMR <- new_CMR
ts.plot(e_1series_CMR)
ts.plot(avg1_CMR)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_CMR))

#Make the dataframe for period average volatilities
avg1_CMR0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_CMR)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CMR_vol1_0220 <- rbind(avg1_CMR02_df, avg1_CMR0319, avg1_CMR20_df)

#Remove the labels in the row number columns
CMR_vol1_fix <- data.frame("year" = CMR_vol1_0220$year, "avg1_vol" = CMR_vol1_0220$avg1_vol)


#===============================================================================
#=============================== COG ===================================
#===============================================================================

#Extract
COG_1ex <- filter(ex, iso3 == "COG") 

#Re-order 
COG_1ex_ord <- COG_1ex[order(COG_1ex$year), ]

#Make a time series of the ordered series
E_1COG <- ts(COG_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1COG,ylab="",xlab="")

#Plot log returns
e_1COG = diff(log(E_1COG))
plot(e_1COG,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1COG <- ugarchfit(g1e,data = e_1COG)

#Extract the VOLATILITIES 
vol1_COG <- ts(garch11e_1COG@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_COG <- data.frame("date" = v_date, "vol" = vol1_COG)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_COG02 <- dfvol1_COG %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_COG20 <- dfvol1_COG %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_COG0319 <- dfvol1_COG %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
COG_vol1_02avg <- mean(dfvol1_COG02$vol)
COG_vol1_20avg <- mean(dfvol1_COG20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_COG02_df <- data.frame("year" = 2002, "avg1_vol" = COG_vol1_02avg)
avg1_COG20_df <- data.frame("year" = 2020, "avg1_vol" = COG_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_COG0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_COG <- dfvol1_COG0319$vol
p=12 #Remove from subsequent codes
new_COG=NULL
for (i in 1:length){
  ydfvol1_COG0319 <- e_1series_COG[((i-1)*p+1):(i*p)]
  yavg1_COG <- mean(ydfvol1_COG0319)
  new_COG = rbind(new_COG, yavg1_COG)
}

avg1_COG <- new_COG
ts.plot(e_1series_COG)
ts.plot(avg1_COG)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_COG))

#Make the dataframe for period average volatilities
avg1_COG0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_COG)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
COG_vol1_0220 <- rbind(avg1_COG02_df, avg1_COG0319, avg1_COG20_df)

#Remove the labels in the row number columns
COG_vol1_fix <- data.frame("year" = COG_vol1_0220$year, "avg1_vol" = COG_vol1_0220$avg1_vol)


#===============================================================================
#=============================== CPV ===================================
#===============================================================================

#Extract
CPV_1ex <- filter(ex, iso3 == "CPV") 

#Re-order 
CPV_1ex_ord <- CPV_1ex[order(CPV_1ex$year), ]

#Make a time series of the ordered series
E_1CPV <- ts(CPV_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1CPV,ylab="",xlab="")

#Plot log returns
e_1CPV = diff(log(E_1CPV))
plot(e_1CPV,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1CPV <- ugarchfit(g1e,data = e_1CPV)

#Extract the VOLATILITIES 
vol1_CPV <- ts(garch11e_1CPV@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_CPV <- data.frame("date" = v_date, "vol" = vol1_CPV)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_CPV02 <- dfvol1_CPV %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_CPV20 <- dfvol1_CPV %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_CPV0319 <- dfvol1_CPV %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
CPV_vol1_02avg <- mean(dfvol1_CPV02$vol)
CPV_vol1_20avg <- mean(dfvol1_CPV20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_CPV02_df <- data.frame("year" = 2002, "avg1_vol" = CPV_vol1_02avg)
avg1_CPV20_df <- data.frame("year" = 2020, "avg1_vol" = CPV_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_CPV0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_CPV <- dfvol1_CPV0319$vol
p=12 #Remove from subsequent codes
new_CPV=NULL
for (i in 1:length){
  ydfvol1_CPV0319 <- e_1series_CPV[((i-1)*p+1):(i*p)]
  yavg1_CPV <- mean(ydfvol1_CPV0319)
  new_CPV = rbind(new_CPV, yavg1_CPV)
}

avg1_CPV <- new_CPV
ts.plot(e_1series_CPV)
ts.plot(avg1_CPV)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_CPV))

#Make the dataframe for period average volatilities
avg1_CPV0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_CPV)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
CPV_vol1_0220 <- rbind(avg1_CPV02_df, avg1_CPV0319, avg1_CPV20_df)

#Remove the labels in the row number columns
CPV_vol1_fix <- data.frame("year" = CPV_vol1_0220$year, "avg1_vol" = CPV_vol1_0220$avg1_vol)


#===============================================================================
#=============================== GAB ===================================
#===============================================================================

#Extract
GAB_1ex <- filter(ex, iso3 == "GAB") 

#Re-order 
GAB_1ex_ord <- GAB_1ex[order(GAB_1ex$year), ]

#Make a time series of the ordered series
E_1GAB <- ts(GAB_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1GAB,ylab="",xlab="")

#Plot log returns
e_1GAB = diff(log(E_1GAB))
plot(e_1GAB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1GAB <- ugarchfit(g1e,data = e_1GAB)

#Extract the VOLATILITIES 
vol1_GAB <- ts(garch11e_1GAB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_GAB <- data.frame("date" = v_date, "vol" = vol1_GAB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_GAB02 <- dfvol1_GAB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_GAB20 <- dfvol1_GAB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_GAB0319 <- dfvol1_GAB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GAB_vol1_02avg <- mean(dfvol1_GAB02$vol)
GAB_vol1_20avg <- mean(dfvol1_GAB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_GAB02_df <- data.frame("year" = 2002, "avg1_vol" = GAB_vol1_02avg)
avg1_GAB20_df <- data.frame("year" = 2020, "avg1_vol" = GAB_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_GAB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_GAB <- dfvol1_GAB0319$vol
p=12 #Remove from subsequent codes
new_GAB=NULL
for (i in 1:length){
  ydfvol1_GAB0319 <- e_1series_GAB[((i-1)*p+1):(i*p)]
  yavg1_GAB <- mean(ydfvol1_GAB0319)
  new_GAB = rbind(new_GAB, yavg1_GAB)
}

avg1_GAB <- new_GAB
ts.plot(e_1series_GAB)
ts.plot(avg1_GAB)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_GAB))

#Make the dataframe for period average volatilities
avg1_GAB0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_GAB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GAB_vol1_0220 <- rbind(avg1_GAB02_df, avg1_GAB0319, avg1_GAB20_df)

#Remove the labels in the row number columns
GAB_vol1_fix <- data.frame("year" = GAB_vol1_0220$year, "avg1_vol" = GAB_vol1_0220$avg1_vol)


#===============================================================================
#=============================== GHA ===================================
#===============================================================================

#Extract
GHA_1ex <- filter(ex, iso3 == "GHA") 

#Re-order 
GHA_1ex_ord <- GHA_1ex[order(GHA_1ex$year), ]

#Make a time series of the ordered series
E_1GHA <- ts(GHA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1GHA,ylab="",xlab="")

#Plot log returns
e_1GHA = diff(log(E_1GHA))
plot(e_1GHA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1GHA <- ugarchfit(g1e,data = e_1GHA)

#Extract the VOLATILITIES 
vol1_GHA <- ts(garch11e_1GHA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_GHA <- data.frame("date" = v_date, "vol" = vol1_GHA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_GHA02 <- dfvol1_GHA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_GHA20 <- dfvol1_GHA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_GHA0319 <- dfvol1_GHA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GHA_vol1_02avg <- mean(dfvol1_GHA02$vol)
GHA_vol1_20avg <- mean(dfvol1_GHA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_GHA02_df <- data.frame("year" = 2002, "avg1_vol" = GHA_vol1_02avg)
avg1_GHA20_df <- data.frame("year" = 2020, "avg1_vol" = GHA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_GHA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_GHA <- dfvol1_GHA0319$vol
p=12 #Remove from subsequent codes
new_GHA=NULL
for (i in 1:length){
  ydfvol1_GHA0319 <- e_1series_GHA[((i-1)*p+1):(i*p)]
  yavg1_GHA <- mean(ydfvol1_GHA0319)
  new_GHA = rbind(new_GHA, yavg1_GHA)
}

avg1_GHA <- new_GHA
ts.plot(e_1series_GHA)
ts.plot(avg1_GHA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_GHA))

#Make the dataframe for period average volatilities
avg1_GHA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_GHA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GHA_vol1_0220 <- rbind(avg1_GHA02_df, avg1_GHA0319, avg1_GHA20_df)

#Remove the labels in the row number columns
GHA_vol1_fix <- data.frame("year" = GHA_vol1_0220$year, "avg1_vol" = GHA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== GIN ===================================
#===============================================================================

#Extract
GIN_1ex <- filter(ex, iso3 == "GIN") 

#Re-order 
GIN_1ex_ord <- GIN_1ex[order(GIN_1ex$year), ]

#Make a time series of the ordered series
E_1GIN <- ts(GIN_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1GIN,ylab="",xlab="")

#Plot log returns
e_1GIN = diff(log(E_1GIN))
plot(e_1GIN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1GIN <- ugarchfit(g1e,data = e_1GIN)

#Extract the VOLATILITIES 
vol1_GIN <- ts(garch11e_1GIN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_GIN <- data.frame("date" = v_date, "vol" = vol1_GIN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_GIN02 <- dfvol1_GIN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_GIN20 <- dfvol1_GIN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_GIN0319 <- dfvol1_GIN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GIN_vol1_02avg <- mean(dfvol1_GIN02$vol)
GIN_vol1_20avg <- mean(dfvol1_GIN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_GIN02_df <- data.frame("year" = 2002, "avg1_vol" = GIN_vol1_02avg)
avg1_GIN20_df <- data.frame("year" = 2020, "avg1_vol" = GIN_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_GIN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_GIN <- dfvol1_GIN0319$vol
p=12 #Remove from subsequent codes
new_GIN=NULL
for (i in 1:length){
  ydfvol1_GIN0319 <- e_1series_GIN[((i-1)*p+1):(i*p)]
  yavg1_GIN <- mean(ydfvol1_GIN0319)
  new_GIN = rbind(new_GIN, yavg1_GIN)
}

avg1_GIN <- new_GIN
ts.plot(e_1series_GIN)
ts.plot(avg1_GIN)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_GIN))

#Make the dataframe for period average volatilities
avg1_GIN0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_GIN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GIN_vol1_0220 <- rbind(avg1_GIN02_df, avg1_GIN0319, avg1_GIN20_df)

#Remove the labels in the row number columns
GIN_vol1_fix <- data.frame("year" = GIN_vol1_0220$year, "avg1_vol" = GIN_vol1_0220$avg1_vol)


#===============================================================================
#=============================== GMB ===================================
#===============================================================================

#Extract
GMB_1ex <- filter(ex, iso3 == "GMB") 

#Re-order 
GMB_1ex_ord <- GMB_1ex[order(GMB_1ex$year), ]

#Make a time series of the ordered series
E_1GMB <- ts(GMB_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1GMB,ylab="",xlab="")

#Plot log returns
e_1GMB = diff(log(E_1GMB))
plot(e_1GMB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1GMB <- ugarchfit(g1e,data = e_1GMB)

#Extract the VOLATILITIES 
vol1_GMB <- ts(garch11e_1GMB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_GMB <- data.frame("date" = v_date, "vol" = vol1_GMB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_GMB02 <- dfvol1_GMB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_GMB20 <- dfvol1_GMB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_GMB0319 <- dfvol1_GMB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GMB_vol1_02avg <- mean(dfvol1_GMB02$vol)
GMB_vol1_20avg <- mean(dfvol1_GMB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_GMB02_df <- data.frame("year" = 2002, "avg1_vol" = GMB_vol1_02avg)
avg1_GMB20_df <- data.frame("year" = 2020, "avg1_vol" = GMB_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_GMB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_GMB <- dfvol1_GMB0319$vol
p=12 #Remove from subsequent codes
new_GMB=NULL
for (i in 1:length){
  ydfvol1_GMB0319 <- e_1series_GMB[((i-1)*p+1):(i*p)]
  yavg1_GMB <- mean(ydfvol1_GMB0319)
  new_GMB = rbind(new_GMB, yavg1_GMB)
}

avg1_GMB <- new_GMB
ts.plot(e_1series_GMB)
ts.plot(avg1_GMB)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_GMB))

#Make the dataframe for period average volatilities
avg1_GMB0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_GMB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GMB_vol1_0220 <- rbind(avg1_GMB02_df, avg1_GMB0319, avg1_GMB20_df)

#Remove the labels in the row number columns
GMB_vol1_fix <- data.frame("year" = GMB_vol1_0220$year, "avg1_vol" = GMB_vol1_0220$avg1_vol)


#===============================================================================
#=============================== GNB ===================================
#===============================================================================

#Extract
GNB_1ex <- filter(ex, iso3 == "GNB") 

#Re-order 
GNB_1ex_ord <- GNB_1ex[order(GNB_1ex$year), ]

#Make a time series of the ordered series
E_1GNB <- ts(GNB_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1GNB,ylab="",xlab="")

#Plot log returns
e_1GNB = diff(log(E_1GNB))
plot(e_1GNB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1GNB <- ugarchfit(g1e,data = e_1GNB)

#Extract the VOLATILITIES 
vol1_GNB <- ts(garch11e_1GNB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_GNB <- data.frame("date" = v_date, "vol" = vol1_GNB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_GNB02 <- dfvol1_GNB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_GNB20 <- dfvol1_GNB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_GNB0319 <- dfvol1_GNB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
GNB_vol1_02avg <- mean(dfvol1_GNB02$vol)
GNB_vol1_20avg <- mean(dfvol1_GNB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_GNB02_df <- data.frame("year" = 2002, "avg1_vol" = GNB_vol1_02avg)
avg1_GNB20_df <- data.frame("year" = 2020, "avg1_vol" = GNB_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_GNB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_GNB <- dfvol1_GNB0319$vol
p=12 #Remove from subsequent codes
new_GNB=NULL
for (i in 1:length){
  ydfvol1_GNB0319 <- e_1series_GNB[((i-1)*p+1):(i*p)]
  yavg1_GNB <- mean(ydfvol1_GNB0319)
  new_GNB = rbind(new_GNB, yavg1_GNB)
}

avg1_GNB <- new_GNB
ts.plot(e_1series_GNB)
ts.plot(avg1_GNB)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_GNB))

#Make the dataframe for period average volatilities
avg1_GNB0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_GNB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
GNB_vol1_0220 <- rbind(avg1_GNB02_df, avg1_GNB0319, avg1_GNB20_df)

#Remove the labels in the row number columns
GNB_vol1_fix <- data.frame("year" = GNB_vol1_0220$year, "avg1_vol" = GNB_vol1_0220$avg1_vol)


#===============================================================================
#=============================== KEN ===================================
#===============================================================================

#Extract
KEN_1ex <- filter(ex, iso3 == "KEN") 

#Re-order 
KEN_1ex_ord <- KEN_1ex[order(KEN_1ex$year), ]

#Make a time series of the ordered series
E_1KEN <- ts(KEN_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1KEN,ylab="",xlab="")

#Plot log returns
e_1KEN = diff(log(E_1KEN))
plot(e_1KEN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1KEN <- ugarchfit(g1e,data = e_1KEN)

#Extract the VOLATILITIES 
vol1_KEN <- ts(garch11e_1KEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_KEN <- data.frame("date" = v_date, "vol" = vol1_KEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_KEN02 <- dfvol1_KEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_KEN20 <- dfvol1_KEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_KEN0319 <- dfvol1_KEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
KEN_vol1_02avg <- mean(dfvol1_KEN02$vol)
KEN_vol1_20avg <- mean(dfvol1_KEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_KEN02_df <- data.frame("year" = 2002, "avg1_vol" = KEN_vol1_02avg)
avg1_KEN20_df <- data.frame("year" = 2020, "avg1_vol" = KEN_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_KEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_KEN <- dfvol1_KEN0319$vol
p=12 #Remove from subsequent codes
new_KEN=NULL
for (i in 1:length){
  ydfvol1_KEN0319 <- e_1series_KEN[((i-1)*p+1):(i*p)]
  yavg1_KEN <- mean(ydfvol1_KEN0319)
  new_KEN = rbind(new_KEN, yavg1_KEN)
}

avg1_KEN <- new_KEN
ts.plot(e_1series_KEN)
ts.plot(avg1_KEN)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_KEN))

#Make the dataframe for period average volatilities
avg1_KEN0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_KEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
KEN_vol1_0220 <- rbind(avg1_KEN02_df, avg1_KEN0319, avg1_KEN20_df)

#Remove the labels in the row number columns
KEN_vol1_fix <- data.frame("year" = KEN_vol1_0220$year, "avg1_vol" = KEN_vol1_0220$avg1_vol)


#===============================================================================
#=============================== MDG ===================================
#===============================================================================

#Extract
MDG_1ex <- filter(ex, iso3 == "MDG") 

#Re-order 
MDG_1ex_ord <- MDG_1ex[order(MDG_1ex$year), ]

#Make a time series of the ordered series
E_1MDG <- ts(MDG_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1MDG,ylab="",xlab="")

#Plot log returns
e_1MDG = diff(log(E_1MDG))
plot(e_1MDG,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1MDG <- ugarchfit(g1e,data = e_1MDG)

#Extract the VOLATILITIES 
vol1_MDG <- ts(garch11e_1MDG@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_MDG <- data.frame("date" = v_date, "vol" = vol1_MDG)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_MDG02 <- dfvol1_MDG %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_MDG20 <- dfvol1_MDG %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_MDG0319 <- dfvol1_MDG %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MDG_vol1_02avg <- mean(dfvol1_MDG02$vol)
MDG_vol1_20avg <- mean(dfvol1_MDG20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_MDG02_df <- data.frame("year" = 2002, "avg1_vol" = MDG_vol1_02avg)
avg1_MDG20_df <- data.frame("year" = 2020, "avg1_vol" = MDG_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_MDG0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_MDG <- dfvol1_MDG0319$vol
p=12 #Remove from subsequent codes
new_MDG=NULL
for (i in 1:length){
  ydfvol1_MDG0319 <- e_1series_MDG[((i-1)*p+1):(i*p)]
  yavg1_MDG <- mean(ydfvol1_MDG0319)
  new_MDG = rbind(new_MDG, yavg1_MDG)
}

avg1_MDG <- new_MDG
ts.plot(e_1series_MDG)
ts.plot(avg1_MDG)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_MDG))

#Make the dataframe for period average volatilities
avg1_MDG0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_MDG)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MDG_vol1_0220 <- rbind(avg1_MDG02_df, avg1_MDG0319, avg1_MDG20_df)

#Remove the labels in the row number columns
MDG_vol1_fix <- data.frame("year" = MDG_vol1_0220$year, "avg1_vol" = MDG_vol1_0220$avg1_vol)


#===============================================================================
#=============================== MLI ===================================
#===============================================================================

#Extract
MLI_1ex <- filter(ex, iso3 == "MLI") 

#Re-order 
MLI_1ex_ord <- MLI_1ex[order(MLI_1ex$year), ]

#Make a time series of the ordered series
E_1MLI <- ts(MLI_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1MLI,ylab="",xlab="")

#Plot log returns
e_1MLI = diff(log(E_1MLI))
plot(e_1MLI,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1MLI <- ugarchfit(g1e,data = e_1MLI)

#Extract the VOLATILITIES 
vol1_MLI <- ts(garch11e_1MLI@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_MLI <- data.frame("date" = v_date, "vol" = vol1_MLI)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_MLI02 <- dfvol1_MLI %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_MLI20 <- dfvol1_MLI %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_MLI0319 <- dfvol1_MLI %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MLI_vol1_02avg <- mean(dfvol1_MLI02$vol)
MLI_vol1_20avg <- mean(dfvol1_MLI20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_MLI02_df <- data.frame("year" = 2002, "avg1_vol" = MLI_vol1_02avg)
avg1_MLI20_df <- data.frame("year" = 2020, "avg1_vol" = MLI_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_MLI0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_MLI <- dfvol1_MLI0319$vol
p=12 #Remove from subsequent codes
new_MLI=NULL
for (i in 1:length){
  ydfvol1_MLI0319 <- e_1series_MLI[((i-1)*p+1):(i*p)]
  yavg1_MLI <- mean(ydfvol1_MLI0319)
  new_MLI = rbind(new_MLI, yavg1_MLI)
}

avg1_MLI <- new_MLI
ts.plot(e_1series_MLI)
ts.plot(avg1_MLI)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_MLI))

#Make the dataframe for period average volatilities
avg1_MLI0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_MLI)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MLI_vol1_0220 <- rbind(avg1_MLI02_df, avg1_MLI0319, avg1_MLI20_df)

#Remove the labels in the row number columns
MLI_vol1_fix <- data.frame("year" = MLI_vol1_0220$year, "avg1_vol" = MLI_vol1_0220$avg1_vol)


#===============================================================================
#=============================== MOZ ===================================
#===============================================================================

#Extract
MOZ_1ex <- filter(ex, iso3 == "MOZ") 

#Re-order 
MOZ_1ex_ord <- MOZ_1ex[order(MOZ_1ex$year), ]

#Make a time series of the ordered series
E_1MOZ <- ts(MOZ_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1MOZ,ylab="",xlab="")

#Plot log returns
e_1MOZ = diff(log(E_1MOZ))
plot(e_1MOZ,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1MOZ <- ugarchfit(g1e,data = e_1MOZ)

#Extract the VOLATILITIES 
vol1_MOZ <- ts(garch11e_1MOZ@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_MOZ <- data.frame("date" = v_date, "vol" = vol1_MOZ)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_MOZ02 <- dfvol1_MOZ %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_MOZ20 <- dfvol1_MOZ %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_MOZ0319 <- dfvol1_MOZ %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MOZ_vol1_02avg <- mean(dfvol1_MOZ02$vol)
MOZ_vol1_20avg <- mean(dfvol1_MOZ20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_MOZ02_df <- data.frame("year" = 2002, "avg1_vol" = MOZ_vol1_02avg)
avg1_MOZ20_df <- data.frame("year" = 2020, "avg1_vol" = MOZ_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_MOZ0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_MOZ <- dfvol1_MOZ0319$vol
p=12 #Remove from subsequent codes
new_MOZ=NULL
for (i in 1:length){
  ydfvol1_MOZ0319 <- e_1series_MOZ[((i-1)*p+1):(i*p)]
  yavg1_MOZ <- mean(ydfvol1_MOZ0319)
  new_MOZ = rbind(new_MOZ, yavg1_MOZ)
}

avg1_MOZ <- new_MOZ
ts.plot(e_1series_MOZ)
ts.plot(avg1_MOZ)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_MOZ))

#Make the dataframe for period average volatilities
avg1_MOZ0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_MOZ)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MOZ_vol1_0220 <- rbind(avg1_MOZ02_df, avg1_MOZ0319, avg1_MOZ20_df)

#Remove the labels in the row number columns
MOZ_vol1_fix <- data.frame("year" = MOZ_vol1_0220$year, "avg1_vol" = MOZ_vol1_0220$avg1_vol)


#===============================================================================
#=============================== MRT ===================================
#===============================================================================

#Extract
MRT_1ex <- filter(ex, iso3 == "MRT") 

#Re-order 
MRT_1ex_ord <- MRT_1ex[order(MRT_1ex$year), ]

#Make a time series of the ordered series
E_1MRT <- ts(MRT_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1MRT,ylab="",xlab="")

#Plot log returns
e_1MRT = diff(log(E_1MRT))
plot(e_1MRT,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1MRT <- ugarchfit(g1e,data = e_1MRT)

#Extract the VOLATILITIES 
vol1_MRT <- ts(garch11e_1MRT@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_MRT <- data.frame("date" = v_date, "vol" = vol1_MRT)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_MRT02 <- dfvol1_MRT %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_MRT20 <- dfvol1_MRT %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_MRT0319 <- dfvol1_MRT %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MRT_vol1_02avg <- mean(dfvol1_MRT02$vol)
MRT_vol1_20avg <- mean(dfvol1_MRT20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_MRT02_df <- data.frame("year" = 2002, "avg1_vol" = MRT_vol1_02avg)
avg1_MRT20_df <- data.frame("year" = 2020, "avg1_vol" = MRT_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_MRT0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_MRT <- dfvol1_MRT0319$vol
p=12 #Remove from subsequent codes
new_MRT=NULL
for (i in 1:length){
  ydfvol1_MRT0319 <- e_1series_MRT[((i-1)*p+1):(i*p)]
  yavg1_MRT <- mean(ydfvol1_MRT0319)
  new_MRT = rbind(new_MRT, yavg1_MRT)
}

avg1_MRT <- new_MRT
ts.plot(e_1series_MRT)
ts.plot(avg1_MRT)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_MRT))

#Make the dataframe for period average volatilities
avg1_MRT0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_MRT)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MRT_vol1_0220 <- rbind(avg1_MRT02_df, avg1_MRT0319, avg1_MRT20_df)

#Remove the labels in the row number columns
MRT_vol1_fix <- data.frame("year" = MRT_vol1_0220$year, "avg1_vol" = MRT_vol1_0220$avg1_vol)


#===============================================================================
#=============================== MUS ===================================
#===============================================================================

#Extract
MUS_1ex <- filter(ex, iso3 == "MUS") 

#Re-order 
MUS_1ex_ord <- MUS_1ex[order(MUS_1ex$year), ]

#Make a time series of the ordered series
E_1MUS <- ts(MUS_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1MUS,ylab="",xlab="")

#Plot log returns
e_1MUS = diff(log(E_1MUS))
plot(e_1MUS,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1MUS <- ugarchfit(g1e,data = e_1MUS)

#Extract the VOLATILITIES 
vol1_MUS <- ts(garch11e_1MUS@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_MUS <- data.frame("date" = v_date, "vol" = vol1_MUS)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_MUS02 <- dfvol1_MUS %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_MUS20 <- dfvol1_MUS %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_MUS0319 <- dfvol1_MUS %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
MUS_vol1_02avg <- mean(dfvol1_MUS02$vol)
MUS_vol1_20avg <- mean(dfvol1_MUS20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_MUS02_df <- data.frame("year" = 2002, "avg1_vol" = MUS_vol1_02avg)
avg1_MUS20_df <- data.frame("year" = 2020, "avg1_vol" = MUS_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_MUS0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_MUS <- dfvol1_MUS0319$vol
p=12 #Remove from subsequent codes
new_MUS=NULL
for (i in 1:length){
  ydfvol1_MUS0319 <- e_1series_MUS[((i-1)*p+1):(i*p)]
  yavg1_MUS <- mean(ydfvol1_MUS0319)
  new_MUS = rbind(new_MUS, yavg1_MUS)
}

avg1_MUS <- new_MUS
ts.plot(e_1series_MUS)
ts.plot(avg1_MUS)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_MUS))

#Make the dataframe for period average volatilities
avg1_MUS0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_MUS)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
MUS_vol1_0220 <- rbind(avg1_MUS02_df, avg1_MUS0319, avg1_MUS20_df)

#Remove the labels in the row number columns
MUS_vol1_fix <- data.frame("year" = MUS_vol1_0220$year, "avg1_vol" = MUS_vol1_0220$avg1_vol)


#===============================================================================
#=============================== NAM ===================================
#===============================================================================

#Extract
NAM_1ex <- filter(ex, iso3 == "NAM") 

#Re-order 
NAM_1ex_ord <- NAM_1ex[order(NAM_1ex$year), ]

#Make a time series of the ordered series
E_1NAM <- ts(NAM_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1NAM,ylab="",xlab="")

#Plot log returns
e_1NAM = diff(log(E_1NAM))
plot(e_1NAM,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1NAM <- ugarchfit(g1e,data = e_1NAM)

#Extract the VOLATILITIES 
vol1_NAM <- ts(garch11e_1NAM@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_NAM <- data.frame("date" = v_date, "vol" = vol1_NAM)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_NAM02 <- dfvol1_NAM %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_NAM20 <- dfvol1_NAM %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_NAM0319 <- dfvol1_NAM %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NAM_vol1_02avg <- mean(dfvol1_NAM02$vol)
NAM_vol1_20avg <- mean(dfvol1_NAM20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_NAM02_df <- data.frame("year" = 2002, "avg1_vol" = NAM_vol1_02avg)
avg1_NAM20_df <- data.frame("year" = 2020, "avg1_vol" = NAM_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_NAM0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_NAM <- dfvol1_NAM0319$vol
p=12 #Remove from subsequent codes
new_NAM=NULL
for (i in 1:length){
  ydfvol1_NAM0319 <- e_1series_NAM[((i-1)*p+1):(i*p)]
  yavg1_NAM <- mean(ydfvol1_NAM0319)
  new_NAM = rbind(new_NAM, yavg1_NAM)
}

avg1_NAM <- new_NAM
ts.plot(e_1series_NAM)
ts.plot(avg1_NAM)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_NAM))

#Make the dataframe for period average volatilities
avg1_NAM0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_NAM)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NAM_vol1_0220 <- rbind(avg1_NAM02_df, avg1_NAM0319, avg1_NAM20_df)

#Remove the labels in the row number columns
NAM_vol1_fix <- data.frame("year" = NAM_vol1_0220$year, "avg1_vol" = NAM_vol1_0220$avg1_vol)


#===============================================================================
#=============================== NER ===================================
#===============================================================================

#Extract
NER_1ex <- filter(ex, iso3 == "NER") 

#Re-order 
NER_1ex_ord <- NER_1ex[order(NER_1ex$year), ]

#Make a time series of the ordered series
E_1NER <- ts(NER_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1NER,ylab="",xlab="")

#Plot log returns
e_1NER = diff(log(E_1NER))
plot(e_1NER,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1NER <- ugarchfit(g1e,data = e_1NER)

#Extract the VOLATILITIES 
vol1_NER <- ts(garch11e_1NER@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_NER <- data.frame("date" = v_date, "vol" = vol1_NER)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_NER02 <- dfvol1_NER %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_NER20 <- dfvol1_NER %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_NER0319 <- dfvol1_NER %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NER_vol1_02avg <- mean(dfvol1_NER02$vol)
NER_vol1_20avg <- mean(dfvol1_NER20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_NER02_df <- data.frame("year" = 2002, "avg1_vol" = NER_vol1_02avg)
avg1_NER20_df <- data.frame("year" = 2020, "avg1_vol" = NER_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_NER0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_NER <- dfvol1_NER0319$vol
p=12 #Remove from subsequent codes
new_NER=NULL
for (i in 1:length){
  ydfvol1_NER0319 <- e_1series_NER[((i-1)*p+1):(i*p)]
  yavg1_NER <- mean(ydfvol1_NER0319)
  new_NER = rbind(new_NER, yavg1_NER)
}

avg1_NER <- new_NER
ts.plot(e_1series_NER)
ts.plot(avg1_NER)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_NER))

#Make the dataframe for period average volatilities
avg1_NER0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_NER)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NER_vol1_0220 <- rbind(avg1_NER02_df, avg1_NER0319, avg1_NER20_df)

#Remove the labels in the row number columns
NER_vol1_fix <- data.frame("year" = NER_vol1_0220$year, "avg1_vol" = NER_vol1_0220$avg1_vol)


#===============================================================================
#=============================== NGA ===================================
#===============================================================================

#Extract
NGA_1ex <- filter(ex, iso3 == "NGA") 

#Re-order 
NGA_1ex_ord <- NGA_1ex[order(NGA_1ex$year), ]

#Make a time series of the ordered series
E_1NGA <- ts(NGA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1NGA,ylab="",xlab="")

#Plot log returns
e_1NGA = diff(log(E_1NGA))
plot(e_1NGA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1NGA <- ugarchfit(g1e,data = e_1NGA)

#Extract the VOLATILITIES 
vol1_NGA <- ts(garch11e_1NGA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_NGA <- data.frame("date" = v_date, "vol" = vol1_NGA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_NGA02 <- dfvol1_NGA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_NGA20 <- dfvol1_NGA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_NGA0319 <- dfvol1_NGA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
NGA_vol1_02avg <- mean(dfvol1_NGA02$vol)
NGA_vol1_20avg <- mean(dfvol1_NGA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_NGA02_df <- data.frame("year" = 2002, "avg1_vol" = NGA_vol1_02avg)
avg1_NGA20_df <- data.frame("year" = 2020, "avg1_vol" = NGA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_NGA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_NGA <- dfvol1_NGA0319$vol
p=12 #Remove from subsequent codes
new_NGA=NULL
for (i in 1:length){
  ydfvol1_NGA0319 <- e_1series_NGA[((i-1)*p+1):(i*p)]
  yavg1_NGA <- mean(ydfvol1_NGA0319)
  new_NGA = rbind(new_NGA, yavg1_NGA)
}

avg1_NGA <- new_NGA
ts.plot(e_1series_NGA)
ts.plot(avg1_NGA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_NGA))

#Make the dataframe for period average volatilities
avg1_NGA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_NGA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
NGA_vol1_0220 <- rbind(avg1_NGA02_df, avg1_NGA0319, avg1_NGA20_df)

#Remove the labels in the row number columns
NGA_vol1_fix <- data.frame("year" = NGA_vol1_0220$year, "avg1_vol" = NGA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== RWA ===================================
#===============================================================================

#Extract
RWA_1ex <- filter(ex, iso3 == "RWA") 

#Re-order 
RWA_1ex_ord <- RWA_1ex[order(RWA_1ex$year), ]

#Make a time series of the ordered series
E_1RWA <- ts(RWA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1RWA,ylab="",xlab="")

#Plot log returns
e_1RWA = diff(log(E_1RWA))
plot(e_1RWA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1RWA <- ugarchfit(g1e,data = e_1RWA)

#Extract the VOLATILITIES 
vol1_RWA <- ts(garch11e_1RWA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_RWA <- data.frame("date" = v_date, "vol" = vol1_RWA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_RWA02 <- dfvol1_RWA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_RWA20 <- dfvol1_RWA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_RWA0319 <- dfvol1_RWA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
RWA_vol1_02avg <- mean(dfvol1_RWA02$vol)
RWA_vol1_20avg <- mean(dfvol1_RWA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_RWA02_df <- data.frame("year" = 2002, "avg1_vol" = RWA_vol1_02avg)
avg1_RWA20_df <- data.frame("year" = 2020, "avg1_vol" = RWA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_RWA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_RWA <- dfvol1_RWA0319$vol
p=12 #Remove from subsequent codes
new_RWA=NULL
for (i in 1:length){
  ydfvol1_RWA0319 <- e_1series_RWA[((i-1)*p+1):(i*p)]
  yavg1_RWA <- mean(ydfvol1_RWA0319)
  new_RWA = rbind(new_RWA, yavg1_RWA)
}

avg1_RWA <- new_RWA
ts.plot(e_1series_RWA)
ts.plot(avg1_RWA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_RWA))

#Make the dataframe for period average volatilities
avg1_RWA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_RWA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
RWA_vol1_0220 <- rbind(avg1_RWA02_df, avg1_RWA0319, avg1_RWA20_df)

#Remove the labels in the row number columns
RWA_vol1_fix <- data.frame("year" = RWA_vol1_0220$year, "avg1_vol" = RWA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== SDN ===================================
#===============================================================================

#Extract
SDN_1ex <- filter(ex, iso3 == "SDN") 

#Re-order 
SDN_1ex_ord <- SDN_1ex[order(SDN_1ex$year), ]

#Make a time series of the ordered series
E_1SDN <- ts(SDN_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1SDN,ylab="",xlab="")

#Plot log returns
e_1SDN = diff(log(E_1SDN))
plot(e_1SDN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1SDN <- ugarchfit(g1e,data = e_1SDN)

#Extract the VOLATILITIES 
vol1_SDN <- ts(garch11e_1SDN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_SDN <- data.frame("date" = v_date, "vol" = vol1_SDN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_SDN02 <- dfvol1_SDN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_SDN20 <- dfvol1_SDN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_SDN0319 <- dfvol1_SDN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SDN_vol1_02avg <- mean(dfvol1_SDN02$vol)
SDN_vol1_20avg <- mean(dfvol1_SDN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_SDN02_df <- data.frame("year" = 2002, "avg1_vol" = SDN_vol1_02avg)
avg1_SDN20_df <- data.frame("year" = 2020, "avg1_vol" = SDN_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_SDN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_SDN <- dfvol1_SDN0319$vol
p=12 #Remove from subsequent codes
new_SDN=NULL
for (i in 1:length){
  ydfvol1_SDN0319 <- e_1series_SDN[((i-1)*p+1):(i*p)]
  yavg1_SDN <- mean(ydfvol1_SDN0319)
  new_SDN = rbind(new_SDN, yavg1_SDN)
}

avg1_SDN <- new_SDN
ts.plot(e_1series_SDN)
ts.plot(avg1_SDN)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_SDN))

#Make the dataframe for period average volatilities
avg1_SDN0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_SDN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SDN_vol1_0220 <- rbind(avg1_SDN02_df, avg1_SDN0319, avg1_SDN20_df)

#Remove the labels in the row number columns
SDN_vol1_fix <- data.frame("year" = SDN_vol1_0220$year, "avg1_vol" = SDN_vol1_0220$avg1_vol)


#===============================================================================
#=============================== SEN ===================================
#===============================================================================

#Extract
SEN_1ex <- filter(ex, iso3 == "SEN") 

#Re-order 
SEN_1ex_ord <- SEN_1ex[order(SEN_1ex$year), ]

#Make a time series of the ordered series
E_1SEN <- ts(SEN_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1SEN,ylab="",xlab="")

#Plot log returns
e_1SEN = diff(log(E_1SEN))
plot(e_1SEN,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1SEN <- ugarchfit(g1e,data = e_1SEN)

#Extract the VOLATILITIES 
vol1_SEN <- ts(garch11e_1SEN@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_SEN <- data.frame("date" = v_date, "vol" = vol1_SEN)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_SEN02 <- dfvol1_SEN %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_SEN20 <- dfvol1_SEN %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_SEN0319 <- dfvol1_SEN %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SEN_vol1_02avg <- mean(dfvol1_SEN02$vol)
SEN_vol1_20avg <- mean(dfvol1_SEN20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_SEN02_df <- data.frame("year" = 2002, "avg1_vol" = SEN_vol1_02avg)
avg1_SEN20_df <- data.frame("year" = 2020, "avg1_vol" = SEN_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_SEN0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_SEN <- dfvol1_SEN0319$vol
p=12 #Remove from subsequent codes
new_SEN=NULL
for (i in 1:length){
  ydfvol1_SEN0319 <- e_1series_SEN[((i-1)*p+1):(i*p)]
  yavg1_SEN <- mean(ydfvol1_SEN0319)
  new_SEN = rbind(new_SEN, yavg1_SEN)
}

avg1_SEN <- new_SEN
ts.plot(e_1series_SEN)
ts.plot(avg1_SEN)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_SEN))

#Make the dataframe for period average volatilities
avg1_SEN0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_SEN)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SEN_vol1_0220 <- rbind(avg1_SEN02_df, avg1_SEN0319, avg1_SEN20_df)

#Remove the labels in the row number columns
SEN_vol1_fix <- data.frame("year" = SEN_vol1_0220$year, "avg1_vol" = SEN_vol1_0220$avg1_vol)


#===============================================================================
#=============================== SLE ===================================
#===============================================================================

#Extract
SLE_11ex <- filter(ex, iso3 == "SLE") 

#Re-order 
SLE_11ex_ord <- SLE_11ex[order(SLE_11ex$year), ]

#Make a time series of the ordered series
E_1SLE <- ts(SLE_11ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1SLE,ylab="",xlab="")

#Plot log returns
e_1SLE = diff(log(E_1SLE))
plot(e_1SLE,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1SLE <- ugarchfit(g1e,data = e_1SLE)

#Extract the VOLATILITIES 
vol1_SLE <- ts(garch11e_1SLE@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_SLE <- data.frame("date" = v_date, "vol" = vol1_SLE)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_SLE02 <- dfvol1_SLE %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_SLE20 <- dfvol1_SLE %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_SLE0319 <- dfvol1_SLE %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SLE_1vol1_02avg <- mean(dfvol1_SLE02$vol)
SLE_1vol1_20avg <- mean(dfvol1_SLE20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_SLE02_df <- data.frame("year" = 2002, "avg1_vol" = SLE_1vol1_02avg)
avg1_SLE20_df <- data.frame("year" = 2020, "avg1_vol" = SLE_1vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_SLE0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_SLE <- dfvol1_SLE0319$vol
p=12 #Remove from subsequent codes
new_SLE=NULL
for (i in 1:length){
  ydfvol1_SLE0319 <- e_1series_SLE[((i-1)*p+1):(i*p)]
  yavg1_SLE <- mean(ydfvol1_SLE0319)
  new_SLE = rbind(new_SLE, yavg1_SLE)
}

avg1_SLE <- new_SLE
ts.plot(e_1series_SLE)
ts.plot(avg1_SLE)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_SLE))

#Make the dataframe for period average volatilities
avg1_SLE0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_SLE)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SLE_1vol1_0220 <- rbind(avg1_SLE02_df, avg1_SLE0319, avg1_SLE20_df)

#Remove the labels in the row number columns
SLE_1vol1_fix <- data.frame("year" = SLE_1vol1_0220$year, "avg1_vol" = SLE_1vol1_0220$avg1_vol)


#===============================================================================
#=============================== SWZ ===================================
#===============================================================================

#Extract
SWZ_1ex <- filter(ex, iso3 == "SWZ") 

#Re-order 
SWZ_1ex_ord <- SWZ_1ex[order(SWZ_1ex$year), ]

#Make a time series of the ordered series
E_1SWZ <- ts(SWZ_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1SWZ,ylab="",xlab="")

#Plot log returns
e_1SWZ = diff(log(E_1SWZ))
plot(e_1SWZ,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1SWZ <- ugarchfit(g1e,data = e_1SWZ)

#Extract the VOLATILITIES 
vol1_SWZ <- ts(garch11e_1SWZ@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_SWZ <- data.frame("date" = v_date, "vol" = vol1_SWZ)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_SWZ02 <- dfvol1_SWZ %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_SWZ20 <- dfvol1_SWZ %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_SWZ0319 <- dfvol1_SWZ %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SWZ_vol1_02avg <- mean(dfvol1_SWZ02$vol)
SWZ_vol1_20avg <- mean(dfvol1_SWZ20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_SWZ02_df <- data.frame("year" = 2002, "avg1_vol" = SWZ_vol1_02avg)
avg1_SWZ20_df <- data.frame("year" = 2020, "avg1_vol" = SWZ_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_SWZ0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_SWZ <- dfvol1_SWZ0319$vol
p=12 #Remove from subsequent codes
new_SWZ=NULL
for (i in 1:length){
  ydfvol1_SWZ0319 <- e_1series_SWZ[((i-1)*p+1):(i*p)]
  yavg1_SWZ <- mean(ydfvol1_SWZ0319)
  new_SWZ = rbind(new_SWZ, yavg1_SWZ)
}

avg1_SWZ <- new_SWZ
ts.plot(e_1series_SWZ)
ts.plot(avg1_SWZ)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_SWZ))

#Make the dataframe for period average volatilities
avg1_SWZ0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_SWZ)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SWZ_vol1_0220 <- rbind(avg1_SWZ02_df, avg1_SWZ0319, avg1_SWZ20_df)

#Remove the labels in the row number columns
SWZ_vol1_fix <- data.frame("year" = SWZ_vol1_0220$year, "avg1_vol" = SWZ_vol1_0220$avg1_vol)


#===============================================================================
#=============================== SYC ===================================
#===============================================================================

#Extract
SYC_1ex <- filter(ex, iso3 == "SYC") 

#Re-order 
SYC_1ex_ord <- SYC_1ex[order(SYC_1ex$year), ]

#Make a time series of the ordered series
E_1SYC <- ts(SYC_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1SYC,ylab="",xlab="")

#Plot log returns
e_1SYC = diff(log(E_1SYC))
plot(e_1SYC,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1SYC <- ugarchfit(g1e,data = e_1SYC)

#Extract the VOLATILITIES 
vol1_SYC <- ts(garch11e_1SYC@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_SYC <- data.frame("date" = v_date, "vol" = vol1_SYC)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_SYC02 <- dfvol1_SYC %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_SYC20 <- dfvol1_SYC %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_SYC0319 <- dfvol1_SYC %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
SYC_vol1_02avg <- mean(dfvol1_SYC02$vol)
SYC_vol1_20avg <- mean(dfvol1_SYC20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_SYC02_df <- data.frame("year" = 2002, "avg1_vol" = SYC_vol1_02avg)
avg1_SYC20_df <- data.frame("year" = 2020, "avg1_vol" = SYC_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_SYC0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_SYC <- dfvol1_SYC0319$vol
p=12 #Remove from subsequent codes
new_SYC=NULL
for (i in 1:length){
  ydfvol1_SYC0319 <- e_1series_SYC[((i-1)*p+1):(i*p)]
  yavg1_SYC <- mean(ydfvol1_SYC0319)
  new_SYC = rbind(new_SYC, yavg1_SYC)
}

avg1_SYC <- new_SYC
ts.plot(e_1series_SYC)
ts.plot(avg1_SYC)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_SYC))

#Make the dataframe for period average volatilities
avg1_SYC0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_SYC)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
SYC_vol1_0220 <- rbind(avg1_SYC02_df, avg1_SYC0319, avg1_SYC20_df)

#Remove the labels in the row number columns
SYC_vol1_fix <- data.frame("year" = SYC_vol1_0220$year, "avg1_vol" = SYC_vol1_0220$avg1_vol)


#===============================================================================
#=============================== TCD ===================================
#===============================================================================

#Extract
TCD_1ex <- filter(ex, iso3 == "TCD") 

#Re-order 
TCD_1ex_ord <- TCD_1ex[order(TCD_1ex$year), ]

#Make a time series of the ordered series
E_1TCD <- ts(TCD_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1TCD,ylab="",xlab="")

#Plot log returns
e_1TCD = diff(log(E_1TCD))
plot(e_1TCD,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1TCD <- ugarchfit(g1e,data = e_1TCD)

#Extract the VOLATILITIES 
vol1_TCD <- ts(garch11e_1TCD@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_TCD <- data.frame("date" = v_date, "vol" = vol1_TCD)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_TCD02 <- dfvol1_TCD %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_TCD20 <- dfvol1_TCD %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_TCD0319 <- dfvol1_TCD %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TCD_vol1_02avg <- mean(dfvol1_TCD02$vol)
TCD_vol1_20avg <- mean(dfvol1_TCD20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_TCD02_df <- data.frame("year" = 2002, "avg1_vol" = TCD_vol1_02avg)
avg1_TCD20_df <- data.frame("year" = 2020, "avg1_vol" = TCD_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_TCD0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_TCD <- dfvol1_TCD0319$vol
p=12 #Remove from subsequent codes
new_TCD=NULL
for (i in 1:length){
  ydfvol1_TCD0319 <- e_1series_TCD[((i-1)*p+1):(i*p)]
  yavg1_TCD <- mean(ydfvol1_TCD0319)
  new_TCD = rbind(new_TCD, yavg1_TCD)
}

avg1_TCD <- new_TCD
ts.plot(e_1series_TCD)
ts.plot(avg1_TCD)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_TCD))

#Make the dataframe for period average volatilities
avg1_TCD0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_TCD)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TCD_vol1_0220 <- rbind(avg1_TCD02_df, avg1_TCD0319, avg1_TCD20_df)

#Remove the labels in the row number columns
TCD_vol1_fix <- data.frame("year" = TCD_vol1_0220$year, "avg1_vol" = TCD_vol1_0220$avg1_vol)


#===============================================================================
#=============================== TGO ===================================
#===============================================================================

#Extract
TGO_1ex <- filter(ex, iso3 == "TGO") 

#Re-order 
TGO_1ex_ord <- TGO_1ex[order(TGO_1ex$year), ]

#Make a time series of the ordered series
E_1TGO <- ts(TGO_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1TGO,ylab="",xlab="")

#Plot log returns
e_1TGO = diff(log(E_1TGO))
plot(e_1TGO,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1TGO <- ugarchfit(g1e,data = e_1TGO)

#Extract the VOLATILITIES 
vol1_TGO <- ts(garch11e_1TGO@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_TGO <- data.frame("date" = v_date, "vol" = vol1_TGO)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_TGO02 <- dfvol1_TGO %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_TGO20 <- dfvol1_TGO %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_TGO0319 <- dfvol1_TGO %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TGO_vol1_02avg <- mean(dfvol1_TGO02$vol)
TGO_vol1_20avg <- mean(dfvol1_TGO20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_TGO02_df <- data.frame("year" = 2002, "avg1_vol" = TGO_vol1_02avg)
avg1_TGO20_df <- data.frame("year" = 2020, "avg1_vol" = TGO_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_TGO0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_TGO <- dfvol1_TGO0319$vol
p=12 #Remove from subsequent codes
new_TGO=NULL
for (i in 1:length){
  ydfvol1_TGO0319 <- e_1series_TGO[((i-1)*p+1):(i*p)]
  yavg1_TGO <- mean(ydfvol1_TGO0319)
  new_TGO = rbind(new_TGO, yavg1_TGO)
}

avg1_TGO <- new_TGO
ts.plot(e_1series_TGO)
ts.plot(avg1_TGO)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_TGO))

#Make the dataframe for period average volatilities
avg1_TGO0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_TGO)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TGO_vol1_0220 <- rbind(avg1_TGO02_df, avg1_TGO0319, avg1_TGO20_df)

#Remove the labels in the row number columns
TGO_vol1_fix <- data.frame("year" = TGO_vol1_0220$year, "avg1_vol" = TGO_vol1_0220$avg1_vol)


#===============================================================================
#=============================== TZA ===================================
#===============================================================================

#Extract
TZA_1ex <- filter(ex, iso3 == "TZA") 

#Re-order 
TZA_1ex_ord <- TZA_1ex[order(TZA_1ex$year), ]

#Make a time series of the ordered series
E_1TZA <- ts(TZA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1TZA,ylab="",xlab="")

#Plot log returns
e_1TZA = diff(log(E_1TZA))
plot(e_1TZA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1TZA <- ugarchfit(g1e,data = e_1TZA)

#Extract the VOLATILITIES 
vol1_TZA <- ts(garch11e_1TZA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_TZA <- data.frame("date" = v_date, "vol" = vol1_TZA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_TZA02 <- dfvol1_TZA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_TZA20 <- dfvol1_TZA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_TZA0319 <- dfvol1_TZA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
TZA_vol1_02avg <- mean(dfvol1_TZA02$vol)
TZA_vol1_20avg <- mean(dfvol1_TZA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_TZA02_df <- data.frame("year" = 2002, "avg1_vol" = TZA_vol1_02avg)
avg1_TZA20_df <- data.frame("year" = 2020, "avg1_vol" = TZA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_TZA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_TZA <- dfvol1_TZA0319$vol
p=12 #Remove from subsequent codes
new_TZA=NULL
for (i in 1:length){
  ydfvol1_TZA0319 <- e_1series_TZA[((i-1)*p+1):(i*p)]
  yavg1_TZA <- mean(ydfvol1_TZA0319)
  new_TZA = rbind(new_TZA, yavg1_TZA)
}

avg1_TZA <- new_TZA
ts.plot(e_1series_TZA)
ts.plot(avg1_TZA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_TZA))

#Make the dataframe for period average volatilities
avg1_TZA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_TZA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
TZA_vol1_0220 <- rbind(avg1_TZA02_df, avg1_TZA0319, avg1_TZA20_df)

#Remove the labels in the row number columns
TZA_vol1_fix <- data.frame("year" = TZA_vol1_0220$year, "avg1_vol" = TZA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== UGA ===================================
#===============================================================================

#Extract
UGA_1ex <- filter(ex, iso3 == "UGA") 

#Re-order 
UGA_1ex_ord <- UGA_1ex[order(UGA_1ex$year), ]

#Make a time series of the ordered series
E_1UGA <- ts(UGA_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1UGA,ylab="",xlab="")

#Plot log returns
e_1UGA = diff(log(E_1UGA))
plot(e_1UGA,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1UGA <- ugarchfit(g1e,data = e_1UGA)

#Extract the VOLATILITIES 
vol1_UGA <- ts(garch11e_1UGA@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_UGA <- data.frame("date" = v_date, "vol" = vol1_UGA)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_UGA02 <- dfvol1_UGA %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_UGA20 <- dfvol1_UGA %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_UGA0319 <- dfvol1_UGA %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
UGA_vol1_02avg <- mean(dfvol1_UGA02$vol)
UGA_vol1_20avg <- mean(dfvol1_UGA20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_UGA02_df <- data.frame("year" = 2002, "avg1_vol" = UGA_vol1_02avg)
avg1_UGA20_df <- data.frame("year" = 2020, "avg1_vol" = UGA_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_UGA0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_UGA <- dfvol1_UGA0319$vol
p=12 #Remove from subsequent codes
new_UGA=NULL
for (i in 1:length){
  ydfvol1_UGA0319 <- e_1series_UGA[((i-1)*p+1):(i*p)]
  yavg1_UGA <- mean(ydfvol1_UGA0319)
  new_UGA = rbind(new_UGA, yavg1_UGA)
}

avg1_UGA <- new_UGA
ts.plot(e_1series_UGA)
ts.plot(avg1_UGA)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_UGA))

#Make the dataframe for period average volatilities
avg1_UGA0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_UGA)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
UGA_vol1_0220 <- rbind(avg1_UGA02_df, avg1_UGA0319, avg1_UGA20_df)

#Remove the labels in the row number columns
UGA_vol1_fix <- data.frame("year" = UGA_vol1_0220$year, "avg1_vol" = UGA_vol1_0220$avg1_vol)


#===============================================================================
#=============================== ZAF ===================================
#===============================================================================

#Extract
ZAF_1ex <- filter(ex, iso3 == "ZAF") 

#Re-order 
ZAF_1ex_ord <- ZAF_1ex[order(ZAF_1ex$year), ]

#Make a time series of the ordered series
E_1ZAF <- ts(ZAF_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1ZAF,ylab="",xlab="")

#Plot log returns
e_1ZAF = diff(log(E_1ZAF))
plot(e_1ZAF,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1ZAF <- ugarchfit(g1e,data = e_1ZAF)

#Extract the VOLATILITIES 
vol1_ZAF <- ts(garch11e_1ZAF@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_ZAF <- data.frame("date" = v_date, "vol" = vol1_ZAF)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_ZAF02 <- dfvol1_ZAF %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_ZAF20 <- dfvol1_ZAF %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_ZAF0319 <- dfvol1_ZAF %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZAF_vol1_02avg <- mean(dfvol1_ZAF02$vol)
ZAF_vol1_20avg <- mean(dfvol1_ZAF20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_ZAF02_df <- data.frame("year" = 2002, "avg1_vol" = ZAF_vol1_02avg)
avg1_ZAF20_df <- data.frame("year" = 2020, "avg1_vol" = ZAF_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_ZAF0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_ZAF <- dfvol1_ZAF0319$vol
p=12 #Remove from subsequent codes
new_ZAF=NULL
for (i in 1:length){
  ydfvol1_ZAF0319 <- e_1series_ZAF[((i-1)*p+1):(i*p)]
  yavg1_ZAF <- mean(ydfvol1_ZAF0319)
  new_ZAF = rbind(new_ZAF, yavg1_ZAF)
}

avg1_ZAF <- new_ZAF
ts.plot(e_1series_ZAF)
ts.plot(avg1_ZAF)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_ZAF))

#Make the dataframe for period average volatilities
avg1_ZAF0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_ZAF)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZAF_vol1_0220 <- rbind(avg1_ZAF02_df, avg1_ZAF0319, avg1_ZAF20_df)

#Remove the labels in the row number columns
ZAF_vol1_fix <- data.frame("year" = ZAF_vol1_0220$year, "avg1_vol" = ZAF_vol1_0220$avg1_vol)


#===============================================================================
#=============================== ZMB ===================================
#===============================================================================

#Extract
ZMB_1ex <- filter(ex, iso3 == "ZMB") 

#Re-order 
ZMB_1ex_ord <- ZMB_1ex[order(ZMB_1ex$year), ]

#Make a time series of the ordered series
E_1ZMB <- ts(ZMB_1ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1ZMB,ylab="",xlab="")

#Plot log returns
e_1ZMB = diff(log(E_1ZMB))
plot(e_1ZMB,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1ZMB <- ugarchfit(g1e,data = e_1ZMB)

#Extract the VOLATILITIES 
vol1_ZMB <- ts(garch11e_1ZMB@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_ZMB <- data.frame("date" = v_date, "vol" = vol1_ZMB)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_ZMB02 <- dfvol1_ZMB %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_ZMB20 <- dfvol1_ZMB %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_ZMB0319 <- dfvol1_ZMB %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZMB_vol1_02avg <- mean(dfvol1_ZMB02$vol)
ZMB_vol1_20avg <- mean(dfvol1_ZMB20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_ZMB02_df <- data.frame("year" = 2002, "avg1_vol" = ZMB_vol1_02avg)
avg1_ZMB20_df <- data.frame("year" = 2020, "avg1_vol" = ZMB_vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_ZMB0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_ZMB <- dfvol1_ZMB0319$vol
p=12 #Remove from subsequent codes
new_ZMB=NULL
for (i in 1:length){
  ydfvol1_ZMB0319 <- e_1series_ZMB[((i-1)*p+1):(i*p)]
  yavg1_ZMB <- mean(ydfvol1_ZMB0319)
  new_ZMB = rbind(new_ZMB, yavg1_ZMB)
}

avg1_ZMB <- new_ZMB
ts.plot(e_1series_ZMB)
ts.plot(avg1_ZMB)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_ZMB))

#Make the dataframe for period average volatilities
avg1_ZMB0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_ZMB)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZMB_vol1_0220 <- rbind(avg1_ZMB02_df, avg1_ZMB0319, avg1_ZMB20_df)

#Remove the labels in the row number columns
ZMB_vol1_fix <- data.frame("year" = ZMB_vol1_0220$year, "avg1_vol" = ZMB_vol1_0220$avg1_vol)


#===============================================================================
#=============================== ZWE ===================================
#===============================================================================

#Extract
ZWE_11ex <- filter(ex, iso3 == "ZWE") 

#Re-order 
ZWE_11ex_ord <- ZWE_11ex[order(ZWE_11ex$year), ]

#Make a time series of the ordered series
E_1ZWE <- ts(ZWE_11ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

#Plot raw series
plot(E_1ZWE,ylab="",xlab="")

#Plot log returns
e_1ZWE = diff(log(E_1ZWE))
plot(e_1ZWE,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

#Modelling the volatility. 

#Not the Conditional Mean Equation adopted follows an AR1 process with intercept
#The full model is specified in my RMD document

garch11e_1ZWE <- ugarchfit(g1e,data = e_1ZWE)

#Extract the VOLATILITIES 
vol1_ZWE <- ts(garch11e_1ZWE@fit$sigma^2,end=c(2020,07),frequency = 12)

#Create Volatility dataframe
dfvol1_ZWE <- data.frame("date" = v_date, "vol" = vol1_ZWE)

#SUBSETTING 

#Get 2002 monthly volatilities
dfvol1_ZWE02 <- dfvol1_ZWE %>% filter(date < '2003-01-01')

#Get 2020 monthly volatilties 
dfvol1_ZWE20 <- dfvol1_ZWE %>% filter(date > '2019-12-01')

#Get monthly volatilities from 2003 t0 2019
dfvol1_ZWE0319 <- dfvol1_ZWE %>% filter(date >= '2003-01-01' & date <= '2019-12-01')

#Averages for 2002 and 2020
ZWE_1vol1_02avg <- mean(dfvol1_ZWE02$vol)
ZWE_1vol1_20avg <- mean(dfvol1_ZWE20$vol)

#Create a df to store the averages for 2002 and 2020.
avg1_ZWE02_df <- data.frame("year" = 2002, "avg1_vol" = ZWE_1vol1_02avg)
avg1_ZWE20_df <- data.frame("year" = 2020, "avg1_vol" = ZWE_1vol1_20avg)

#Next I define objects to help me compute annual averages for 2003 to 2019
l1 <- nrow(dfvol1_ZWE0319)
l <- l1/12 
length <- l1%/%12

#write the loop to calculate the average for each year
e_1series_ZWE <- dfvol1_ZWE0319$vol
p=12 #Remove from subsequent codes
new_ZWE=NULL
for (i in 1:length){
  ydfvol1_ZWE0319 <- e_1series_ZWE[((i-1)*p+1):(i*p)]
  yavg1_ZWE <- mean(ydfvol1_ZWE0319)
  new_ZWE = rbind(new_ZWE, yavg1_ZWE)
}

avg1_ZWE <- new_ZWE
ts.plot(e_1series_ZWE)
ts.plot(avg1_ZWE)

#make the year_s1 manually
year_s1 <- rep(2002+1:length(avg1_ZWE))

#Make the dataframe for period average volatilities
avg1_ZWE0319 <- data.frame("year" = year_s1, "avg1_vol" = avg1_ZWE)

#Add 2002, 2003:2019, and 2020 averages to the dataframe
#Not the order of the arguments in the parenthesis in the 
#code below matters
ZWE_1vol1_0220 <- rbind(avg1_ZWE02_df, avg1_ZWE0319, avg1_ZWE20_df)

#Remove the labels in the row number columns
ZWE_1vol1_fix <- data.frame("year" = ZWE_1vol1_0220$year, "avg1_vol" = ZWE_1vol1_0220$avg1_vol)

sGARCH11e_1avg <- rbind(AGO_vol1_fix, BDI_vol1_fix, BEN_vol1_fix, BWA_vol1_fix, CAF_vol1_fix,
                      CIV_vol1_fix, CMR_vol1_fix, COG_vol1_fix, CPV_vol1_fix, GAB_vol1_fix, GHA_vol1_fix,
                      GIN_vol1_fix, GMB_vol1_fix, GNB_vol1_fix, KEN_vol1_fix, MDG_vol1_fix, MLI_vol1_fix,
                      
                      MOZ_vol1_fix, MRT_vol1_fix, MUS_vol1_fix, NAM_vol1_fix,
                      NER_vol1_fix, NGA_vol1_fix, RWA_vol1_fix, SDN_vol1_fix, SEN_vol1_fix, SLE_1vol1_fix,
                      SWZ_vol1_fix, SYC_vol1_fix, TCD_vol1_fix,
                      TGO_vol1_fix, TZA_vol1_fix, UGA_vol1_fix, ZAF_vol1_fix, ZMB_vol1_fix, ZWE_1vol1_fix)

