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

