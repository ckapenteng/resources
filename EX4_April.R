
#GARCH PRACTICALS
save.image(file = ".RData")
#Required Packages
library(zoo)
library(AER)
library(plm)
library(stargazer)
library(parallel)
library(rugarch)
library(zoo)
library(base)
library(forecast)
library(gridExtra)
library(ggplot2)
library(stats)
library(gridExtra)
library(FinTS) #for arch test
library(xts)
library(TTR)
library(quantmod)
library(tseries)


data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/JPYUSD.csv")
tail(data)

head(data)


#make a time series of the dataframe
#the frequency is usually the number of times
# the data points were collected in a year
# say monthly data, the freq will be 12 

E<-ts(data[,2],end=c(2019,10),frequency = 12)
e=diff(log(E))

autoplot(e) +
  xlab("Years") + ylab("Ex Rates (LCU/US$)") + 
  geom_abline(intercept = 0, slope = 0, color="dark grey", lty=2, lwd=1.5)
  ggtitle("Exchange Rate Series over time")

grid.arrange(ggAcf(e)+ggtitle("ACF"),ggPacf(e)+ggtitle("PACF"),nrow=1)

ar1<-arima(e,c(1,0,0))
ar1

coeftest(arima(e, c(1,0,1)))

res2 <- residuals(naive(E))
res <- residuals(naive(e))
autoplot(res2) + xlab("Year") + ylab("") +
  ggtitle("Residuals from naïve method")

checkresiduals(naive(E))

Box.test(res2, lag=1)

efit <- auto.arima(e, seasonal=FALSE) #vip
efit


###installing and loading multiple packages
list.packages<-c("fGarch", "PerformanceAnalytics","rugarch","tseries","xts","FinTS", "urca")
new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#Loading Packages
invisible(lapply(list.packages, require, character.only = TRUE))



cxx_x <- function(x) {
  for (i in 1:nlevels(df2$iso3)){
    df2$iso3[[i]] <- filter(df2, iso3 == x)
    # rez[[i]] <- df5
    new_df <- paste0("df2", i)
    assign(new_df, df2)
  }
}

cxx_x("AGO")


rm("df56", "rez")



#_________________________Croissant_2008___________________

EmplUK <- plm.data(EmplUK, index = c("firm", "year"))

#_________lagging and differencing______________

log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(wage), 2) +
  + lag(log(wage), 3) + diff(capital, 2) + diff(capital, 3)

#_____________Models_____________________




plot(e,ylab="",xlab="")
abline(h=0,col="red",lty=3,lwd=3)

par(mfrow = c(1, 1))

acf(e)
pacf(e)

ar1<-arima(e,c(1,0,0))
ar1



autoplot(e,main='e')
ggar1 <- ggAcf(e)
ggPar1 <- ggPacf(e)
grid.arrange(ggar1,ggPar1,nrow=1)


summary(ar1)

Box.test(E, lag = 1, type = "Ljung")

tryale=auto.arima(e,ic="aic",trace = TRUE)

arma11<-arima(e,c(1,0,1))

g1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
               mean.model=list(armaOrder=c(1,0)),distribution.model="std")

garch11<-ugarchfit(g1,data = e)
garch11

vole <- ts(garch11@fit$sigma^2,end=c(2019,10),frequency = 12)
vole 

plot(vole,xlab="",ylab="",main="JPYUSD Volatility (GARCH[1,1])")

autoplot(vole)

#eGARCH is tried and tested below

g1e<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                mean.model=list(armaOrder=c(1,0)),distribution.model="std")

garch11e<-ugarchfit(g1e,data = e)

coef(garch11e)

vole2 <- ts(garch11e@fit$sigma^2,end=c(2019,10),frequency = 12)

par(mfrow = c(1, 1))

plot(vole2,xlab="",ylab="",main="JPYUSD Volatility (EGARCH[1,1])")

autoplot(vole2)

cor(vole,vole2)
ts.plot(vole,vole2,col=c("green","red"),xlab="")
legend("topright",legend=c("Standard","Exponential"),col=c("green","red"),lty=c(1,1))

ArchTest(e)

garch(e, grad="numerical", trace=FALSE)




 
 # Lets create a variable in the existing data frame, df
 df <- df %>% mutate(new_var_name = log(e)) 
 
 # Add new column from existing column
 df$new_pages <- df$pages-2
 
 #Add multiple columns to dataframe
 df3 <- cbind(df, chapters, price)
 
 # Using $ notation
 chapters = c(76,86)
 df$chapters <- chapters
 df
 
 # Output
 #  id pages   name chapters
 #1 11    32  spark       76
 #2 22    45 python       86

 library(dplyr)
 library(tidy)
 str(ssaData)
 
 ssaData$iso3c <- as.factor(ssaData$iso3c)

 ssaData <- ssaData %>% mutate(FDI = BX.KLT.DINV.CD.WD) 

 
 list.of.packages <- c("gapminder", "dplyr")
 
 
 {
   new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
   if(length(new.packages)) install.packages(new.packages)
   lapply(list.of.packages, require, character.only = TRUE)
 }
 
#      mean(ssaData$NY.GDP.MKTP.CD, na.rm = TRUE)
 

 library(plm)
library(texreg)
data("Grunfeld", package = "plm")
 
 
 
 
 
 
 
 
 
 
 



flowers <- read.table(file = "data/flower.txt", 
                      header = TRUE, sep = "\t", 
                      stringsAsFactors = TRUE)
# CLEARS ENVIRONMENT 
rm(list = ls())

plot(flowers$height)
with(flowers, plot(weight))
summary(flowers)
str()

brk <- seq(from=0, to=18, by=1)
hist(flowers$height, breaks = brk,
     main = "Putunia Heights Histogram",
     freq = FALSE
     )
lines(density(flowers$height))


brk <- seq(from=0, to=18, by=1)
hist(flowers$height, breaks = brk,
     main = "Putunia Heights Histogram",
)

#Box plots

boxplot(flowers$height, 
        xlab = "All Factors", 
        ylab = "Weights (g)")


boxplot(weight ~ nitrogen, 
        data = flowers,
        xlab = "Nitrogen Levels",
        ylab = "Weights (g)")

flowers$nitrogen <- factor(flowers$nitrogen, 
                           levels = c("low", "medium", "high"))
boxplot(weight ~ nitrogen, 
        data = flowers,
        xlab = "Nitrogen Levels",
        ylab = "Weights (g)")


flowers$nitrogen <- factor(flowers$nitrogen, 
                           levels = c("high", "medium", "low"))
boxplot(weight ~ nitrogen, 
        data = flowers,
        xlab = "Nitrogen Levels",
        ylab = "Weights (g)")

# str(flowers)
# 
# 'data.frame':	96 obs. of  8 variables:
#   $ treat    : Factor w/ 2 levels "notip","tip": 2 2 2 2 2 2 2 2 2 2 ...
# $ nitrogen : Factor w/ 3 levels "high","medium",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ block    : int  1 1 1 1 1 1 1 1 2 2 ...
# $ height   : num  7.5 10.7 11.2 10.4 10.4 9.8 6.9 9.4 10.4 12.3 ...
# $ weight   : num  7.62 12.14 12.76 8.78 13.58 ...
# $ leafarea : num  11.7 14.1 7.1 11.9 14.5 12.2 13.2 14 10.5 16.1 ...
# $ shootarea: num  31.9 46 66.7 20.3 26.9 72.7 43.1 28.5 57.8 36.9 ...
# $ flowers  : int  1 10 10 1 4 9 7 6 5 8 ...

flowers$treat <- factor(flowers$treat,
                        levels = c("tip", "notip"))
boxplot(weight~nitrogen*treat,
        data = flowers,
        xlab = "Nitrogen Levels",
        ylab = "Heights (cm)",
        cex.axis = 0.6)

library(zoo)
library(vioplot)
library(sm)

vioplot(weight~nitrogen,
        data = flowers,
        xlab = "nitrogen levels",
        ylab = "Weights(g)",
        col = "lightblue",
        cex.axis = 0.7)

dotchart(flowers$height,
         xlab = "Heights",
         ylab = "Frequency",
         main = "Dot Plots of Heights",
         groups = flowers$nitrogen)

# The pairs() function creates
# a multi-panel scatterplot 
# (sometimes called a scatterplot matrix) 


head(flowers)

# treat nitrogen block height weight leafarea shootarea flowers
# 1   tip   medium     1    7.5   7.62     11.7      31.9       1
# 2   tip   medium     1   10.7  12.14     14.1      46.0      10
# 3   tip   medium     1   11.2  12.76      7.1      66.7      10
# 4   tip   medium     1   10.4   8.78     11.9      20.3       1
# 5   tip   medium     1   10.4  13.58     14.5      26.9       4
# 6   tip   medium     1    9.8  10.08     12.2      72.7       9

pairs(flowers[, 4:8],
      panel = panel.smooth)

?pairs


# Add panel correlation coefients 
# to the scatter matrix 


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(flowers[, 4:8],
      lower.panel = panel.cor,
      diag.panel = panel.hist,
      upper.panel = panel.smooth)

# 
# When examining the relationship between two numeric variables, 
# it is often useful to be able to determine whether a third variable
# is obscuring or changing any relationship. 
# 
# A really handy plot to use in these situations is a conditioning plot 
# (also known as conditional scatterplot plot) 
# which we can create in R by using the coplot() function. 
# The coplot() function plots two variables but each plot 
# is conditioned (|) by a third variable. 
# This third variable can be either numeric or a factor. 
# 
# As an example, let’s look at how the relationship between 
# the number of flowers (flowers variable) and the weight 
# of petunia plants changes dependent on leafarea.


coplot(flowers~weight | leafarea, data = flowers)


coplot(flowers~weight | leafarea, data = flowers, overlap = 0)




# >>>>>>>>>>>CONCLUSION>>>>>>>>>>>>>
#   
# Graph type	          lattice function	              Base R function
# scatterplot	          xyplot()	                      plot()
# frequency histogram	  histogram(type = "count")	      hist()
# boxplot	              bwplot()	                      boxplot()
# Cleveland             dotplot	dotplot()	              dotchart()
# scatterplot matrix	  splom()	                        pairs()
# conditioning plot	    xyplot(y ~ x | z)	              coplot()

# 
# Section3

?plotmath

plot(flowers$weight, flowers$shootarea,
     xlab = "weight (g)",
     ylab = "shoot area (cm2)")


plot(flowers$weight, flowers$shootarea,
     xlab = "weight (g)",
     ylab = expression(paste("shoot areas (cm"^"2",")")))

# The par() function is the main function for setting 
# graphical parameters in base R and the mar = argument
# sets the size of the margins that surround the plot. 
# You can adjust the size of the margins using the notation 
# par(mar = c(bottom, left, top, right) where the arguments
# bottom, left, top and right are the size of
# the corresponding margins. 


#  xaxs="i", yaxs="i"     >>>>>> makes the x and y axes intersect


par(mar = c(5, 5, 4, 2), xaxs="i", yaxs="i")                   #sets margin around graph
plot(flowers$weight, flowers$shootarea,                        #defines variables to plot
     xlab = "weight (g)",                                      #labels the x-axis
     ylab = expression(paste("shoot areas (cm"^"2",")")),      #pastes expression for y axis
     xlim = c(0, 30),                                          #sets values for the x-axis  
     ylim = c(0, 200),                                         #sets values for the y-axis
     bty = "l",                                                #defines the box type to L or Rectangle
     las = 1,                                                  #rotates the values on the y axis
     cex.axis = 0.8,                                           #reduces the font size of values on x and y axes
     tcl = -0.2,                                               #reduce tick mark lengths on x and y axes
     pch = 16,                                                 #plot symbol (choose numbers between 1 and 25 inclusive)
     col = "dodgerblue1",                                      #color of plot symbol
     cex = 0.9)                                                #size of plot symbol
text(x=28, y=190, label="CK", cex=2, col="red")                #Adds texts and decorates


par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs = "i", yaxs = "i")
plot(flowers$weight, flowers$shootarea,
     type = "n",
     xlab = "weight (g)",
     ylab = expression(paste("shoot area (cm"^"2",")")),
     xlim = c(0, 30), ylim = c(0, 200), bty = "l",
     las = 1, cex.axis = 0.8, tcl = -0.2)
points(x=flowers$weight[flowers$nitrogen=="low"],
       y=flowers$shootarea[flowers$nitrogen=="low"],
       pch=16, col="deepskyblue", cex=0.9)

points(x=flowers$weight[flowers$nitrogen=="medium"],
       y=flowers$shootarea[flowers$nitrogen=="medium"],
       pch=16, col="yellowgreen", cex=0.9)

points(x=flowers$weight[flowers$nitrogen=="high"],
       y=flowers$shootarea[flowers$nitrogen=="high"],
       pch=16, col="red", cex=0.9)
text(x = 28, y = 190, label = "A", cex = 2)

leg_col <- c("deepskyblue", "yellowgreen", "red")
leg_sym <- c(16, 16, 16)
leg_lab <- c("low", "medium", "high")

legend(x=1, y=200, col = leg_col, pch = leg_sym,
       legend = leg_lab, bty = "n", 
       title = "Nitrogen Levels")



# >>>>>>>>>>>>>SIDE BY SIDE>>>>>>>>.
# ====================================
#           MULTIPLE GRAPHS


# 
# For example, to plot two graphs side by side
# we would use par(mfrow = c(1, 2)) to split 
# the device into 1 row and two columns.
# 



par(mfrow = c(1, 2))
plot(flowers$weight, flowers$shootarea, 
     xlab = "weight",
     ylab = "shoot area")
boxplot(shootarea ~ nitrogen, 
        data = flowers, 
        cex.axis = 0.6)



# Or if we wanted to plot four plots we can split
# our plotting device into 2 rows and 2 columns.
#par(mfrow = c(2, 2))   >>> splits space into 2 rows 2 columns

par(mfrow = c(2, 2))
plot(flowers$weight, flowers$shootarea, xlab = "weight",
     ylab = "shoot area")
boxplot(shootarea ~ nitrogen, cex.axis = 0.8, data = flowers)
hist(flowers$weight, main ="")
dotchart(flowers$weight)

par(mfrow = c(1,1))
layout_mat <- matrix(c(2, 0, 1, 3),
                     nrow = 2, 
                     ncol = 2,
                     byrow = TRUE)
layout_mat


par(mar = c(5, 5, 4, 2))  


my_lay <- layout(mat = layout_mat, 
                 heights = c(1, 3),
                 widths = c(3, 1), respect =TRUE)
layout.show(my_lay)


# 
# 
# par(mar = c(4, 4, 4, 0))
# plot(flowers$weight, flowers$shootarea, 
#      xlab = "weight (g)", 
#      ylab = "shoot area (cm2)")
# 
# par(mar = c(0, 4, 0, 0))
# boxplot(flowers$weight, 
#         horizontal = TRUE,
#         frame = FALSE,
#         axes =FALSE)
# 
# par(mar = c(4, 0, 0, 0))
# boxplot(flowers$shootarea, 
#         frame = FALSE, 
#         axes = FALSE)
# 


par(mar = c(4, 4, 0, 0))
plot(flowers$weight, flowers$shootarea, 
     xlab = "weight (g)", ylab = "shoot area (cm2)")
par(mar = c(0, 4, 0, 0))
boxplot(flowers$weight, horizontal = TRUE, frame = FALSE,
        axes =FALSE)
par(mar = c(4, 0, 0, 0))
boxplot(flowers$shootarea, frame = FALSE, axes = FALSE)



pdf(file = 'output/my_plot.pdf')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(flowers$weight, flowers$shootarea, 
     xlab = "weight (g)",
     ylab = expression(paste("shoot area (cm"^"2",")")),
     xlim = c(0, 30), ylim = c(0, 200), bty = "l",
     las = 1, cex.axis = 0.8, tcl = -0.2,
     pch = 16, col = "dodgerblue1", cex = 0.9)
text(x = 28, y = 190, label = "A", cex = 2)


par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs = "i", yaxs = "i")
plot(flowers$weight, flowers$shootarea,
     type = "n",
     xlab = "weight (g)",
     ylab = expression(paste("shoot area (cm"^"2",")")),
     xlim = c(0, 30), ylim = c(0, 200), bty = "l",
     las = 1, cex.axis = 0.8, tcl = -0.2)
points(x=flowers$weight[flowers$nitrogen=="low"],
       y=flowers$shootarea[flowers$nitrogen=="low"],
       pch=16, col="deepskyblue", cex=0.9)

points(x=flowers$weight[flowers$nitrogen=="medium"],
       y=flowers$shootarea[flowers$nitrogen=="medium"],
       pch=16, col="yellowgreen", cex=0.9)

points(x=flowers$weight[flowers$nitrogen=="high"],
       y=flowers$shootarea[flowers$nitrogen=="high"],
       pch=16, col="red", cex=0.9)
text(x = 28, y = 190, label = "A", cex = 2)

leg_col <- c("deepskyblue", "yellowgreen", "red")
leg_sym <- c(16, 16, 16)
leg_lab <- c("low", "medium", "high")

legend(x=1, y=200, col = leg_col, pch = leg_sym,
       legend = leg_lab, bty = "n", 
       title = "Nitrogen Levels")
dev.set()

save.image(file = "graphics.Rproj")


ssaCountries <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF",
                  "CIV", "CMR", "COD", "COG", "COM", "CPV",
                  "ERI", "ETH", "GAB", "GHA", "GIN", "GMB",
                  "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG",
                  "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM",
                  "NER", "NGA", "RWA", "SDN", "SEN", "SLE",
                  "SOM", "SSD", "STP", "SWZ", "SYC", "TCD",
                  "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

myIndicators <- c("fdi"="BX.KLT.DINV.CD.WD",
                  "exr"="PA.NUS.FCRF",
                  "exRates"="DPANUSLCU",
                  "gdp"="NY.GDP.MKTP.CD", 
                  "gdp_pc"="NY.GDP.PCAP.CD",
                  "export"="NE.EXP.GNFS.CD",
                  "import"="NE.IMP.GNFS.CD",
                #  "cpi"="FP.CPI.TOTL.ZG",
                  "political"="PV.EST",
                  "inflation"="NY.GDP.DEFL.KD.ZG")

ssaCountrySorted <- c("AGO", "BDI", "BEN", "BWA", "CAF",
                      "CIV", "CMR", "COG", "CPV","GAB", "GHA",
                      "GIN", "GMB", "GNB", "KEN", "MDG", "MLI",
                  
                      "MOZ", "MRT", "MUS", "NAM",
                      "NER", "NGA", "RWA", "SDN", "SEN", "SLE",
                             "SWZ", "SYC", "TCD",
                      "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")


myIndi_sorted <- c("fdi"="BX.KLT.DINV.CD.WD",
                  "exr"="PA.NUS.FCRF",
                  "exRates"="DPANUSLCU",
                  "gdp"="NY.GDP.MKTP.CD", 
                  "gdp_pc"="NY.GDP.PCAP.CD",
                  "export"="NE.EXP.GNFS.CD",
                  "import"="NE.IMP.GNFS.CD",
                  #  "cpi"="FP.CPI.TOTL.ZG",
                  "political"="PV.EST",
                  "inflation"="NY.GDP.DEFL.KD.ZG")


ssaFDI <- WDI(
  country = ssaCountries,
  indicator = myIndicators,
  start = 1999,
  end = 2021,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

# ssa_data_try <- wb_data(myIndicators, country = ssaCountries,
#                     start_date = 1999, end_date = 2021)

library(WDI)

ssaFDI_try1 <- WDI(
  country = ssaCountries,
  indicator = myIndicators,
  start = 2002,
  end = 2021,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


exR <- WDI(indicator = 'DPANUSSPB',
           country = ssaCountrySorted,
           start = '2002M01', end = '2021M01')

# library(ggplot2)
# ggplot(exR, aes(year, DPANUSSPB))
#   xlab('Year') + ylab('Exchange Rates')

  
  World_GDPpc <- WDI(
    country = "WLD",
    indicator = "NY.GDP.PCAP.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  #============Collecting the Data=========
  
  fdi_1 <- WDI(
    country = ssaCountrySorted,
    indicator = "BX.KLT.DINV.CD.WD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  
  fdi_to_gdp <- WDI(
    country = ssaCountrySorted,
    indicator = "BX.KLT.DINV.WD.GD.ZS",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  
  export <- WDI(
    country = ssaCountrySorted,
    indicator = "NE.EXP.GNFS.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )

  
  import <- WDI(
    country = ssaCountrySorted,
    indicator = "NE.IMP.GNFS.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  political_stab <- WDI(
    country = ssaCountrySorted,
    indicator = "PV.EST",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  
  # cpi <- WDI(
  #   country = ssaCountrySorted,
  #   indicator = "FP.CPI.TOTL.ZG",
  #   start = 2002,
  #   end = 2020,
  #   extra = FALSE,
  #   cache = NULL,
  #   latest = NULL,
  #   language = "en"
  # )
  # 

  inflation <- WDI(
    country = ssaCountrySorted,
    indicator = "NY.GDP.DEFL.KD.ZG",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  # OEXR <- WDI(
  #   country = ssaCountrySorted,
  #   indicator = "PA.NUS.FCRF",
  #   start = 2002,
  #   end = 2020,
  #   extra = FALSE,
  #   cache = NULL,
  #   latest = NULL,
  #   language = "en"
  # )
  # 

  population <- WDI(
    country = ssaCountrySorted,
    indicator = "SP.POP.TOTL",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )

  
  gdp_1 <- WDI(
    country = ssaCountrySorted,
    indicator = "NY.GDP.MKTP.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )  

  
  gdp_pc_1 <- WDI(
    country = ssaCountrySorted,
    indicator = "NY.GDP.PCAP.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )  
  
  
  #======== GENERATE BIG PANEL ========

  thesis_data <- WDI(
    country = ssaCountrySorted,
    indicator = myIndi_sorted,
    start = 2002,
    end = 2020,
    extra = TRUE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )  
  
  #Using dplyr when columns are same
  
  df2 <- fdi_1 %>% inner_join( fdi_to_gdp, 
                                by='BX.KLT.DINV.CD.WD')
  
  
  
 df1 <- merge(export, import, 
              by.x = "NE.EXP.GNFS.CD", by.y = "NE.IMP.GNFS.CD",
              all.x = TRUE, all.y = FALSE)
  

 
 df2 <- data.frame("country"=export$country,
                   "iso3" = export$iso3c,
                   "year" = export$year,
                   "fdi_1" = fdi_1$BX.KLT.DINV.CD.WD,
                   "fdi_2_gdp" = fdi_to_gdp$BX.KLT.DINV.WD.GD.ZS,
                   "gdp_1" = gdp_1$NY.GDP.MKTP.CD,
                   "gdp_pc_1" = gdp_pc_1$NY.GDP.PCAP.CD,
                  "population" = population$SP.POP.TOTL,
                   "exp" = export$NE.EXP.GNFS.CD, 
                   "imp" = import$NE.IMP.GNFS.CD,
                   "inflation" = inflation$NY.GDP.DEFL.KD.ZG,
                   "political" = political_stab$PV.EST,
                  "income" = extra1$income)
 
 
 # Generate the monthly data
 
 ssaCountry_35 <- c("AGO", "BDI", "BEN", "BWA", "CAF",
                       "CIV", "CMR", "COG", "CPV","GAB", "GHA",
                       "GIN", "GMB", "GNB", "KEN", "MDG", "MLI",
                       
                       "MOZ", "MRT", "MUS", "NAM",
                       "NER", "NGA", "RWA", "SDN", "SEN", "SLE",
                       "SWZ", "SYC", "TCD",
                       "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
 
 
 
 
 AGO_0220 <- filter(big, iso3c == "AGO")
 
 
 #library(WDI)
 exr_back <- WDI(
   country = ssaCountrySorted,
   indicator = "DPANUSSPB",
   start = "2002M01",
   end = "2020M12",
   extra = FALSE,
   cache = NULL,
   latest = NULL,
   language = "en"
 )

 
# For Sudan
 
 SDN_exr <- WDI(
   country = "SDN",
   indicator = "DPANUSSPB",
   start = "2002M01",
   end = "2020M12",
   extra = FALSE,
   cache = NULL,
   latest = NULL,
   language = "en"
 )
 
 library(WDI)
 #congo
 #.  DPANUSLCU
 
 COG1_exr <- WDI(
   country = "COG",
   indicator = "DPANUSLCU",
   start = "2002M01",
   end = "2021M12",
   extra = FALSE,
   cache = NULL,
   latest = NULL,
   language = "en"
 )
 
 
 # 
 extra2 <- estra1[-c(2, 5, 6, 13:16, 18)] #remove some columns
 
 
 df2$income <- as.factor(df2$income) #Transform the income variable to a factor
 df2$country <- as.factor(df2$country)
 df2$iso3 <- as.factor(df2$iso3)
 
 #Import monthly exchange rates
 monthly_exr <- read.csv(file = 'exported_data/exr_mon.csv') 
 
 mydates <- seq(
  from = floor_date(as.Date("2002-01-01"), unit = "month"),
  to = ceiling_date(as.Date("2020-07-01"), "month") - 1,
  by = "month"
)
 
 mydates <- seq(
   from = floor_date(as.Date("2002-01-01"), unit = "month"),
   to = ceiling_date(as.Date("2020-07-01"), "month") - 1,
   by = "month"
 )
  
 df2 <- df[order(df$publish_date, decreasing=TRUE),]
 
 mydat_test <- sort(mydates, decreasing = TRUE)
 
 
 big2 <- big[order(big$year, decreasing=FALSE),]
 
 
# mydates <- seq(as.Date("2021-07-06"),as.Date("2021-07-25"),by = 1)
 
 dates_exr <- rep(mydat_test, times=36)
 
 
 # GARCH Trial May 4, 2023
 
 ago_ex_ord <- ago_ex[order(ago_ex$year), ]
 
 ago_ex <- filter(ex, iso3 == "AGO")
 E_ago <- ts(ago_ex[,4],start = c(2002,01), end=c(2020,07),frequency = 12)
 e_ago = diff(log(E_ago))

 E_ago_ord <- ts(ago_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)
 
 
 plot(e_ago,ylab="",xlab="")
 abline(h=0,col="dark grey",lty=3,lwd=3)
 
 
 
 # ======================
 
 
 
 ar1<-arima(e_ago,c(1,0,0))
 ar1
 

 
 efit <- auto.arima(e_ago, seasonal=FALSE) #vip
 efit
 
 
 
 v_date <- seq(
   from = floor_date(as.Date("2002-02-01"), unit = "month"),
   to = ceiling_date(as.Date("2020-07-01"), "month") - 1,
   by = "month"
 )
 
 mydates <- seq(
   from = floor_date(as.Date("2002-01-01"), unit = "month"),
   to = ceiling_date(as.Date("2020-07-01"), "month") - 1,
   by = "month"
 )
 
 
 mydat_test <- sort(mydates, decreasing = TRUE)
 
 # mydates <- seq(as.Date("2021-07-06"),as.Date("2021-07-25"),by = 1)
 
 dates_exr <- rep(mydat_test, times=36)
 
 #Get 2002 monthly volatilities
 dfvol_ago2002 <- dfvol_ago %>% filter(date < '2003-01-01')
 
 #Get 2020 monthly volatilties 
 dfvol_ago2020 <- dfvol_ago %>% filter(date > '2019-12-01')

 #Get monthly volatilities from 2003 t0 2019
 dfvol_ago0319 <- dfvol_ago %>% filter(date >= '2003-01-01' & date <= '2019-12-01')
 
 ago_vol_02avg <- mean(dfvol_ago02$vol)
 ago_vol_20avg <- mean(dfvol_ago20$vol)
 
 #### For loop for annual averages from monthly volatilities
 
 #Combine obsns from 1-12, 13-24, etc
 #gaps of p = 12 with no zeroes
 
 l1 <- nrow(dfvol_ago0319)
 l <- l1/12 
 l
 length <- l1%/%12
 length
 
 #write the loop to calculate the average with each
 e_series_ago <- dfvol_ago0319$vol
 p=12 #Remove from subsequent codes
 new_ago=NULL
 for (i in 1:length){
   ydfvol_ago0319 <- e_series_ago[((i-1)*p+1):(i*p)]
   yavg_ago <- mean(ydfvol_ago0319)
   new_ago = rbind(new_ago, yavg_ago)
 }
 
 avg_ago <- new_ago
 avg_ago
 ts.plot(e_series_ago)
 ts.plot(avg_ago)
 
 #make the year_s manually
 year_s <- rep(2002+1:length(avg_ago))
 year_s
 
 avg_ago0319 <- data.frame("year" = year_s, "avg_vol" = avg_ago)
 
 
 
 #Alternative year
 
 year_s1 <- dfvol_ago0319[1:(12*length), 1]
 year_s1 <- strsplit(year_s1, "/")
 year_s1
 
 #avg_ago22 <- noquote(cbind(year_s1, avg_ago))
 
 avg_ago02_df <- data.frame("year" = 2002, "avg_vol" = ago_vol_02avg)
 
 avg_ago20_df <- data.frame("year" = 2020, "avg_vol" = ago_vol_20avg)
 
 
 ago_vol_0220 <- rbind(avg_ago02_df, avg_ago0319, avg_ago20_df)
 
 
AGO_0220 <- filter(big, iso3 == "AGO")
AGO_0220 <- AGO_0220[order(AGO_0220$year, decreasing=FALSE),] 


mypanel <- mutate(big1, 
                  sGARCH = sGARCH11_avg$avg_vol,
                  eGARCH = sGARCH11e_1avg$avg1_vol)


attr(mypanel[["sGARCH"]], "label") <- "Standard GARCH"
attr(mypanel[["eGARCH"]], "label") <- "Exponetial GARCH"
