dir.create("output")
dir.create("output/processed_data")
#First_Analysis
dir.create("Output")
dir.create("Output")
dir.create("Output/Processed_data")
# Exercise 2
diameter <- 20
rad_ <- diameter/2
area_circle <- pi*(rad_)^2
area_circle

#INTRO 
# FUNCTIONS 
a <- 14*0.51
cuberoot <- function(x){
  if(x<0){
    -(-x)^(1/3)
  }
  else{x^(1/3)}
}
cuberoot(64)
cuberoot(-64)
cuberoot(a)
rm(cuberoot)

a_circle <- function(y){
  y_area <- pi*y^2
  return(y_area)
}

a_circle(7)


weight <- c(69, 62, 58, 59, 71, 64, 56, 66, 67, 66)

# reps <- rep(seq(from=1, to=5, by=2), times=3)

seq1 <- seq(from = 1, to = 20, by = 2)
reps <- rep(seq(from=1, to=5, by=2), each=3)

height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)
rm(rad_)

summary(height)


some_child <- height[c(1, 3, 10)]
height[height >100]
height>100

height[c(TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE)]
height[height>90 & height<100]

height_rev <- rev(height)


sort(height, decreasing = FALSE)
weight
mean(weight)
mean(weight, na.rm = TRUE)
summary(weight)

str(weight)

help.search(list)

ls()

mydata <- data.frame(Heights = height, Weights = weight)
mydata
BMI <- weight/(height^2)

help(cbin)

cbind(mydata, BMI)

save.image(file = "First_Practice_Ex1_2.Rproj")

?logLik

mydata$Heights

mydata[7:10, 1:2]


mydata
mydata2 <- cbind(mydata, BMI)
head(mydata)
head(mydata2)
mydata2[7:10, 1:3]
mean(mydata$Heights[7:10])
save.image(file = "First_Practice_Ex1_2.Rproj")


mydata2[c(1, 4, 8), c(1,3)]

mydata2[c(1:3, 6:8), c(1,3)]
mydata2[1:5, ]
mydata2[-c(2, 5:8),]
mydata2[c(2, 5:8), c("Heights", "BMI")]

?order
mydata3 <- mydata2[order(mydata2$Heights, mydata2$Weights),]
mydata3
mydata2

mydata3$Heights_sqrt <- sqrt(mydata3$Heights)
mydata3$Weights_sqrt <- sqrt(mydata3$Weights)
str(mydata3)
head(mydata3)
# 
# EXPORTING DATA 
# USING R IN_BUILT 
# FUNCTIONS 

write.table(mydata3, file = "Data/mydata3.txt", 
col.names = TRUE, row.names = FALSE, sep = "\t")


height_trans <- function(t){
  t_height <- ((1/2)*t)+10
  return(t_height)
}

mydata3$Height_trans <- height_trans(mydata3$Heights)

head(mydata3)

write.table(mydata3, file = "Data/mydata3_trans.txt", 
            col.names = TRUE, row.names = FALSE, sep = "\t")

save.image(file = "First_Practice_Ex1_2.Rproj")



library(WDI)

#======== GENERATE BIG PANEL ========

thesis_data <- WDI(
  country = c("AGO", "BDI", "BEN", "BWA", "CAF",
              "CIV", "CMR", "COG", "CPV","GAB", "GHA",
              "GIN", "GMB", "GNB", "KEN", "MDG", "MLI",
              
              "MOZ", "MRT", "MUS", "NAM",
              "NER", "NGA", "RWA", "SDN", "SEN", "SLE",
              "SWZ", "SYC", "TCD",
              "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"),
  
  indicator = c("BX.KLT.DINV.CD.WD",
                "BX.KLT.DINV.WD.GD.ZS",
                "NY.GDP.MKTP.CD", 
                "NY.GDP.PCAP.CD",
                "NE.EXP.GNFS.CD",
                "NE.IMP.GNFS.CD",
                "SP.POP.TOTL",
                "PV.EST",
                "NY.GDP.DEFL.KD.ZG")
  ,
  start = 2002,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)  


for (i in 1:link_gr$n.groups){
  grp <- NULL
  grp[i] <- make.seq(link_gr, i)
  grp[i].od <- order.seq(input.seq=grp[i], n.init = 5,  subset.search = "twopt",  
                         twopt.alg = "rcd", THRES = 3, draw.try = TRUE, wait = 1, touchdown=TRUE)
} 

cxxx <- function(x) {
  rez <- c()
  for (i in 1:nlevels(df2$iso3)){
    df5 <- filter(df2, iso3 == x)
    rez[[i]] <- df5
    new_df <- paste0("coun_tries", i)
    assign(new_df, df5)
  }
  return(rez)
}

write.table(mypanel, file = 'exported_data/panel.txt', col.names = TRUE,
            row.names = FALSE, sep = "\t")


write.csv(mypanel, file = 'exported_data/thesis.csv', row.names = FALSE)


fem2 <- plm(fdi_2_gdp~sGARCH+EGG_cts+inflation+LN_political, 
            data = ppan, model = "within")
summary(fem2)

fem3 <- plm(fdi_2_gdp~sGARCH+EGG_ts+inflation+LN_political, 
            data = ppan, model = "within")
summary(fem3)

fem4 <- plm(fdi_2_gdp~log(sGARCH)+log(EGG_ts)+inflation+log(gdp_pc_1)+LN_political, 
            data = ppan, model = "within")
summary(fem4)


rem4 <- plm(fdi_2_gdp~log(sGARCH)+log(EGG_ts)+inflation+log(gdp_pc_1)+LN_political, 
            data = ppan, model = "random")
summary(rem4)



pgmm1 <- pgmm(fdi_2_gdp~lag(fdi_2_gdp, 1:2)+log(sGARCH)+log(EGG_ts)+
                LN_political + log(gdp_pc_1) + ToT + inflation | lag(fdi_2_gdp, 2:200),
              data = ppan, effect = "twoways", model = "twosteps")

summary(pgmm1)


pgmm2 <- pgmm(fdi_2_gdp~lag(fdi_2_gdp, 1:2)+lag(log(sGARCH), 0:1)+lag(log(EGG_ts), 0:1)+
                LN_political + lag(log(gdp_pc_1), 0:1) + ToT + inflation | lag(fdi_2_gdp, 2:200),
              data = ppan, effect = "twoways", model = "onestep")

summary(pgmm2)



pgmm3 <- pgmm(fdi_2_gdp~lag(fdi_2_gdp, 1:2)+lag(log(sGARCH), 1)+lag(log(EGG_ts), 1)+
                LN_political + lag(log(gdp_pc_1), 1) + ToT + lag(inflation, 1) | lag(fdi_2_gdp, 2:200),
              data = ppan, effect = "twoways", model = "onestep")

summary(pgmm3)


pgmm4 <- pgmm(fdi_2_gdp~lag(fdi_2_gdp, 1:2)+lag(log(sGARCH), 1)+lag(log(EGG_ts), 1)+
                LN_political + lag(log(gdp_pc_1), 1) + ToT + lag(inflation, 1) | lag(fdi_2_gdp, 2:200),
              data = ppan, effect = "twoways", model = "onestep",
              transformation = "ld")

summary(pgmm4)



pgmm5 <- pgmm(fdi_2_gdp~lag(fdi_2_gdp, 1:2)+lag(log(sGARCH), 0:1)+lag(log(EGG_ts), 0:1)+
                LN_political + lag(log(gdp_pc_1), 0:1) + ToT + lag(inflation, 0:1) | lag(fdi_2_gdp, 2:200),
              data = ppan, effect = "twoways", model = "onestep",
              transformation = "ld")
summary(pgmm5)


write.csv(sGARCH11_avg, file = 'exported_data/sGARCH_avg.csv', row.names = FALSE)

ex <- read.csv(file = 'exported_data/ex.csv')

library(lubridate)
#load lubridates package for this 
ex$year <- ymd(ex$year)
