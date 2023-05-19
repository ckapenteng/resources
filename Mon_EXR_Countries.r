# GARCH Process Started May 4, 2023

ago_ex <- filter(ex, iso3 == "AGO")     #extract country US/LCU from ex df

ago_ex_ord <- ago_ex[order(ago_ex$year), ]    #Re-order the extracted for ts creation 

#E_ago <- ts(ago_ex[,4],start = c(2002,01), end=c(2020,07),frequency = 12)
E_ago <- ts(ago_ex_ord[,4],start = c(2002,01), end=c(2020,07),frequency = 12)

e_ago = diff(log(E_ago))

plot(e_ago,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)