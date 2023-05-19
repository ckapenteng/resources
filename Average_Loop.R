# Annual average from monthly e_series_ago
#Get the dat and let's go

#Combine obsns from 1-12, 13-24, etc
#gaps of p = 12 with no zeroes

l1 <- nrow(dfvol_ago0319)
l <- l1/12 
l
length <- l1%/%12
length

#write the loop to calculate the average with each
e_series_ago <- dfvol_ago0319$vol
p=12
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
year_s <- rep(2001+1:length(avg_ago))
year_s

#Alternative to year_s
year_s1 <- dfvol_ago0319[1:(12*length), 1]
year_s1 <- strsplit(year_s1, "/")
year_s1

year_s1 <- dfvol_ago0319.frame(Reduce(rbind, year_s1))
year_s1

year_s1 <- unique(year_s$x3)

#add the year
avg_ago <- noquote(cbind(year_s1, avg_ago))

#simple time e_series_ago plot... Not interested in this one though
plot(avg_ago, type = "l", main ="Title Here", )