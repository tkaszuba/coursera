minutes <- c(30, 45, 180, 95, 130, 140, 80, 60, 110, 0, 80)
grade <- c(74, 68, 87, 90, 94, 84, 88, 82, 93, 65, 90)

cor(minutes,grade)
plot(minutes,grade)

data.frame(minutes,grade)    

year <- c(1996,1998)
since <- c(1,3)
number <- c(25,45)

ds <- data.frame(year,since,number)

expFit(ds$since, ds$number)
logisticFit(ds$since, ds$number)

e <- expFitPred(ds$since, ds$number,10)
