#Subset the data
menshot <- wr[wr$Event=='Mens Shotput',]
womenshot <- wr[wr$Event=='Womens Shotput',] 

#Create scatterplots
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)

library(SDSFoundations)

mensmile <- wr[wr$Event=='Mens Mile',]
womensmile <- wr[wr$Event=='Womens Mile',] 

plot(mensmile$Year,mensmile$Record,main='Mens Mile Records',xlab='Year',ylab='Time (s)',pch=16)
plot(womensmile$Year,womensmile$Record,main='Womens Mile Records',xlab='Year',ylab='Time (s)',pch=16)

linFit(mensmile$Year, mensmile$Record)
linFit(womensmile$Year,womensmile$Record)

menspole <- wr[wr$Event=='Mens Polevault',]
menspole1970 <- menspole[menspole$Year>=1970,]
fivenum(menspole1970$Record)

plot(menspole1970$Year,menspole1970$Record,main='Mens Mile Records',xlab='Year',ylab='Time (s)',pch=16)
linFit(menspole1970$Year, menspole1970$Record)
