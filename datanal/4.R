bull <- BullRiders

#Subset for riders that participated in at least one event in 2013
new_bull <- bull[bull$Rides14  > 0 ,]
RidesPerEvent14 <- new_bull$Rides14/new_bull$Events14

# Visualize and describe the first variable of interest
hist(RidesPerEvent14)
fivenum(RidesPerEvent14)
mean(RidesPerEvent14)
sd(RidesPerEvent14)

plot(new_bull$Rank14,RidesPerEvent14)
abline(lm(RidesPerEvent14~new_bull$Rank14))

# Calculate the correlation coefficient
cor(RidesPerEvent14,new_bull$Rank14)
cor(new_bull$Rank14,RidesPerEvent14)


# Create a correlation matrix 
vars <- c("Earnings12", "RidePer12", "CupPoints12")
cor(new_bull12[,vars])
cor(nooutlier[,vars])

# Create a scatterplot
plot(new_bull12$RidePer12,new_bull12$Earnings12)
plot(new_bull12$CupPoints12,new_bull12$Earnings12)

# identify specific case
which(new_bull12$Earnings12 == max(new_bull12$Earnings12))

nooutlier <- new_bull12[new_bull12$Earnings12 < 1000000 ,]

# Visualize and describe the second variable of interest
hist(new_bull$Top10_13)
fivenum(new_bull$Top10_13)
mean(new_bull$Top10_13)
sd(new_bull$Top10_13)

# Create a scatterplot
plot(new_bull$Rides13,new_bull$Top10_13)

# Add line of best fit
abline(lm(new_bull$Top10_13~new_bull$Rides13))

# Calculate the correlation coefficient
cor(new_bull$Rides13,new_bull$Top10_13)

# Create a correlation matrix 
vars <- c("Earnings13", "Rides13")
cor(new_bull[,vars])

#identify a specific record
which(new_bull$Top10_13==2 & new_bull$Rides13==22)