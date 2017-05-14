# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA",]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Create a new variable in our datset called internet.mil to make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

world$proportion <- world$internet.users / world$population
world_select <- world[world$year >= 1995,]

braz_sel <- world_select[world_select$Country == "Brazil",]
braz_sel$time <- braz_sel$year - 1995
braz_sel$mobile.mil <- braz_sel$mobile.users / 1000000


world_select$time <- world_select$year - 1990

den_select <- world_select[world_select$Country == "Denmark",]

expFit(braz_sel$time, braz_sel$mobile.mil)
logisticFit(braz_sel$time, braz_sel$mobile.mil)

# Create a new variable in our dataset called time that represents "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10,]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(den_select$time, den_select$proportion, 16.076)
l <- logisticFitPred(braz_sel$time, braz_sel$mobile.mil, 30)

t <- c()
