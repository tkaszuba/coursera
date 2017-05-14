
old <- animaldata[animaldata$Age.Intake >= 1,]
table(old$Weight)
weight <- old$Weight

hist(weight)
fivenum(weight)
mean(weight)
sd(weight)
(13-30.29539)/23.6356

table(old$Animal.Type)

dogs <- old[old$Animal.Type == "Dog",]
cats <- old[old$Animal.Type == "Cat",]

hist(dogs$Weight)
hist(cats$Weight)

mean(cats$Weight)
sd(cats$Weight)

1-pnorm((13-mean(cats$Weight))/sd(cats$Weight))

fivenum(dogs$Weight)

sd(dogs$Weight)

fivenum(cats$Weight)


table(animaldata$Intake.Type)
d <- animaldata[animaldata$Animal.Type == "Dog",]
c <- animaldata[animaldata$Animal.Type == "Cat",]

table(d$Intake.Type)

owner <- d[d$Intake.Type == "Owner Surrender",]
table(owner$Outcome.Type)

returned <- owner[owner$Outcome.Type == "Return to Owner",]

mean(returned$Days.Shelter)
fivenum(returned$Days.Shelter)

hist(d$Days.Shelter,breaks = 100)
hist(owner$Days.Shelter, breaks = 100)
mean()
#Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type=="Adoption",]

#Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter

t <- c(2,2,3,5,6,7,9,13,15,18)
fivenum(t)
mean(t)

x <- c(4,4,6,10,12,14,18,26,30,36)
fivenum(x)
mean(x)


barplot(t)

#Visualize and describe this variable
hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)
which(animaldata$Days.Shelter==max(daystoadopt))
animaldata[425,]
