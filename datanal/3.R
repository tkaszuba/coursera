#Subset the data for artists age 30 or older
older <-acl[acl$Age>=30,]

face <-acl[acl$Facebook.100k==1,]
age <- table(face$Age.Group)
age
prop.table(age)

males <-acl[acl$Gender=='M',]
genre <- table(males$Genre)
genre
grammy <- table(males$Grammy)
grammy

twoway <- table(males$Grammy, males$Genre)
twoway

barplot(twoway, legend=T, beside=T)

prop.table(grammy)
prop.table(twoway,1)

# Create tables of marginal distributions
genre <- table(older$Genre)
genre
gender <- table(older$Gender)
gender

# Create contingency table 
twoway <- table(older$Gender,older$Genre)
twoway

# Visualize the counts
barplot(twoway, legend=T, beside=T)

# Calculate P(A): the probability of each genre being played
prop.table(genre)

# Calculate P(A|B): the probability of each genre being played, given the artist???s gender
prop.table(twoway,1)

View(acl)
gender_grammy <- table(acl$Gender, acl$Grammy)
View(gender_grammy)
prop.table(acl$Gender, acl$Grammy,2)
