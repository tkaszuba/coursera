library(SDSFoundations)
bike <- BikeData 
bike[7,2]
ten<-bike[bike$user_id<=10,]
table(ten$cyc_freq=='Daily')

female<-bike[bike$gender=='F' && bike$cyc_freq=='Less than once a month',]
speed <- female[female$cyc_freq=='Less than once a month',]
speed[1,9]

# Import the BikeData dataset, name it "bike"

# Find the number of students in the dataset
table(bike$student)

# Pull out student data into a new dataframe
student <-bike[bike$student==1,]

# Find how often the students ride, using the new dataframe
table(student$cyc_freq)

# Create a vector for the distance variable
distance <-student$distance

# Find average distance ridden
mean(distance)
