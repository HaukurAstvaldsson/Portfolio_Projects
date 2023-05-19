#After importing the data sets for the last twelve months, 
#I renamed the four data sets needed for reference, using 
#the following code.

q2_2019<-Divvy_Trips_2019_Q2
q3_2019<-Divvy_Trips_2019_Q3
q4_2019<-Divvy_Trips_2019_Q4
q1_2020<-Divvy_Trips_2020_Q1

#Next, I wanted to compare the column names in each of the four files.
#In order for me to join the sets into one, the column names have to 
#match. 

colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

#It turns out there is quite a bit of discrepancy between the data 
#sets in terms of column names. While quarters three and four of 2019 
#are consistent, quarter one contains a whole different way of naming 
#the columns. The data set from quarter one of 2020 has yet another 
#naming system for the columns. To fix this, I rename all the columns 
#in the three data sets from 2019 to make them consistent with the 
#column names in q1_2020.

(q4_2019 <- rename(q4_2019, ride_id = trip_id, rideable_type = bikeid, started_at = start_time, ended_at = end_time, start_station_name = from_station_name, start_station_id = from_station_id, end_station_name = to_station_name, end_station_id = to_station_id, member_casual = usertype))
(q3_2019 <- rename(q3_2019, ride_id = trip_id, rideable_type = bikeid, started_at = start_time, ended_at = end_time, start_station_name = from_station_name, start_station_id = from_station_id, end_station_name = to_station_name, end_station_id = to_station_id, member_casual = usertype))
(q2_2019 <- rename(q2_2019, ride_id = "X01...Rental.Details.Rental.ID", rideable_type = "X01...Rental.Details.Bike.ID", started_at = "X01...Rental.Details.Local.Start.Time", ended_at = "X01...Rental.Details.Local.End.Time", start_station_name = "X03...Rental.Start.Station.Name", start_station_id = "X03...Rental.Start.Station.ID", end_station_name = "X02...Rental.End.Station.Name", end_station_id = "X02...Rental.End.Station.ID", member_casual = "User.Type"))

#Once the column names have been aligned, it's a good idea to inspect
#the data frames and look for incongruities using the 'str' function.

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#The columns titled ride_id and rideable_type contain integers. 
#In order for them to stack correctly, they should be converted to 
#character.

q4_2019<- mutate(q4_2019, ride_id = as.character(ride_id), ridable_type = as.character(rideable_type))
q3_2019<- mutate(q3_2019, ride_id = as.character(ride_id), ridable_type = as.character(rideable_type))
q2_2019<- mutate(q2_2019, ride_id = as.character(ride_id), ridable_type = as.character(rideable_type))

#Now it's time to stack the four data frames into one big data frame
#called "all_trips".

all_trips<-bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#The collection of data around latitude and longitude, as well as 
#birthyear and gender was halted in 2020. Therefore we are removing 
#those columns from our data set.

all_trips<-all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "X01...Rental.Details.Duration.In.Seconds.Uncapped", "X05...Member.Details.Member.Birthday.Year", "Member.Gender", "tripduration"))

#The next few steps involve cleaning up and adding data to prepare 
#for analysis. I begin by inspecting the new table.

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#It appears there are still some issues to take care of before moving 
#ahead with analysis. I start with, in the "member_casual" column, 
#there are two different names for members ("member" and "Subscriber"), 
#and two different names for casual riders ("Customer" and "casual"). 
#I replace the "Subscriber" with "member", and "Customer" with "casual". 
#First, I look how many observations fall under each usertype.

table(all_trips$member_casual)

#Converting the observations so there's only one for each, "member" 
#and "casual".

all_trips<- all_trips %>%
  mutate(member_casual=recode(member_casual, "Subscriber"="member", "Customer"="casual"))

#To double-check if we have just two values in "member_casual":

table(all_trips$member_casual)

#In order to be able to analyse ride data per month, I need to add 
#columns that list the date, month, day, and yeara of each ride.

all_trips$date<-as.Date(all_trips$started_at)
all_trips$month<-format(as.Date(all_trips$date), "%m")
all_trips$day<-format(as.Date(all_trips$date), "%d")
all_trips$year<-format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week<-format(as.Date(all_trips$date), "%A")

#Adding "ride_length" calculations in seconds to all all_trips

all_trips$ride_length<-difftime(all_trips$ended_at, all_trips$started_at)

#To inspect the structure of the column:

str(all_trips)

#Now, converting "ride_length" from Factor to numeric so we can 
#run calculations on the data.

is.factor(all_trips$ride_length)
all_trips$ride_length<--as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#The following code creates a new version of all_trips without the 
#instances when bikes were taken out of docks and checked for quality
#or when ride_length is negative.

all_trips_v2<-all_trips[!(all_trips$start_station_name=="HQ QR" | all_trips$ride_length<0),]

#The following four codes serve the purpose of conducting descriptive analysis
#on ride_length in seconds, starting with the mean (straight average)

mean(all_trips_v2$ride_length)

#The median:

median(all_trips_v2$ride_length)

#The longest ride:

max(all_trips_v2$ride_length)

#The shortest ride:

min(all_trips_v2$ride_length)

#Compare members and casual users:

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual, FUN = mean)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual, FUN = median)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual, FUN = max)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual, FUN = min)

#To see the average ride time by each day for members vs casual users:

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=median)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=max)

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=min)

#Now, to arrange the days in oreder:

all_trips_v2$day_of_week<-ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Repeat the average calculation:

aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)

#Finally, to analyse ridership data by type and weekday:

all_trips_v2 %>%
  mutate(weekday=wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#Here's for visualizing the number of rides by rider type:
all_trips_v2 %>%
  mutate(weekday=wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position="dodge")

#And for the average duration:

all_trips_v2 %>%
  mutate(weekday=wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position="dodge")
