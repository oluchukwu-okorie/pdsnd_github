# These are the assignments to the three files to be used for the analysis
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

wash[['Start.Time']] <- as.POSIXct(wash[['Start.Time']],format = "%Y-%m-%d %H:%M:%S")
pop_time = data.frame(cities=c('Newyork','Washington','Chicago'))
#A new column is created in order to hold month,day and hour 
#months
ny$Start.month =format(as.Date(ny$Start.Time), "%m")#https://stackoverflow.com/questions/37704212/extract-month-and-year-from-date-in-r/37704385
wash$Start.month =format(as.Date(wash$Start.Time), "%m")
chi$Start.month = format(as.Date(chi$Start.Time), "%m")
#days
ny$Start.day=weekdays(as.Date(ny$Start.Time))
wash$Start.day =weekdays(as.Date(wash$Start.Time))
chi$Start.day=weekdays(as.Date(chi$Start.Time))
#hours
ny$Start.hr= format(as.POSIXct(ny$Start.Time), format = "%H")#https://www.geeksforgeeks.org/how-to-extract-time-from-datetime-in-r/
wash$Start.hr= format(as.POSIXct(wash$Start.Time), format = "%H")
chi$Start.hr= format(as.POSIXct(chi$Start.Time), format = "%H")
#constants are then created for the columns
ny.month=ny$Start.month 
wash.month=wash$Start.month 
chi.month=chi$Start.month
#for the days
ny.day=ny$Start.day
wash.day=wash$Start.day 
chi.day=chi$Start.day
#for the hrs
ny.hr=ny$Start.hr
wash.hr=wash$Start.hr
chi.hr=chi$Start.hr 

#The first function to return the frequent time
pop_Start_Timing = function (time){
    result = tail(names(sort(table(time))), 1)
    return(result)
}
# second fuction to return all the counts of the different time for  trips
freq_timing = function(time){
    y= data.frame(table(time))
    y=y[which.max(y$Freq), ]
    y= y[, -1]
    frequent=y
    result=frequent
    return(result)
}
#Creating the aggregate variables of the combined data frame
pop_time['Most Frequent months'] = c(pop_Start_Timing(ny.month),pop_Start_Timing(wash.month),pop_Start_Timing(chi.month))
pop_time['Counts of the most frequent months'] = c(freq_timing(ny.month),freq_timing(wash.month),freq_timing(chi.month))
pop_time['Most Frequent day of the week'] =  c(pop_Start_Timing(ny.day),pop_Start_Timing(wash.day),pop_Start_Timing(chi.day))
pop_time['Counts of the most frequent day of the week'] = c(freq_timing(ny.day),freq_timing(wash.day),freq_timing(chi.day))
pop_time['Most Frequent hour of the day'] =  c(pop_Start_Timing(ny.hr),pop_Start_Timing(wash.hr),pop_Start_Timing(chi.hr))
pop_time['Counts of the most frequent hour of the day'] = c(freq_timing(ny.hr),freq_timing(wash.hr),freq_timing(chi.hr))
head(pop_time)

counts_duration=subset(pop_time, select=c("cities", "Counts of the most frequent months","Counts of the most frequent day of the week","Counts of the most frequent hour of the day"))

require(tidyr)
counts_duration_long = gather(counts_duration,frequency,counts, -cities)
counts_duration_long

#Since the data contains categorical data and counts(discrete measures),bar charts are the best to represent the data
library(ggplot2)
ggplot(data = counts_duration_long, aes(x = cities, y = counts, fill = frequency)) +
geom_col(position = position_dodge()) +
ggtitle("Counts of the most frequent times of travel\nof three cities in US")+
labs(y="Counts", x = "Cities")+
labs(fill = "Frequencies")

#creating a dataframe to combine all the counts across the three cities
count_stations <- data.frame(cities=c('Newyork','Washington','Chicago'))

#Creating constants for each variable for the start or end stations of each city
ny_start=ny$Start.Station
wash_start=wash$Start.Station
chi_start = chi$Start.Station
ny_end=ny$End.Station
wash_end=wash$End.Station
chi_end = chi$End.Station
#Functions
#a-functions to call the most popular station(start and end for each city)in order to avoid using repeated loops 
Station_topping = function (station){#the stations represent the constant variables depending on which station is chosen for each city?
    result = tail(names(sort(table(station))), 1)#this sorts the stations in descending order and returns the first in the list
    return(result)
}
#b-second function for the most popular trip (most popular start-end station)
Top_stations_combining=function(city){ # the name of the city is imputed
    x=subset(city, select=c("Start.Station", "End.Station"))#first a subset to select all unique trips and frequencies
    y= data.frame(table(x))#converted to dataframe
    y=y[which.max(y$Freq), ]#returns the trip with the highest frequency
    frequent=y
    result=frequent
    return(result)
}
#c-the third fuction to return all the counts of the different modes of stations and trips
top_station_counting = function(station){
    y= data.frame(table(station))
    y=y[which.max(y$Freq), ]
    y= y[, -1]
    frequent=y
    result=frequent
    return(result)
}
#https://stackoverflow.com/questions/18570149/find-most-frequent-combination-of-values-in-a-data-frame
#Using the already created function to create dataframes to store the most frequent
                                               # trips(start,end stations and frequencies)
ny_start_end=Top_stations_combining(ny)
wash_start_end=Top_stations_combining(wash)
chi_start_end = Top_stations_combining(chi)
#variables to store the different counts using the functions
count_stations['Counts of Popular Start Stations'] = c(top_station_counting(ny_start),top_station_counting(wash_start),
                                                       top_station_counting(chi_start) )
count_stations['Counts of Popular End Stations'] = c(top_station_counting(ny_end),top_station_counting(wash_end),
                                                     top_station_counting(chi_end))
for(i in c('ny','wash','chi')){
    count_stations[("Counts of Popular trips")]=c(ny_start_end[1,3],wash_start_end[1,3],chi_start_end[1,3])
}
#to pick the different counts for the modes of stations and trips
count_stations
#Data frame to hold the names of the popular stations and trip
pop_stations = data.frame(cities=c('ny','wash','chi'))
pop_stations['Most Frequent Start.Stations'] = c(Station_topping(ny_start),Station_topping(wash_start),
                                                 Station_topping(chi_start))
pop_stations['Most Frequent End Stations'] = c(Station_topping(ny_end),Station_topping(wash_end),
                                               Station_topping(chi_end) )
for(i in c('ny','wash','chi')){
   pop_stations[("Most Frequent trips")]=c('E 7 St & Avenue A- Cooper Square & E 7 St',
                                           'Jefferson Dr & 14th St SW-Jefferson Dr & 14th St SW',
                                           'Lake Shore Dr & Monroe St-Streeter Dr & Grand Ave')
}
pop_stations

#inorder to create a grouped bar chart,the structure of the data has to change to long format  using tidyr
require(tidyr)
counts_long = gather(count_stations,frequency,counts, -cities)
counts_long

#Since the data contains categorical data and counts(discrete measures),bar charts are the best to represent the data
library(ggplot2)
ggplot(data = counts_long, aes(x = cities, y = counts, fill = frequency)) +
geom_col(position = position_dodge()) +
ggtitle("Counts of the most frequent stations\nof three cities in US")+
labs(y="Counts", x = "Cities")+
labs(fill = "Frequencies")

#creating a dataframe to combine all the counts across the three cities
duration_trips <- data.frame(cities=c('Newyork','Washington','Chicago'))

#Creating constants for each variable for the start or end stations of each city
ny_trip=ny$Trip.Duration
wash_trip=wash$Trip.Duration
chi_trip= chi$Trip.Duration
#Functions
#a-functions to call the total duration in each city in order to avoid using repeated loops 
Trips_add= function(trip){#the stations represent the constant variables depending on which station is chosen for each city?
    y = sum(trip,na.rm=TRUE)
    result =round(y %/% (60*60*24*7)) #https://stackoverflow.com/questions/51694112/r-convert-seconds-in-to-hours-and-minutes 
    return(result)
}

#b-second function for the average trip duration in each city
Trip_averaging= function(trip){#the stations represent the constant variables depending on which station is chosen for each city?
    y = mean(trip,na.rm=TRUE)
    result = round(y %/% 60 )#this sorts the stations in descending order and returns the first in the list
    return(result)
}

#variables to store the different counts using the functions
duration_trips['Total Duration on Trips in weeks'] = c(Trips_add(ny_trip),Trips_add(wash_trip),Trips_add(chi_trip) )
duration_trips['Average of Trips in seconds'] = c(Trip_averaging(ny_trip),Trip_averaging(wash_trip),Trip_averaging(chi_trip))                 
duration_trips

##inorder to create a grouped bar chart,the structure of the data has to change to long format  using tidyr
require(tidyr)
duration_trips_long = gather(duration_trips,duration,time, -cities)
duration_trips_long

#grouped bar charts representing the rounded data
library(ggplot2)#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
ggplot(data = duration_trips_long, aes(x = cities, y = time, fill = duration)) +
geom_col(position = position_dodge()) +
ggtitle("Duration in minutes of total trip duration\nof three cities in US")+
labs(y="Duration(mins)", x = "Cities")+
labs(fill = "Duration")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
