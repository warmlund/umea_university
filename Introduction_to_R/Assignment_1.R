#####-----Title------####
# Introduction to R - Assignment 1



#####-----Q1------####
mtcars <- get(data("mtcars"))

car_countries <- c("Japan", "Japan", "Japan", "USA", "USA",
                   "USA", "USA", "Germany", "Germany", "Germany", "Germany",
                   "Germany", "Germany", "Germany", "USA",
                   "USA", "USA", "Italy", "Japan",
                   "Japan", "Japan", "USA", "USA",
                   "USA", "USA", "Italy", "Germany", "UK",
                   "USA", "Italy", "Italy", "Sweden")


mtcars$car_countries <- car_countries

average_mpg <- mean(mtcars$mpg)

below_average <- c()
above_average <- c()

for(i in 1:length(mtcars$mpg)){
  if(mtcars$mpg[i] <= average_mpg){
    below_average <- c(below_average,rownames(mtcars)[i])
  }else{
    above_average <- c(above_average,rownames(mtcars)[i])
  }
}


#####-----Q2------####

USA_cars <- mtcars |> filter(mtcars$car_countries == "USA")
Japan_cars <- mtcars |> filter(mtcars$car_countries == "Japan")


#####-----Q3------####

mpg_per_cyl_USA <- mtcars[mtcars$car_countries == "USA",]
mpg_per_cyl_USA <- as.numeric(mean( mpg_per_cyl_USA$mpg / mpg_per_cyl_USA$cyl))

mpg_per_cyl_Japan <- mtcars[mtcars$car_countries == "Japan",] 
mpg_per_cyl_Japan <- as.numeric(mean( mpg_per_cyl_Japan$mpg / mpg_per_cyl_Japan$cyl))

#####-----Q4------####

#install.packages('billboard') 

#You need to run the install only once.
# Comment it out by removing the #, then comment it out again 

library(billboard)
spotify_track_data <-  get(data("spotify_track_data"))
print(head(spotify_track_data))
dim(spotify_track_data)

my_playlist <- spotify_track_data[spotify_track_data$artist_name %in% c("Rihanna", "Michael Jackson", "Eminem", "Elvis Presley" ),]

#####-----Q5------####

median_danceability <- median(my_playlist$danceability)
dance_tracks <- my_playlist[my_playlist$danceability > median_danceability,]

#####-----Q6------####
all_my_playlist_tracks <- length(my_playlist$track_name)
Rihanna_dance_tracks <- length(dance_tracks$artist_name[dance_tracks$artist_name == "Rihanna"]) / all_my_playlist_tracks

#####-----Q7------####

# Note: Do not alter the original spotify_track_data dataset!
# You should alter only the corrected_playlist dataset.

corrected_playlist <- spotify_track_data
corrected_playlist$danceability[corrected_playlist$artist_name == "Michael Jackson"] <- corrected_playlist$danceability[corrected_playlist$artist_name == "Michael Jackson"] - 0.05



