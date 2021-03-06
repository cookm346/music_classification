# install.packages("spotifyr")
library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = "")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "")

access_token <- get_spotify_access_token()

artists <- read.csv("artist_data.csv", stringsAsFactors = FALSE)
artists <- artists[artists$genre %in% c("rap", "folk", "jazz", "rock", "electronic"), ]

for(i in 1:nrow(artists)){
    data <- get_artist_audio_features(artists$name[i])
    data$genre <- artists$genre[i]
    data$album_images <- data$artists <- data$available_markets <- NULL
    artist_name <- gsub("[^[:alnum:] ]", " ", artists$name[i])
    write.csv(data, paste0("./data/", artist_name, ".csv"), row.names = FALSE)
    
    Sys.sleep(10)
}
