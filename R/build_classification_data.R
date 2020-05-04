datasets <- list.files(path = "./data")

all_data <- NULL

for(i in 1:length(datasets)){
    data <- read.csv(paste0("./data/", datasets[i]))
    data <- data[ , c("artist_name", "track_name", "danceability", "energy", 
                      "loudness", "speechiness", "acousticness", 
                      "instrumentalness", "liveness", "valence", 
                      "tempo", "time_signature", "duration_ms", "genre")]
    
    all_data <- rbind(all_data, data)
}

write.csv(all_data, "all_data.csv", row.names = FALSE)
