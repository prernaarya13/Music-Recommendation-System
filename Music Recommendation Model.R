Sys.setenv(SPOTIFY_CLIENT_ID = "your_spotify_client_id")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "your_spotify_client_secret")

sp <- get_spotify_access_token()

number_cols <- c('valence', 'year', 'acousticness', 'danceability', 'duration_ms', 'energy', 'explicit',
                 'instrumentalness', 'key', 'liveness', 'loudness', 'mode', 'popularity', 'speechiness', 'tempo')
find_song <- function(name, year) {
  #define dataframe to save the input song
  song_data <- data.frame(
    name = character(),
    year = numeric(),
    explicit = numeric(),
    duration_ms = numeric(),
    popularity = numeric()
  )
  
  results <- search_spotify(q = paste0("track:", name, " year:", year), type = "track", limit = 1)
  
  if (nrow(results) == 0) {
    return(NULL)
  }
  
  result <- results[1, ]
  track_id <- result$id
  audio_features <- get_audio_features(track_id)
  
  song_data$name <- name
  song_data$year <- year
  song_data$explicit <- as.integer(result$explicit)
  song_data$duration_ms <- result$duration_ms
  song_data$popularity <- result$popularity
  
  for (key in names(audio_features)) {
    song_data[[key]] <- audio_features[[key]]
  }
  
  return(song_data)
}

result_df <- find_song("song_name", "year")


get_song_data <- function(song, spotify_data) {
  tryCatch({
    song_data <- spotify_data %>%
      filter(name == song$name & year == song$year) %>%
      slice(1)
    song_data
  }, error = function(e) {
    find_song(song$name, as.character(song$year))
  })
}


get_mean_vector <- function(song_list, spotify_data) {
  song_vectors <- list()
  
  for (i in 1:length(song_list)) {
    song_data <- get_song_data(song_list[[i]], spotify_data)
    
    if (is.null(song_data)) {
      warning(paste0('Warning: ', song_list[[i]]$name, ' does not exist in Spotify or in the database'))
      next
    }
    
    song_vector <- song_data[number_cols]
    song_vectors[[i]] <- song_vector
  }
  
  song_matrix <- do.call(rbind, song_vectors)
  return(colMeans(song_matrix))
}


flatten_dict_list <- function(dict_list) {
  flattened_dict <- list()
  
  for (key in names(dict_list[[1]])) {
    flattened_dict[[key]] <- vector()
  }
  
  for (dictionary in dict_list) {
    for (key in names(dictionary)) {
      flattened_dict[[key]] <- c(flattened_dict[[key]], dictionary[[key]])
    }
  }
  
  return(flattened_dict)
}


recommend_songs <- function(song_list, spotify_data, n_songs = 10) {
  metadata_cols <- c('name', 'year', 'artists')
  song_dict <- flatten_dict_list(song_list)
  
  song_center <- get_mean_vector(song_list, spotify_data)
  scaled_data <- predict(song_cluster_pipelineY$steps$step_scale, spotify_data[, number_cols])
  scaled_song_center <- predict(song_cluster_pipelineY$steps$step_scale, as.data.frame(t(song_center)))
  
  distances <- sapply(1:nrow(scaled_data), function(i) {
    stringdist::stringdist(as.character(scaled_song_center), as.character(scaled_data[i, ]), method = "cosine")
  })
  index <- order(distances)[1:n_songs]
  
  rec_songs <- spotify_data[index, ]
  rec_songs <- rec_songs[!rec_songs$name %in% song_dict$name, ]
  return(rec_songs[, metadata_cols])
}

# Define the song_list
song_list <- list(
  list(name = 'Centuries', year = 2015),
  list(name = 'Play Date', year = 2015),
  list(name = 'The Less I Know The Better', year = 2015),
  list(name = 'Immortals', year = 2015)
)

# Call the recommend_songs function
recommendations <- recommend_songs(song_list, data, n_songs = 5)
print(recommendations)
