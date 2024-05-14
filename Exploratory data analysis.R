#Ploting various features against ppopularity
feature_names <- c('acousticness', 'danceability', 'energy', 'instrumentalness',
                   'liveness', 'loudness', 'speechiness', 'tempo', 'valence', 'duration_ms', 'explicit', 'key', 'mode', 'year')

X <- data[feature_names]
y <- data$popularity

features <- as.data.frame(t(as.matrix(feature_names)))

cor_matrix <- cor(X)

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = FALSE, lab_size = 5)

options(repr.plot.width = 8, repr.plot.height = 8)


#Coorelation matrix for all parameters
cor_matrix <- cor(X)

cor_df <- reshape2::melt(cor_matrix)

ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  labs(x = "", y = "") +
  theme_minimal()


#Distribution of Music point over years
data_per_decade <- data.frame(table(data$year))

options(repr.plot.width = 11, repr.plot.height = 6)

ggplot(data_per_decade, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Count") +
  theme_minimal()


#Sound features over the years
sound_features <- c('acousticness', 'danceability', 'energy', 'instrumentalness', 'liveness', 'valence')

year_data_long <- year_data %>% gather(key = "feature", value = "value", one_of(sound_features))
options(repr.plot.width = 11, repr.plot.height = 6)
fig <- plot_ly(year_data_long, x = ~year, y = ~value, color = ~feature, type = 'scatter', mode = 'lines')

fig %>% layout(title = "Sound Features Over the Years", xaxis = list(title = "Year"), yaxis = list(title = "Value"))


#Genre Analysis
top30_genres <- head(genre_data[order(genre_data$popularity, decreasing = TRUE), ], 30)

ggplot(top30_genres, aes(x = reorder(genres, -popularity), y = popularity)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Genres", y = "Popularity") +
  ggtitle("Top 30 Genres by Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Characteristics of Top 10 Different Genres
top10_genres <- head(genre_data[order(genre_data$popularity, decreasing = TRUE), ], 10)

options(repr.plot.width = 11, repr.plot.height = 6)
fig <- plot_ly(top10_genres, x = ~genres) %>%
  add_trace(y = ~valence, name = 'Valence', type = 'bar', marker = list(color = 'blue')) %>%
  add_trace(y = ~energy, name = 'Energy', type = 'bar', marker = list(color = 'green')) %>%
  add_trace(y = ~danceability, name = 'Danceability', type = 'bar', marker = list(color = 'orange')) %>%
  add_trace(y = ~acousticness, name = 'Acousticness', type = 'bar', marker = list(color = 'purple')) %>%
  layout(barmode = 'group')

fig
