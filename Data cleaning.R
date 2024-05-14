#Read outlier data
selected_columns <- c("valence", "acousticness", "danceability", "energy", "explicit", "instrumentalness", "liveness", "loudness", "popularity", "speechiness", "tempo")

selected_data <- data %>%
  select(all_of(selected_columns))

boxplot_data <- pivot_longer(selected_data, everything(), names_to = "Column", values_to = "Value")

ggplot(boxplot_data, aes(x = Column, y = Value)) +
  geom_boxplot() +
  labs(x = "Column", y = "Value") +
  ggtitle("Boxplot for Selected Columns")



#Counting and removing outlier data
Q1 <- quantile(data$<colname>, 0.25)
Q3 <- quantile(data$<colname>, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outlier_count <- sum(data$<colname> < lower_bound | data$<colname> > upper_bound)
print("<colname> outlier count")
outlier_count

outlier_data <- data[data$<colname> < lower_bound | data$<colname> > upper_bound, ]
data <- data[!data$<colname> %in% outlier_data$<colname>, ]
