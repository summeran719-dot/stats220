library(tidyverse)


csv_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRbOviXondmj168y3_6LPF_RSh00aQlFdERVjgg3t_4knTCD-vK9eObUbdlnKZZXRrv4Ehc9C5Z5K5k/pub?output=csv"
logged_data <- read_csv(csv_url)

# Part D: 
# rename
latest_data <- logged_data %>%
  rename(
    timestamp = 1,
    video_type = 2,
    video_title = 3,
    duration_min = 4,
    device = 5,
    focus_level = 6,
    quality_rating = 7
  )



# Summary values
total_videos <- nrow(latest_data)
mean_duration <- mean(latest_data$duration_min, na.rm = TRUE)
median_focus <- median(latest_data$focus_level, na.rm = TRUE)

# Bar charts
# attempt 1:View Count Statistics by Platform/Content Type
latest_data %>%
  ggplot(aes(x = video_type, fill = video_type)) +
  geom_bar() +
  labs(
    title = "Count of Videos by Content Type",
    x = "Video Type",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# attempt 2: Average Subjective Audiovisual Quality Scores for Different Devices (Using geom_col)
latest_data %>%
  group_by(device) %>%
  summarise(avg_quality = mean(quality_rating, na.rm = TRUE)) %>%
  ggplot(aes(x = device, y = avg_quality, fill = device)) +
  geom_col() +
  labs(
    title = "Average Audiovisual Quality Rating by Device",
    x = "Device Used",
    y = "Average Rating (1-5)"
  ) +
  theme_minimal()

# ---The final code used for dynamic reporting---


# total_obs <- nrow(latest_data)
# avg_duration_report <- round(mean(latest_data$duration_min, na.rm = TRUE), 1)
# 

# plot_1 <- ggplot(latest_data, aes(x = video_type)) +
#   geom_bar(fill = "steelblue") +
#   labs(title = "Observation Count by Video Type", x = "Content Type", y = "Count") +
#   theme_minimal()
# 
# plot_2 <- ggplot(latest_data, aes(x = as.factor(quality_rating))) +
#   geom_bar(fill = "coral") +
#   labs(title = "Distribution of Audiovisual Quality Ratings", x = "Rating (1-5)", y = "Count") +
#   theme_minimal()