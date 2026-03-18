library(ecotourism)
library(dplyr)
library(lubridate)
library(ggplot2)
#For Hard Task: I had to write a function that takes occurrence and weather data and predicts the top five days and times to spot an organism.

#Load the data
data("glowworms")
data("weather")

#Join occurrence with weather and add month
joined <- glowworms |>
  left_join(weather, by = c("date", "ws_id")) |>
  mutate(month = month(date))

#Count sightings per date-hour combination
hourly_counts <- joined |>
  filter(!is.na(hour), !is.na(temp), !is.na(prcp), !is.na(wind_speed), !is.na(dewp)) |>
  group_by(date, ws_id, hour, temp, prcp, wind_speed, dewp, month) |>
  summarise(n_sightings = n(), .groups = "drop")

#Fit Poisson GLM [Poisson GLM because we're counting sightings — counts can't be negative or fractional]
model <- glm(
  n_sightings ~ temp + prcp + wind_speed + dewp + month + hour,
  data   = hourly_counts,
  family = poisson(link = "log")
)
summary(model)

#Predict and extract top 5 days and times
hourly_counts$predicted <- predict(model, type = "response")

top5 <- hourly_counts |>
  arrange(desc(predicted)) |>
  select(date, hour, temp, prcp, n_sightings, predicted) |>
  slice_head(n = 5)

print(top5)

#Visualisation
ggplot(top5, aes(x = paste0(date, "  |  ", hour, ":00h"), y = predicted)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 5 Predicted Times to Spot Glowworms",
    x = "Date & Hour",
    y = "Predicted Sightings"
  ) +
  theme_minimal()
