library(readxl)
library(ggplot2)

df <- read.csv("C:\\Users\\HP\\OneDrive\\Documents\\Service data new.csv")
str(df)


plot_data <- data.frame(
  Time = df[[14]], 
  Rate = df[[20]]
)
plot_data <- na.omit(plot_data)

# 3. CRITICAL STEP: Lock the order of the Time Interval
# This prevents R from sorting alphabetically
plot_data$Time <- factor(plot_data$Time, levels = unique(plot_data$Time))

# 4. Plot
ggplot(plot_data, aes(x = Time, y = Rate, group = 1)) +
  geom_line(color = "green", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  theme_classic() +
  labs(title = "Time Interval vs. Service Rate on Time",
       x = "Time Interval",
       y = "Service Rate on Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))