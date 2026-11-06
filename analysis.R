
# ==========================================
# PROJECT: MOBILE GAMING HABITS AND
# ACADEMIC PERFORMANCE
# ==========================================

# Install ggplot2 if not yet installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load the dataset
gaming_data <- read.csv("mobile_gaming_academic.csv")

# View dataset
View(gaming_data)

# ==========================================
# GRAPH 1: Average Daily Gaming Hours
# ==========================================

gaming_average <- aggregate(
  Daily_Gaming_Hours ~ Gaming_Frequency,
  data = gaming_data,
  FUN = mean
)

graph1 <- ggplot(
  gaming_average,
  aes(x = Gaming_Frequency, y = Daily_Gaming_Hours, fill = Gaming_Frequency)
) +
  geom_col() +
  labs(
    title = "Average Daily Gaming Hours by Gaming Frequency",
    x = "Gaming Frequency",
    y = "Average Daily Gaming Hours"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph1)

ggsave("graph1_gaming_hours.png", graph1, width = 8, height = 5)

# ==========================================
# GRAPH 2: Gaming Hours vs Academic Performance
# ==========================================

graph2 <- ggplot(
  gaming_data,
  aes(x = Daily_Gaming_Hours, y = Academic_Performance_Score)
) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Daily Gaming Hours and Academic Performance",
    x = "Daily Gaming Hours",
    y = "Academic Performance Score"
  ) +
  theme_minimal()

print(graph2)

ggsave("graph2_gaming_vs_performance.png", graph2, width = 8, height = 5)

# ==========================================
# GRAPH 3: Academic Performance by Game Genre
# ==========================================

graph3 <- ggplot(
  gaming_data,
  aes(x = Favorite_Game_Genre, y = Academic_Performance_Score,
      fill = Favorite_Game_Genre)
) +
  geom_boxplot() +
  labs(
    title = "Academic Performance by Favorite Game Genre",
    x = "Favorite Game Genre",
    y = "Academic Performance Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph3)

ggsave("graph3_performance_by_genre.png", graph3, width = 8, height = 5)

# ==========================================
# SUMMARY STATISTICS
# ==========================================

cat("Average Daily Gaming Hours:",
    mean(gaming_data$Daily_Gaming_Hours), "\n")

cat("Average Weekly Study Hours:",
    mean(gaming_data$Weekly_Study_Hours), "\n")

cat("Average Academic Performance Score:",
    mean(gaming_data$Academic_Performance_Score), "\n")

cat("Analysis completed successfully!\n")

