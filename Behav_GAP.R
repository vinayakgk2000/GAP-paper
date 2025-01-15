# Load the necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)

# Create the data frame
data <- data.frame(
  Test = c("Control", 
           "1x Mid", 
           "2x Mid", 
           "1x Multi", 
           "2x Multi"),
  Response = c(20, 13, 10, 7, 1)
)

# Calculate the proportion of successes in the Control group
control_proportion <- data$Response[data$Test == "Control"] / 20

# Perform pairwise binomial tests between Control and other groups
pairwise_results <- data %>%
  filter(Test != "Control") %>%
  mutate(
    p_value = sapply(Test, function(group) {
      # Ensure valid group-specific response exists
      test_response <- data$Response[data$Test == group]
      if (length(test_response) == 0) {
        return(NA) # Avoid errors if no matching group
      }
      binom.test(
        x = test_response,  # Successes in the test group
        n = 20,             # Total trials in the test group
        p = control_proportion # Proportion of successes in Control
      )$p.value
    }),
    significance = case_when(
      p_value < 0.001 ~ "***",  # Highly significant
      p_value < 0.01 ~ "**",    # Significant
      p_value < 0.05 ~ "*",     # Marginally significant
      TRUE ~ "ns"               # Not significant
    )
  )

# Print pairwise results for verification
print(pairwise_results)

# Ensure the correct order of bars (Control first)
data$Test <- factor(data$Test, levels = c("Control", "1x Mid", "2x Mid", "1x Multi", "2x Multi"))

# Melt the data into a long format for ggplot2
plot_data <- data %>%
  gather(key = "Response_Type", value = "Count", -Test) %>%
  filter(!is.na(Count))

# Ensure the order of Test levels is consistent
plot_data$Test <- factor(plot_data$Test, levels = c(
  "Control", 
  "1x Mid", 
  "2x Mid", 
  "1x Multi", 
  "2x Multi"
))

# Define a 6-color blind-friendly palette
color_palette <- brewer.pal(6, "Set3")


# Plot with grouped significance for 1x Mid, 2x Mid, 1x Multi, 2x Multi vs Control
p <- ggplot(data, aes(x = Test, y = Response)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_text(aes(label = Response), vjust = -1.2, size = 5, fontface = "bold") + # Add response counts
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0)) + # Adjust y-axis limits for the segment
  labs(
    x = "Stimuli Presentation",
    y = "Number of Females Approaching the Stimulus"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) +
  # Add a curly bracket for the grouped bars (1x Mid, 2x Mid, 1x Multi, 2x Multi)
  geom_segment(aes(x = 1.5, xend = 5.5, y = 24, yend = 24), size = 0.8) + # Horizontal line for the grouped bars
  geom_segment(aes(x = 1.5, xend = 1.5, y = 24, yend = 23), size = 0.8) + # Left vertical line
  geom_segment(aes(x = 5.5, xend = 5.5, y = 24, yend = 23), size = 0.8) + # Right vertical line
  geom_text(aes(x = 3.5, y = 26, label = "***"), size = 6, fontface = "bold") # Add stars above the comparison

# Display the plot
print(p)
