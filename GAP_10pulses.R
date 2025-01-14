# Load the necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Create the data frame
data <- data.frame(
  Test = c("Control", 
           "1x Mid", 
           "2x Mid", 
           "1x Multi", 
           "2x Multi"),
  Response = c(20, 13, 10, 7, 1)
)

# Create a contingency table
contingency_table <- data %>%
  select(-Test) %>%
  as.matrix()

# Perform the Chi-Square Test for Independence
chi_square_test <- chisq.test(contingency_table)

# Print the results of the Chi-Square Test
print(chi_square_test)

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


# Plotting
p <- ggplot(plot_data, aes(x = Test, y = Count, fill = Response_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_y_continuous(limits = c(0,22), expand = c(0, 0)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(
    values = color_palette[1:2],  # Use the first two colors from the palette
  ) + # Color-blind-friendly colors
  labs(
    x = "Stimuli Presentation",
    y = "Number of females approaching the Stimulus",
    subtitle = paste("Chi-Square Test p-value:", round(chi_square_test$p.value, 5))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.9, vjust = -5, size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(), # Remove grid
    panel.grid.minor = element_blank(), # Remove grid
    axis.line.x = element_line(color = "black"), # Add x-axis line
    axis.line.y = element_line(color = "black"), # Add y-axis line
    axis.ticks = element_line(color = "black"),
    legend.position="none"
  )


# Display the plot
print(p)
