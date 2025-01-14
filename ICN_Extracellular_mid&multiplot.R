# Load necessary libraries
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(tidyr)    # For handling NaN values
library(ggpubr)   # For adding statistical annotations

# Step 1: Load the data
data <- read.csv("~/Documents/Gappapertrialanalysis.csv")  # Replace with the correct file path

# Step 2: Filter rows by Neuron_type and Pulse_number_threshold
filtered_data <- data %>%
  filter(Neuron_type == "ICN", Pulse_number_threshold >= 4)

# Step 3: Normalize columns from No_gap to 6xmulti_gap
normalized_data <- filtered_data %>%
  rowwise() %>%
  mutate(across(No_gap:X6xmulti_gap, ~ ifelse(is.na(.), NA, . / max(c_across(No_gap:X6xmulti_gap), na.rm = TRUE)), .names = "norm_{col}")) %>%
  ungroup()

# Step 4: Subset rows for Plot 1 (No_gap to 3xmid_gap) where all relevant columns have values
plot1_data <- normalized_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmid_gap) & !is.na(norm_X2xmid_gap) & !is.na(norm_X3xmid_gap)) %>%
  select(norm_No_gap, norm_X1xmid_gap, norm_X2xmid_gap, norm_X3xmid_gap)

# Step 5: Subset rows for Plot 2 (No_gap to 3xmulti_gap) where all relevant columns have values
plot2_data <- normalized_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmulti_gap) & !is.na(norm_X2xmulti_gap) & !is.na(norm_X3xmulti_gap)) %>%
  select(norm_No_gap, norm_X1xmulti_gap, norm_X2xmulti_gap, norm_X3xmulti_gap)

# Step 6: Calculate means and SEM for Plot 1
plot1_summary <- plot1_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

# Step 7: Calculate means and SEM for Plot 2
plot2_summary <- plot2_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

# Step 8: Prepare data for ggplot (Plot 1)
plot1_df <- tibble(
  Treatment = c("No_gap", "1xmid_gap", "2xmid_gap", "3xmid_gap"),
  Mean = unlist(plot1_summary[1, seq(1, ncol(plot1_summary), by = 2)]),
  SEM = unlist(plot1_summary[1, seq(2, ncol(plot1_summary), by = 2)])
)

# Step 9: Prepare data for ggplot (Plot 2)
plot2_df <- tibble(
  Treatment = c("No_gap", "1xmulti_gap", "2xmulti_gap", "3xmulti_gap"),
  Mean = unlist(plot2_summary[1, seq(1, ncol(plot2_summary), by = 2)]),
  SEM = unlist(plot2_summary[1, seq(2, ncol(plot2_summary), by = 2)])
)

# Step 10: Create Plot 1
plot1 <- ggplot(plot1_df, aes(x = factor(Treatment, levels = c("No_gap", "1xmid_gap", "2xmid_gap", "3xmid_gap")), y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Plot 1: Mid Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_x_discrete(labels = c("No_gap" = "Standard", "1xmid_gap" = "1x MI", "2xmid_gap" = "2x MI", "3xmid_gap" = "3x MI")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot1_data)), 
           hjust = 1, vjust = 1, size = 5) 

# Step 11: Create Plot 2
plot2 <- ggplot(plot2_df, aes(x = factor(Treatment, levels = c("No_gap", "1xmulti_gap", "2xmulti_gap", "3xmulti_gap")), y = Mean)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # Enlarging and styling x-axis title
    axis.title.y = element_text(size = 16, face = "bold"), # Enlarging and styling y-axis title
    axis.text = element_text(size = 14), # Enlarging x and y-axis text
    axis.line = element_line(color = "black"), # Making axes visible
    panel.grid = element_blank() # Removing grid lines
  ) +
  labs(
    title = "Plot 2: Multi Gaps",
    x = "Inter-pulse interval stimulus type", # Updated x-axis label
    y = "Mean Normalized Response" # Updated y-axis label
  ) +
  scale_x_discrete(labels = c("No_gap" = "Standard", "1xmulti_gap" = "1x AI", "2xmulti_gap" = "2x AI", "3xmulti_gap" = "3x AI")) + # Custom x-axis labels
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + # Set y-axis limit to 1 and ensure bars start at base
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot2_data)), 
           hjust = 1, vjust = 1, size = 5)

# Step 12: Save or display the plots
ggsave("plot1.png", plot = plot1, width = 8, height = 6)
ggsave("plot2.png", plot = plot2, width = 8, height = 6)

# Display plots
print(plot1)
print(plot2)


#Now for LINs
# Subset for LIN neurons and normalize for Plot 3
plot3_data <- data %>%
  filter(Neuron_type == "LIN") %>%
  rowwise() %>%
  mutate(across(No_gap:X6xmid_gap, ~ ifelse(is.na(.), NA, . / max(c_across(No_gap:X6xmid_gap), na.rm = TRUE)), .names = "norm_{col}")) %>%
  ungroup() %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmid_gap) & !is.na(norm_X2xmid_gap) & !is.na(norm_X3xmid_gap) & 
           !is.na(norm_X4xmid_gap) & !is.na(norm_X5xmid_gap) & !is.na(norm_X6xmid_gap))

# Calculate means and SEM for Plot 3
plot3_summary <- plot3_data %>%
  summarise(across(starts_with("norm_"), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

# Prepare data for ggplot (Plot 3)
plot3_df <- tibble(
  Treatment = c("Standard", "1x MI", "2x MI", "3x MI", "4x MI", "5x MI", "6x MI"),
  Mean = unlist(plot3_summary[1, seq(1, ncol(plot3_summary), by = 2)]),
  SEM = unlist(plot3_summary[1, seq(2, ncol(plot3_summary), by = 2)])
)
plot3_df <- plot3_df %>%
  mutate(Treatment = factor(Treatment, levels = c("Standard", "1x MI", "2x MI", "3x MI", "4x MI", "5x MI", "6x MI")))
# Plot 3
plot3 <- ggplot(plot3_df, aes(x = Treatment, y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Plot 3: LIN Mid Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  annotate("text", x = 7.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot3_data)), 
           hjust = 1, vjust = 1, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = list(
    c("Standard", "1x MI"), c("1x MI", "2x MI"), c("2x MI", "3x MI"), 
    c("3x MI", "4x MI"), c("4x MI", "5x MI"), c("5x MI", "6x MI")
  ), size = 4, label.y = 0.9)

# Subset for LIN neurons and normalize for Plot 4
plot4_data <- data %>%
  filter(Neuron_type == "LIN") %>%
  rowwise() %>%
  mutate(across(No_gap:X6xmulti_gap, ~ ifelse(is.na(.), NA, . / max(c_across(No_gap:X6xmulti_gap), na.rm = TRUE)), .names = "norm_{col}")) %>%
  ungroup() %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmulti_gap) & !is.na(norm_X2xmulti_gap) & !is.na(norm_X3xmulti_gap) & 
           !is.na(norm_X4xmulti_gap) & !is.na(norm_X5xmulti_gap) & !is.na(norm_X6xmulti_gap))

# Calculate means and SEM for Plot 4
plot4_summary <- plot4_data %>%
  summarise(
    Mean_No_gap = mean(norm_No_gap, na.rm = TRUE),
    SEM_No_gap = sd(norm_No_gap, na.rm = TRUE) / sqrt(n()),
    Mean_1xmulti_gap = mean(norm_X1xmulti_gap, na.rm = TRUE),
    SEM_1xmulti_gap = sd(norm_X1xmulti_gap, na.rm = TRUE) / sqrt(n()),
    Mean_2xmulti_gap = mean(norm_X2xmulti_gap, na.rm = TRUE),
    SEM_2xmulti_gap = sd(norm_X2xmulti_gap, na.rm = TRUE) / sqrt(n()),
    Mean_3xmulti_gap = mean(norm_X3xmulti_gap, na.rm = TRUE),
    SEM_3xmulti_gap = sd(norm_X3xmulti_gap, na.rm = TRUE) / sqrt(n()),
    Mean_4xmulti_gap = mean(norm_X4xmulti_gap, na.rm = TRUE),
    SEM_4xmulti_gap = sd(norm_X4xmulti_gap, na.rm = TRUE) / sqrt(n()),
    Mean_5xmulti_gap = mean(norm_X5xmulti_gap, na.rm = TRUE),
    SEM_5xmulti_gap = sd(norm_X5xmulti_gap, na.rm = TRUE) / sqrt(n()),
    Mean_6xmulti_gap = mean(norm_X6xmulti_gap, na.rm = TRUE),
    SEM_6xmulti_gap = sd(norm_X6xmulti_gap, na.rm = TRUE) / sqrt(n())
  )

# Prepare data for ggplot (Plot 4)
plot4_df <- tibble(
  Treatment = c("Standard", "1x AI", "2x AI", "3x AI", "4x AI", "5x AI", "6x AI"),
  Mean = unlist(plot4_summary[1, seq(1, ncol(plot4_summary), by = 2)]),
  SEM = unlist(plot4_summary[1, seq(2, ncol(plot4_summary), by = 2)])
)
plot4_df <- plot4_df %>%
  mutate(Treatment = factor(Treatment, levels = c("Standard", "1x AI", "2x AI", "3x AI", "4x AI", "5x AI", "6x AI")))

# Plot 4
plot4 <- ggplot(plot4_df, aes(x = Treatment, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Plot 4: LIN Multi Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  annotate("text", x = 7.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot4_data)), 
           hjust = 1, vjust = 1, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = list(
    c("Standard", "1x AI"), c("1x AI", "2x AI"), c("2x AI", "3x AI"), 
    c("3x AI", "4x AI"), c("4x AI", "5x AI"), c("5x AI", "6x AI")
  ), size = 4, label.y = 0.9)

# Display plots
print(plot3)
print(plot4)
