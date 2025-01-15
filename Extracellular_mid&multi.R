# Load necessary libraries
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(tidyr)    # For handling NaN values
library(ggpubr)   # For adding statistical annotations

# Load the data

data <- read.csv("~/Documents/Gappapertrialanalysis.csv")  # Replace with the correct file path

# Filter rows by Neuron_type

filtered_ICN_data <- data %>%
  filter(Neuron_type == "ICN", Pulse_number_threshold >= 4)

filtered_LIN_data <- data %>%
  filter(Neuron_type == "LIN") 

#Normalize columns

normalized_ICN_data <- filtered_ICN_data %>%
  rowwise() %>%
  mutate(across(No_gap:X6xmulti_gap, ~ ifelse(is.na(.), NA, . / max(c_across(No_gap:X6xmulti_gap), na.rm = TRUE)), .names = "norm_{col}")) %>%
  ungroup()

normalized_LIN_data <- filtered_LIN_data %>%
  rowwise() %>%
  mutate(across(No_gap:X6xmulti_gap, ~ ifelse(is.na(.), NA, . / max(c_across(No_gap:X6xmulti_gap), na.rm = TRUE)), .names = "norm_{col}")) %>%
  ungroup() 

# Subset rows for diferent plots
#Plot1 : ICN mid
#Plot2 : ICN multi
#Plot3 : LIN mid
#Plot4 : LIN multi

plot1_data <- normalized_ICN_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmid_gap) & !is.na(norm_X2xmid_gap) & !is.na(norm_X3xmid_gap)) %>%
  select(norm_No_gap, norm_X1xmid_gap, norm_X2xmid_gap, norm_X3xmid_gap)

plot2_data <- normalized_ICN_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmulti_gap) & !is.na(norm_X2xmulti_gap) & !is.na(norm_X3xmulti_gap)) %>%
  select(norm_No_gap, norm_X1xmulti_gap, norm_X2xmulti_gap, norm_X3xmulti_gap)

plot3_data <- normalized_LIN_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmid_gap) & !is.na(norm_X2xmid_gap) & !is.na(norm_X3xmid_gap) & !is.na(norm_X4xmid_gap) & !is.na(norm_X5xmid_gap)& !is.na(norm_X6xmid_gap)) %>%
  select(norm_No_gap, norm_X1xmid_gap, norm_X2xmid_gap, norm_X3xmid_gap, norm_X4xmid_gap, norm_X5xmid_gap, norm_X6xmid_gap)

plot4_data <- normalized_LIN_data %>%
  filter(!is.na(norm_No_gap) & !is.na(norm_X1xmulti_gap) & !is.na(norm_X2xmulti_gap) & !is.na(norm_X3xmulti_gap) & !is.na(norm_X4xmulti_gap) & !is.na(norm_X5xmulti_gap)& !is.na(norm_X6xmulti_gap)) %>%
  select(norm_No_gap, norm_X1xmulti_gap, norm_X2xmulti_gap, norm_X3xmulti_gap, norm_X4xmulti_gap, norm_X5xmulti_gap, norm_X6xmulti_gap)

# Function to add significance stars without "ns"

add_significance_no_ns <- function(data_long, mean_standard) {
  data_long %>%
    group_by(Treatment) %>%
    summarise(
      p_value = t.test(Normalized_Response, mu = mean_standard)$p.value, # Compare with Standard mean
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ NA_character_ # Replace "ns" with NA to avoid plotting it
      )
    )
}

# Calculate means and SEM for each plot

plot1_summary <- plot1_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

plot2_summary <- plot2_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

plot3_summary <- plot3_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

plot4_summary <- plot4_data %>%
  summarise(across(everything(), list(mean = mean, sem = ~ sd(.) / sqrt(n()))))

# Prepare data for ggplot

plot1_df <- tibble(
  Treatment = c("Standard", "1x MI", "2x MI", "3x MI"),
  Mean = unlist(plot1_summary[1, seq(1, ncol(plot1_summary), by = 2)]),
  SEM = unlist(plot1_summary[1, seq(2, ncol(plot1_summary), by = 2)])
)

plot2_df <- tibble(
  Treatment = c("Standard", "1x AI", "2x AI", "3x AI"),
  Mean = unlist(plot2_summary[1, seq(1, ncol(plot2_summary), by = 2)]),
  SEM = unlist(plot2_summary[1, seq(2, ncol(plot2_summary), by = 2)])
)

plot3_df <- tibble(
  Treatment = c("Standard", "1x MI", "2x MI", "3x MI", "4x MI", "5x MI", "6x MI"),
  Mean = unlist(plot3_summary[1, seq(1, ncol(plot3_summary), by = 2)]),
  SEM = unlist(plot3_summary[1, seq(2, ncol(plot3_summary), by = 2)])
)

plot4_df <- tibble(
  Treatment = c("Standard", "1x AI", "2x AI", "3x AI", "4x AI", "5x AI", "6x AI"),
  Mean = unlist(plot4_summary[1, seq(1, ncol(plot4_summary), by = 2)]),
  SEM = unlist(plot4_summary[1, seq(2, ncol(plot4_summary), by = 2)])
)

mean_standard_plot1 <- mean(plot1_data$norm_No_gap, na.rm = TRUE)
mean_standard_plot2 <- mean(plot2_data$norm_No_gap, na.rm = TRUE)
mean_standard_plot3 <- mean(plot3_data$norm_No_gap, na.rm = TRUE)
mean_standard_plot4 <- mean(plot4_data$norm_No_gap, na.rm = TRUE)

plot1_long <- plot1_data %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Normalized_Response") %>%
  mutate(Treatment = recode(Treatment,
                            norm_No_gap = "Standard",
                            norm_X1xmid_gap = "1x MI",
                            norm_X2xmid_gap = "2x MI",
                            norm_X3xmid_gap = "3x MI"))

plot2_long <- plot2_data %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Normalized_Response") %>%
  mutate(Treatment = recode(Treatment,
                            norm_No_gap = "Standard",
                            norm_X1xmulti_gap = "1x AI",
                            norm_X2xmulti_gap = "2x AI",
                            norm_X3xmulti_gap = "3x AI"))

plot3_long <- plot3_data %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Normalized_Response") %>%
  mutate(Treatment = recode(Treatment,
                            norm_No_gap = "Standard",
                            norm_X1xmid_gap = "1x MI",
                            norm_X2xmid_gap = "2x MI",
                            norm_X3xmid_gap = "3x MI",
                            norm_X4xmid_gap = "4x MI",
                            norm_X5xmid_gap = "5x MI",
                            norm_X6xmid_gap = "6x MI"))

plot4_long <- plot4_data %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Normalized_Response") %>%
  mutate(Treatment = recode(Treatment,
                            norm_No_gap = "Standard",
                            norm_X1xmulti_gap = "1x AI",
                            norm_X2xmulti_gap = "2x AI",
                            norm_X3xmulti_gap = "3x AI",
                            norm_X4xmulti_gap = "4x AI",
                            norm_X5xmulti_gap = "5x AI",
                            norm_X6xmulti_gap = "6x AI"))

pairwise_results_plot1 <- add_significance_no_ns(plot1_long %>% filter(Treatment != "Standard"), mean_standard_plot1)
pairwise_results_plot2 <- add_significance_no_ns(plot2_long %>% filter(Treatment != "Standard"), mean_standard_plot2)
pairwise_results_plot3 <- add_significance_no_ns(plot3_long %>% filter(Treatment != "Standard"), mean_standard_plot3)
pairwise_results_plot4 <- add_significance_no_ns(plot4_long %>% filter(Treatment != "Standard"), mean_standard_plot4)

# Merge `pairwise_results` with `plot_df` to get Mean and SEM

pairwise_results_plot1 <- pairwise_results_plot1 %>%
  left_join(plot1_df, by = c("Treatment"))

pairwise_results_plot2 <- pairwise_results_plot2 %>%
  left_join(plot2_df, by = c("Treatment"))

pairwise_results_plot3 <- pairwise_results_plot3 %>%
  left_join(plot3_df, by = c("Treatment"))

pairwise_results_plot4 <- pairwise_results_plot4 %>%
  left_join(plot4_df, by = c("Treatment"))

# Create Plot1

plot1 <- ggplot(plot1_df, aes(x = factor(Treatment, levels = c("Standard", "1x MI", "2x MI", "3x MI")), y = Mean)) +
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
  scale_x_discrete(labels = c("Standard" = "Standard", "1x MI" = "1x MI", "2x MI" = "2x MI", "3x MI" = "3x MI")) +
  scale_y_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot1_data)), 
           hjust = 1, vjust = 1, size = 5) +
  geom_text(data = pairwise_results_plot1, aes(x = Treatment, y = Mean + SEM + 0.05, label = significance),
            size = 5, fontface = "bold") 

print(plot1)

# Create Plot2

plot2 <- ggplot(plot2_df, aes(x = factor(Treatment, levels = c("Standard", "1x AI", "2x AI", "3x AI")), y = Mean)) +
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
    title = "Plot 2: Multi Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_x_discrete(labels = c("Standard" = "Standard", "1x AI" = "1x AI", "2x AI" = "2x AI", "3x AI" = "3x AI")) +
  scale_y_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot2_data)), 
           hjust = 1, vjust = 1, size = 5) +
  geom_text(data = pairwise_results_plot2, aes(x = Treatment, y = Mean + SEM + 0.05, label = significance),
            size = 5, fontface = "bold") 

print(plot2)

# Create Plot3

plot3 <- ggplot(plot3_df, aes(x = factor(Treatment, levels = c("Standard", "1x MI", "2x MI", "3x MI", "4x MI", "5x MI", "6x MI")), y = Mean)) +
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
    title = "Plot 3: Mid Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_x_discrete(labels = c("Standard" = "Standard", "1x MI" = "1x MI", "2x MI" = "2x MI", "3x MI" = "3x MI", "4x MI" = "4x MI", "5x MI" = "5x MI", "6x MI" = "6x MI")) +
  scale_y_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot3_data)), 
           hjust = 1, vjust = 1, size = 5) +
  geom_text(data = pairwise_results_plot3, aes(x = Treatment, y = Mean + SEM + 0.05, label = significance),
            size = 5, fontface = "bold") 

print(plot3)

# Create Plot4

plot4 <- ggplot(plot4_df, aes(x = factor(Treatment, levels = c("Standard", "1x AI", "2x AI", "3x AI", "4x AI", "5x AI", "6x AI")), y = Mean)) +
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
    title = "Plot 4: Multi Gaps",
    x = "Inter-pulse interval stimulus type",
    y = "Mean Normalized Response"
  ) +
  scale_x_discrete(labels = c("Standard" = "Standard", "1x AI" = "1x AI", "2x AI" = "2x AI", "3x AI" = "3x AI", "4x AI" = "4x AI", "5x AI" = "5x AI", "6x AI" = "6x AI")) +
  scale_y_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  annotate("text", x = 4.5, y = 1, 
           label = paste0("Total sample size: ", nrow(plot4_data)), 
           hjust = 1, vjust = 1, size = 5) +
  geom_text(data = pairwise_results_plot4, aes(x = Treatment, y = Mean + SEM + 0.05, label = significance),
            size = 5, fontface = "bold") 

print(plot4)
