#committed to github
# Load required libraries
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)

# Function to check normality for each analyte

#scale_x_log10()- transform this 
xrf_longer$logConcentration <- log(xrf_longer$Concentration)
normality_results <- xrf_longer %>%
  group_by(Analyte) %>%
  summarise(p_value = shapiro.test(logConcentration)$p.value)

# Determine whether to use parametric or nonparametric test
normal_analytes <- normality_results %>% filter(p_value > 0.05) %>% pull(Analyte)
non_normal_analytes <- normality_results %>% filter(p_value <= 0.05) %>% pull(Analyte)

# Perform ANOVA + Tukey HSD for normal analytes
anova_results <- xrf_longer %>%
  filter(Analyte %in% normal_analytes) %>%
  group_by(Analyte) %>%
  do(tidy(TukeyHSD(aov(Concentration ~ Location, data = .)))) %>%
  rename(group1 = term, group2 = contrast, p_value = adj.p.value) %>%
  select(Analyte, group1, group2, p_value)

# Perform Kruskal-Wallis + Dunn's test for non-normal analytes
kruskal_results <- xrf_longer %>%
  filter(Analyte %in% non_normal_analytes) %>%
  group_by(Analyte) %>%
  kruskal_test(Concentration ~ Location) %>%
  dunn_test(Concentration ~ Location, p.adjust.method = "bonferroni") %>%
  select(Analyte, group1, group2, p.adj) %>%
  rename(p_value = p.adj)

# Combine results from both tests
stat_results <- bind_rows(anova_results, kruskal_results) %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "NS"
  ))

# Generate significance labels
stat_labels <- stat_results %>%
  filter(significance != "NS") %>%
  select(analytes, group1, group2, significance)

# Boxplot with significance labels
ggplot(xrf_longer, aes(x = location, y = concentration, fill = location)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = location), width = 0.2, alpha = 0.5) +
  stat_pvalue_manual(stat_labels, label = "significance", y.position = max(xrf_longer$concentration, na.rm = TRUE) * 1.1) +
  facet_wrap(~ analytes, scales = "free_y") + # Separate plots for each analyte
  theme_minimal() +
  labs(title = "Concentration of Analytes by Location",
       x = "Location",
       y = "Concentration",
       fill = "Location",
       color = "Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels



#New code=====
plot_analytes_with_group_labels <- function(df, analyte_col, value_col, group_col = NULL, output_dir = NULL) {
  
  # Ensure the column names are correct
  analyte_col <- deparse(substitute(analyte_col)) # Extract column name
  value_col <- deparse(substitute(value_col)) # Extract column name
  group_col <- deparse(substitute(group_col)) # Extract column name for grouping
  
  # Get the unique analytes
  analytes <- unique(df[[analyte_col]])
  
  # Create the directory if it doesn't exist
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir) # Create the directory
  }
  
  # Loop through each analyte and generate a plot
  for (analyte in analytes) {
    
    # Subset the data for the current analyte
    analyte_data <- subset(df, df[[analyte_col]] == analyte)
    
    # Check if there is data for the current analyte
    if (nrow(analyte_data) == 0) {
      message(paste("No data for analyte:", analyte))
      next # Skip to the next analyte if no data is found
    }
    
    # Perform pairwise comparison and get the group labels (e.g., "a", "b", "ab")
    comparison <- aov(as.formula(paste(value_col, "~", group_col)), data = analyte_data)
    posthoc <- TukeyHSD(comparison)
    
    # Access the TukeyHSD results correctly
    summary_posthoc <- summary(posthoc)[[1]]  # The first element contains the results matrix for the comparison
    
    # Check if summary_posthoc is a matrix or data frame
    if (is.data.frame(summary_posthoc)) {
      comparison_results <- data.frame(
        group_pair = rownames(summary_posthoc),
        p_value = summary_posthoc[, "p adj"]
      )
    } else {
      comparison_results <- data.frame(
        group_pair = rownames(summary_posthoc),
        p_value = summary_posthoc[, "p adj"]
      )
    }
    
    # Assign group labels based on p-values (using 'a', 'b', 'ab', etc.)
    comparison_results$group_label <- ifelse(comparison_results$p_value < 0.05, "a", "b")
    
    # Generate the plot for this analyte
    plot <- ggplot(analyte_data, aes_string(x = group_col, y = value_col, fill = group_col)) +
      geom_boxplot(outlier.shape = 8, outlier.size = 3, alpha = 0.7) + # Customizing outliers and alpha
      scale_fill_brewer(palette = "Set3") + # Custom color palette
      labs(title = paste("Boxplot for", analyte),
           x = group_col,
           y = value_col) +
      theme_minimal(base_size = 14) + # Use minimal theme with a larger base size
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Center title with bold text
        axis.title = element_text(face = "bold", size = 14), # Bold axis titles
        axis.text = element_text(size = 12), # Size for axis text
        legend.position = "top", # Position the legend on top
        panel.grid.major = element_line(color = "gray90"), # Lighter grid lines for a cleaner look
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.border = element_rect(color = "black", size = 0.5) # Add a border around the plot
      )
    
    # Add group labels (a, b, ab, etc.) to the plot using geom_signif
    for (i in 1:nrow(comparison_results)) {
      group_pair <- comparison_results$group_pair[i]
      group_label <- comparison_results$group_label[i]
      
      # Extract the groups in the pair (split the group_pair string)
      groups <- strsplit(group_pair, "-")[[1]]
      group1 <- groups[1]
      group2 <- groups[2]
      
      # Add significance label to the plot
      plot <- plot +
        geom_signif(
          comparisons = list(c(group1, group2)),
          map_signif_level = TRUE,
          annotations = group_label,
          tip_length = 0.03,
          vjust = 0.5
        )
    }
    
    # Print the plot to the console
    print(plot)
    
    # Optionally save the plot as an image (if output_dir is provided)
    if (!is.null(output_dir)) {
      ggsave(filename = paste(output_dir, paste0(analyte, "_boxplot_group_labels.png"), sep = "/"), plot = plot)
    }
  }
}

plot_analytes_with_group_labels(xrf_longer, analyte_col = Analyte, value_col = Concentration, group_col = Type, output_dir = "plots")



# Replace analyte columns with yours
analyte_cols <- c("As", "Pb", "Cd", "Mn", "U", "Cr", "Cu", "Zn") 
# Function to create plots for each analyte
plot_list <- xrf_longer %>%
  group_split(Analyte) %>%
  map(~{
    ggplot(.x, aes(x = interaction(Location, Type), 
                   y = Concentration, 
                   fill = Type)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.2, size = 1, alpha = 0.8) +
      labs(title = unique(.x$Analyte),
           x = "Location & Type", y = "Concentration") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2") +
      stat_compare_means(aes(group = Type), 
                         method = "wilcox.test", 
                         label = "p.signif", 
                         label.y.npc = "top")
  })
pdf("Analyte_Boxplots.pdf", width = 10, height = 6)
walk(plot_list, print)
dev.off()


library(ggplot2)
library(dplyr)
library(ggpubr)
library(purrr)




plot_list <- xrf_longer %>%
  filter(Analyte %in% analyte_cols) %>%
  group_split(Analyte) %>%
  map(~{
    ggplot(.x, aes(x = interaction(Location, Type, sep = " "),  # Interaction of Location and Type
                   y = Concentration,
                   fill = Type)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8,
                   color = "black", width = 0.6) +
      geom_jitter(aes(color = Type),
                  position = position_jitter(width = 0.2), size = 1.5, alpha = 0.85) +
      geom_signif(
        comparisons = list(c("Background composite", "Mine Impacted composite"), 
                           c("Background surface", "Mine Impacted surface"),
                           c("Background subsurface", "Mine impacted subsurface" ),
                           c( "Mine impacted insitu", "Stream Channel insitu")),  # Interaction levels here
        map_signif_level = TRUE,
        size = 2  # Adjust size for significance labels
      ) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Dark2") +
      labs(
        title = paste("Analyte:", unique(.x$Analyte)),
        x = "Location × Type",
        y = expression("Concentration ("*mu*"g/g)")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "grey90"),
        legend.position = "top"
      )
  })

# Save all plots to PDF
pdf("Analyte_Boxplots_Formattednew.pdf", width = 11, height = 7)
walk(plot_list, print)
dev.off()


# Open PNG device
png("Asconcentration.png", res = 400, height = 14, width = 18, units = "in")

# Find the maximum value for 'As'
max_value <- max(xrf_short$As, na.rm = TRUE)

# Set y-axis limit with a small buffer above the max value (10% buffer in this case)
y_axis_limit <- max_value * 1.1  # Add 10% buffer

# Create the plot
ggplot(xrf_short, aes(x = `Sample ID`, y = As, fill = Location)) +  # Color by Location
  geom_col(position = "stack", width = 0.75) +
  geom_text(aes(x = `Sample ID`, y = As, label = paste(round(As))),
            size = 5, vjust = 0.5, hjust = -0.15) +
  coord_flip(ylim = c(0, y_axis_limit)) +  # Adjust y-axis limit with buffer
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = "Sample ID", y = "Arsenic Concentration (mg/kg)") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Adjusted legend position to the bottom
        axis.text = element_text(size = 16, face = "bold", color= "black"),
        axis.title = element_text(size = 20, face = "bold", color= "black"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.major = element_blank(),
        plot.background = element_rect(color = "darkgreen", fill = "white", size = 1),
        panel.background = element_rect(color = "darkgreen", fill = "white", size = 1),
        legend.text = element_text(size = 12),  # Increase legend text size
        legend.title = element_text(size = 12)) +  # Adjust legend title size if necessary
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(name = "Regulatory Screening Levels", values = c(
    "ADEQ As Soil Remediation Levels (10 mg/kg)" = "purple")) +
  geom_hline(aes(yintercept = 10, color = "ADEQ As Soil Remediation Levels (10 mg/kg)"),
             linewidth = 1, show.legend = TRUE) +
  guides(color = guide_legend(title = "Regulatory Screening Levels")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# Close the device
dev.off()


png("Pbconcentration.png", res = 400, height = 14, width = 18, units = "in")

# Find the maximum value for 'As'
max_value <- max(xrf_short$Pb, na.rm = TRUE)

# Set y-axis limit with a small buffer above the max value (10% buffer in this case)
y_axis_limit <- max_value * 1.1  # Add 10% buffer

# Create the plot
ggplot(xrf_short, aes(x = `Sample ID`, y = Pb, fill = Location)) +  # Color by Location
  geom_col(position = "stack", width = 0.75) +
  geom_text(aes(x = `Sample ID`, y = Pb, label = paste(round(Pb))),
            size = 5, vjust = 0.5, hjust = -0.15) +
  coord_flip(ylim = c(0, y_axis_limit)) +  # Adjust y-axis limit with buffer
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = "Sample ID", y = "Lead Concentration (mg/kg)") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Adjusted legend position to the bottom
        axis.text = element_text(size = 16, face = "bold", color= "black"),
        axis.title = element_text(size = 20, face = "bold", color= "black"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.grid.major = element_blank(),
        plot.background = element_rect(color = "darkgreen", fill = "white", size = 1),
        panel.background = element_rect(color = "darkgreen", fill = "white", size = 1),
        legend.text = element_text(size = 12),  # Increase legend text size
        legend.title = element_text(size = 12)) +  # Adjust legend title size if necessary
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(name = "Regulatory Screening Levels", values = c(
    "ADEQ Pb Soil Remediation Levels (2000 mg/kg)" = "purple")) +
  geom_hline(aes(yintercept = 2000, color = "ADEQ Pb Soil Remediation Levels (2000 mg/kg)"),
             linewidth = 1, show.legend = TRUE) +
  guides(color = guide_legend(title = "Regulatory Screening Levels")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# Close the device
dev.off()


