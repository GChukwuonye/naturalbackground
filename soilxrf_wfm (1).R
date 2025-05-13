#Author: Gift Chukwuonye
#Date:March 26 2025
#Project Description: XRF validation project for watershed improvement and the validation of the SOP for establishing natural background concentration
#for more information, contact Gift on chukwuonye.gift@azdeq.gov 
#committed to github

#all libraries====
library(readxl)
library(tidyverse)
library(table1)
library(dplyr)
library(writexl)

#set working directory====
#setwd("\\Users\\237708\\Downloads")
setwd("/Users/Gift/Downloads")

xrf <- read_excel("WFM_XRF_Worksheet.xlsx", 
                                sheet = "Data")
#View(xrf)

#data manipulation====
xrf<- xrf %>%
  mutate(across(everything(), ~ifelse(. =="ND", 0, .)))

xrf<- xrf %>%
  mutate(across(4:37, as.numeric ))

xrf_short<- xrf %>%
  select(1:3, 7, 10, 23, 25, 28, 31, 32, 36)
xrf_longer<- xrf_short %>%
  pivot_longer(4:11,
               names_to = "Analyte",
               values_to = "Concentration")
xrf_longer<- as.data.frame(xrf_longer)
print(names(xrf_longer))
xrf_longer <- xrf_longer %>% 
  mutate(across(Concentration, ~as.numeric(as.character(.)))) # Ensure conversion to numeric


#concentration tables====
t1<- table1(~Concentration | Analyte, 
       data= xrf_longer,
       overall = F)
t1
t1_df<- as.data.frame(t1)
write_xlsx(t1_df, "xrfconc_overall.xlsx")


t2<- table1(~Concentration | Analyte + Type, 
            data= xrf_longer,
            overall = F)
t2
t2_df<- as.data.frame(t2)
write_xlsx(t1_df, "xrfconc_type.xlsx")


t3<- table1(~Concentration | Analyte + Location, 
            data= xrf_longer,
            overall = F)
t3
t3_df<- as.data.frame(t3)
write_xlsx(t1_df, "xrfconc_location.xlsx")

#detection tables====
xrf_longer$detect<- xrf_longer$Concentration

xrf_longer$detect<- ifelse(xrf_longer$detect>0, "Detect", "Non-Detect")
t4<- table1(~detect | Analyte , 
            data= xrf_longer,
            overall = F)
t4


t5<- table1(~detect | Analyte + Type, 
            data= xrf_longer,
            overall = F)
t5


t6<- table1(~detect | Analyte + Location, 
            data= xrf_longer,
            overall = F)
t6

#Basic Viz
ggplot(data= xrf_longer,aes (x= Location, y= Concentration))+
  geom_boxplot(outlier.color = "red",outlier.shape = 16, outlier.size = 3, notch= TRUE)+
  #geom_jitter(aes(color= Analyte))
  facet_wrap(~Analyte, scales= "free_y")+
    labs(
      x= "Location",
      y= "Concentration",
      fill= "Location",
      color= "Type"
    )+
    scale_fill_brewer(palette= "Set2") +
    scale_color_brewer(palette = "Dark2")+
    theme_minimal(base_size= 15)+
    theme(
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_text (face = "bold"),
      legend.text = element_text (size= 12))




library(RColorBrewer) # For color palettes
library(ggthemes) # For themes like theme_minimal

# Define a function to generate aesthetically enhanced plots for each analyte
plot_analytes <- function(df, analyte_col, value_col, group_col = NULL, output_dir = NULL) {
  
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
    
    # Generate the plot for this analyte, grouped by 'group_col' if provided
    plot <- ggplot(analyte_data, aes_string(x = group_col, y = value_col, fill = group_col)) +
      geom_boxplot(outlier.shape = 7, outlier.size = 3, alpha = 0.7) + # Customizing outliers and alpha
      #geom_jitter(aes(color= group_col))
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
       # panel.border = element_rect(color = "black", size = 0.5) # Add a border around the plot
      )
    
    # Print the plot to the console
    print(plot)
    
    # Optionally save the plot as an image (if output_dir is provided)
    if (!is.null(output_dir)) {
      ggsave(filename = paste(output_dir, paste0(analyte, "_boxplot.png"), sep = "/"), plot = plot)
    }
  }
}



# Call the function to generate plots for each analyte, grouped by 'Group' and save them in "plots" directory
plot_analytes(xrf_longer, analyte_col = Analyte, value_col = Concentration, group_col = Location, output_dir = "plots")





library(ggplot2)
library(ggsignif)
library(dplyr)

# Define a function to generate aesthetically enhanced plots with group labels
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
  summary_posthoc <- summary(posthoc)[[2]] # Get the summary of TukeyHSD results
  
  # Create a data frame for the comparison results
  comparison_results <- data.frame(
    group_pair = rownames(summary_posthoc),
    p_value = summary_posthoc[, "p adj"]
  )
  
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


# Call the function to generate plots for each analyte, grouped by 'Group' and save them in "plots" directory
plot_analytes_with_group_labels(xrf_longer, analyte_col = Analyte, value_col = Concentration, group_col = Type, output_dir = "plots")



# Ensure necessary packages are loaded
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggsignif)) install.packages("ggsignif")
library(ggplot2)
library(ggsignif)

# Define the function with debugging steps
plot_analytes_with_group_labels <- function(df, analyte_col, value_col, group_col = NULL, output_dir = NULL) {
  
  # Ensure the column names are correct
  analyte_col <- deparse(substitute(analyte_col)) # Extract column name
  value_col <- deparse(substitute(value_col)) # Extract column name
  group_col <- deparse(substitute(group_col)) # Extract column name for grouping
  
  # Get the unique analytes
  analytes <- unique(df[[analyte_col]])
  
  # Check if there are any analytes
  if (length(analytes) == 0) {
    stop("No unique analytes found in the data.")
  }
  
  # Create the directory if it doesn't exist
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir) # Create the directory
  }
  
  # Loop through each analyte and generate a plot
  for (analyte in analytes) {
    print(paste("Processing analyte:", analyte)) # Debugging: check current analyte being processed
    
    # Subset the data for the current analyte
    analyte_data <- subset(df, df[[analyte_col]] == analyte)
    print(paste("Subset data for", analyte, "contains", nrow(analyte_data), "rows.")) # Debugging: check data size for analyte
    
    # Check if there is data for the current analyte
    if (nrow(analyte_data) == 0) {
      message(paste("No data for analyte:", analyte))
      next # Skip to the next analyte if no data is found
    }
    
    # Perform pairwise comparison using pairwise.t.test
    comparison_results <- pairwise.t.test(analyte_data[[value_col]], analyte_data[[group_col]], p.adjust.method = "bonferroni")
    
    # Extract the p-values from pairwise comparisons
    comparison_pairs <- rownames(comparison_results$p.value)
    p_values <- comparison_results$p.value[lower.tri(comparison_results$p.value)]
    
    # Debugging: Print pairwise comparisons and p-values
    print("Pairwise comparisons:")
    print(comparison_pairs)
    print("P-values:")
    print(p_values)
    
    # Create a list of comparisons for ggsignif
    comparisons <- list()
    for (i in seq_along(comparison_pairs)) {
      pair <- strsplit(comparison_pairs[i], " ")[[1]]
      comparisons[[i]] <- pair
    }
    
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
      ) +
      # Add pairwise comparisons using ggsignif
      geom_signif(
        comparisons = comparisons,
        map_signif_level = TRUE,
        tip_length = 0.03,
        vjust = 0.5
      )
    
    # Print the plot to the console
    print(plot)
    
    # Optionally save the plot as an image (if output_dir is provided)
    if (!is.null(output_dir)) {
      ggsave(filename = paste(output_dir, paste0(analyte, "_boxplot_group_labels.png"), sep = "/"), plot = plot)
    }
  }
}

# Example usage:
plot_analytes_with_group_labels(xrf_longer, analyte_col = "Analyte", value_col = "Concentration", group_col = "Type", output_dir = "plots")















