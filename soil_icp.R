library(readxl)
library(tidyverse)
library(table1)
library(dplyr)
library(writexl)

#set working directory====
setwd("\\Users\\237708\\Documents\\naturalbackground")


icp <- read_excel("WFM_XRF_Worksheet.xlsx", 
                  sheet = "icp")
icp<- as.numeric(icp)
  
icp_short<- icp %>%
  select(cols= 3, 7, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79, 82)
icp_short<- icp_short %>% slice(-1)

colnames(icp_short)[2] <- "Type"
colnames(icp_short)[1] <- "Sample_ID"





icp_longer<- icp_short %>%
  pivot_longer(cols= -c(Sample_ID, Type),
               names_to = "Analyte",
               values_to = "Concentration")         



icp_longer<- icp_longer %>%
  drop_na()
colnames(icp_longer)

icp_longer$Test <- c(rep("Total", 240), (rep("SPLP", nrow(icp_longer)-240)))
colnames(icp_longer)


icp_wide<-  pivot_wider(
  data= icp_longer,
  names_from= Analyte,
  values_from = Concentration)

icp_wide<- icp_wide %>%
  mutate(across(everything(), ~ifelse(. =="ND", 0, .)))


soil_icp<- icp_wide %>% 
  filter (Type == "Total")
soil_icp<- as.data.frame(soil_icp)

splp<- icp_wide%>%
  filter(Type== "SPLP West")
splp<- as.data.frame(splp)


colnames(splp) <- paste0(colnames(splp), "SPLP")
combined<- bind_cols(soil_icp, splp)


soil_icp$Typesoil_icp$Lead <- as.numeric (soil_icp$Lead)
splp$Lead <- as.numeric (splp$Lead)


combined<- as.data.frame(combined)
combined$Lead<- as.numeric(combined$Lead)
combined$LeadSPLP<- as.numeric(combined$LeadSPLP)
combined$Cadmium<- as.numeric(combined$Cadmium)
combined$CadmiumSPLP<- as.numeric(combined$CadmiumSPLP)
combined$Copper<- as.numeric(combined$Copper)
combined$CopperSPLP<- as.numeric(combined$CopperSPLP)
combined$Zinc<- as.numeric(combined$Zinc)
combined$ZincSPLP<- as.numeric(combined$ZincSPLP)
combined$Nickel<- as.numeric(combined$Nickel)
combined$NickelSPLP<- as.numeric(combined$NickelSPLP)
cor.test(combined$Lead, combined$LeadSPLP, method= "pearson")
a<-lm(Lead ~ LeadSPLP, data= combined)
a
colnames(combined)

metal <- c("Lead", "Cadmium", "Copper", "Zinc", "Nickel")


results <- lapply(metal, function(metal) {
  total_col_name <- metal
  splp_col_name <- paste0(metal, "SPLP")
  
  if (!(total_col_name %in% colnames(combined)) || !(splp_col_name %in% colnames(combined))) {
    message(paste("Skipping", metal, "- columns not found"))
    return(NULL)
  }
  
  total_vals <- as.numeric(combined[[total_col_name]])
  splp_vals <- as.numeric(combined[[splp_col_name]])
  
  # Remove NA values for cor.test and lm
  valid_idx <- complete.cases(total_vals, splp_vals)
  total_vals <- total_vals[valid_idx]
  splp_vals <- splp_vals[valid_idx]
  
  if (length(total_vals) < 3) {
    message(paste("Skipping", metal, "- not enough data"))
    return(NULL)
  }
  
  # Correlation
  cor_res <- cor.test(total_vals, splp_vals, method = "pearson")
  
  # Regression
  lm_model <- lm(splp_vals ~ total_vals)
  
  # Ratio
  ratio <- splp_vals / total_vals
  
  list(
    metal = metal,
    correlation = cor_res$estimate,
    cor_p_value = cor_res$p.value,
    lm_p_value = summary(lm_model)$coefficients[2, 4],
    ratio_mean = mean(ratio, na.rm = TRUE),
    ratio_sd = sd(ratio, na.rm = TRUE)
  )
})

clean_results <- Filter (Negate(is.null), results)

results_df<- do.call(rbind, lapply(clean_results, as.data.frame))

print(results_df)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste0("metal_analysis_results_", timestamp, ".csv")

# Save to a desired directory (adjust as needed)
write.csv(results_df, file = file.path("C:\\Users\\237708\\Downloads", filename), row.names = FALSE)











# Loop through and plot each
for (m in metal) {
  total_col <- m
  splp_col <- paste0(m, "SPLP")
  
  if (!(total_col %in% colnames(combined)) || !(splp_col %in% colnames(combined))) {
    message(paste("Skipping", m, "- columns not found"))
    next
  }
  
  df <- combined[, c(total_col, splp_col)]
  colnames(df) <- c("Total", "SPLP") # Rename for plotting
  df <- na.omit(df)
  
  if (nrow(df) < 3) {
    message(paste("Skipping", m, "- not enough data"))
    next
  }
  
  cor_val <- round(cor(df$Total, df$SPLP), 2)
  p <- ggplot(df, aes(x = Total, y = SPLP)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    ggtitle(paste(m, "SPLP vs Total Metal Concentration")) +
    annotate("text",
             x = Inf, y = Inf,
             label = paste("r =", cor_val),
             hjust = 1.1, vjust = 2, size = 4, color = "black") +
    theme_minimal()
  
  print(p)
}

# Create a vector of total and SPLP columns
cols <- c("Lead", "LeadSPLP",
          "Cadmium", "CadmiumSPLP",
          "Copper", "CopperSPLP",
          "Zinc", "ZincSPLP",
          "Nickel", "NickelSPLP")

# Filter only available columns
cols_available <- cols[cols %in% colnames(combined)]

# Subset the data and remove rows with any NA
cor_data <- na.omit(combined[, cols_available])


cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")


# If not installed:
# install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Melt correlation matrix into long format
cor_melted <- melt(cor_matrix)

# Plot heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")




# If not installed:
# install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", addCoef.col = "black",
         number.cex = 0.7, tl.cex = 0.8)
