# Load necessary packages
library(ggplot2)
library(patchwork)

# Step 1: Read the data frame and filter out relevant data
result_data <- read.csv("data/inputs/final_result_n_t_NA.csv", header = TRUE)

num_row <- nrow(result_data) # Get the actual number of rows in the data frame

# Calculate the number of rows that meet the requirements
a <- 2
b <- 6
temp_cal <- 0  # Temporary variable for calculation

data_for_x <- vector("numeric", length = 0)  # Define an empty vector to store the horizontal coordinates of the image
data_for_y1 <- vector("numeric", length = 0) # Define an empty vector to store the vertical coordinates 1 of the image
data_for_y2 <- vector("numeric", length = 0) # Define an empty vector to store the vertical coordinates 2 of the image
data_for_y3 <- vector("numeric", length = 0) # Define an empty vector to store the vertical coordinates 3 of the image
data_for_y4 <- vector("numeric", length = 0) # Define an empty vector to store the vertical coordinates 4 of the image

for (i in 1:num_row) {  # This loop is used to find independent variables that meet the conditions
  temp_cal <- a + (i - 1) * b  # This term is used to calculate temporary values
  
  if (temp_cal <= num_row) {  # Place a condition here: the calculated value is less than the total number of rows
    data_for_x <- append(data_for_x, result_data[temp_cal, 1], after = 0)     # Add elements at the end  
  } else {
    break  # If the number of rows is exceeded, end the loop
  }
}

# Calculate values that meet the conditions (ensure relevant content)
c <- 3
d <- 6
temp_cal_1 <- 0  # Temporary variable for calculating y1   
temp_cal_2 <- 0  # Temporary variable for calculating y2   Leader_P
temp_cal_3 <- 0  # Temporary variable for calculating y3   Follower_P
temp_cal_4 <- 0  # Temporary variable for calculating y4   Leader_Q
temp_cal_5 <- 0  # Temporary variable for calculating y5   Follower_Q

# This loop is used to find dependent variables 1&2 that meet the conditions
for (j in 1:num_row) {  
  temp_cal_2 <- 0  # Reset parameter to zero  Leader_P
  temp_cal_3 <- 0  # Temporary variable for calculating y3   Follower_P
  temp_cal_4 <- 0  # Leader_Q
  temp_cal_5 <- 0  # Follower_Q
  
  temp_cal_1 <- c + (j - 1) * d  # This term is used to find the corresponding serial number 
  if (temp_cal_1 <= num_row) {  # Place a condition here: the calculated value is less than the total number of rows
    # Calculate Leader's average selling price
    temp_cal_2 <- temp_cal_2 + result_data[temp_cal_1, 2] + result_data[temp_cal_1 + 1, 2] + 
      result_data[temp_cal_1 + 2, 2] + result_data[temp_cal_1 + 3, 2] + result_data[temp_cal_1 + 4, 2]  # Combine five data points
    temp_cal_2 <- temp_cal_2 / 5  # Calculate the average
    data_for_y1 <- append(data_for_y1, temp_cal_2, after = 0)
    
    # Calculate Follower's average selling price
    temp_cal_3 <- temp_cal_3 + result_data[temp_cal_1, 3] + result_data[temp_cal_1 + 1, 3] + 
      result_data[temp_cal_1 + 2, 3] + result_data[temp_cal_1 + 3, 3] + result_data[temp_cal_1 + 4, 3]  # Combine five data points 
    temp_cal_3 <- temp_cal_3 / 5  # Calculate the average
    data_for_y2 <- append(data_for_y2, temp_cal_3, after = 0)  # Add the value represented by temp_cal_2 at the end of the data frame
    
    # Calculate Leader's total sales
    temp_cal_4 <- temp_cal_4 + result_data[temp_cal_1, 4] + result_data[temp_cal_1 + 1, 4] + 
      result_data[temp_cal_1 + 2, 4] + result_data[temp_cal_1 + 3, 4] + result_data[temp_cal_1 + 4, 4]  # Combine five data points
    data_for_y3 <- append(data_for_y3, temp_cal_4, after = 0)
    
    # Calculate Follower's total sales
    temp_cal_5 <- temp_cal_5 + result_data[temp_cal_1, 5] + result_data[temp_cal_1 + 1, 5] + 
      result_data[temp_cal_1 + 2, 5] + result_data[temp_cal_1 + 3, 5] + result_data[temp_cal_1 + 4, 5]  # Combine five data points 
    data_for_y4 <- append(data_for_y4, temp_cal_5, after = 0)
  } else {
    break  # If the number of rows is exceeded, end the loop
  }
}

# Create data frame
df <- data.frame(
  PotentialMarketDemand = data_for_x,
  Leader_AvgPrice = data_for_y1,
  Follower_AvgPrice = data_for_y2,
  Leader_TotalSales = data_for_y3,
  Follower_TotalSales = data_for_y4
)

# Create graph a: average selling price
p_a <- ggplot() +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Leader_AvgPrice, color = "IDL"), size = 1) +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Follower_AvgPrice, color = "ICF"), size = 1) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Leader_AvgPrice, color = "IDL"), size = 2) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Follower_AvgPrice, color = "ICF"), size = 2) +
  labs(x = "Number of supply chain network nodes", 
       y = "Average selling price") +
  scale_color_manual(values = c("IDL" = "purple", "ICF" = "#FFD700"),
                     name = "") +  # Set legend title as empty string
  theme_classic() +  # Use classic theme, no grid background
  theme(legend.position = "top",
        legend.title = element_blank(),  # Ensure legend title is empty
        axis.title = element_text(size = 11),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5))

# Create graph b: total sales
p_b <- ggplot() +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Leader_TotalSales, color = "IDL"), size = 1) +
  geom_line(aes(x = df$PotentialMarketDemand, y = df$Follower_TotalSales, color = "ICF"), size = 1) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Leader_TotalSales, color = "IDL"), size = 2) +
  geom_point(aes(x = df$PotentialMarketDemand, y = df$Follower_TotalSales, color = "ICF"), size = 2) +
  labs(x = "Number of supply chain network nodes", 
       y = "Total sales") +
  scale_color_manual(values = c("IDL" = "purple", "ICF" = "#FFD700"),
                     name = "") +  # Set legend title as empty string
  theme_classic() +  # Use classic theme, no grid background
  theme(legend.position = "top",
        legend.title = element_blank(),  # Ensure legend title is empty
        axis.title = element_text(size = 11),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5))

# Combine two graphs from left to right
combined_plot <- p_a + p_b + 
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") &
  theme(legend.position = "top")

# Based on your data file path, construct the image save path
data_path <- "outputs/"
output_file <- paste0(data_path, "Figure_12.pdf")

# Output as high-quality PDF format
ggsave(filename = output_file,
       plot = combined_plot,
       device = cairo_pdf,  # Use cairo_pdf device for better font and graphics quality
       width = 16,          # Total width (cm)
       height = 8,          # Height (cm)
       units = "cm",
       dpi = 600)           # High resolution

# Display combined plot on screen
print(combined_plot)

# Output success message
cat("Combined graph has been successfully saved as '", output_file, "'\n", sep = "")
