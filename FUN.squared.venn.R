library(ggplot2)
options(repr.plot.width = 17, repr.plot.height = 12)  # Set default size
par(mfrow=c(2,2))

# Define proportions (cumulative)
horizontal_breaks <- c(0.605, 0.908, 1)  # 60.5%, 30.3%, 9.2%

# Vertical splits (as proportions WITHIN each row)
vertical_splits <- list(
  row1 = 0.351/(0.351 + 0.254),  # 35.1% vs 25.4%
  row2 = 0.08/(0.08 + 0.223),     # 8% vs 22.3%
  row3 = 0.023/(0.023 + 0.069)    # 2.3% vs 6.9%
)

# Define colors with transparency
light_blue <- adjustcolor("lightblue", alpha.f = 0.7)
light_red <- adjustcolor("indianred1", alpha.f = 0.7)
green <- adjustcolor("forestgreen", alpha.f = 0.7)
dark_red <- adjustcolor("darkred", alpha.f = 0.7)

# Create all rectangular regions
regions <- data.frame(
  # Bottom row (60.5%)
  rbind(
    data.frame(xmin = 0, xmax = vertical_splits$row1, ymin = 0, ymax = horizontal_breaks[1], 
               fill = green, border = "black"),
    data.frame(xmin = vertical_splits$row1, xmax = 1, ymin = 0, ymax = horizontal_breaks[1], 
               fill = light_red, border = "black"),
    
    # Middle row (30.3%)
    data.frame(xmin = 0, xmax = vertical_splits$row2, ymin = horizontal_breaks[1], ymax = horizontal_breaks[2], 
               fill = "darkblue", border = "black"),
    data.frame(xmin = vertical_splits$row2, xmax = 1, ymin = horizontal_breaks[1], ymax = horizontal_breaks[2], 
               fill = dark_red, border = "black"),
    
    # Top row (9.2%)
    data.frame(xmin = 0, xmax = vertical_splits$row3, ymin = horizontal_breaks[2], ymax = 1, 
               fill = dark_red, border = "black"),
    data.frame(xmin = vertical_splits$row3, xmax = 1, ymin = horizontal_breaks[2], ymax = 1, 
               fill = dark_red, border = "black")
  )
)

# Create plot
library(ggplot2) 
# [Keep your existing data preparation code...]

# Create plot
library(ggplot2)

# [Keep your existing data preparation code...]

# Create plot
pp1=ggplot() +
  # Original regions with black borders
  geom_rect(data = regions, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = regions$fill, color = "black", linewidth = 0.5) +
  
  # SINGLE OUTLINE around combined areas
  geom_polygon(data = data.frame(
    x = c(
      vertical_splits$row2,  # Start at 22.3% left
      1,                     # Bottom-right corner
      1,                     # Up to 6.9% right
      1,#vertical_splits$row3,  # Across to 2.3% right
      1, #vertical_splits$row3,  # Down to 2.3% left
      0,                     # Across bottom of 2.3%
      0,
      vertical_splits$row2,                     # Up to 22.3% left
      vertical_splits$row2   # Close the path
    ),
    y = c(
      horizontal_breaks[1],  # Start height
      horizontal_breaks[1],  # Bottom
      1, #horizontal_breaks[2],  # Top of 22.3%
      1, #horizontal_breaks[1],  # Top of 6.9%
      1,                     # Top of 2.3%
      1,                     # Top-left
      horizontal_breaks[2],  # Bottom-left
      horizontal_breaks[2],  # Bottom-left
      horizontal_breaks[2]   # Return to start
    )
  ), aes(x = x, y = y), 
  fill = NA, color = "darkblue", linewidth = 2.5) +
  # SINGLE OUTLINE around combined areas
  geom_polygon(data = data.frame(
    x = c(
      vertical_splits$row2,  # Start at 22.3% left
      1,                     # Bottom-right corner
      1,                     # Up to 6.9% right
      1,#vertical_splits$row3,  # Across to 2.3% right
      1, #vertical_splits$row3,  # Down to 2.3% left
      0,                     # Across bottom of 2.3%
      0,
      vertical_splits$row2,                     # Up to 22.3% left
      vertical_splits$row2   # Close the path
    ),
    y = c(
      horizontal_breaks[1],  # Start height
      horizontal_breaks[1],  # Bottom
      1, #horizontal_breaks[2],  # Top of 22.3%
      1, #horizontal_breaks[1],  # Top of 6.9%
      1,                     # Top of 2.3%
      1,                     # Top-left
      horizontal_breaks[2],  # Bottom-left
      horizontal_breaks[2],  # Bottom-left
      horizontal_breaks[2]   # Return to start
    )
  ), aes(x = x, y = y), 
  fill = NA, color = "yellow", linewidth = 2) +
  
  # [Keep your existing label/theme code...]
  
  # Add percentage labels
  # Row 1 (60.5%)
  annotate("text", x = vertical_splits$row1/2, y = horizontal_breaks[1]/2, 
           label = "35.1%", size = 7, color = "white") +
  annotate("text", x = (vertical_splits$row1 + 1)/2, y = horizontal_breaks[1]/2, 
           label = "25.4%", size = 7, color = "black") +
  
  # Row 2 (30.3%)
  annotate("text", x = vertical_splits$row2/2, y = mean(horizontal_breaks[1:2]), 
           label = "8%", size = 7, color = "white") +
  annotate("text", x = (vertical_splits$row2 + 1)/2, y = mean(horizontal_breaks[1:2])-0.02, 
           label = "22.3%", size = 7, color = "white") +
  
  # Clinical Obesity label
  annotate("text", x = (vertical_splits$row2 + 1)/2 - 0.06, y = mean(horizontal_breaks[1:2]) + 0.1, 
           label = "Confirmed Obesity", size = 11, color = "yellow", fontface = "bold") +
  annotate("text", x = (vertical_splits$row2 + 1)/2 - 0.06, y = mean(horizontal_breaks[1:2]) + 0.04, 
           label = "(31.5%)", size = 11, color = "yellow", fontface = "bold") +
  
  # Row 3 (9.2%)
  annotate("text", x = vertical_splits$row3/2, y = mean(horizontal_breaks[2:3]), 
           label = "2.3%", size = 7, color = "white") +
  annotate("text", x = (vertical_splits$row3 + 1)/2, y = mean(horizontal_breaks[2:3]), 
           label = "6.9%", size = 7, color = "white") +
  
  # Horizontal section labels
  annotate("text", x = 1.05, y = .85, color="blue", 
           label = "BMI obese", size = 10, hjust = 0) +
  annotate("text", x = 1.05, y = .8, color="blue",
           label = "(39.5%)", size = 10, hjust = 0) +
  
  # Horizontal section labels
  annotate("text", x = 1.05, y = horizontal_breaks[1]/2, 
           label = "60.5%", size = 7, hjust = 0) +
  annotate("text", x = 1.05, y = mean(horizontal_breaks[1:2]), 
           label = "30.3%", size = 7, hjust = 0) +
  annotate("text", x = 1.05, y = mean(horizontal_breaks[2:3]), 
           label = "9.2%", size = 7, hjust = 0) +
  
  coord_fixed(xlim = c(0, 1.3)) +
  theme_void() +
  theme(plot.margin = margin(2, 3, 2, 3, "cm"))