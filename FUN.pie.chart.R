library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#sex	agec	dystatus	prop
#Female	18_29	With dysfunction	0.19396918
#Female	18_29	uncertain	0.11580595
#Female	18_29	Without dysfunction	0.69022487

 
# Set plot dimensions
options(repr.plot.width = 16, repr.plot.height = 18)


# Define colors
color1 <- "red"  # Red for "With dysfunction"
color3 <- "green"  # Blue for "Without dysfunction"
n_steps <- 50  


# Color interpolation function
interpolate_colors <- function(start_color, end_color, steps) {
  col1_rgb <- col2rgb(start_color)/255
  col3_rgb <- col2rgb(end_color)/255
  colors <- character(steps)
  for (i in 1:steps) {
    t <- (i - 1)/(steps - 1)
    rgb_val <- col1_rgb * (1 - t) + col3_rgb * t
    colors[i] <- rgb(rgb_val[1], rgb_val[2], rgb_val[3])
  }
  return(colors)
}

# Generate gradient colors
gradient_colors <- interpolate_colors(color1, color3, n_steps)

# Prepare data function
create_gradient_data <- function(data) {
  data %>%
    group_by(sex, agec) %>%
    group_modify(~ {
      uncertain_prop <- .x$prop[.x$dystatus == "uncertain"]
      
      if (length(uncertain_prop) > 0 && !is.na(uncertain_prop)) {
        slice_value <- uncertain_prop / n_steps
        uncertain_data <- data.frame(
          dystatus = paste("uncertain", sprintf("%02d", 1:n_steps), sep = "."),
          prop = rep(slice_value, n_steps),
          order = 1:n_steps
        )
        
        bind_rows(
          .x %>% filter(dystatus == "With dysfunction") %>% mutate(order = 0),
          uncertain_data,
          .x %>% filter(dystatus == "Without dysfunction") %>% mutate(order = n_steps + 1)
        ) %>%
          arrange(order)
      } else {
        .x %>%
          arrange(factor(dystatus, levels = c("With dysfunction", "uncertain", "Without dysfunction")))
      }
    }) %>%
    ungroup()
}

# Prepare data
gradient_data <- create_gradient_data(df_with_uncertain)

# Create boundary data
boundary_data <- df_with_uncertain %>%
  group_by(sex, agec) %>%
  summarize(
    y_with = 1 - coalesce(prop[dystatus == "With dysfunction"], 0),
    y_uncertain = 1 - coalesce(sum(prop[dystatus %in% c("With dysfunction", "uncertain")]), 0),
    .groups = "drop"
  ) %>%
  filter(!is.na(y_with) & !is.na(y_uncertain))

# Create curve data
curve_data <- boundary_data %>%
  group_by(sex, agec) %>%
  summarize(
    x = 1,
    y = c(y_with, mean(c(y_with, y_uncertain)), y_uncertain),
    .groups = "drop"
  ) %>%
  group_by(sex, agec) %>%
  mutate(
    is_end = row_number() == n(),
    dx = x - lag(x, default = first(x)),
    dy = y - lag(y, default = first(y)),
    tangent_angle = atan2(dy, dx),
    perp_angle = tangent_angle + pi/18
  ) %>%
  ungroup()

# Define facet layout with annotations
age_levels <- unique(gradient_data$agec)
age_annotations <- setNames(
  c("18-29", 
    "30-39", 
    "40-49", 
    "50-59", 
    "60-69", 
    "70-79", 
    "80+"),
  age_levels
)


layout_df <- expand.grid(
  sex = unique(gradient_data$sex),
  agec = age_levels
) %>%
  mutate(
    row_num = ifelse(match(agec, age_levels) <= 4, 1, 2),  # Keep 2 rows per sex
    col_num = ifelse(row_num == 1, match(agec, age_levels), match(agec, age_levels) - 4),
    #    position = sex,  # Remove "1" and "2" but maintain row structure
    position = factor(sex, levels = c("Female", "Male")),
    
    age_label =  paste("", agec, "\n", sex, "")   # Add descriptive annotations
  )


# Join layout to data
gradient_data <- left_join(gradient_data, layout_df, by = c("sex", "agec"))
boundary_data <- left_join(boundary_data, layout_df, by = c("sex", "agec"))
curve_data <- left_join(curve_data, layout_df, by = c("sex", "agec"))

# Define colors
color_mapping <- c(
  "With dysfunction" = color1,
  setNames(gradient_colors, paste0("uncertain.", sprintf("%02d", 1:n_steps))),
  "Without dysfunction" = color3
)

midpoint_color <- gradient_colors[round(length(gradient_colors) / 2)]

gradient_data <- gradient_data %>%
  mutate(sex = str_remove(sex, " \\d+$"))  # Removes space and numbers


arrow_length=0.003

# Create the plot with annotations
p <- ggplot(gradient_data, aes(x = "", y = prop, fill = factor(dystatus, levels = names(color_mapping)))) +
  geom_col(width = 1, position = "stack") +
  
  # Base and boundary lines
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = 1, yend = 1),
               color = "white", size = 1, inherit.aes = FALSE) +
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = y_with, yend = y_with),
               color = "blue", size = 1, inherit.aes = FALSE) +
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = y_uncertain, yend = y_uncertain),
               color = "blue", size = 1, inherit.aes = FALSE) +
  
  
  geom_segment(
    data = filter(curve_data, is_end),
    aes(
      x = x - arrow_length * cos(perp_angle),
      y = y - arrow_length * sin(perp_angle),
      xend = x,
      yend = y
    ),
    color = "blue",
    arrow = arrow(
      length = unit(0.15, "inches"),
      ends = "last",  # Arrowhead at the end only
      type = "closed"
    ),
    size = 0.7,
    inherit.aes = FALSE
  )  +
  
  
  # Connecting curve
  geom_path(data = curve_data, aes(x = x, y = y, group = interaction(sex, agec)),
            color = "blue", size = 1, inherit.aes = FALSE) +
  
  
  # Add age group annotations
  geom_text(
    data = distinct(gradient_data, sex, agec, age_label, col_num, row_num),
    aes(x = -0.5, y = 0, label = age_label, color = sex),  # map color by sex
    inherit.aes = FALSE,
    size = 9,
    show.legend = FALSE
  ) +
  scale_color_manual(
    # values = c("Female" = "#A020F0", "Male" = "#00BFC4")  # or any fallback color for "Male"
    values = c("Female" = 1, "Male" = 1)  # or any fallback color for "Male"
  )+
  
  
  coord_polar("y", start = 0) +
  scale_fill_manual(values = color_mapping,
                    labels = c("Clinical Obesity", "Uncertainty range", "pre-clinical Obesity"),
                    name = " ",
                    breaks = c("With dysfunction", "uncertain.01", "Without dysfunction"),
                    guide = guide_legend(
                      override.aes = list(
                        fill = c(color1, midpoint_color, color3)
                      )
                    )) +
  
  
  facet_grid(rows = vars(sex, row_num), cols = vars(col_num), switch = "y") +
  
  theme_void() +
  theme(
    strip.text.y = element_text(size = 1, face = "bold"),
    strip.text = element_blank(),  # Hide default facet labels
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.text = element_text(size = 31),                  # Larger legend items
    legend.box = "vertical",
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", margin = margin(b = 20) )
  ) + 
  
  labs(title = " ",
       caption = " ") 


print(p)

