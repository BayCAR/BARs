library(ggplot2)
library(dplyr)
library(tidyr)
options(repr.plot.width = 16, repr.plot.height = 22)  # Set default size

# Define colors
color1 <- "#d62728"  # Red for "With dysfunction"
color3 <- "#1f77b4"  # Blue for "Without dysfunction"
n_steps <- 20  

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

# Generate gradient colors (red to blue)
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
    perp_angle = tangent_angle + pi/8
  ) %>%
  ungroup()

# Define facet layout
age_levels <- unique(gradient_data$agec)
layout_df <- expand.grid(
  sex = unique(gradient_data$sex),
  agec = age_levels
) %>%
  mutate(
    row_num = ifelse(match(agec, age_levels) <= 4, 1, 2),
    col_num = ifelse(row_num == 1, match(agec, age_levels), match(agec, age_levels) - 4),
    position = paste(sex, row_num, col_num, sep = "_")
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

# Create the plot
p <- ggplot(gradient_data, aes(x = "", y = prop, fill = factor(dystatus, levels = names(color_mapping)))) +
  geom_col(width = 1, position = "stack") +
  
  # Base and boundary lines
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = 1, yend = 1),
               color = "black", size = 1, inherit.aes = FALSE) +
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = y_with, yend = y_with),
               color = "green", size = 0.5, inherit.aes = FALSE) +
  geom_segment(data = boundary_data, aes(x = 0.5, xend = 1.5, y = y_uncertain, yend = y_uncertain),
               color = "green", size = 0.5, inherit.aes = FALSE) +
  
  # Connecting curve
  geom_path(data = curve_data, aes(x = x, y = y, group = interaction(sex, agec)),
            color = "green", size = 0.5, inherit.aes = FALSE) +
  
  # Single perpendicular arrow at end
  geom_segment(data = filter(curve_data, is_end),
               aes(x = x, y = y, xend = x + 0.003 * cos(perp_angle), yend = y + 0.003 * sin(perp_angle)),
               color = "green", arrow = arrow(length = unit(0.15, "inches")),
               size = 0.7, inherit.aes = FALSE) +
  
  coord_polar("y", start = 0) +
  scale_fill_manual(values = color_mapping,
                    labels = c("With dysfunction", "range", "Without dysfunction"),
                    name = "Dysfunction Status",
                    breaks = c("With dysfunction", "uncertain.01", "Without dysfunction")) +
  
  scale_fill_manual(
    values = color_mapping,
    labels = c("Clinical obesity      ", "Possible range   ", "pre-clinical obesity"),
    name = "Prevalence",
    breaks = c("With dysfunction", "uncertain.01", "Without dysfunction"),
    guide = guide_legend(
      override.aes = list(
        fill = c(color1, "#483d8b", color3)  # This changes only legend colors
      )
    )
  )+
  
  facet_grid(rows = vars(sex, row_num), cols = vars(col_num), switch = "y") +
  
  theme_void() +
  theme(
    strip.text.x = element_text(size = 10),  
    strip.text.y = element_text(size = 10, angle = 0),  
    strip.placement = "outside",
    panel.spacing = unit(1, "lines")
  ) +
  
  labs(title = "Preclinical and Clinical Obesity Prevalence by Age and Sex")

print(p)

