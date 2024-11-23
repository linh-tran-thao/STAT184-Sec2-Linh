
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(ggplot2)
  
  # Define the selected names
  selected_names <- c("James", "Olivia", "Ethan", "Mary")
  
  # Filter and wrangle the data for the selected names
  filtered_babynames <- babynames %>%
    filter(name %in% selected_names) %>%
    distinct(year, sex, name, n) %>%
    rename(count = n) %>%
    arrange(year, name, sex)
  
  # Create the line plot
  ggplot(filtered_babynames, aes(x = year, y = count, color = name, linetype = sex)) +
    geom_line(size = 1) +
    labs(
      title = "Popularity of Selected Names Over Time",
      x = "Year",
      y = "Total Number of Babies Given Name",
      color = "Name",
      linetype = "Gender"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.position = "right"
    )
  
