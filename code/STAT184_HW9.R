library(ggplot2)
data(diamonds)

  
  # Load the ggplot2 library and diamonds dataset
  library(ggplot2)
  data(diamonds)
  
  # Improved Scatter Plot
  # Aesthetics: Mapping variables
  ggplot(  
    data = diamonds,  
    mapping = aes(  
      x = carat,
      y = price,
      color = price  
    )
  ) +
    # Geometry: Adding scatter plot points
    geom_point(size = 1, alpha = 0.6) + 
    # Geometry: Adding a trend line
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "darkblue") +  
    # Facet: Creating small multiples by cut quality
    facet_wrap(~ cut) +  
    # Labels: Setting plot title and axis labels
    labs(  
      title = "Diamond Price by Carat Size and Cut Quality",
      x = "Carat (Diamond Size)",
      y = "Price (USD)",
      color = "Price Level"
    ) +
    # Scale: Gradient scale for price levels
    scale_color_gradient(low = "#CCE5FF", high = "#003366") +  
    # Theme: Applying a minimal theme
    theme_minimal() +  
    theme(  
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.background = element_rect(fill = "#f2f2f2"),
      strip.text = element_text(face = "bold")
    )
  
  
  # Install and load the palmerpenguins package 
  install.packages("palmerpenguins")
  library(palmerpenguins)
  library(ggplot2)
  # Load the penguins dataset
  data("penguins", package = "palmerpenguins")
  
  # Create Scatter Plot: Flipper Length vs. Body Mass by Species
  ggplot(
    data = penguins, 
    aes(x = flipper_length_mm, 
        y = body_mass_g, 
        color = species)) +
    geom_point() +  
    labs(
      title = "Penguin Body Mass and Flipper Length by Species",
      x = "Flipper Length (mm)",
      y = "Body Mass (g)",
      color = "Species"
    ) +
    theme_minimal()  
  
  
  # Install and load the palmerpenguins package
  install.packages("palmerpenguins")
  library(palmerpenguins)
  library(ggplot2)
  
  # Load the penguins dataset
  data("penguins", package = "palmerpenguins")
  
  # Improved Scatter Plot: Flipper Length vs. Body Mass by Species
  ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
    
    # Geometry: Scatter plot with adjusted point size and transparency for readability
    geom_point(size = 2, alpha = 0.7) +

    
    # Labels: Add a descriptive title and axis labels
    labs(
      title = "Penguin Body Mass and Flipper Length by Species and Sex",
      x = "Flipper Length (mm)",
      y = "Body Mass (g)",
      color = "Species",
      shape = "Species"
    ) +
    
    # Theme: Use a minimal theme with adjusted title alignment and font sizes
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 10),
      legend.position = "right",
      strip.text = element_text(face = "bold", size = 12)
    )
  
  
  
  # Install and load the palmerpenguins package
  install.packages("palmerpenguins")
  library(palmerpenguins)
  library(ggplot2)
  
  # Load the penguins dataset
  data("penguins", package = "palmerpenguins")
  
  # Scatter Plot: Flipper Length vs Body Mass by Species
  ggplot(
    data = penguins,
    aes(
      x = flipper_length_mm,
      y = body_mass_g,
      color = species, # Color by species for distinction
      shape = species # Use different shapes for each species
    )
  )+
  # Geometry: Scatter plot with changed point size and transparency
  geom_point(size = 3, alpha = 0.7)+
  # Labels: Add title, axis labels, and legend labels
  labs(
    title = "Body Mass and Flipper Length of Penguin Species",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    color = "Species",
    shape = "Species"
  )+
  # Theme: Minimal theme with changes in titles, legend, and font sizes
  theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 10),
      legend.position = "right",
      strip.text = element_text(face="bold", size = 12)
    )
  
  # Install and load the dcData package
  install.packages("dcData")
  library(dcData)
  
  # Load the BabyNames data set
  data("BabyNames", package = "dcData")
  
  # View the first few rows of the data
  head(BabyNames)
  
  
  
  # Load necessary libraries
  # Uncomment and install if needed
  # install.packages("dcData") 
  
  library(babynames)
  library(dplyr)
  
  # Load the BabyNames dataset
  data("BabyNames")  # Adjust this if the dataset name or package is different
  
  # Count unique rows based on year, sex, and name
  unique_rows_count <- BabyNames %>%
    distinct(year, sex, name) %>%
    nrow()
  
  # Display the count of unique rows
  print(unique_rows_count)
  
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  
  # Get unique names
  unique_names <- babynames %>%
    distinct(name) %>%
    pull(name)
  
  # Display the unique names
  print(unique_names)
  
  
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(tidyr)
  
  # Tidy the babynames data
  tidy_babynames <- babynames %>%
    # Ensure data has distinct rows for year, sex, name, and count
    distinct(year, sex, name, n) %>%
    # Rename columns if needed for clarity
    rename(count = n) %>%
    arrange(year, name, sex)
  
  # Display the first few rows of the tidy data
  print(head(tidy_babynames))
  
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(tidyr)
  
  # Tidy the babynames data
  tidy_babynames <- babynames %>%
    distinct(year, sex, name, n) %>%
    rename(count = n) %>%
    arrange(year, name, sex)
  
  # Count the rows in the tidied data
  row_count <- nrow(tidy_babynames)
  
  # Display the row count
  print(row_count)
  
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(ggplot2)
  
  # Define the names of interest
  selected_names <- c("James", "Olivia", "Ethan", "Mary")
  
  # Filter the data for the selected names
  filtered_babynames <- babynames %>%
    filter(name %in% selected_names) %>%
    rename(count = n)
  
 # Line Plot
  ggplot(
    filtered_babynames, 
    aes(x = year, y = count, color=name))+
    geom_line()+
    labs(
      title = "Popularity of Selected Names Over Time",
      x = "Year",
      y = "Count"
    )
  
  
  
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
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(ggplot2)
  
  # Define the selected names
  selected_names <- c("James", "Olivia", "Ethan")
  
  # Data Wrangling: Filter and prepare the data
  filtered_babynames <- babynames %>%
    filter(name %in% selected_names) %>%
    distinct(year, name, n) %>%
    rename(count = n) %>%
    arrange(year, name)
  
  # Finalized Data Visualization
  ggplot(filtered_babynames, aes(x = year, y = count, color = name, linetype = )) +
    geom_line(size = 1) +
    labs(
      title = "Popularity Trends of Selected Names Over Time",
      x = "Year",
      y = "Popularity (Count)",
      color = "Name",
      linetype = "Name"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )
  
  
  
  # Load necessary libraries
  library(babynames)
  library(dplyr)
  library(ggplot2)
  
  # Define the selected names
  selected_names <- c("James", "Olivia", "Ethan")
  
  # Data Wrangling: Filter and prepare the data
  filtered_babynames <- babynames %>%
    filter(name %in% selected_names) %>%
    distinct(year, name, n) %>%
    rename(count = n) %>%
    arrange(year, name)
  
  # Define custom line types and colors
  line_types <- c("James" = "solid", "Olivia" = "dashed", "Ethan" = "dotted")
  line_colors <- c("James" = "blue", "Olivia" = "red", "Ethan" = "green")
  
  # Create the plot
  ggplot(filtered_babynames, aes(x = year, y = count, color = name, linetype = name)) +
    geom_line(size = 1) +
    scale_color_manual(values = line_colors) +      # Apply custom colors
    scale_linetype_manual(values = line_types) +     # Apply custom line types
    labs(
      title = "Popularity of Selected Names Over Time",
      x = "Year",
      y = "Total Number of People with Name",
      color = "Name",
      linetype = "Name"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )
  
  