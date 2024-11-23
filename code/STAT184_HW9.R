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

