#############################
### THEMES AND AESTHETICS ###
#############################

# ----------------------------
# COLOR PALLETS 
# ----------------------------
ember_tide <- c(
  "A" = "#FEC495",
  "B" = "#F99A3F",
  "C" = "#C1292E",
  "D" = "#60110C",
  "E" = "#006A79",
  "F" = "#BCE1E5",
  "G" = "#DEEDEE")

ember_tide2 <- c(
  "A" = "#FEC495",
  "B" = "#F99A3F",
  "C" = "#C1292E",
  "D" = "#60110C",
  "E" = "#0A2F4A",  
  "F" = "#4B79A6",  
  "G" = "#D3E6F4"   
)

multisensory <- c(
  "A" = "#FDDDBC",
  "B" = "#EF8A62",
  "C" = "#B2182B",
  "D" = "#2E0F3A",
  "E" = "#2166AC",
  "F" = "#67A9CF",
  "G" = "#D1E5F0"
)

multisensory2 <- c(
  "A" = "#FDDDBC",
  "B" = "#EF8A62",
  "C" = "#B2182B",
  "D" = "#244F3F",
  "E" = "#2166AC",
  "F" = "#67A9CF",
  "G" = "#D1E5F0"
)

multisensory3 <- c(
  "A" = "#FDDDBC",
  "B" = "#EF8A62",
  "C" = "#B2182B",
  "D" = "#5A235A",
  "E" = "#2166AC",
  "F" = "#67A9CF",
  "G" = "#D1E5F0"
)



# ----------------------------
# PLOT THEMES 
# ----------------------------
# Poster (Dark) ----
poster_dark <- theme_minimal(base_size = 26, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "#161521", color = NA),
    panel.background = element_rect(fill = "#161521", color = NA),
    panel.grid.major = element_line(color = "#2A2938"),
    panel.grid.minor = element_line(color = "#2A2938"),
    
    axis.text  = element_text(color = "white", size = 26),
    axis.title = element_text(color = "white", size = 30),
    
    plot.title = element_text(
      hjust = 0.5, size = 36, face = "bold", color = "white",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 26, color = "white",
      margin = margin(t = 0, b = 10)
    ),
    
    strip.text = element_text(face = "bold", color = "white", size = 28),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", color = "white", size = 28),
    legend.text  = element_text(color = "white", size = 24),
    legend.background = element_blank()
  )

# Poster (Light) ----
poster_light <- theme_minimal(base_size = 26, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#CCCCCC"),
    panel.grid.minor = element_line(color = "#E0E0E0"),
    
    axis.text  = element_text(color = "#333333", size = 26),
    axis.title = element_text(color = "#333333", size = 30),
    
    plot.title = element_text(
      hjust = 0.5, size = 36, face = "bold", color = "#333333",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 26, color = "#333333",
      margin = margin(t = 0, b = 10)
    ),
    
    strip.text = element_text(face = "bold", color = "#333333", size = 28),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", color = "#333333", size = 28),
    legend.text  = element_text(color = "#333333", size = 24),
    legend.background = element_blank()
  )

# Paper (Dark) ----
paper_dark <- theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "#161521", color = NA),
    panel.background = element_rect(fill = "#161521", color = NA),
    panel.grid.major = element_line(color = "#2A2938"),
    panel.grid.minor = element_line(color = "#2A2938"),
    
    axis.text  = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12),
    
    plot.title = element_text(
      hjust = 0.5, size = 14, face = "bold", color = "white",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11, color = "white",
      margin = margin(t = 0, b = 6)
    ),
    
    strip.text = element_text(face = "bold", color = "white", size = 11),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", color = "white", size = 11),
    legend.text  = element_text(color = "white", size = 10),
    legend.background = element_blank()
  )

# Paper (Light) ----
paper_light <- theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    # Backgrounds
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Gridlines
    panel.grid.major = element_line(color = "#D0D0D0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Axis lines (add back)
    axis.line.x = element_line(color = "#333333", linewidth = 0.5),
    axis.line.y = element_line(color = "#333333", linewidth = 0.5),
    
    # Axis text & titles
    axis.text  = element_text(color = "#333333", size = 10),
    axis.title = element_text(color = "#333333", size = 12),
    
    # Titles
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold",
      color = "#333333",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11,
      color = "#333333",
      margin = margin(t = 0, b = 6)
    ),
    
    # Facets
    strip.text = element_text(face = "bold", color = "#333333", size = 11),
    
    # Legend
    legend.position = "right",
    legend.title = element_text(face = "bold", color = "#333333", size = 11),
    legend.text  = element_text(color = "#333333", size = 10),
    legend.background = element_blank()
  )

paper_light_tnr <- theme_minimal(base_size = 9, base_family = "Times New Roman") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    panel.grid.major = element_line(color = "#D0D0D0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(color = "#333333", linewidth = 0.4),
    axis.line.y = element_line(color = "#333333", linewidth = 0.4),
    
    axis.text  = element_text(color = "#333333", size = 8),
    axis.title = element_text(color = "#333333", size = 9),
    
    plot.title = element_text(
      hjust = 0.5,
      size = 11,
      face = "bold",
      color = "#333333"
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 9,
      color = "#333333"
    ),
    
    strip.text = element_text(face = "bold", color = "#333333", size = 9),
    
    legend.title = element_text(face = "bold", size = 9),
    legend.text  = element_text(size = 8)
  )
