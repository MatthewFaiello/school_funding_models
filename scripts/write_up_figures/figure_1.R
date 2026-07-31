library(dplyr)
library(readr)
library(ggplot2)
library(scales)

# Assumes the working directory is the school_funding_model project root
staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

# Categories shown in the report figure
figure_categories <- c(
  "Assistant Principal",
  "Administrative Support Professionals",
  "Principal",
  "Food Services Supervisor",
  "Superintendent",
  "11-Month Supervisor",
  "Director",
  "Reading Cadre",
  "Assistant Superintendent",
  "Instructional Supports"
)

staffing_plot_data <- staffing_components |>
  filter(
    AnalysisSection == "Staffing rules",
    ComparisonCategory %in% figure_categories
  ) |>
  mutate(
    funding_difference_m = (
      ProposedKnownFundingAmount - CurrentKnownFundingAmount
    ) / 1e6,
    
    category_label = case_when(
      ComparisonCategory == "Administrative Support Professionals" ~
        "Administrative Support*",
      ComparisonCategory == "Food Services Supervisor" ~
        "Food Services Supervisor*",
      ComparisonCategory == "Instructional Supports" ~
        "Instructional Supports*",
      TRUE ~ ComparisonCategory
    ),
    
    value_label = paste0(
      if_else(funding_difference_m > 0, "+", ""),
      number(funding_difference_m, accuracy = 0.1)
    )
  ) |>
  arrange(funding_difference_m) |>
  mutate(
    category_label = factor(category_label, levels = category_label)
  )

# Optional audit view
staffing_plot_data |>
  select(
    ComparisonCategory,
    CurrentKnownFundingAmount,
    ProposedKnownFundingAmount,
    funding_difference_m,
    ComparisonStatus
  ) |>
  print(n = Inf)

# Figure
largest_staffing_differences_plot <- ggplot(
  staffing_plot_data,
  aes(
    x = category_label,
    y = funding_difference_m
  )
) +
  geom_col(
    width = 0.72,
    fill = "#2C7FB8"
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = value_label,
      hjust = if_else(funding_difference_m >= 0, -0.15, 1.15)
    ),
    size = 3.2
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    breaks = seq(-10, 20, by = 5),
    limits = c(-13, 20),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = NULL, #"Largest working staffing differences by category\n",
    x = NULL,
    y = "\nIV&V proposed minus recreated current funding ($ millions)",
    caption = "*Provisional comparison category"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8
    ),
    plot.caption.position = "plot",
    plot.margin = margin(
      t = 8,
      r = 24,
      b = 8,
      l = 8
    )
  )

largest_staffing_differences_plot

ggsave(
  filename = "data/output/final/largest_working_staffing_differences.png",
  plot = largest_staffing_differences_plot,
  width = 8,
  height = 5.4,
  dpi = 300
)
