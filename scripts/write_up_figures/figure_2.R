library(dplyr)
library(readr)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------------
# Distribution of comparable staffing funding changes by LEA type
#
# Comparable amounts follow the statewide comparable-amount subtotal.
# That subtotal removes categories with no current funding amount, but it can
# still include provisional categories with partially known current amounts.
# BASSE is included and DAFB is excluded.
# -----------------------------------------------------------------------------

# Assumes the working directory is the school_funding_model project root

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

staffing_lea <- read_csv(
  "data/output/final/11_staffing_lea_comparison.csv",
  show_col_types = FALSE
)

staffing_components <- read_csv(
  "data/output/final/11_staffing_component_comparison.csv",
  show_col_types = FALSE
)

proposed_detail <- read_csv(
  "data/output/intermediate/08_proposed_model_funding_detail.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Identify working categories excluded from the comparable subtotal
# -----------------------------------------------------------------------------

noncomparable_categories <- staffing_components |>
  filter(
    IncludedInWorkingTotal,
    !IncludedInComparableAmountSubtotal
  ) |>
  distinct(ComparisonCategory) |>
  pull(ComparisonCategory)

# Expected result:
# "Buildings and Grounds Supervisor"
print(noncomparable_categories)

provisional_comparable_categories <- staffing_components |>
  filter(
    IncludedInComparableAmountSubtotal,
    ComparisonStatus != "Confirmed"
  ) |>
  distinct(ComparisonCategory) |>
  pull(ComparisonCategory)

# Expected result under the current run:
# "Food Services Supervisor"
print(provisional_comparable_categories)

# -----------------------------------------------------------------------------
# Calculate excluded proposed funding by LEA
# -----------------------------------------------------------------------------

noncomparable_proposed_lea <- proposed_detail |>
  filter(
    IncludeInStatewide,
    Component %in% noncomparable_categories
  ) |>
  group_by(DistrictCode) |>
  summarise(
    NoncomparableProposedFundingAmount = sum(
      FundingAmount,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Construct LEA-level comparable amounts
# -----------------------------------------------------------------------------

lea_changes <- staffing_lea |>
  filter(
    AnalysisSection == "Staffing rules",
    LEAType %in% c("District", "Charter")
  ) |>
  left_join(
    noncomparable_proposed_lea,
    by = "DistrictCode"
  ) |>
  mutate(
    NoncomparableProposedFundingAmount = coalesce(
      NoncomparableProposedFundingAmount,
      0
    ),
    
    # The current working total omits unknown current amounts. The resulting
    # LEA comparison remains provisional where an included category has
    # incomplete current inputs, such as Food Services Supervisor.
    ComparableCurrentFundingAmount =
      WorkingCurrentFundingAmount,
    
    ComparableProposedFundingAmount =
      WorkingProposedFundingAmount -
      NoncomparableProposedFundingAmount,
    
    ComparableFundingDifference =
      ComparableProposedFundingAmount -
      ComparableCurrentFundingAmount,
    
    ComparablePercentDifference = if_else(
      abs(ComparableCurrentFundingAmount) > 1e-8,
      100 * ComparableFundingDifference /
        ComparableCurrentFundingAmount,
      NA_real_
    ),
    
    LEA_type = recode(
      LEAType,
      District = "Districts",
      Charter = "Charters"
    ),
    
    LEA_type = factor(
      LEA_type,
      levels = c("Charters", "Districts")
    ),
    
    percent_change = ComparablePercentDifference,
    
    # Shorten names for chart labels
    LEA_label = gsub(
      " School District$| Charter School$",
      "",
      DistrictName
    )
  )

# Confirm the expected 43-LEA scope
stopifnot(
  n_distinct(lea_changes$DistrictCode) == 43,
  sum(lea_changes$LEAType == "District") == 19,
  sum(lea_changes$LEAType == "Charter") == 24
)

# -----------------------------------------------------------------------------
# Calculate medians
# -----------------------------------------------------------------------------

lea_medians <- lea_changes |>
  group_by(LEA_type) |>
  summarise(
    median_change = median(
      percent_change,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(lea_medians)

# -----------------------------------------------------------------------------
# Identify the highest and lowest LEA in each sector
# -----------------------------------------------------------------------------

lowest_leas <- lea_changes |>
  filter(!is.na(percent_change)) |>
  arrange(LEA_type, percent_change, DistrictName) |>
  group_by(LEA_type) |>
  slice_head(n = 1) |>
  ungroup()

highest_leas <- lea_changes |>
  filter(!is.na(percent_change)) |>
  arrange(LEA_type, desc(percent_change), DistrictName) |>
  group_by(LEA_type) |>
  slice_head(n = 1) |>
  ungroup()

# Audit the four labeled LEAs
bind_rows(
  lowest_leas |>
    mutate(Extreme = "Lowest"),
  highest_leas |>
    mutate(Extreme = "Highest")
) |>
  select(
    LEA_type,
    Extreme,
    DistrictName,
    percent_change
  ) |>
  arrange(LEA_type, Extreme) |>
  print(n = Inf)

# -----------------------------------------------------------------------------
# Plot settings
# -----------------------------------------------------------------------------

# Reusable jitter so LEA points do not completely overlap
point_position <- position_jitter(
  width = 0,
  height = 0.08,
  seed = 123
)

# Blue palette consistent with the staffing-category chart
lea_colors <- c(
  "Charters" = "#74A9CF",
  "Districts" = "#2C7FB8"
)

lea_fills <- c(
  "Charters" = alpha("#74A9CF", 0.22),
  "Districts" = alpha("#2C7FB8", 0.18)
)

# Begin the displayed scale at -10% and set the upper end dynamically
x_upper <- ceiling(
  (max(lea_changes$percent_change, na.rm = TRUE) + 3) / 5
) * 5

x_breaks <- seq(
  from = -10,
  to = x_upper,
  by = 5
)

# -----------------------------------------------------------------------------
# Create plot
# -----------------------------------------------------------------------------

provisional_comparable_note <- if (
  length(provisional_comparable_categories) > 0
) {
  paste0(
    "The comparable subtotal includes provisional comparisons for ",
    paste(provisional_comparable_categories, collapse = ", "),
    "; LEA values remain provisional where current inputs are incomplete."
  )
} else {
  "All categories in the comparable subtotal are confirmed."
}

funding_change_distribution_plot <- ggplot(
  lea_changes,
  aes(
    x = percent_change,
    y = LEA_type,
    color = LEA_type,
    fill = LEA_type
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.5,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_boxplot(
    width = 0.42,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_point(
    position = point_position,
    size = 2.2,
    alpha = 0.75
  ) +
  
  # Median diamonds
  geom_point(
    data = lea_medians,
    aes(
      x = median_change,
      y = LEA_type,
      color = LEA_type
    ),
    inherit.aes = FALSE,
    shape = 18,
    size = 4
  ) +
  
  # Median labels
  geom_text(
    data = lea_medians,
    aes(
      x = median_change,
      y = LEA_type,
      label = paste0(
        "Median: ",
        number(
          median_change,
          accuracy = 0.1
        ),
        "%"
      )
    ),
    inherit.aes = FALSE,
    nudge_y = 0.27,
    size = 3.2,
    color = "black"
  ) +
  
  # Lowest LEA in each sector
  geom_text(
    data = lowest_leas,
    aes(
      x = percent_change,
      y = LEA_type,
      label = LEA_label
    ),
    inherit.aes = FALSE,
    nudge_x = -0.6,
    nudge_y = -0.18,
    hjust = 1,
    size = 3,
    color = "black"
  ) +
  
  # Highest LEA in each sector
  geom_text(
    data = highest_leas |> mutate(LEA_label = if_else(LEA_label == "Bryan Allen Stevenson School of Excellence",
                                                      "BASSE",
                                                      LEA_label)),
    aes(
      x = percent_change,
      y = LEA_type,
      label = LEA_label
    ),
    inherit.aes = FALSE,
    nudge_x = 0.6,
    nudge_y = 0.18,
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  scale_color_manual(
    values = lea_colors
  ) +
  scale_fill_manual(
    values = lea_fills
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = label_number(
      accuracy = 1,
      suffix = "%"
    ),
    expand = expansion(
      mult = c(0.01, 0.03)
    )
  ) +
  coord_cartesian(
    xlim = c(-10, x_upper),
    clip = "off"
  ) +
  labs(
    title = paste(
      "Distribution of comparable staffing funding",
      "changes by LEA type"
    ),
    x = "\nIV&V proposed minus recreated current funding (%)",
    y = NULL,
    caption = stringr::str_wrap(
      paste(
        "Each point represents one LEA; diamonds mark medians.",
        "LEA amounts follow the statewide comparable-amount subtotal.",
        provisional_comparable_note,
        "BASSE is included; DAFB is excluded."
      ),
      width = 150
    )
  ) +
  guides(
    color = "none",
    fill = "none"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_blank(),#element_text(face = "bold", color = "black"),
    
    axis.title = element_text(
      color = "black"
    ),
    
    axis.text = element_text(
      color = "black"
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 7,
      color = "black"
    ),
    
    plot.caption.position = "plot",
    
    plot.margin = margin(
      t = 8,
      r = 45,
      b = 8,
      l = 45
    )
  )

funding_change_distribution_plot

# -----------------------------------------------------------------------------
# Save plot
# -----------------------------------------------------------------------------

ggsave(
  filename = paste0(
    "data/output/final/",
    "comparable_staffing_funding_change_distribution.png"
  ),
  plot = funding_change_distribution_plot,
  width = 8,
  height = 4.8,
  dpi = 300
)