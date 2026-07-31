library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

# Run from the school_funding_model project root
proposed_detail <- read_csv(
  "data/output/intermediate/08_proposed_model_funding_detail.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Component order, labels, and colors
# -----------------------------------------------------------------------------

component_levels <- c(
  "Opportunity Funding - Low Income",
  "Opportunity Funding - Multilingual Learner",
  "Operational Funding - Enrollment",
  "Operational Funding - Low Income",
  "Operational Funding - Multilingual Learner",
  "Operational Funding - Basic Special Education",
  "Operational Funding - Intensive Special Education",
  "Operational Funding - Complex Special Education",
  "Operational Funding - Vocational"
)

component_labels <- c(
  "Opportunity Funding - Low Income" =
    "Opportunity: Low income",
  
  "Opportunity Funding - Multilingual Learner" =
    "Opportunity: Active MLL",
  
  "Operational Funding - Enrollment" =
    "Operational: Total enrollment",
  
  "Operational Funding - Low Income" =
    "Operational: Low income",
  
  "Operational Funding - Multilingual Learner" =
    "Operational: Active MLL",
  
  "Operational Funding - Basic Special Education" =
    "Operational: Basic special education",
  
  "Operational Funding - Intensive Special Education" =
    "Operational: Intensive special education",
  
  "Operational Funding - Complex Special Education" =
    "Operational: Complex special education",
  
  "Operational Funding - Vocational" =
    "Operational: Vocational enrollment"
)

component_colors <- c(
  "Opportunity: Low income"                  = "#1F4E79",
  "Opportunity: Active MLL"                  = "#56B4E9",
  "Operational: Total enrollment"            = "#7F7F7F",
  "Operational: Low income"                  = "#009E73",
  "Operational: Active MLL"                  = "#CC79A7",
  "Operational: Basic special education"     = "#B79F00",
  "Operational: Intensive special education" = "#E69F00",
  "Operational: Complex special education"   = "#D55E00",
  "Operational: Vocational enrollment"       = "#6A3D9A"
)

# -----------------------------------------------------------------------------
# Charter display-name helper
# -----------------------------------------------------------------------------

make_charter_label <- function(x) {
  case_when(
    str_detect(x, regex("Odyssey", ignore_case = TRUE)) ~
      "Odyssey Charter",
    
    str_detect(x, regex("ASPIRA", ignore_case = TRUE)) ~
      "ASPIRA Delaware",
    
    str_detect(x, regex("Newark Charter", ignore_case = TRUE)) ~
      "Newark Charter",
    
    str_detect(
      x,
      regex("Antonia Alonso|Academia Antonia", ignore_case = TRUE)
    ) ~
      "Academia Antonia Alonso",
    
    str_detect(
      x,
      regex("Edison|Thomas A", ignore_case = TRUE)
    ) ~
      "Edison (Thomas A.)",
    
    str_detect(
      x,
      regex("Charter School of New Castle", ignore_case = TRUE)
    ) ~
      "Charter School of New Castle",
    
    str_detect(x, regex("Kuumba", ignore_case = TRUE)) ~
      "Kuumba Academy",
    
    str_detect(x, regex("East.?Side", ignore_case = TRUE)) ~
      "East Side",
    
    str_detect(x, regex("\\bMOT\\b", ignore_case = TRUE)) ~
      "MOT Charter",
    
    str_detect(x, regex("Academy of Dover", ignore_case = TRUE)) ~
      "Academy of Dover",
    
    str_detect(x, regex("Early College", ignore_case = TRUE)) ~
      "Early College High School",
    
    str_detect(x, regex("Sussex Academy", ignore_case = TRUE)) ~
      "Sussex Academy",
    
    str_detect(x, regex("Freire", ignore_case = TRUE)) ~
      "Freire Charter School Wilmington",
    
    str_detect(x, regex("Providence Creek", ignore_case = TRUE)) ~
      "Providence Creek Academy",
    
    str_detect(x, regex("Campus Community", ignore_case = TRUE)) ~
      "Campus Community",
    
    str_detect(x, regex("Sussex Montessori", ignore_case = TRUE)) ~
      "Sussex Montessori School",
    
    str_detect(x, regex("First State Montessori", ignore_case = TRUE)) ~
      "First State Montessori",
    
    str_detect(x, regex("First State Military", ignore_case = TRUE)) ~
      "First State Military Academy",
    
    str_detect(x, regex("Gateway", ignore_case = TRUE)) ~
      "Gateway",
    
    str_detect(x, regex("Delaware Military", ignore_case = TRUE)) ~
      "Delaware Military Academy",
    
    str_detect(
      x,
      regex("Charter School of Wilmington", ignore_case = TRUE)
    ) ~
      "Charter School of Wilmington",
    
    str_detect(x, regex("Great Oaks", ignore_case = TRUE)) ~
      "Great Oaks",
    
    str_detect(x, regex("Positive Outcomes", ignore_case = TRUE)) ~
      "Positive Outcomes",
    
    str_detect(
      x,
      regex("Bryan Allen|BASSE", ignore_case = TRUE)
    ) ~
      "BASSE",
    
    TRUE ~ x
  )
}

# -----------------------------------------------------------------------------
# Prepare charter-level component funding
# -----------------------------------------------------------------------------

charter_lookup <- proposed_detail |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide)
  ) |>
  filter(
    IncludeInStatewide,
    LEAType == "Charter"
  ) |>
  distinct(
    DistrictCode,
    DistrictName
  )

charter_component_funding <- proposed_detail |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide)
  ) |>
  filter(
    IncludeInStatewide,
    LEAType == "Charter",
    FundingSection %in% c(
      "Opportunity Funding (State Support)",
      "Operational Funding (State Support)"
    ),
    Component %in% component_levels
  ) |>
  summarise(
    FundingAmount = sum(FundingAmount, na.rm = TRUE),
    .by = c(
      DistrictCode,
      DistrictName,
      Component
    )
  )

# Add explicit zeroes for components not present for a charter
charter_plot_data <- crossing(
  charter_lookup,
  Component = component_levels
) |>
  left_join(
    charter_component_funding,
    by = c(
      "DistrictCode",
      "DistrictName",
      "Component"
    )
  ) |>
  mutate(
    FundingAmount = coalesce(FundingAmount, 0),
    
    LEA = make_charter_label(DistrictName),
    
    ComponentLabel = recode(
      Component,
      !!!component_labels
    ),
    
    ComponentLabel = factor(
      ComponentLabel,
      levels = unname(component_labels)
    )
  )

# -----------------------------------------------------------------------------
# Calculate totals and establish chart order
# -----------------------------------------------------------------------------

charter_totals <- charter_plot_data |>
  summarise(
    TotalFunding = sum(FundingAmount),
    .by = c(
      DistrictCode,
      LEA
    )
  ) |>
  arrange(TotalFunding) |>
  mutate(
    LEA = factor(
      LEA,
      levels = LEA
    ),
    
    TotalLabel = sprintf(
      "$%.1fM",
      TotalFunding / 1e6
    )
  )

charter_plot_data <- charter_plot_data |>
  left_join(
    charter_totals |>
      select(
        DistrictCode,
        LEA,
        TotalFunding
      ),
    by = c(
      "DistrictCode",
      "LEA"
    )
  ) |>
  mutate(
    LEA = factor(
      LEA,
      levels = levels(charter_totals$LEA)
    )
  )

# -----------------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------------

charter_reconciliation <- charter_plot_data |>
  summarise(
    ComponentTotal = sum(FundingAmount),
    .by = DistrictCode
  ) |>
  left_join(
    charter_totals |>
      select(
        DistrictCode,
        TotalFunding
      ),
    by = "DistrictCode"
  ) |>
  mutate(
    Difference = ComponentTotal - TotalFunding
  )

stopifnot(
  n_distinct(charter_plot_data$DistrictCode) == 24,
  
  n_distinct(charter_plot_data$ComponentLabel) == 9,
  
  abs(
    sum(charter_plot_data$FundingAmount) -
      50063905
  ) < 0.05,
  
  all(
    abs(charter_reconciliation$Difference) < 0.01
  )
)

message("Figure 4 data validation checks passed.")

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------

maximum_total <- max(charter_totals$TotalFunding)

axis_break_max <- ceiling(maximum_total / 1e6) * 1e6

figure4 <- ggplot(
  charter_plot_data,
  aes(
    x = FundingAmount,
    y = LEA,
    fill = ComponentLabel
  )
) +
  geom_col(
    width = 0.72,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = charter_totals,
    aes(
      x = TotalFunding,
      y = LEA,
      label = TotalLabel
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 2.5,
    color = "#404040"
  ) +
  scale_fill_manual(
    values = component_colors,
    breaks = unname(component_labels),
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      axis_break_max,
      by = 1e6
    ),
    labels = label_dollar(
      scale = 1e-6,
      suffix = "M",
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.14)
    )
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = FALSE
    )
  ) +
  labs(
    x = "\nProposed Opportunity and Operational Funding",
    y = NULL,
    fill = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_family = "Arial",
    base_size = 9
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#D9E2F3",
      linewidth = 0.35
    ),
    
    axis.text.y = element_text(
      color = "#262626",
      size = 7
    ),
    
    axis.text.x = element_text(
      color = "#404040",
      size = 7
    ),
    
    axis.title.x = element_text(
      color = "#262626",
      size = 8.5,
      margin = margin(t = 7)
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 6.2,
      color = "#262626"
    ),
    
    legend.key.height = grid::unit(0.25, "cm"),
    legend.key.width = grid::unit(0.25, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin = margin(t = 2),
    
    plot.margin = margin(
      t = 5,
      r = 30,
      b = 5,
      l = 5
    )
  )

figure4

ggsave(
  filename = paste0(
    "data/output/final/",
    "figure_4_weighted_funding_by_charter.png"
  ),
  plot = figure4,
  width = 8.6,
  height = 7.0,
  units = "in",
  dpi = 300,
  bg = "white"
)
