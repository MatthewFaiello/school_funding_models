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

weighted_lea_comparison <- read_csv(
  "data/output/final/11_opportunity_operational_lea_comparison.csv",
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
  "Opportunity: Low income",
  "Opportunity: Active MLL",
  "Operational: Total enrollment",
  "Operational: Low income",
  "Operational: Active MLL",
  "Operational: Basic special education",
  "Operational: Intensive special education",
  "Operational: Complex special education",
  "Operational: Vocational enrollment"
)

names(component_labels) <- component_levels

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
# Prepare district-level component funding
# -----------------------------------------------------------------------------

district_lookup <- proposed_detail |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide)
  ) |>
  filter(
    IncludeInStatewide,
    LEAType == "District"
  ) |>
  distinct(
    DistrictCode,
    DistrictName
  )

district_component_funding <- proposed_detail |>
  mutate(
    IncludeInStatewide = as.logical(IncludeInStatewide)
  ) |>
  filter(
    IncludeInStatewide,
    LEAType == "District",
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

# Add explicit zeroes for any LEA-component combination not present in the file
district_plot_data <- crossing(
  district_lookup,
  Component = component_levels
) |>
  left_join(
    district_component_funding,
    by = c(
      "DistrictCode",
      "DistrictName",
      "Component"
    )
  ) |>
  mutate(
    FundingAmount = coalesce(FundingAmount, 0),
    
    LEA = DistrictName |>
      str_remove(" School District$") |>
      str_replace(
        "^New Castle County Vocational-Technical$",
        "New Castle County Vo-Tech"
      ) |>
      str_replace("^POLYTECH$", "Polytech"),
    
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
# Calculate totals and establish LEA order
# -----------------------------------------------------------------------------

district_totals <- district_plot_data |>
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

district_plot_data <- district_plot_data |>
  left_join(
    district_totals |>
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
      levels = levels(district_totals$LEA)
    )
  )

# -----------------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------------

expected_district_total <- weighted_lea_comparison |>
  filter(
    LEAType == "District",
    FundingCategory %in% c(
      "Opportunity Funding",
      "Operational Funding"
    )
  ) |>
  summarise(
    ProposedFundingAmount = sum(ProposedFundingAmount, na.rm = TRUE)
  ) |>
  pull(ProposedFundingAmount)

stopifnot(
  n_distinct(district_plot_data$DistrictCode) == 19,
  
  n_distinct(district_plot_data$ComponentLabel) == 9,
  
  !any(district_plot_data$DistrictCode == 14L),
  
  abs(
    sum(district_plot_data$FundingAmount) -
      expected_district_total
  ) < 0.05,
  
  all(
    abs(
      district_plot_data |>
        summarise(
          ComponentTotal = sum(FundingAmount),
          .by = DistrictCode
        ) |>
        left_join(
          district_totals,
          by = "DistrictCode"
        ) |>
        transmute(
          Difference = ComponentTotal - TotalFunding
        ) |>
        pull(Difference)
    ) < 0.01
  )
)

message("Figure 3 data validation checks passed.")

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------

maximum_total <- max(district_totals$TotalFunding)

axis_break_max <- floor(maximum_total / 1e7) * 1e7

figure3 <- ggplot(
  district_plot_data,
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
    data = district_totals,
    aes(
      x = TotalFunding,
      y = LEA,
      label = TotalLabel
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 2.6,
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
      by = 1e7
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
    fill = NULL,
    caption = stringr::str_wrap(
      paste(
        "District allocations under the independently reproduced proposed model.",
        "Charters are not displayed, DAFB is excluded, and current funding analogues",
        "remain pending confirmation from OMB and CGO."
      ),
      width = 150
    )
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
      size = 7.5
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
      size = 6.3,
      color = "#262626"
    ),
    legend.key.height = grid::unit(0.25, "cm"),
    legend.key.width = grid::unit(0.25, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin = margin(t = 2),
    
    plot.caption = element_text(
      size = 6.7,
      color = "#595959",
      hjust = 0,
      margin = margin(t = 7)
    ),
    plot.margin = margin(
      t = 5,
      r = 30,
      b = 5,
      l = 5
    )
  )

figure3

ggsave(
  filename = paste0(
    "data/output/final/",
    "figure_3_weighted_funding_by_district.png"
  ),
  plot = figure3,
  width = 8.6,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "white"
)
