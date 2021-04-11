# Load packages
library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)
library(here)

# Load font
font_add_google("Raleway")
showtext_auto()

# Generate synthetic data
set.seed(0)
grid <- rnorm(10000)
mar_right <- plogis(grid)
mar_left <- 1 - mar_right
mar_mid <- plogis(0.75 - abs(grid))
mar_tail <- plogis(-0.75 + abs(grid))

mechanism <- c(
  rep("MARRIGHT", length(grid)),
  rep("MARLEFT", length(grid)),
  rep("MARMID", length(grid)),
  rep("MARTAIL", length(grid))
)

mar_tbl <- tibble(
    x = rep(grid, 4),
    y = c(mar_right, mar_left, mar_mid, mar_tail),
    mechanism = mechanism 
  )

# Make logistic cumulative density plot
mar_tbl %>%
  ggplot(aes(x, y, colour = mechanism)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  scale_y_continuous(n.breaks = 3) +
  scale_colour_manual(
    values = c("#29ab87", "#1979a9", "#ffaa1d", "#d1395e"),
    guide = "legend",
    name = NULL
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(
    x = "Missingness covariate",
    y = "Probablity to be missing",
    title = "MISSING AT RANDOM<br>NOT MUCH <span style='color: palevioletred'>RANDOM</span> AS YOU THINK",
    subtitle = "\nMissing data is classified under three mechanisms: Missing completely random (MCAR),
missing at random (MAR) and missing not at random (MNAR). Two of these, MAR and MCAR
are frequently confused by data scientists, but the distinction between them is substantial.

When the probability of being missing is the same for all cases, the data is MCAR. This means
that incomplete data is a random sample of complete data. When the probability of being
missing is the same only within groups defined by the observed cases, the data is MAR.
However, the term MAR is quiet misleading because it implies that the data are missing
haphazardly like a coin toss. In fact, MAR means that a systematic relationship between
measured variables and the probability of missingness. Thus, incomplete data is no longer
a random sample of complete data.

MAR mechanism could be generalized in four types of model: MARLEFT, MARRIGHT, MARMID
and MARTAIL. These names simply indicate where the probability of being missing is
more on distribution of data.\n\n",
    caption = "\n\n
Source: Van Buuren, S. (2018). Flexible imputation of missing data (Second edition). Boca Raton, CRC Press, Taylor & Francis Group
Visualization: Botan Ağın
#30DayChartChallenge"
  ) +
  theme_void(base_family = "Raleway") +
  theme(
    axis.ticks = element_line(colour = "grey10", size = 0.4),
    axis.ticks.length = unit(0.2, "cm"), 
    axis.title.x = element_text(hjust = 0.51, size = 12, margin = unit(c(3, 0, 0, 0), "mm")),
    axis.title.y = element_text(size = 12, angle = 90, margin = unit(c(0, 3, 0, 0), "mm")),
    axis.text = element_text(size = 10, margin = unit(c(2, 2, 0, 0), "mm")),
    legend.key = element_rect(fill = "NA", size = 3, colour = "NA"),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),
    plot.title = element_markdown(size = 20, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(1, 1, 0.5, 1, unit = "cm")
  )
ggsave(here("plots", "day9-statistics.png"), width = 8, height = 9.5, dpi = 320)