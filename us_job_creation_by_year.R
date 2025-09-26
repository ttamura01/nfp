setwd("/Users/takayukitamura/Documents/R_Computing/us_employment")
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(showtext)
library(scales)

# Load and prepare the full data
nfp_data <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PAYEMS&scale=left&cosd=1939-01-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-08-01&revision_date=2025-08-01&nd=1939-01-01") %>%
  rename(date = observation_date, nfp = PAYEMS) %>%
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date, label = TRUE))

# Get December NFP of previous year for baseline
dec_nfp_prev_year <- nfp_data %>%
  filter(month(date) == 12) %>%
  mutate(year = year(date) + 1) %>%
  select(year, nfp) %>%
  rename(nfp_dec_prev = nfp)

# Compute job creation
nfp_cumulative <- nfp_data %>%
  filter(year >= 2010 & year <= 2025) %>%
  left_join(dec_nfp_prev_year, by = "year") %>%
  mutate(job_creation = nfp - nfp_dec_prev) %>%
  filter(year != 2020 & !is.na(job_creation)) %>%
  mutate(line_type = case_when(
    year == 2025 ~ "highlight_2025",
    year == 2010 ~ "highlight_2010",
    # year == 2008 ~ "highlight_2008",
    # year == 2009 ~ "highlight_2009",
    TRUE ~ "normal"
  ))

# Label only years 2021–2025
label_points <- nfp_cumulative %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  filter(year >= 2021|year == 2010)

# Plot
ggplot(nfp_cumulative, aes(x = month, y = job_creation, group = year)) +
  geom_line(aes(color = line_type, size = line_type, linetype = line_type), show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "highlight_2025" = "red",
      "highlight_2010" = "blue",
      "normal" = "gray70"
    ),
    labels = c("highlight_2010" = "2010", "highlight_2025" = "2025"),
    breaks = c("highlight_2010", "highlight_2025")
  ) +
  scale_size_manual(
    values = c(
      "highlight_2025" = 1.6,
      "highlight_2010" = 1.2,
      "normal" = 0.6
    ),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c(
      "highlight_2025" = "solid",
      "highlight_2010" = "solid",
      "normal" = "solid"
    ),
    guide = "none"
  ) +
  geom_text(
    data = label_points,
    aes(label = year, color = line_type),
    hjust = -0.2, vjust = 0.5,
    show.legend = FALSE, size = 6
  ) +
  scale_x_discrete(expand = expansion(add = c(0.1, 1.5))) +
  scale_y_continuous(
    limits = c(-200, 8000),
    breaks = seq(0, 8000, 2000),
    labels = scales::label_number(big.mark = ",")) +
  # labs(
  #   title = "US Job Creation by Year (2010–2025, excluding 2020)",
  #   subtitle = "Cumulative job gains from December of previous year\n2025 (red), 2010 (blue, dashed), others gray; labels shown for 2021–2025",
  #   x = "Month",
  #   y = "Cumulative Job Creation (Millions)",
  #   color = "Highlighted Years"
  # ) +
  labs(
    title = "US Job Creation by Year (2010–2025, excluding 2020)",
    subtitle = "Cumulative job creation in 2025 is the lowest since 2010",
    x = NULL,
    y = "Cumulative Job Creation (x1,000)",
    caption = "<i>FRED(Federal Reserve Economic Data)"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 16),
    plot.caption.position = "panel",
    plot.caption = element_markdown(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

ggsave("us_job_creation_1.png", height = 6, width = 6.5)
