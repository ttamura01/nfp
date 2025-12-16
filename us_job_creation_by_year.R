setwd("/Users/takayukitamura/Documents/R_Computing/us_employment")
getwd()
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(showtext)
library(scales)
library(fredr)

# Load and prepare the full data
nfp_data <-fredr(series_id = "PAYEMS" ) %>%
  select(date, nfp = value) %>% 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date, label = TRUE))

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

# Overall unemployment
# nfp_data <- fredr(series_id = "PAYEMS") %>% 
#   select(date, nfp = value)

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
    subtitle = "Cumulative job creations in 2025 is one of the lowest since 2010",
    x = NULL,
    y = "Cumulative Job Creation (x1,000)",
    caption = "<i>FRED(Federal Reserve Economic Data) by Takayuki Tamura"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_textbox_simple(size = 15),
    plot.caption.position = "panel",
    plot.caption = element_markdown(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

ggsave("us_job_creation_1.png", height = 6, width = 6.5)
