# US Job Creation by Year
setwd("/Users/takayukitamura/Documents/R_Computing/nfp")
library(tidyverse)
library(lubridate)
library(ggtext)
library(glue)
library(patchwork)
library(showtext)
library(scales)
library(fredr)

# Load data
#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

# Overall unemployment
nfp_data <- fredr(series_id = "PAYEMS") %>% 
  select(date = date, nfp = value) %>% 
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label = TRUE)
  ) %>%
  filter(date >= "2009-12-01")
  
# nfp_data <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PAYEMS&scale=left&cosd=1939-01-01&coed=2025-07-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-08-01&revision_date=2025-08-01&nd=1939-01-01") %>%
#   rename(date = observation_date, nfp = PAYEMS) %>% 
#   mutate(
#     date = as.Date(date),
#     year = year(date),
#     month = month(date, label = TRUE)
#   ) %>%
#   filter(date >= "2009-12-01")

# December NFP of previous year
dec_nfp_prev_year <- nfp_data %>% 
  filter(month == "Dec") %>% 
  mutate(year = year(date) + 1) %>% 
  select(year, nfp) %>% 
  rename(nfp_dec_prev = nfp) %>% 
  filter(year != 2020)

# Join and compute job creation
nfp_cumulative <- nfp_data %>% 
  left_join(dec_nfp_prev_year, by = "year") %>% 
  filter(year >= 2010) %>% 
  mutate(job_creation = nfp - nfp_dec_prev)

# Identify special years for styling
plot_data <- nfp_cumulative %>% 
  filter(!is.na(job_creation)) %>% 
  mutate(line_type = case_when(
    year == 2025 ~ "2025",
    year == 2010 ~ "2010",
    TRUE ~ "Other"
  ))

# Label latest month of each year
label_points <- plot_data %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  filter(year >= 2021 | year == 2010) %>% 
  mutate(label_color = case_when(
    year == 2025 ~ "red",
    year == 2010 ~ "blue",
    TRUE ~ "gray"
  ))

# Plot
ggplot(plot_data, aes(x = month, y = job_creation, group = factor(year))) +
  geom_line(aes(color = line_type), linewidth = 1.2, show.legend = FALSE) +
  scale_color_manual(values = c("2025" = "red", "2010" = "blue", "Other" = "gray70")) +
  geom_text(data = label_points,
            aes(label = year),
            color = I(label_points$label_color),  # <- fix to use literal colors
            hjust = -0.2,
            vjust = 0,
            size = 3.5,
            show.legend = FALSE) +
  scale_x_discrete(expand = expansion(add = c(0.1, 1.5))) +
  scale_y_continuous(
    limits = c(-200, 8000),
    breaks = seq(0, 8000, 2000),
    labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "US Job Creation by Year (2010–2025)",
    subtitle = "Cumulative increase in total nonfarm payrolls from previous December",
    x = NULL,
    y = "Jobs Created (x1,000)",
    caption = "Source: FRED (PAYEMS) | Created by Takayuki Tamura"
  ) +
  theme_classic(base_family = "Helvetica") +
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

