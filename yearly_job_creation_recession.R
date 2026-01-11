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

fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

start_date <- as.Date("1999-01-01")   # include 2019 Dec to compute 2020 change

# --- PAYEMS (monthly level, thousands)
payems <- fredr(series_id = "PAYEMS", observation_start = start_date) %>%
  transmute(date = as.Date(date), nfp_k = value) %>%
  arrange(date) %>%
  mutate(year = year(date), month = month(date))

# Keep only Dec observations for annual (Dec-to-Dec) job creation
nfp_yearly <- payems %>%
  filter(month == 12) %>%
  arrange(date) %>%
  mutate(
    annual_job_creation_k = nfp_k - lag(nfp_k)   # thousand jobs
  ) %>%
  filter(year >= 2000)

# --- USREC (recession indicator)
usrec <- fredr(series_id = "USREC", observation_start = start_date) %>%
  transmute(date = as.Date(date), rec = as.integer(value)) %>%
  mutate(year = year(date))

# recession by year: flag year if ANY month is recession
rec_year <- usrec %>%
  group_by(year) %>%
  summarise(recession_year = max(rec, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2000)

# combine
df_year <- nfp_yearly %>%
  left_join(rec_year, by = "year") %>%
  mutate(
    recession_year = replace_na(recession_year, 0),
    year_f = factor(year),
    if_latest_year = year == max(year)
  )

highlight <- df_year %>% 
  filter(if_latest_year == TRUE)

median <- format(median(df_year$annual_job_creation_k), big.mark = ",", scientific = FALSE)
latest_year <- highlight$year
latest_job_creation <- highlight$annual_job_creation_k

ggplot(df_year, aes(x = year_f, y = annual_job_creation_k, fill = if_latest_year)) +
  # Shade recession years
  geom_rect(
    data = df_year %>% filter(recession_year == 1),
    aes(xmin = as.numeric(year_f) - 0.5,
        xmax = as.numeric(year_f) + 0.5,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    alpha = 0.1
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = comma(round(annual_job_creation_k, 0))),
    vjust = ifelse(df_year$annual_job_creation_k >= 0, -1, 1.5),
    size = 3
  ) +
  geom_hline(yintercept = 2049, color = "red", linetype = "dashed") +
  # scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(breaks = c(FALSE, TRUE),
                    values = c("steelblue","orange")) +
  labs(
    # title = "Annual Job Creation in 2025 was 584,000, one of the lowest, except recession years",
    title = glue("Annual Job Creation in {latest_year} was {latest_job_creation},000 vs meidan {median},000 (since 2000), one of the lowest, except recession years"),
    subtitle = "Recession (USREC) year shaded in gray",
    x = NULL,
    y = "Jobs (thousands)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
  )


