setwd("/Users/takayukitamura/Documents/R_Computing")
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(showtext)
library(scales)
library(fredr)

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

# Overall unemployment
nfp_data <- fredr(series_id = "PAYEMS") %>% 
  select(date, nfp = value) # %>% 
 # mutate(month = month(date, label = TRUE, abbr = FALSE))

nfp_health_data <- fredr(series_id = "usehs") %>% 
  select(date, nfp = value) # %>% 
 # mutate(month = month(date, label = TRUE, abbr = FALSE))

nfp_1 <- nfp_data %>% 
  left_join(., nfp_health_data, by = "date") %>% 
  rename(nfp = nfp.x, nfp_ed_health = nfp.y) 

nfp_1$monthly_chg <- c(NA, diff(nfp_1$nfp))
nfp_1$monthly_chg_1 <- c(NA, diff(nfp_1$nfp_ed_health))
nfp_1 <- nfp_1 %>% 
  mutate(monthly_chg_ex = monthly_chg - monthly_chg_1) %>% 
  # filter(date >= "2025-01-01") %>% 
  select(date, monthly_chg, monthly_chg_1, monthly_chg_ex)

# nfp_long <- nfp_1 %>% 
#   pivot_longer(-date, names_to = "sector", values_to = "monthly_change")
# 
# nfp_long %>% 
#   ggplot(aes(x = date, y = monthly_change, colour = sector)) +
#   geom_line()

# Get december nfp of previous year for base line

# Get December NFP of previous year for baseline
dec_nfp_prev_year <- nfp_1 %>%
  select(date, nfp, nfp_ed_health) %>% 
  filter(month(date) == 12) %>%
  mutate(year = year(date) + 1) %>%
  select(year, nfp, nfp_ed_health) %>%
  rename(nfp_dec_prev = nfp, nfp_ed_health_prev =nfp_ed_health)

# Compute job creation
nfp_cumulative <- nfp_1 %>%
  select(date, nfp, nfp_ed_health) %>% 
  mutate(year = year(date)) %>% 
  filter(date >= "2025-01-01") %>%
  left_join(dec_nfp_prev_year, by = "year") %>%
  mutate(job_creation = nfp - nfp_dec_prev, 
         job_creation_1 = nfp_ed_health - nfp_ed_health_prev,
         job_creation_ex = (job_creation - job_creation_1)) %>%
  select(date, job_creation, job_creation_1, job_creation_ex)

nfp_cum <- nfp_cumulative %>% 
  filter(date != "2026-01-01")

update <- tribble(~date, ~job_creation, ~job_creation_1, ~job_creation_ex,
                  "2026-01-01", 311, 834, -477)

nfp_cum <- rbind(nfp_cum, update)

nfp_cum %>% 
  pivot_longer(-date, names_to = "sector", values_to = "job_creation") %>% 
  ggplot(aes(x = date, y = job_creation, colour = sector)) +
  geom_line(show.legend = F) +
  annotate(geom = "label",
           x = as.Date("2026-01-01"),
           y = c(834, 311, -477),
           label = c("Private Education\n and Healthcare Services",
                     "Total",
                     "Total Ex-Education \n& Healthcare"),
           vjust = -0.1,
           hjust = 1) +
  coord_cartesian(clip = "off", expand = FALSE, 
                  xlim = as.Date(c("2025-01-01", "2026-01-01")),
                  ylim = c(-550, 950)) +
  labs(title = "U.S. Accumurative Job Creation since January 2026 by sector",
       x = NULL,
       y = "Job Creation (thouthand)",
       caption = "source: FRED, by Takayuki Tamura") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin_auto(10, 10, 20, 10))
  )

ggsave("cummulative_job.png", width = 5.1, height = 5.1)
  
  
