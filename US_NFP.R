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
  select(date, nfp = value)

## import NFP data ("PAYEMS") to 'nfp_data'
# nfp_data <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PAYEMS&scale=left&cosd=1939-01-01&coed=2025-08-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-09-05&revision_date=2025-09-05&nd=1939-01-01") %>% 
#   rename(date = observation_date, nfp = PAYEMS)
  

# updates <- tribble(
#   ~date, ~nfp,
#   "2025-04-01", 159575
# )

# nfp_data <- rbind(nfp_data, updates)

head(nfp_data)
tail(nfp_data, n = 10)

# nfp_062024 <- data.frame("date" = c("2024-05-01", "2024-06-01"),
#                          "nfp" = c(158558, 158638))

# tail(rbind(nfp_data, nfp_062024))

# nfp_data <- rbind(nfp_data, nfp_062024)
# 
# tail(nfp_data)

# Calculate month-to-month changes
nfp_data$monthly_change <- c(NA, diff(nfp_data$nfp))

head(nfp_data)
tail(nfp_data, 20)
sapply(nfp_data,class)

# nfp_data %>% 
#   filter(date >= "2025-01-01" & date <= "2025-08-01") %>% 
#   mutate(total = sum(monthly_change))

# Convert Date column to Date format
nfp_data$date <- as.Date(nfp_data$date, format = "%Y-%m-%d")

# Plot the data
# highlight_data <- nfp_data %>% 
#   filter(date == "2024-04-01")

highlight_data <- nfp_data %>% 
  filter(date == max(date))

latest_month <- month(highlight_data$date, label = TRUE, abbr = FALSE)
year <- year(highlight_data$date)
latest_job_change <- highlight_data$monthly_change
latest_nfp <- nfp_data %>% 
  filter(latest_month == T) %>% 
  mutate(monthly_change_status = if_else(is.na(monthly_change), NA_character_,
                                         if_else(monthly_change > 0, "increased",
                                                 if_else(monthly_change < 0, "decreased", 
                                                         "flat"))))

latest_monthly_status <- latest_nfp$monthly_change_status

consensus <- 45
latest_jobless <- 4.6
previsou_jobless <- 4.4

jobless_change <- latest_jobless - previsou_jobless
jobless_status <- if_else(jobless_change > 0, "rose",
                          if_else(jobless_change < 0, "fell",
                                  "flat"))


compared_consensus <- latest_nfp %>% 
  mutate(vs_consensus = if_else(monthly_change > consensus, "beat",
                                if_else(monthly_change < consensus, "fell short",
                         "flat")))
    
comp_consensus <- compared_consensus$vs_consensus

# month("2025-08-01", )

tail(nfp_data)

# nfp_data_filter <- nfp_data
# nfp_data_filter$latest_month <- ifelse(nfp_data$date == max(nfp_data$date), TRUE, FALSE)

# Create a new variable to indicate whether it's the latest month or not
nfp_data$latest_month <- ifelse(nfp_data$date == max(nfp_data$date), TRUE, FALSE)

tail(nfp_data)

nfp_data %>% 
  #mutate(highlight=(date =="2024-04-01")) %>% 
  #filter(date >"1988-01-01" & (date < "2020-01-01" | date > "2020-10-01")) %>%
  filter(date >= "2022-01-01") %>% 
  ggplot(aes(x = date, y = monthly_change, fill = latest_month)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Monthly Changes in US Non-Farm Payrolls",
       x = NULL,
       y = "Monthly Change in NFP(x1,000)",
       caption = "FRED(Federal Reserve Economic Data)") +
  scale_fill_manual(breaks = c(F,T),
                     values = c("#AAAAAA", "#0000FF")) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple())
  
## add the latest label 

nfp_data %>%  
  drop_na(monthly_change) %>% 
  #filter(date >"1988-01-01" & (date < "2020-01-01" | date > "2020-10-01")) %>%
  filter(date >= "2021-01-01") %>% 
  ggplot(aes(x = date, y = monthly_change, fill = latest_month)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = glue("Monthly Changes in US Non-Farm Payrolls in {latest_month} {year}, {latest_monthly_status} {latest_job_change},000, {comp_consensus} to the consensus at {consensus},000; Jobless Rate {jobless_status} to {latest_jobless}% from {previsou_jobless}%"),
    #title = "US added only 22,000 jobs in August vs. market expected 75,000",
       x = NULL,
       y = "Monthly Change in NFP(x1,000)",
       caption = "<i>FRED(Federal Reserve Economic Data)") +
  scale_fill_manual(breaks = c(F,T),
                    values = c("#AAAAAA", "#0000FF")) +
  geom_text(data = subset(nfp_data, latest_month == TRUE), 
            aes(label = glue("{monthly_change}")), vjust = -0.5, hjust = 0.5,
            color = "blue") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 20, face = "bold"),
    plot.caption.position = "panel",
    # plot.caption = element_textbox_simple())
    plot.caption = element_markdown())

ggsave("monthly_nfp.png", height = 6, width = 6.5)

## Just NFP data
nfp_data %>% 
  ggplot(aes(x = date, y = nfp, fill = latest_month)) +
  geom_line() +
  scale_fill_manual(values = c("grey", "blue"), guide = FALSE) + # Specify colors for latest and other months
  geom_text(data = subset(nfp_data, latest_month == TRUE), 
            aes(label = glue("Latest: {comma(nfp)}")), vjust = -0.5, hjust = 0.85,
            color = "blue") +
  scale_y_continuous(limits = c(0, 170000),
                     breaks = seq(0, 160000, 40000),
                     labels =  label_comma(accuracy = 0.1)) +
  labs(title = "US Non-Farm Payrolls",
       y = "NFP (x 1,000)",
       x = NULL) 

ggsave("us_nfp.png",height = 6.6, width = 6 )






