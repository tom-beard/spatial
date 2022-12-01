library(tidyverse)
library(lubridate)
library(fs)

source_url <- "https://www.stats.govt.nz/information-releases/building-consents-issued-september-2022/"

data_dir <- "D:/GIS/building-consents"
bc_file <- "New dwellings consented by 2021 statistical area 2 (Monthly).csv"
bc_df <- read_csv(path(data_dir, bc_file))

highlight_sa2s <- c("Mount Cook West", "Aro Valley", "Wellington Central", "Thorndon", "Wadestown", "Karori South")
other_central_sa2s <- c("Dixon Street", "Vivian West", "Vivian East", "Courtenay", "Mount Victoria",
                        "Mount Cook East", "Pipitea-Kaiwharawhara")
combined_sa2s <- union(highlight_sa2s, other_central_sa2s)

bc_df %>% 
  filter(SA2_name %in% combined_sa2s, month > ymd(20160901)) %>% 
  mutate(SA2_type = ifelse(SA2_name %in% highlight_sa2s, "Low consent SA2s", "Other SA2s")) %>% 
  ggplot(aes(x = month, y = total_dwelling_units, fill = SA2_name)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "", fill = "SA2",
       title = "Wellington residential building consents for selected central Statistical Area 2s",
       subtitle = "Total dwellings, up to September 2022",
       caption = str_glue("source: {source_url}")) +
  facet_grid(rows = vars(SA2_type)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.spacing.y = unit(1, "line"))
