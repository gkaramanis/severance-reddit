library(tidyverse)
library(RedditExtractoR)
library(camcorder)

gg_record(here::here("severance-reddit-temp"), width = 10, height = 8, dpi = 320)

severance_episode_discussion <- find_thread_urls(subreddit = "SeveranceAppleTVPlus", keywords = "Episode Discussion", period = "all")

sped <- severance_episode_discussion %>% 
  filter(str_detect(title, "2x")) %>% 
  filter(str_detect(title, "Pre-", negate = TRUE)) %>% 
  filter(str_detect(title, "Live", negate = TRUE)) %>% 
  mutate(title = str_remove(title, "Severance - ")) %>% 
  mutate(title = str_remove(title, "( - )*(Post-)*Episode Discussion")) %>% 
  mutate(title = str_remove_all(title, '"')) %>% 
  mutate(title = str_replace(title, "s ", "'s ")) %>% 
  mutate(title = str_replace(title, "Ovaltine", "Hello, Ms. Cobel"))

pal <- c("#F8AAB6", "#8BE076", "#4CB4E7", "#A25056", "#00957E", "#235BA8")


f1 <- "Sofia Sans Extra Condensed"
f2 <- "Bricolage Grotesque 12pt Condensed"

ggplot(sped) +
  geom_col(aes(comments, title), fill = pal[6]) +
  geom_text(aes(2e2, title, label = title), hjust = 0, size = 7, family = f1, color = "white") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = c(0, 0), labels = scales::number) +
  labs(
    title = "Number of Comments in Severance Post-Episode Discussion Threads as of March 23",
    caption = "Data: r/SeveranceAppleTVPlus · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = pal[3], color = NA),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 17, color = "white"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 28, color = "white", margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = 12, color = "white"),
    plot.margin = margin(10, 10, 10, 10)
  )
  

