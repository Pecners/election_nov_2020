library(tidyverse)
library(sf)

# Base Plot

wards <- st_read("data/wards/ward.shp")

names(wards) <- str_to_lower(names(wards))

wards %>%
  ggplot() +
  geom_sf()

# Data

vote_counts <- read_csv("data/unofficial_vote_counts.csv") %>%
  mutate(ward = str_extract(str_trim(ward_name, side = "right"), "\\d*$"))

voter_turnout <- read_csv("data/voter_turnout.csv") %>%
  mutate(ward = str_extract(str_trim(ward_name, side = "right"), "\\d*$"))

mke_vote_counts <- vote_counts %>%
  filter(str_detect(ward_name, "City of Milwaukee"))

mke_turnout <- voter_turnout %>%
  filter(str_detect(ward_name, "City of Milwaukee"))


turnout_sf <- left_join(wards, mke_turnout) %>%
  mutate(percent_of_total = ballots_cast_total / registered_voters_total)

vote_counts_sf <- left_join(wards, mke_vote_counts) %>%
  select(ward, biden:geometry) %>%
  pivot_longer(cols = biden:write_in, names_to = "candidate", values_to = "votes") %>%
  group_by(ward) %>%
  mutate(vote_share = votes / sum(votes)) %>%
  ungroup() 

turnout_sf %>%
  ggplot(aes(fill = percent_of_total)) +
  geom_sf(color = "white") +
  scale_color_gradient(aesthetics = "fill" ,low = "yellow", high = "blue") +
  theme_void() +
  labs(title = "Voter Turnout as Percent of Registered Voters")

# Ward Winner

t <- vote_counts_sf %>%
  group_by(ward) %>%
  filter(votes == max(votes) & votes > 0) %>%
  group_by(candidate) %>%
  tally()
  ggplot(aes(fill = candidate, geometry = geometry)) +
  geom_sf(color = "white") +
  theme_void() +
  labs(title = "Winning Candidate by Ward")

# Vote Share
  
vote_counts_sf %>%
  filter(candidate == "biden") %>%
  ggplot(aes(fill = vote_share, geometry = geometry)) +
  geom_sf(color = "white") +
  scale_color_gradient(aesthetics = "fill" ,low = "red", high = "blue") +
  theme_void() +
  labs(title = "Biden Vote Share")

# Turnout vs. Biden

full <- left_join(turnout_sf, vote_counts_sf) %>%
  group_by(ward) %>%
  mutate(winner = ifelse(votes == max(votes), "Biden", "Trump")) %>%
  filter(candidate == "biden")

full %>%
  mutate(percent_of_total = ifelse(percent_of_total > 1, 1, percent_of_total)) %>%
  ggplot(aes(percent_of_total, vote_share, color = winner)) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(labels = scales::percent, limits = c(.3, 1)) +
  scale_x_continuous(labels = scales::percent, limits = c(.3, 1)) +
  labs(title = "Milwaukee Wards with Higher Turnout Saw Lower Vote Share for Biden",
       subtitle = "Each Point Represents One City of Milwaukee Voting Ward",
       y = "Biden's Share of the Vote",
       x = "Voter Turnout as Percent of Registered Voters",
       caption = "One ward was adjusted down from 120% turnout to 100%; 5 votes were cast where 4 were registered before the election.",
       color = "Ward Winner") +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

turnout_sf %>%
  ggplot(aes(fill = ifelse(percent_of_total >= .8, ">80%", "<80%"))) +
  geom_sf(color = "white") +
  theme_void() +
  labs(title = "Voter Turnout as Percent of Registered Voters") 



model <- lm(vote_share ~ percent_of_total, data = full %>%
              mutate(percent_of_total = ifelse(percent_of_total > 1, 1, percent_of_total)))
summary(model)

# How did turnout relate to ward demographics?