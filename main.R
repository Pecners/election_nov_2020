library(tidyverse)
library(sf)

# Base Plot

wards <- st_read("data/wards/ward.shp")

names(wards) <- str_to_lower(names(wards))

wards %>%
  ggplot() +
  geom_sf()

# Data

turnout_16 <- "https://data.milwaukee.gov/dataset/c48ea33c-c332-430b-8c76-7269986c21ed/resource/feffd844-d2ef-4c4d-aa03-c474c5364455/download/voterturnoutbywardnov82016.csv"

t_16 <- read_csv(turnout_16) %>%
  select(-c(4:5)) 

names(t_16) <- str_replace_all(str_to_lower(names(t_16)), " ", "_")

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

t_20 <- voter_turnout %>%
  filter(str_detect(ward_name, "City of Milwaukee")) %>%
  select(-c(1:2))

all_turnout <- left_join(t_20, t_16 %>% modify_at("ward", as.character), by = "ward", suffix = c("_20", "_16")) %>%
  mutate(per_16 = ballots_cast_total_16 / registered_voters_total_16,
         per_20 = ballots_cast_total_20 / registered_voters_total_20,
         diff_rv = registered_voters_total_20 - registered_voters_total_16,
         diff_turnout_per = per_20 - per_16)

all_turnout_sf <- left_join(wards, all_turnout, by = c("WARD" = "ward"))

all_turnout_sf %>%
  ggplot(aes(fill = ifelse(diff_rv > 0, "More", "Less"))) +
  geom_sf(color = "white")

all_turnout %>%
  ggplot(aes(diff_rv, diff_turnout_per)) +
  geom_point() +
  theme_minimal()

