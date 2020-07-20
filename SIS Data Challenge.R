library(tidyverse)
library(tidylog)
library(readxl)
library(scales)
library(glmnet)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(Matrix)
library(fastDummies)
library(scales)

#Ryan Davis RAPM - https://github.com/rd11490/NBA_Tutorials/tree/master/rapm
#RAPM Evolving Hockey https://github.com/evolvingwild/hockey-all/blob/master/RAPM_Goals.R
#BAysian Plus Minus Basketball https://medium.com/@akashpendyala/bayesian-adjusted-plus-minus-3d989b94a93d
#NFL Playing Surface Analytics - https://www.kaggle.com/c/nfl-playing-surface-analytics/notebooks

#Which is the most valuable defensive line position? You can define the positions however you like.
#What is the nature of the distribution of talent between the defensive line positions, as you define them?
#Not all situations are created equal. In which in-game or roster construction scenarios would the answer to Question 1 change?


#Each play includes anybody who was either
  #(a) in a 3 point stance
  #(b) lined up standing on the edge on the line of scrimmage
  #(c) usually lines up as a DL, even if they might have been up or off the ball on this play.

# https://www.pff.com/news/pro-defensive-line-techniques-the-2017-prototypes

sis_data <- read_excel("~/Downloads/sis_data.xlsx", 
                       na = "NULL") %>% 
  mutate(is_challenged = ifelse(EventType %in% c("challenge rush", "challenge pass"), 1, 0),
         EventType = case_when(
         EventType == "challenge pass" ~ "pass",
         EventType == "challenge rush" ~ "rush",
         TRUE ~ as.character(EventType)),
         Position = paste0(OnFieldPosition, "-", SideOfBall, "-", TechniqueName),
         Player = paste0(Name, "-", OnFieldPosition, "-", TechniqueName),
         StadiumName = ifelse(GameID == 2710, "NRG Stadium", StadiumName),
         YardsToGoal = ifelse(SideOfField == "Own", 100 - StartYard, StartYard),
         AndGoal = ifelse(SideOfField == "Oppo" & ToGo == StartYard, 1, 0),
         Down = case_when(
           Down == 1 ~ "1st",
           Down == 2 ~ "2nd",
           Down == 3 ~ "3rd",
           Down == 4 ~ "4th"
         ),
         QuarterName = case_when(
           Quarter == 1 ~ "1st",
           Quarter == 2 ~ "2nd",
           Quarter == 3 ~ "3rd",
           Quarter == 4 ~ "4th",
           Quarter == 5 ~ "OT"
         ),
         Prototype = case_when(
           TechniqueName %in% c("2", "2i", "3") ~ "4-3 Pass Rush Tackle",
           TechniqueName %in% c("4", "4i", "5") ~ "3-4 Defensive End",
           TechniqueName %in% c("7", "6", "9") ~ "4-3 DE / 3-4 OLB",
           TechniqueName %in% c("0", "1") ~ "Nose Tackle",
           TRUE ~ as.character(TechniqueName)
         ),
         PlayerProto = paste0(Name, ";", Prototype),
         situation = case_when(
           Down == "1st" & AndGoal == 0 & ToGo == 10 ~ "1st and 10",
           Down == "1st" & AndGoal == 0 & ToGo < 10 ~ "1st and Less than 10",
           Down == "1st" & AndGoal == 1 ~ "1st and Goal",
           Down == "2nd" & AndGoal == 0 & ToGo <= 3 ~ "2nd and Short",
           Down == "2nd" & AndGoal == 0 & ToGo > 3 & ToGo <= 6 ~ "2nd and Medium",
           Down == "2nd" & AndGoal == 0 & ToGo > 6 & ToGo <= 9 ~ "2nd and Long",
           Down == "2nd" & AndGoal == 0 & ToGo > 9 ~ "2nd and Very Long",
           Down == "2nd" & AndGoal == 1 ~ "2nd and Goal",
           Down == "3rd" & AndGoal == 0 & ToGo <= 3 ~ "3rd and Short",
           Down == "3rd" & AndGoal == 0 & ToGo > 3 & ToGo <= 6 ~ "3rd and Medium",
           Down == "3rd" & AndGoal == 0 & ToGo > 6 & ToGo <= 9 ~ "3rd and Long",
           Down == "3rd" & AndGoal == 0  &ToGo > 9 ~ "3rd and Very Long",
           Down == "4th" & AndGoal == 1 ~ "4th and Goal",
           Down == "4th" & AndGoal == 0 ~ "4th Down"
         )) %>% 
  filter(Spike != 1 | is.na(Spike)) #Remove Spikes


## ------------------------ ##
## BEGIN MERGE WITH NFL PBP ##
## ------------------------ ##


games <- sis_data %>% 
  select(Season, StadiumName, Week, GameID) %>% 
  distinct() 

games_nfl_fastr <- nflfastR::fast_scraper_schedules(2019) %>%
  mutate(StadiumName = case_when(
    stadium == "Azteca Stadium" ~ "Estadio Azteca",
    stadium == "TIAA Bank Stadium" ~ "TIAA Bank Field",
    stadium == "StubHub Center" ~ "Dignity Health Sports Park",
    stadium == "Ring Central Coliseum" ~ "O.co Coliseum",
    stadium == "Empower Field at Mile High" ~ "Broncos Stadium at Mile High",
    stadium == "FedExField" ~ "FedEx Field",
    TRUE ~ as.character(stadium)
  )) %>%
  right_join(games, by = c("StadiumName" = "StadiumName", "week" = "Week")) %>% 
  select(week, game_id, GameID) 

pbp <- purrr::map_df(2019:2019, function(x) { #Copied straight from, obviously not very good code
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
  )
}) %>% 
  inner_join(games_nfl_fastr)

play_details_nfl_fastr <- pbp %>% 
  select(game_id, GameID, play_id, desc, posteam_type, qtr, quarter_seconds_remaining, shotgun, no_huddle, 
         weather, play_clock, surface, cp, cpoe, qb_hit, qb_spike, qb_scramble, qb_dropback, pass_length, pass_location,
         air_yards, yards_after_catch, contains("epa"), -contains("total"), passer_player_name, receiver_player_name, rusher_player_name,
         score_differential)


sis_data_one_line <- 
  sis_data %>% 
  group_by(GameID, EventID) %>% 
  mutate(PlayerOnPlay = paste0("player_", 1:n())) %>% 
  select(GameID, EventID, OffensiveTeam, DefensiveTeam, Quarter, QuarterName, Down, ToGo, AndGoal, EventType, EPA, TimeLeft, PlayerOnPlay, PlayerProto) %>% 
  spread(key = PlayerOnPlay, value = PlayerProto) %>% 
  select(-player_10, everything(), player_10)

player_matrix <- sis_data_one_line %>% select(GameID, EventID, starts_with("player_")) %>% 
  pivot_longer(cols = c("player_1", "player_2", "player_3", "player_4", "player_5",
                        "player_6", "player_7", 'player_8', "player_9", "player_10"))  %>% 
  add_column(count = 1) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(id_cols = c(GameID, EventID),
              names_from = value,
              values_from = count, 
              values_fill = list(count = 0), 
              values_fn = list(count = mean))


merged_data <- sis_data_one_line %>% 
  inner_join(play_details_nfl_fastr, by = c("GameID" = "GameID", "Quarter" = "qtr", 
                                            "TimeLeft" = "quarter_seconds_remaining")) %>%
  separate(weather, into = c("Temp", "Humidity", "Wind"), sep = ",") %>% 
  separate(Temp, into = c("Conditions", "Temp"), sep = "Temp:") %>% 
  mutate(Conditions = tolower(Conditions),
         Conditions = case_when(
           Conditions %in% c("sunny ", "fair" , "mostly sunny ", "clear skies ", "partly sunny ", "clear ", "mostly clear ",
                             "sunny and cool ") ~ "Sunny",
           Conditions %in% c("rain ", "freezing rain ", "light rain ", "rain and wind ", "raining ") ~ "Rain",
           Conditions %in% c("partly cloudy ", "cloudy ", "cloudy", "cloudy and cool ", "mostly cloudy ", "overcast ", "fair ", "cold ") ~ "Cloudy",
           Conditions %in% c("n/a ", "n/a (indoors) ", "controlled climate ", "") ~ "Indoors",
           Conditions %in% c("snow ") ~ "Snow",
           TRUE ~ as.character(Conditions)
         ),
         Temp = parse_number(Temp),
         Humidity = parse_number(Humidity))



######################### 


## ------------------------ ##
##        BEGIN EDA         ##
## ------------------------ ##


sis_data %>% 
  summarise(games = n_distinct(GameID),
            events = n_distinct(EventID),
            players = n_distinct(PlayerId)) 

sis_data %>% 
  group_by(OnFieldPosition, SideOfBall, TechniqueName) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

event_type_ggplot <- ggplot(sis_data, aes(x = EventType)) + 
  geom_bar()

event_type_ggplot + facet_wrap(~OffensiveTeam)
event_type_ggplot + facet_wrap(~DefensiveTeam)
event_type_ggplot + facet_wrap(~Down)

rushers_per_play <- sis_data %>% 
  group_by(GameID, EventID, Down, ToGo, OffensiveTeam, DefensiveTeam) %>% 
  summarise(PassRushers = sum(IsRushing),
            Players = n_distinct(PlayerId)) 


rusher_table <- rushers_per_play %>% 
  group_by(Down, ToGo, PassRushers) %>% 
  summarise(n = n()) %>% 
  group_by(Down, ToGo) %>% 
  mutate(sample_size = sum(n),
         frequency = n / sample_size) %>% 
  filter(ToGo <= 10,
         sample_size > 100) %>% 
  select(-n, sample_size) %>% 
  filter(Down %in% c("2nd", "3rd"))

ggplot(rusher_table, aes(x = as.factor(PassRushers), y = as.factor(ToGo), fill = frequency)) +
  geom_raster() +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  facet_wrap(~Down, scales = "free_x") + 
  labs(x = "Pass Rushers", y = "Yards To Go", title = "Number of Pass Rushers by Yardage - 2nd and 3rd Down")

#While I am not a fan of binning, I believe it provides an interpretable framework to answer the question at hand
#, and also increases the sample size, 


# Player Stat Summary
sis_data %>% 
  group_by(PlayerId, Name, DefensiveTeam) %>% 
  summarise(n = n(), 
            EPARate = sum(EPA)/n,
            Rushes = sum(IsRushing),
            Pressures = sum(Pressure),
            PressuresPerRush = Pressures / Rushes,
            SoloTackles = sum(SoloTackle),
            SoloAssists = sum(SoloTackle),
            PassBreakup = sum(PassBreakup),
            ForcedFumble = sum(ForcedFumble)
  ) %>% 
  arrange(desc(PressuresPerRush)) %>% 
  filter(Rushes > 25)

sis_data %>% 
  group_by(OnFieldPosition, TechniqueName) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% View()

sis_data %>% 
  filter(EventType == "rush") %>% 
  ggplot(aes(x = UsedDesignedGap, y = EPA, group = UsedDesignedGap)) + 
  geom_boxplot() +
  coord_flip()


merged_data %>% 
  group_by(DefensiveTeam) %>% 
  summarise(plays = n(),
            CPOE = mean(cpoe, na.rm = TRUE),
            EPA = mean(EPA)) %>% 
  inner_join(nflfastR::teams_colors_logos, by = c("DefensiveTeam" = "team_nick")) %>% 
  ggplot(aes(x = CPOE, y = EPA)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  ggtitle("Defensive CPOE vs EPA") +
  theme_bw() 



## ------------------------ ##
##      PASS RUSH EPA +/-   ##
## ------------------------ ##

#sparse matrix 
# Continuous - ToGo, TimeLeft, 
#Dummies - Down, QuarterName, Player, Distance, Score_Diff, shotgun, no_huddle, surface, Conditions, 

# Merged data - 
pass <- merged_data %>% 
  filter(EventType == "pass",
         !is.na(posteam_type)) %>% 
  select(-starts_with("player_"))
matrix <- pass %>% 
  select(GameID, EventID, OffensiveTeam, QuarterName, Down, ToGo, AndGoal, EPA, shotgun, no_huddle, posteam_type, score_differential) %>% 
  rename(Quarter = QuarterName)  %>% 
  fastDummies::dummy_cols(select_columns = c("OffensiveTeam", "Quarter", "Down", "posteam_type"),
                          remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>% 
  inner_join(player_matrix) %>% 
  filter(!(GameID == 2757 & EventID == 915))

epa <- matrix$EPA
metrics <- matrix %>% select(-EPA, -GameID, -EventID, -ToGo, -score_differential)
metrics_matrix <- map(metrics, Matrix::Matrix, sparse = T) %>% 
  reduce(cbind2)
weights <- matrix %>% select(ToGo, score_differential)

cross_validated <- cv.glmnet(metrics_matrix, epa,
                             alpha = 0, 
                             nfolds = 10)
gc()

lambda_pass <- cross_validated$lambda.min

ridge_pass <- glmnet(metrics_matrix, 
                     epa, 
                     family = c("gaussian"), 
                     alpha = 0, 
                     lambda = lambda_pass)

betas <- ridge_pass$beta %>% as.numeric()
metric_names <- colnames(metrics)

plays <- matrix %>% select(48:ncol(matrix)) %>% 
  gather(key = "player", value = "value") %>% 
  group_by(player) %>% 
  summarise(plays= sum(value)) %>% 
  filter(plays > 50)


apm <- data.frame(betas, metric_names) %>% 
  arrange(desc(betas)) %>% 
  inner_join(plays, by = c("metric_names" = "player")) %>% 
  mutate(plusminus = plays * betas) %>% 
  separate(metric_names, into = c("Name", "Position"), sep = ";") %>% 
  as_tibble() %>% 
  arrange(plusminus)


# Top 10 Players By Plus MInus
apm %>% 
  arrange(plusminus)

# Bottom 10 Players by Plus Minus
apm %>%
  arrange(desc(plusminus))

apm %>% 
  group_by(Position) %>% 
  summarise(n = n(),
            mean_beta = mean(betas),
            mean_apm = mean(plusminus),
            median_beta = median(betas),
            median_apm = median(plusminus)) %>% 
  arrange(desc(median_apm))

# Distribution of talent for defensive positions - Widest for outside, tighest for nose tackle 

apm %>% 
  filter(Position != "Off Ball") %>% 
  ggplot(aes(x = Position, y = plusminus, group = Position)) +
  geom_boxplot() +
  scale_y_reverse() +
  labs(x = "Position", y = "Adjusted Plus-Minus", title = "Adjusted Plus Minus for Defenders",
       subtitle = "Minimum 50 plays")

ggplot(apm, aes(x = Position, y = betas, group = Position)) +
  geom_boxplot()

ggplot(apm, aes(x = plusminus)) +
  geom_histogram() +
  facet_wrap(~Position)

