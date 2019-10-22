library(tidyverse)


# 1.0 LOADING DATA ----
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|") %>% 
    mutate(self = str_detect(role, "self|selves"),
           season = parse_number(season))


# 2.0 VISUALIZATIONS ----
# 2.1 Figure 1 ----
simpsons %>% 
    filter(self) %>% 
    count(guest_star, sort = TRUE) %>% 
    filter(n > 1) %>% 
    mutate(guest_star = fct_reorder(guest_star, n)) %>% 
    ggplot(aes(guest_star, n)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Who has played themselves in multiple Simpsons episodes?",
        x = "Guest Star",
        y = "Count"
    )


# 2.2 Figure 2 ----
simpsons %>% 
    separate_rows(role, sep = ";\\s+") %>% 
    add_count(role) %>% 
    filter(n >= 8) %>% 
    count(season, role) %>% 
    mutate(role = fct_reorder(role, -n, sum)) %>% 
    ggplot(aes(season, n)) +
    geom_col() +
    facet_wrap(~ role)

# 3.0 BRINGING IN SIMPSONS DIALOGUE DATASET ----
# 3.1 Figure 1 ----
dialogue <- read_csv("data_raw/simpsons_dataset.csv") %>% 
    select(role = raw_character_text, line = spoken_words)


guests_processed <- simpsons %>% 
    separate_rows(role, sep = ";\\s+") %>% 
    mutate(role = ifelse(self, guest_star, role),
           role = ifelse(role == "Edna Krabappel", "Edna Krabappel-Flanders", role))


guests_summarized <- guests_processed %>% 
    filter(season <= 27) %>% 
    group_by(guest_star, role, self) %>% 
    summarise(nb_episodes = n(),
              first_season = min(season),
              last_season = max(season)) %>% 
    arrange(desc(nb_episodes)) %>% 
    group_by(role) %>% 
    filter(n() == 1) %>% 
    ungroup() %>% 
    filter(!is.na(role))


dialogue_summarized <- dialogue %>% 
    group_by(role) %>% 
    summarize(nb_lines = n(),
              random_line = sample(line, 1)) %>% 
    arrange(desc(nb_lines)) %>% 
    ungroup()


guest_roles <- guests_summarized %>% 
    inner_join(dialogue_summarized, by = "role") %>% 
    mutate(lines_per_episode = nb_lines / nb_episodes)


guest_roles %>% 
    mutate(self = ifelse(self, "Playing Themselves", "Playing a Character")) %>% 
    ggplot(aes(lines_per_episode)) +
    geom_histogram(binwidth = 2, center = 1) +
    facet_wrap(~ self, ncol = 1) +
    labs(
        x = "Average # of lines per episode",
        title = "Most guest stars, specially those playing themselves, have relatively few lines per episode"
    )


guest_roles %>% 
    arrange(desc(lines_per_episode))


# Figure 2 ----
library(tidytext)

role_words <- dialogue %>% 
    filter(!is.na(line), !is.na(role)) %>% 
    mutate(line_number = row_number()) %>% 
    unnest_tokens(word, line) %>% 
    anti_join(stop_words, by = "word") %>% 
    distinct(role, line_number, word) %>% 
    count(role, word, sort = TRUE)


role_words %>% 
    mutate(role_word = paste0(role, ": ", word)) %>% 
    head(20) %>% 
    mutate(role_word = fct_reorder(role_word, n)) %>% 
    ggplot(aes(role_word, n)) +
    geom_col() +
    coord_flip() +
    labs(title = "Using # time a word spoken by a role as a Simpsons catchphrase detector",
         x = "",
         y = "Count")


role_word_tf_idf <- role_words %>% 
    group_by(role) %>% 
    mutate(total_words = sum(n)) %>% 
    ungroup() %>% 
    bind_tf_idf(word, role, n) %>% 
    arrange(desc(tf_idf))


role_word_tf_idf %>% 
    filter(total_words >= 500) %>% 
    distinct(role, .keep_all = TRUE) %>% 
    mutate(role_word = paste0(role, ": ", word)) %>% 
    head(20) %>% 
    mutate(role_word = fct_reorder(role_word, tf_idf)) %>% 
    ggplot(aes(role_word, tf_idf)) +
    geom_col() +
    coord_flip() +
    labs(title = "Using TF-IDF as a Simpsons catchphrase detector",
         subtitle = "Only the 53 characters that speak at least 500 words in 27 seasons",
         x = "",
         y = "TF-IDF")


guests_summarized %>% 
    filter(nb_episodes > 1) %>% 
    inner_join(role_word_tf_idf, by = "role") %>% 
    filter(total_words >= 100) %>% 
    arrange(desc(tf_idf)) %>% 
    distinct(role, .keep_all = TRUE) %>% 
    select(guest_star, role, word, tf_idf)



