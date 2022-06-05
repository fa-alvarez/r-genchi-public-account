library(googleLanguageR)

# One-word - Reduce df for frequencies ------------------------------------

frequencies_2021_en <- frequencies_2021 %>% 
  slice_head(n = 10) 
frequencies_2020_en <- frequencies_2020 %>% 
  slice_head(n = 10) 
frequencies_2019_en <- frequencies_2019 %>% 
  slice_head(n = 10) 
frequencies_2018_en <- frequencies_2018 %>% 
  slice_head(n = 10) 


# One-word - Translation & corrections for frequencies --------------------

oogleLanguageR::gl_auth(Sys.getenv("GL_AUTH"))

frequencies_2021_en <- frequencies_2021_en %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "Health", "health")) %>% 
  mutate(word = replace(word, word == "developing", "development"))

frequencies_2020_en <- frequencies_2020_en %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n)  %>% 
  mutate(word = replace(word, word == "internal", "inmates")) %>% 
  mutate(word = replace(word, word == "Rights", "rights")) 

frequencies_2019_en <- frequencies_2019_en %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n)%>% 
  mutate(word = replace(word, word == "worked", "work")) %>% 
  mutate(word = replace(word, word == "officials", "employees")) %>% 
  mutate(word = replace(word, word == "Program", "program")) %>% 
  mutate(word = replace(word, word == "establishments", "facilities"))

frequencies_2018_en <- frequencies_2018_en %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "Program", "program")) %>% 
  mutate(word = replace(word, word == "establishments", "facilities")) %>% 
  mutate(word = replace(word, word == "worked", "work"))


# One-word - Translation, corrections & plotting for TF-IDF ---------------

one_word_plot_en <- messages_tfidf %>%
  group_by(discurso) %>%
  top_n(5) %>% 
  mutate(
    word = gl_translate(
      palabra,
      target = "en",
      format = "text",
      source = "es")$translatedText
  ) %>% 
  select(-palabra) %>% 
  ungroup %>%
  mutate(word = replace(word, word == "establishments", "facilities")) %>% 
  mutate(word = replace(word, word == "means", "resources")) %>% 
  mutate(word = replace(word, word == "estates", "establishments")) %>% 
  mutate(word = replace(word, word == "expenses", "outgoing")) %>% 
  mutate(discurso = as.factor(discurso),
         word = reorder_within(word, tf_idf, discurso)
  ) %>%
  ggplot(aes(word, tf_idf, fill = discurso)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~discurso, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "tf-idf", x = NULL)
one_word_plot_en


# Two-word - Translation & corrections for frequencies --------------------

bigram_2021_en <- bigram_2021 %>% 
  slice_head(n = 10) %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "penitentiary establishments", "penitentiary facilities")) %>% 
  mutate(word = replace(word, word == "Budget Execution", "budget execution")) %>% 
  mutate(word = replace(word, word == "Psychological attention", "psychological attention")) 

bigram_2020_en <- bigram_2020 %>% 
  slice_head(n = 10) %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "penitentiary establishments", "penitentiary facilities")) %>% 
  mutate(word = replace(word, word == "Physical spaces", "physical spaces")) %>% 
  mutate(word = replace(word, word == "Nacional level", "national level")) 

bigram_2019_en <- bigram_2019 %>% 
  slice_head(n = 10) %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "penitentiary establishments", "penitentiary facilities")) %>% 
  mutate(word = replace(word, word == "regional addresses", "regional managements")) %>% 
  mutate(word = replace(word, word == "Nacional level", "national level")) 

bigram_2018_en <- bigram_2018 %>% 
  slice_head(n = 10) %>% 
  mutate(
    word = gl_translate(
      palabra, 
      target = "en", 
      format = "text", 
      source = "es")$translatedText
  ) %>% 
  select(word, n) %>% 
  mutate(word = replace(word, word == "penitentiary establishments", "penitentiary facilities")) %>% 
  mutate(word = replace(word, word == "Nacional level", "national level")) %>% 
  mutate(word = replace(word, word == "prison system", "penitentiary system")) 


# Two-word - Translation, corrections & plotting for TF-IDF  --------------

two_words_plot_en <- messages_tfidf %>%
  group_by(discurso) %>%
  top_n(5) %>%
  mutate(
    word = gl_translate(
      palabra,
      target = "en",
      format = "text",
      source = "es")$translatedText
  ) %>% 
  select(-palabra) %>% 
  ungroup %>%
  mutate(word = replace(word, word == "Higher cost", "higher cost")) %>%
  mutate(word = replace(word, word == "regional addresses", "regional managements")) %>%
  mutate(word = replace(word, word == "Physical spaces", "physical spaces")) %>%
  mutate(word = replace(word, word == "executive Summary", "executive summary")) %>%
  mutate(discurso = as.factor(discurso),
         word = reorder_within(word, tf_idf, discurso)) %>%
  ggplot(aes(word, tf_idf, fill = discurso)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~discurso, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "tf-idf", x = NULL)
two_words_plot_en
