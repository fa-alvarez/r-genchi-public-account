source("1_libraries.r")
source("2_source.r")
source("3_cleaning1.r")
source("3_cleaning2.r")
source("3_cleaning3.r")


# Defining new stopwords --------------------------------------------------

more_stopwords <- tibble(palabra = c("cdp", "gendarmería", "fecha", "período",
                                     "viii", "monto", "diariamente", "diferentes", 
                                     "impacta", "enfocar", "deberá", "2017", "igual"))


# Additional corrections --------------------------------------------------

# diffChr(speech_2020_modif1, speech_2020_modif2, mode="sidebyside")

speech_2020 <- speech_2020 %>% 
  str_replace_all("unidades penales", "establecimiento penitenciario") %>% 
  str_replace_all("unidad penal", "establecimiento penitenciario") %>% 
  str_replace_all("establecimientos penitenciarios", "establecimiento penitenciario")


# Speech 2020 (management 2019) Calculating bigram frequencies ------------

bigrams_2020 <- speech_2020 %>% 
  tibble(speech = speech_2020) %>% 
  unnest_tokens(input = speech,
                output = word,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(word)) %>% 
  count(word, sort = TRUE) %>% 
  separate(word,
           into = c("word_1", "word_2"),
           sep = " ") %>% 
  filter(!word_1 %in% stopwords_es$palabra) %>% 
  filter(!word_2 %in% stopwords_es$palabra) %>% 
  filter(!word_1 %in% my_stopwords$palabra) %>% 
  filter(!word_2 %in% my_stopwords$palabra) %>%
  filter(!word_1 %in% more_stopwords$palabra) %>% 
  filter(!word_2 %in% more_stopwords$palabra) %>%
  mutate(palabra = paste(word_1, word_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(word_1, word_2))
head(bigrams_2020)


# Speech 2019 (management 2018) Calculating bigram frequencies ------------

bigrams_2019 <- speech_2019 %>% 
  tibble(speech = speech_2019) %>% 
  unnest_tokens(input = speech,
                output = word,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(word)) %>% 
  count(word, sort = TRUE) %>% 
  separate(word,
           into = c("word_1", "word_2"),
           sep = " ") %>% 
  filter(!word_1 %in% stopwords_es$palabra) %>% 
  filter(!word_2 %in% stopwords_es$palabra) %>% 
  filter(!word_1 %in% my_stopwords$palabra) %>% 
  filter(!word_2 %in% my_stopwords$palabra) %>%
  filter(!word_1 %in% more_stopwords$palabra) %>% 
  filter(!word_2 %in% more_stopwords$palabra) %>%
  mutate(palabra = paste(word_1, word_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(word_1, word_2))
head(bigrams_2019)


# Speech 2018 (management 2017) Calculating bigram frequencies ------------

bigrams_2018 <- speech_2018 %>% 
  tibble(speech = speech_2018) %>% 
  unnest_tokens(input = speech,
                output = word,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(word)) %>% 
  count(word, sort = TRUE) %>% 
  separate(word,
           into = c("word_1", "word_2"),
           sep = " ") %>% 
  filter(!word_1 %in% stopwords_es$palabra) %>% 
  filter(!word_2 %in% stopwords_es$palabra) %>% 
  filter(!word_1 %in% my_stopwords$palabra) %>% 
  filter(!word_2 %in% my_stopwords$palabra) %>%
  filter(!word_1 %in% more_stopwords$palabra) %>% 
  filter(!word_2 %in% more_stopwords$palabra) %>%
  mutate(palabra = paste(word_1, word_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(word_1, word_2))
head(bigrams_2018)


# Picturing bigram frequencies --------------------------------------------

pic_2020 <- bigrams_2020 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#619cff") +
  geom_text(aes(label = n), size = 3, hjust = -0.5) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2020") +
  theme(plot.title = element_text(hjust = 0.5))

pic_2019 <- bigrams_2019 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#00ba38") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2019") +
  theme(plot.title = element_text(hjust = 0.5))

pic_2018 <- bigrams_2018 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#f8766d") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2018") +
  theme(plot.title = element_text(hjust = 0.5))

(pic_2018 + pic_2019) / pic_2020 

