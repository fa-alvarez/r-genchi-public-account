source("1_libraries.r")
source("2_source.r")
source("3_cleaning1.r")
source("3_cleaning2.r")
source("3_cleaning3.r")

# Picturing frequencies per word ------------------------------------------

pic_2020 <- frequencies_2020 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#619cff") +
  geom_text(aes(label = n), size = 3, hjust = -0.5) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2020") +
  theme(plot.title = element_text(hjust = 0.5))

pic_2019 <- frequencies_2019 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#00ba38") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2019") +
  theme(plot.title = element_text(hjust = 0.5))

pic_2018 <- frequencies_2018 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#f8766d") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2018") +
  theme(plot.title = element_text(hjust = 0.5))

(pic_2018 + pic_2019) / pic_2020 


# TF-IDF Analysis - one data frame ----------------------------------------

frequencies_2020 <- frequencies_2020 %>% 
  mutate(discurso = "C.P.P. 2020", .before = palabra) 
frequencies_2019 <- frequencies_2019 %>% 
  mutate(discurso = "C.P.P. 2019", .before = palabra) 
frequencies_2018 <- frequencies_2018 %>% 
  mutate(discurso = "C.P.P. 2018", .before = palabra) 

messages <- bind_rows(frequencies_2018, frequencies_2019, frequencies_2020)
head(messages)


# TF-IDF Analysis - Inverse document frequency ----------------------------

messages_tfidf <- bind_tf_idf(messages, 
                              term = palabra, 
                              document = discurso,
                              n = n)
head(messages_tfidf) 


# TF-IDF Analysis - Plotting TF-IDF ---------------------------------------

messages_tfidf %>%
  group_by(discurso) %>%
  top_n(9) %>%
  ungroup %>%
  mutate(discurso = as.factor(discurso),
         palabra = reorder_within(palabra, tf_idf, discurso)) %>%
  ggplot(aes(palabra, tf_idf, fill = discurso)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~discurso, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "tf-idf", x = NULL)


