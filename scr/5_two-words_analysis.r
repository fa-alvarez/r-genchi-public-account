
# Adding stopwords --------------------------------------------------------

bi_stopwords <- tibble(palabra = c("cuenta", "cuentas", "deberán", "realizar", "diferentes")) 


# Speech 2021 (management 2020) Corrections -------------------------------

speech_2021 <- speech_2021 %>% 
  str_remove_all("políticas,") %>% str_remove_all("planes") %>% str_remove_all("programas") %>% 
  str_remove_all("Intrapenitenciaria") %>% str_remove_all("INTRAPENITENCIARIA") %>% 
  str_replace_all("\\Snidad \\Senal", "establecimientos penitenciarios") %>% 
  str_replace_all("del establecimiento", "de los establecimientos penitenciarios") %>% 
  str_remove_all("\\Suenta \\Sública \\Sarticipativa") %>% 
  stripWhitespace()


# Speech 2021 (management 2020) Document version comparison ---------------

# speech_2021_modif1 <- speech_2021
# speech_2021_modif2 <- speech_2021 %>% 
#   stripWhitespace()
# 
# #diffObj(speech_2021_modif1, speech_2021_modif2, mode="sidebyside")
# diffChr(speech_2021_modif1, speech_2021_modif2, mode="sidebyside")


# Speech 2020 (management 2019) Corrections -------------------------------

speech_2020 <- speech_2020 %>% 
  str_replace_all("\\Snidad \\Senal", "unidades penales") %>% 
  str_replace_all("\\Snidades \\Senales", "establecimientos penitenciarios") %>% 
  str_replace_all("visita virtuales", "visitas virtuales") %>% 
  str_replace_all("visita virtual", "visitas virtuales") %>% 
  str_replace_all("espacio físico", "espacios físicos") %>% 
  str_replace_all("\\Sstablecimiento \\Senitenciario", "establecimientos penitenciarios") %>% 
  str_replace_all("establecimiento penal", "establecimientos penitenciarios") %>% 
  str_replace_all("teléfono celular", "teléfonos móviles") %>% 
  str_replace_all("teléfono móvil", "teléfonos móviles") %>% 
  str_remove_all("Centro de Estudios Justicia & Sociedad") %>% 
  stripWhitespace()


# Speech 2020 (management 2019) Document version comparison ---------------

# speech_2020_modif1 <- speech_2020
# speech_2020_modif2 <- speech_2020 %>% 
#   stripWhitespace()
# 
# #diffObj(speech_2020_modif1, speech_2020_modif2, mode="sidebyside")
# diffChr(speech_2020_modif1, speech_2020_modif2, mode="sidebyside")


# Speech 2019 (management 2018) Corrections -------------------------------

speech_2019 <- speech_2019 %>% 
  str_remove_all("igual forma") %>% 
  str_remove_all("Centro de Cumplimiento Penitenciario") %>% 
  str_remove_all("Centros de Cumplimiento Penitenciario") %>% 
  str_replace_all("unidades penales", "establecimientos penitenciarios") %>% 
  str_replace_all("unidad penal", "establecimientos penitenciarios") %>% 
  str_replace_all("\\Sstablecimiento \\Senal", "establecimientos penitenciarios") %>% 
  str_replace_all("\\Sstablecimientos \\Senales", "establecimientos penitenciarios") %>% 
  str_replace_all("diferentes penales", "diferentes establecimientos penitenciarios") %>% 
  str_replace_all("\\Sirección \\Segional", "direcciones regionales") %>% 
  stripWhitespace()


# Speech 2019 (management 2018) Document version comparison ---------------

# speech_2019_modif1 <- speech_2019
# speech_2019_modif2 <- speech_2019 %>% 
#   stripWhitespace()
# 
# #diffObj(speech_2019_modif1, speech_2019_modif2, mode="sidebyside")
# diffChr(speech_2019_modif1, speech_2019_modif2, mode="sidebyside")


# Speech 2018 (management 2017) Corrections -------------------------------

speech_2018 <- speech_2018 %>% 
  str_replace_all("establecimiento penitenciario", "establecimientos penitenciarios") %>% 
  str_replace_all("establecimientos penales", "establecimientos penitenciarios") %>% 
  str_replace_all("algunos establecimientos", "algunos establecimientos penitenciarios") %>% 
  str_replace_all("73 establecimientos", "73 establecimientos penitenciarios") %>% 
  str_replace_all("prohibidas en establecimientos", "prohibidas en establecimientos penitenciarios") %>% 
  str_replace_all("establecimientos institucionales", "establecimientos penitenciarios") %>% 
  str_replace_all("condena en establecimientos", "condena en establecimientos penitenciarios") %>% 
  str_replace_all("otros establecimientos", "otros establecimientos penitenciarios") %>% 
  str_replace_all("últimos establecimientos", "últimos establecimientos penitenciarios") %>% 
  str_replace_all("equipamiento del establecimiento", "equipamiento de establecimientos penitenciarios") %>% 
  str_replace_all("nuevo establecimiento", "nuevos establecimientos penitenciarios") %>% 
  str_replace_all("tener un establecimiento", "tener establecimientos penitenciarios") %>% 
  str_replace_all("10 establecimientos", "10 establecimientos penitenciarios") %>% 
  str_remove_all("Departamento del \\Sistema \\Serrado") %>% 
  str_replace_all("\\s\\Sistema \\Serrado", " subsistema cerrado") %>% 
  str_replace_all("unidades penales", "establecimientos penitenciarios") %>% 
  str_remove_all("siguientes iniciativas") %>% 
  str_replace_all("direcciones técnicas regionales", "unidades técnicas") %>%
  str_replace_all("unidades técnicas regionales", "unidades técnicas") %>% 
  str_replace_all("\\s\\Sonvenio\\s", " convenios suscritos ") %>% 
  str_replace_all("convenios con", "convenios suscritos con") %>% 
  str_replace_all("convenios en", "convenios suscritos en") %>% 
  str_replace_all("de allanamiento en", "de allanamientos simultáneos en") %>% 
  str_replace_all("estos allanamientos", "estos allanamientos simultáneos") %>% 
  stripWhitespace()


# Speech 2018 (management 2017) Document version comparison ---------------

# speech_2018_modif1 <- speech_2018
# speech_2018_modif2 <- speech_2018 %>% 
#   stripWhitespace()
# 
# #diffObj(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")
# diffChr(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")


# Speech 2021 (management 2020) Bigram frequencies ------------------------

bigram_2021 <- speech_2021 %>% 
  tibble(speech = speech_2021) %>% 
  unnest_tokens(input = speech,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% stopwords_es$palabra) %>% 
  filter(!palabra_2 %in% stopwords_es$palabra) %>% 
  filter(!palabra_1 %in% my_stopwords$palabra) %>% 
  filter(!palabra_2 %in% my_stopwords$palabra) %>%
  filter(!palabra_1 %in% bi_stopwords$palabra) %>% 
  filter(!palabra_2 %in% bi_stopwords$palabra) %>%
  mutate(palabra = paste(palabra_1, palabra_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(palabra_1, palabra_2))
head(bigram_2021)


# Speech 2020 (management 2019) Bigram frequencies ------------------------

bigram_2020 <- speech_2020 %>% 
  tibble(speech = speech_2020) %>% 
  unnest_tokens(input = speech,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% stopwords_es$palabra) %>% 
  filter(!palabra_2 %in% stopwords_es$palabra) %>% 
  filter(!palabra_1 %in% my_stopwords$palabra) %>% 
  filter(!palabra_2 %in% my_stopwords$palabra) %>%
  filter(!palabra_1 %in% bi_stopwords$palabra) %>% 
  filter(!palabra_2 %in% bi_stopwords$palabra) %>%
  mutate(palabra = paste(palabra_1, palabra_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(palabra_1, palabra_2))
head(bigram_2020)


# Speech 2019 (management 2018) Bigram frequencies ------------------------

bigram_2019 <- speech_2019 %>% 
  tibble(speech = speech_2019) %>% 
  unnest_tokens(input = speech,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% stopwords_es$palabra) %>% 
  filter(!palabra_2 %in% stopwords_es$palabra) %>% 
  filter(!palabra_1 %in% my_stopwords$palabra) %>% 
  filter(!palabra_2 %in% my_stopwords$palabra) %>%
  filter(!palabra_1 %in% bi_stopwords$palabra) %>% 
  filter(!palabra_2 %in% bi_stopwords$palabra) %>%
  mutate(palabra = paste(palabra_1, palabra_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(palabra_1, palabra_2))
head(bigram_2019)


# Speech 2018 (management 2017) Bigram frequencies ------------------------

bigram_2018 <- speech_2018 %>% 
  tibble(speech = speech_2018) %>% 
  unnest_tokens(input = speech,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% stopwords_es$palabra) %>% 
  filter(!palabra_2 %in% stopwords_es$palabra) %>% 
  filter(!palabra_1 %in% my_stopwords$palabra) %>% 
  filter(!palabra_2 %in% my_stopwords$palabra) %>%
  filter(!palabra_1 %in% bi_stopwords$palabra) %>% 
  filter(!palabra_2 %in% bi_stopwords$palabra) %>%
  mutate(palabra = paste(palabra_1, palabra_2, sep = " "), .before = n) %>% 
  dplyr::select(-c(palabra_1, palabra_2))
head(bigram_2018)


# Bigram frequencies plot -------------------------------------------------

fig_2021 <- bigram_2021 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#dbb012") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2021") +
  theme(plot.title = element_text(hjust = 0.5))

fig_2020 <- bigram_2020 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#619cff") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2020") +
  theme(plot.title = element_text(hjust = 0.5))

fig_2019 <- bigram_2019 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#00ba38") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2019") +
  theme(plot.title = element_text(hjust = 0.5))

fig_2018 <- bigram_2018 %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(y = reorder(palabra, n), n)) +
  geom_col(fill = "#f8766d") +
  geom_text(aes(label = n), size = 3, hjust = 0.2) +
  theme_minimal() +
  labs(y = NULL, x = "frecuencia") +
  ggtitle("Discurso 2018") +
  theme(plot.title = element_text(hjust = 0.5))

(fig_2018 + fig_2019) / (fig_2020 + fig_2021)


# Save frequencies figure -------------------------------------------------

png("../figs/two-words_frequencies.png", width = 1344, height = 960)
(fig_2018 + fig_2019) / (fig_2020 + fig_2021)
dev.off()


# TF-IDF Analysis - one data frame ----------------------------------------

frequencies_2021 <- bigram_2021 %>% 
  mutate(discurso = "C.P.P. 2021", .before = palabra) 
frequencies_2020 <- bigram_2020 %>% 
  mutate(discurso = "C.P.P. 2020", .before = palabra) 
frequencies_2019 <- bigram_2019 %>% 
  mutate(discurso = "C.P.P. 2019", .before = palabra) 
frequencies_2018 <- bigram_2018 %>% 
  mutate(discurso = "C.P.P. 2018", .before = palabra) 

messages <- bind_rows(frequencies_2018, frequencies_2019, frequencies_2020, frequencies_2021)
head(messages)


# TF-IDF Analysis - Inverse document frequency ----------------------------

messages_tfidf <- bind_tf_idf(messages, 
                              term = palabra, 
                              document = discurso,
                              n = n)
head(messages_tfidf) 


# TF-IDF Analysis - Plotting TF-IDF ---------------------------------------

two_words_plot <-
  messages_tfidf %>%
  group_by(discurso) %>%
  top_n(5) %>%
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
two_words_plot


# TF-IDF Analysis - Save plot ---------------------------------------------

png("../figs/two-words_tf-idf.png", width = 1344, height = 960)
two_words_plot
dev.off()



