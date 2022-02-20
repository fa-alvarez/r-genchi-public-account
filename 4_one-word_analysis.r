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

