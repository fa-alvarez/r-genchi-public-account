
# Reading ------------------------------------------------------------------

speech_2021 <- pdf_text("../data/discurso_2021_2020.pdf")
speech_2020 <- pdf_text("../data/discurso_2020_2019.pdf")
speech_2019 <- pdf_text("../data/discurso_2019_2018.pdf")
speech_2018 <- pdf_text("../data/discurso_2018_2017.pdf")


# Removing unnecessary pages -----------------------------------------------

speech_2021 <- speech_2021 %>% 
  .[-1] %>% 
  .[-13]
speech_2020 <- speech_2020 %>%
  .[-1:-2] %>% 
  .[-53:-56]
speech_2019 <- speech_2019 %>% 
  .[-1:-2]
speech_2018 <- speech_2018 %>% 
  .[-1]
  

# Concatenating objects for each speech ------------------------------------

speech_2021 <- paste(speech_2021, collapse = " ")
speech_2020 <- paste(speech_2020, collapse = " ")
speech_2019 <- paste(speech_2019, collapse = " ")
speech_2018 <- paste(speech_2018, collapse = " ")


# Preparing stopwords -----------------------------------------------------

# Stopwords in vacias.txt have been taken from:
# https://github.com/7PartidasDigital/AnaText/blob/master/datos/diccionarios/vacias.txt
stopwords_es <- read_csv("../data/vacias.txt", col_names = TRUE, show_col_types = FALSE)
my_stopwords <- tibble(palabra = c("mil", "millones", "año", "años", "chile", "dado", 
                                   "dar", "debido", "decir", "acerca", "pesos",
                                   "fin", "ser", "respecto", "debe", "gran", "tiene",
                                   "tienen", "puede", "ir", "hace"))
more_stopwords <- tibble(palabra = c("cdp", "cerrado", "gendarmería", "fecha", "período", 
                                     "cuenta", "informe", "viii", "monto", "diariamente",
                                     "diferentes", "impacta", "enfocar", "deberá"))

