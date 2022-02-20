# source("1_libraries.r")
# source("2_source.r")

# Speech 2019 (management 2018) Removing regular expressions --------------

speech_2019 <- speech_2019 %>% 
  str_remove_all("\r\n+\\s+\\d+\r\n") %>%  # Page number  
  str_replace_all("\r\n", " ") %>%
  str_replace_all("MINJU-DDHH", "MINJUDDHH") %>% # Ministerio de Justicia y DDHH
  str_replace_all("MINJU,", "MINJUDDHH") %>% 
  str_replace_all("(MINJU)", "MINJUDDHH") %>% 
  str_remove_all("\\s+•\\s+") %>% # Bullets
  str_remove_all("\\s+\\-+\\s") %>% # Bullets  
  str_remove_all("\\s+Tabla relación entre programas y objetivos principales definidos.+Fuente: Departamento de Infraestructura de Gendarmería de Chile") %>%   
  str_remove_all("\\s+Capacitaciones en cifras.+Fuente: Escuela Institucional") %>% 
  str_remove_all("\\s+Presupuesto Inicial.+Fuente: Departamento de Contabilidad y Presupuesto, Gendarmería de Chile") %>% 
  str_replace_all("N°+\\s+\\d+\\.+\\s", " ") %>%  # N° 1.
  str_replace_all("\\s+\\d+\\.+\\s", " ") %>% # 1.
  str_replace_all("\\s+\\d+\\.+\\d+\\.+\\s", " ") %>%  # 4.1.
  str_replace_all("\\s+\\d+\\.+\\d+\\.+\\d+\\.+\\s", " ") %>%  # 4.1.1.  
  stripWhitespace()


# Speech 2019 (management 2018) Calculating first frequencies -------------

frequencies_2019 <- tibble(speech = speech_2019) %>% 
  unnest_tokens(output = palabra, input = speech, strip_numeric = TRUE) %>%
  count(palabra, sort = TRUE)
frequencies_2019


# Speech 2019 (management 2018) Removing stopwords & recalculating freq----

frequencies_2019 <- frequencies_2019 %>% 
  anti_join(stopwords_es) %>% 
  anti_join(my_stopwords) %>% 
  anti_join(more_stopwords)
head(frequencies_2019)

