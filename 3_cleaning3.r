# source("0_run.r")

# Speech 2018 (management 2017) Removing regular expressions --------------

speech_2018 <- speech_2018 %>% 
  str_replace_all("\r\n", " ") %>%
  str_replace_all("MINJU-DDHH", "MINJUDDHH") %>% # Ministerio de Justicia y DDHH
  str_replace_all("MINJU,", "MINJUDDHH") %>% 
  str_replace_all("(MINJU)", "MINJUDDHH") %>% 
  str_remove_all("\\s+•\\s+") %>% # Bullets
  str_remove_all("\\s+\\-+\\s") %>% # Bullets  
  str_remove_all("\\s+Allanamientos.+7+\\.+854") %>%   
  str_remove_all("\\s+Establecimientos Penitenciarios.+43") %>% 
  str_remove_all("\\s+Para el año 2018 se desarrollarán.+305") %>% 
  str_remove_all("\\s+N° de horas.+\\(+LA ARAUCANÍA+\\)") %>%   
  str_remove_all("\\s+Los beneficiados de los programas.+DIPLOMADO GERENCIA PUBLICA+\\s+\\d") %>%     
  str_replace_all("\\s+\\d+\\.+\\s", " ") %>%  # 1. 
  str_replace_all("I+\\.+\\s+AVANCES", "AVANCE") %>%
  str_replace_all("\\s+II+\\.+\\s", " ") %>%  
  stripWhitespace() 


# Speech 2018 (management 2017) Calculating first frequencies -------------

frequencies_2018 <- tibble(speech = speech_2018) %>% 
  unnest_tokens(output = palabra, input = speech, strip_numeric = TRUE) %>%
  count(palabra, sort = TRUE)
frequencies_2018


# Speech 2018 (management 2017) Removing stopwords & recalculating --------

frequencies_2018 <- frequencies_2018 %>% 
  anti_join(stopwords_es) %>% 
  anti_join(my_stopwords) %>% 
  anti_join(more_stopwords)
head(frequencies_2018)



