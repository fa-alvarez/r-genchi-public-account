# source("1_libraries.r")
# source("2_source.r")

# Speech 2018 (management 2017) Removing regular expressions --------------

# diffObj(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")
# diffChr(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")

speech_2018 <- speech_2018 %>% 
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_remove_all("\\s+Allanamientos\\s+.+854") %>% # Remove Tables
  str_remove_all("\\s+PROGRAMAS\\s+RE.+LACTANTES") %>% 
  str_remove_all("Distribución Regional.+NACIONAL\\s+43") %>% 
  str_remove_all("siguiente detalle.+Total\\s+305") %>% 
  str_remove_all("Aspirante Oficiales P.+327 Hombres\\)") %>% 
  str_remove_all("\\s+N° de horas.+ARAUCANÍA\\)") %>% 
  str_remove_all("AÑO NOMBRE.+PUBLICA 30")


# Speech 2018 (management 2017) - Additional corrections ------------------

# diffChr(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")

speech_2018 <- speech_2018 %>% 
  str_remove_all("N°") %>% 
  str_replace_all("Departamento del Sistema Cerrado\\.", " ") %>% 
  str_replace_all("Departamento de Recursos Humanos", " ") %>% 
  str_replace_all("multidisclinarios", "multidisciplinarios") %>% 
  str_replace_all("asociadas:Implementación", "asociadas Implementación") %>% 
  str_replace_all("pesos\\.Reposición", "pesos Reposición") %>% 
  str_replace_all("incendios:Programa", "incendios Programa") %>% 
  str_replace_all("Coyhaique\\.Mantención", "Coyhaique Mantención") %>% 
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



