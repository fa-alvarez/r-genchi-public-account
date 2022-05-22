
# Speech 2018 (management 2017) Removing regular expressions --------------

speech_2018 <- speech_2018 %>% 
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_remove_all("\\s+Allanamientos\\s+.+854") %>% # Remove Tables
  str_remove_all("\\s+PROGRAMAS\\s+REINSERCIÓN\\s.+\\sLACTANTES") %>% 
  str_remove_all("Distribución Regional.+NACIONAL\\s+43") %>% 
  str_remove_all("siguiente detalle.+Total\\s+305") %>% 
  str_remove_all("Aspirante Oficiales P.+327 Hombres\\)") %>% 
  str_remove_all("\\s+N° de horas.+ARAUCANÍA\\)") %>% 
  str_remove_all("AÑO\\s+NOMBRE.+PUBLICA\\s+30") %>% 
  str_remove_all("II\\.\\s") %>% str_remove_all("I\\.\\s") %>% 
  str_replace_all("\\s\\d\\.\\s+", " ") %>% 
  str_replace_all("\\s•\\s", " ") %>% 
  str_remove_all("1 Fuente de datos: Estadística General Penitenciaria\\. Unidad de Estadística\\. Gendarmería de Chile") 


# Speech 2018 (management 2017) - Additional corrections ------------------

speech_2018 <- speech_2018 %>% 
  str_remove_all("N°") %>% 
  str_replace_all("Departamento del Sistema Cerrado\\.", " ") %>% 
  str_replace_all("Departamento de Recursos Humanos", " ") %>% 
  str_replace_all("multidisclinarios", "multidisciplinarios") %>% 
  str_replace_all("asociadas:Implementación", "asociadas Implementación") %>% 
  str_replace_all("pesos\\.Reposición", "pesos Reposición") %>% 
  str_replace_all("incendios:Programa", "incendios Programa") %>% 
  str_replace_all("Coyhaique\\.Mantención", "Coyhaique Mantención") %>% 
  str_replace_all("intervenidos", "intervenido") %>% str_replace_all("intervenido", "intervenidos") %>% 
  str_replace_all("\\Sstablecimientos", "establecimientos") %>% 
  str_replace_all("Laborales", "laboral") %>% 
  stripWhitespace()  


# Speech 2019 (management 2017) Document version comparison ---------------

# speech_2018_modif1 <- speech_2018
# speech_2018_modif2 <- speech_2018 %>% 
#   stripWhitespace()  
# 
# #diffObj(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")
# diffChr(speech_2018_modif1, speech_2018_modif2, mode="sidebyside")


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

