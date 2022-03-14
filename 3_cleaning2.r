# source("1_libraries.r")
# source("2_source.r")

# Speech 2019 (management 2018) Removing regular expressions --------------

# diffObj(speech_2019_modif1, speech_2019_modif2, mode="sidebyside")
# diffChr(speech_2019_modif1, speech_2019_modif2, mode="sidebyside")

speech_2019 <- speech_2019 %>% 
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_remove_all("Tabla rela.+\\s+Fuente: Departamento de Infraestructura de Gendarmería de Chile") %>%  # Remove Tables
  str_remove_all("Capacitaciones en cifras.+\\s+Fuente: Escuela Institucional") %>% 
  str_remove_all("Presupuesto Inicial.+\\s+Contabilidad y Presupuesto, Gendarmería de Chile") %>% 
  str_replace_all("\\s\\s\\s\\s\\d+\\s", " ") %>% # Page number
  str_replace_all("\\s\\d\\.\\s", " ") %>% # Numbering & bullets
  str_remove_all("1. PRESENTACIÓN ") %>% 
  str_replace_all("\\s\\d\\.\\d\\.\\d\\.\\s", " ") %>% 
  str_replace_all("\\s\\d\\.\\d\\.\\s", " ") %>% 
  str_replace_all("\\s•\\s", " ") %>% 
  str_replace_all("\\s-\\s", " ") %>% 
  str_replace_all("\\s\\S\\.\\s", " ") %>% 
  str_replace_all("Ministerio de Justicia y Derechos Humanos", "MINJUDDHH") %>% 
  str_replace_all("Ministerio de Justicia y DD.HH.", "MINJUDDHH") %>% 
  str_remove_all("N°")


# Speech 2019 (management 2018) - Additional corrections ------------------

# diffChr(speech_2019_modif1, speech_2019_modif2, mode="sidebyside")

speech_2019 <- speech_2019 %>% 
  str_replace_all("ETIntervención", "Intervención") %>% 
  str_replace_all("ETCapacitación", "Capacitación") %>% 
  str_replace_all("ETColocación", "Colocación") %>% 
  str_replace_all("APpsicosocial", "psicosocial") %>% 
  str_replace_all("", " ") %>% 
  str_replace_all("APestablecimiento", "establecimiento") %>%
  str_replace_all("Aapoyo", "apoyo") %>%
  str_replace_all("Aopermisos", "permisos") %>%
  str_replace_all("1terceros", "terceros") %>% 
  str_remove_all("“") %>% str_remove_all("”") %>% 
  str_replace_all("de \\+R", "de Proyecto +R") %>% 
  str_replace_all("Departamento de Contabilidad y Presupuesto", " ") %>% 
  str_replace_all("Departamento de Gestión de Personas", " ") %>% 
  str_replace_all("Departamento de Gestión y Desarrollo de Personas", " ") %>% 
  str_replace_all("Departamento de Salud", " ") %>% 
  str_replace_all("Departamentos de Inteligencia Penitenciaria\\s+y de\\s+Investigación Criminal", " ") %>% 
  str_replace_all("Departamento de Investigación y Análisis Penitenciario \\(DIAP\\)", " ") %>% 
  str_replace_all("departamentos de Salud e Informática", " ") %>% 
  str_replace_all("Departamento de Promoción y Protección de los Derechos Humanos", " ") %>% 
  str_replace_all("Departamento de Infraestructura", " ") %>% 
  str_replace_all("Subdepartamento de Servicios\\s+Especializados", " ") %>% 
  str_replace_all("Departamento de Control Penitenciario", " ") %>% 
  str_replace_all("departamentos y/o unidades", " ") %>% 
  str_remove_all("a Departamento") %>% 
  str_remove_all("términos") %>% 
  str_remove_all("mejorando") %>% 
  str_remove_all("desarrollar") %>% 
  str_remove_all("porcentaje") %>% 
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

