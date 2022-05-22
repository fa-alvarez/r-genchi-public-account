
# Speech 2021 (management 2020) Removing regular expressions --------------

speech_2021 <- speech_2021 %>%
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_remove_all("“") %>% str_remove_all("”") %>% # Remove "" 
  str_remove_all("INTRODUCCIÓN") %>% 
  str_replace_all("\\d+\\s+\\S+gob\\.cl", " ") %>% # Pages
  str_replace_all("INFORME FINAL", " ") %>%
  str_replace_all("\\s\\S+\\.-", " ") %>% str_replace_all("VI\\.", " ") %>% # Numbering
  str_replace_all("\\s\\d+-\\s", " ") %>% 
  str_replace_all("\\s\\d\\.\\s", " ") %>% 
  str_replace_all("5\\.1", " ") %>% str_replace_all("5\\.2-", " ") %>% 
  str_replace_all("\\s[abcde]+\\)\\s", " ") %>% 
  str_replace_all("http\\S+\\s", " ") %>% # urls
  str_replace_all("SOCIEDAD\\S+\\s", " ") %>% 
  str_replace_all("Gendarmería de Chile", " ") %>% str_replace_all("Gendarmería", " ") %>% 
  str_replace_all("Institución.+Manríquez", " ") %>% 
  str_replace_all("con 64\\s+.+Nacional de Chile", " ") %>% str_replace_all("64 \\(1\\)", " ") %>% 
  str_replace_all("públicas65", "públicas") %>% str_replace_all("decisiones\\.66", "decisiones") %>% 
  str_replace_all("65\\s+Instructivo.+Transparencia\\.2015\\.", " ") %>% 
  str_replace_all("Informe Ejecutivo", " ") %>% 
  str_replace_all("informe Resumen Ejecutivo", " ") %>% 
  str_replace_all("\\Snforme final", " ") %>% 
  str_replace_all("\\Snforme.", " ")


# Speech 2021 (management 2020) Additional corrections --------------------

speech_2021 <- speech_2021 %>% 
  str_replace_all("Cuentas Públicas Participativas", "Cuenta Pública Participativa ") %>% 
  str_replace_all("cuentas públicas", "cuenta pública") %>% 
  str_replace_all("COVID 19", "COVID-19") %>% 
  str_replace_all("\\sCOVID\\s", " COVID-19 ") %>% 
  str_remove_all("\\(Consejo para la transparencia\\.2015\\)") %>% 
  str_replace_all("Consejo de la Sociedad Civil \\(COSOC\\)", "Consejo de la Sociedad Civil") %>% 
  str_replace_all("Consejo de la Sociedad Civil", "COSOC") %>% 
  str_replace_all("CONSEJO DE LA SOCIEDAD CIVIL", "COSOC") %>% 
  str_remove_all("Unidad de Atención y Participación Ciudadana") %>% 
  str_remove_all("Unidad de Comunicaciones") %>% 
  str_replace_all("Unidades Penales", "Unidad Penal") %>% 
  str_remove_all("Departamento de Estadística y Estudios penitenciarios") %>% 
  str_remove_all("disponer") %>% 
  str_replace_all("pacientes", "paciente") %>% 
  str_replace_all("PACIENTES", "paciente") %>% 
  str_replace_all("paciente", "pacientes") %>% 
  str_replace_all("clínico", "clínica") %>% 
  stripWhitespace()


# Speech 2021 (management 2020) Document version comparison ---------------

# speech_2021_modif1 <- speech_2021
# speech_2021_modif2 <- speech_2021 %>% 
#   stripWhitespace()
# 
# #diffObj(speech_2021_modif1, speech_2021_modif2, mode="sidebyside")
# diffChr(speech_2021_modif1, speech_2021_modif2, mode="sidebyside")


# Speech 2021 (management 2020) Calculating first frequencies -------------

frequencies_2021 <- tibble(speech = speech_2021) %>% 
  unnest_tokens(output = palabra, input = speech, strip_numeric = TRUE) %>%
  count(palabra, sort = TRUE)
frequencies_2021


# Speech 2021 (management 2020) Removing stopwords & recalculating freq----

frequencies_2021 <- frequencies_2021 %>% 
  anti_join(stopwords_es) %>% 
  anti_join(my_stopwords) %>% 
  anti_join(more_stopwords)
head(frequencies_2021)

