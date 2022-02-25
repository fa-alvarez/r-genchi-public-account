source("1_libraries.r")
source("2_source.r")

# Speech 2020 (management 2019) Removing regular expressions --------------

write_file(speech_2020_modif, "speech_2020_modif.txt", FALSE)

speech_2020_modif <- speech_2020 %>%
  str_remove_all("\"") %>% # Remove "\"" 
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_replace_all("MINJU", "MINJUDDHH") %>% # Ministerio de Justicia y DDHH
  str_replace_all("MINJUDDHH-DDHH", "MINJUDDHH") %>% 
  str_remove_all("\\s+•\\s+") %>% # Bullets
  str_remove_all("http\\S*") %>% # urls
  str_remove_all("www.\\S*") %>% # Remove web pages
  str_replace_all("\\s+\\-+\\s", " ") %>% # Bullets
  str_remove_all("\\d+-\\s") %>% # Numbering
  str_replace_all("\\s+\\d+\\.+\\s", " ") %>% # Replace 1. by space
  str_replace_all("\\d\\.", " ") %>% # Replace 1.2. by space
  str_remove_all("\\s+[abcde]+\\)+\\s") %>% # Remove type of numbering a)  
  str_replace_all("\\S+\\.+\\-", " ") %>% # Replace something like V.- by space 
  str_remove_all("Twitter.+gendarmeriacl") %>%  # Remove social networks
  str_remove_all("N° de internos heridos.+\\(S\\.I\\.G\\)") %>% # Remove Tables
  str_remove_all("A continuación, se expone un desglose.+Fuente: Departamento de Infraestructura") %>% # Remove Tables 
  str_remove_all("MATRICULADOS EN EDUCACIÓN SUPERIOR DICIEMBRE 2019.+Total.+163.+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Tabla 1. Privados de Libertad Inscritos para dar PSU.+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Tabla 2. Resultados PSU 2019 de Privados de Libertad, por región:.+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Intervención:.+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Prestaciones.+Fuente: Departamento Subsistema Cerrado") %>% # Remove Tables   
  str_remove_all("\\s+CANTIDAD DE CELULARES.+Fuente: Subdirección Operativa") %>% # Remove Tables 
  str_remove_all("\\s+18. COVID: Estadística de contagios por región.+Fuente: Subdirección Operativa") %>% # Remove Tables 
  str_remove_all("\\s+Catastro de beneficios otorgados.+Fuente: Subdirección Operativa") %>% # Remove Tables 
  stripWhitespace() # Remove unnecessary spaces
speech_2020 <- gsub("COVID 19", "COVID", speech_2020) # Standardize COVID-19
speech_2020 <- gsub("COVID- 19", "COVID", speech_2020) # Standardize COVID-19


# Speech 2020 (management 2019) Calculating first frequencies -------------

frequencies_2020 <- tibble(speech = speech_2020) %>% 
  unnest_tokens(output = palabra, input = speech, strip_numeric = TRUE) %>%
  count(palabra, sort = TRUE)
frequencies_2020


# Speech 2020 (management 2019) Removing stopwords & recalculating freq----

frequencies_2020 <- frequencies_2020 %>% 
  anti_join(stopwords_es) %>% 
  anti_join(my_stopwords) %>% 
  anti_join(more_stopwords)
head(frequencies_2020)
