
# Speech 2020 (management 2019) Removing regular expressions --------------

speech_2020 <- speech_2020 %>%
  str_replace_all("\n", " ") %>% # Replace "\n" by space
  str_remove_all("“") %>% str_remove_all("”") %>% # Remove "" 
  str_replace_all("MINJU", "MINJUDDHH") %>% # Ministerio de Justicia y DDHH
  str_replace_all("MINJUDDHH-DDHH", "MINJUDDHH") %>% 
  str_replace_all("\\s+•\\s+", " ") %>% # Bullets
  str_replace_all("\\s\\S+\\.-\\s", " ") %>% 
  str_replace_all("\\s\\d-\\s", " ") %>% # Numbering
  str_replace_all("\\s\\d\\.\\s", " ") %>% 
  str_replace_all("\\s\\d\\d\\.\\s", " ") %>% 
  str_replace_all("\\s\\d\\.\\d\\s", " ") %>% 
  str_replace_all("\\s+[abcde]+\\)+\\s", " ") %>% 
  str_replace_all("\\s[abcde]\\.\\d\\)", " ") %>% 
  str_replace_all("19\\.-", " ") %>% # 
  str_remove_all("http\\S*") %>% # urls
  str_remove_all("www.\\S*") %>% # Remove web pages
  str_remove_all("Twitter.+gendarmeriacl") %>%  # Remove social networks
  str_remove_all("N° de internos heridos.+\\(S\\.I\\.G\\)") %>% # Remove Tables
  str_remove_all("A continuación, se expone un desglose.+Fuente: Departamento de Infraestructura") %>% # Remove Tables 
  str_remove_all("MATRICULADOS EN EDUCACIÓN SUPERIOR DICIEMBRE 2019.+Total\\s+163\\s+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Tabla Privados de Libertad Inscritos para dar PSU.+\\s+2046\\s+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Tabla Resultados PSU 2019 de Privados de Libertad, por región:.+\\s+13\\s+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+INTERNOS PARTICIPANDO.+\\s+Automotriz\\s+Fuente: Departamento Sistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+Eliminación de antecedentes:.+Fuente: Departamento Post Penitenciario") %>% # Remove Tables 
  str_remove_all("\\s+Intervención:.+\\s+37\\s+Fuente: Departamento Subsistema Cerrado") %>% # Remove Tables 
  str_remove_all("\\s+CANTIDAD DE CELULARES.+\\s+256\\s+Fuente: Subdirección Operativa") %>% # Remove Tables 
  str_remove_all("\\sCOVID: Estadística de contagios por región.+\\s+1357\\s+Fuente: Subdirección Operativa") %>% # Remove Tables 
  str_remove_all("\\s+Catastro.+\\s+167\\s+Fuente: Subdirección Operativa") %>% # Remove Tables 
  str_remove_all("\\sfecha:.+Fuente: Subdirección Operativa") %>% # Remove Tables 
  stripWhitespace() # Remove unnecessary spaces


# Speech 2020 (management 2019) Additional corrections --------------------

speech_2020 <- speech_2020 %>% 
  str_replace_all("COVID\\s19", "COVID-19") %>% # Standardize COVID-19
  str_replace_all("COVID-\\s19", "COVID-19") %>% 
  str_replace_all("COVID:", "COVID-19") %>% 
  str_replace_all("Covid-19", "COVID-19") %>% 
  str_replace_all("Covid19", "COVID-19") %>% 
  str_replace_all("COVID y", "COVID-19 y") %>% 
  str_replace_all("cas2", "cas") %>% # Others
  str_replace_all("ransparencia\\.", "ransparencia ") %>% 
  str_replace_all("para la transparencia", "para la Transparencia") %>% 
  str_replace_all("s e g u r i d a d in t e r na", "seguridad interna") %>% 
  str_replace_all("d e l o s re c i n to s", "de los recintos") %>% 
  str_replace_all("p e n i te nc i a r io s", "penitenciarios") %>% 
  str_remove_all("Departamento Sistema Cerrado") %>% 
  str_remove_all("Departamento de Salud") %>% 
  str_remove_all("Departamento de DDHH") %>% 
  str_remove_all("Departamento DDHH") %>% 
  str_remove_all("Departamento de Promoción y Protección de los DDHH") %>% 
  str_remove_all("Departamento de Promoción y Protección de Derechos Humanos") %>% 
  str_remove_all("Departamento de Promoción y Protección de los Derechos Humanos") %>% 
  str_remove_all("Departamento de Infraestructura") %>% 
  str_remove_all("Departamento de Informática") %>% 
  str_remove_all("Departamento en el Sistema Cerrado") %>% 
  str_remove_all("Departamento") %>% 
  str_replace_all("autoridades sanitarias", "autoridad sanitaria") %>% 
  str_replace_all("condiciones sanitarias", "condición sanitaria") %>% 
  str_replace_all("residencias sanitarias", "residencia sanitaria") %>% 
  str_replace_all("restricciones sanitarias", "restricción sanitaria") %>% 
  str_replace_all("sanitarias", "sanitaria") %>% 
  str_replace_all("Sanitarias", "sanitaria") %>% 
  str_replace_all("contagios", "contagio") %>% str_replace_all("contagio", "contagios") %>% 
  str_remove_all("Departamento de Estadística y Estudios penitenciarios") %>%  
  str_replace_all("lesión", "lesiones") %>% 
  str_replace_all("Egresos", "egreso") %>% str_replace_all("egresos", "egreso") %>% 
  str_replace_all("egreso", "egresos") %>% 
  stripWhitespace() 


# Speech 2020 (management 2019) Document version comparison ---------------
 
# speech_2020_modif1 <- speech_2020
# speech_2020_modif2 <- speech_2020 %>% 
#   stripWhitespace() 
# 
# #diffObj(speech_2020_modif1, speech_2020_modif2, mode="sidebyside")
# diffChr(speech_2020_modif1, speech_2020_modif2, mode="sidebyside")


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
