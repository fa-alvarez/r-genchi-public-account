
# Loading libraries -------------------------------------------------------

source("1_libraries.r")


# Reading data source -----------------------------------------------------

speech_2020 <- pdf_text("input/discurso_2020_2019.pdf")
speech_2019 <- pdf_text("input/discurso_2019_2018.pdf")
speech_2018 <- pdf_text("input/discurso_2018_2017.pdf")


# Removing unnecessary pages -----------------------------------------------

speech_2020 <- speech_2020 %>%
  .[-1:-2] %>% 
  .[-53:-56]
speech_2019 <- speech_2019 %>% 
  .[-1:-2]
speech_2018 <- speech_2018 %>% 
  .[-1]
  

# Concatenating objects for each speech ------------------------------------

speech_2020 <- paste(speech_2020, collapse = " ")
speech_2020 <- paste(speech_2020, collapse = " ")
speech_2020 <- paste(speech_2020, collapse = " ")

