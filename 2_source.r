
# Loading libraries -------------------------------------------------------

source("1_libraries.r")


# Reading data source -----------------------------------------------------

speech_2020 <- pdf_text("input/discurso_2020_2019.pdf")
speech_2019 <- pdf_text("input/discurso_2019_2018.pdf")
speech_2018 <- pdf_text("input/discurso_2018_2017.pdf")
