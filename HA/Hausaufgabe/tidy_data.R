library(readxl)
library(tidyverse)
excel_sheets("dateien.xlsx")

#Ortsauswahl
ortauswahl <- read_excel("dateien.xlsx", sheet=1)

#Standardisierung
standardisierung <- read_excel("dateien.xlsx", sheet=2)

#Versuchplan
versuchplan <- read_excel("dateien.xlsx", sheet=3)

#Steepest Ascent
steepest_ascent <- read_excel("dateien.xlsx", sheet=4)


