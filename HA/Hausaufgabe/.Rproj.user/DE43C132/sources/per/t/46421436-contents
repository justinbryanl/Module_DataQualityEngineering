library(tidyverse)

#centrepoint tabelle

group <- c(1,2,3,4)
kelas <- c(38-40, 40-42, 42-44, 44-46)
absoulthauf <- c(3,0,5,2)
relatifhauf <- c(0.3, 0, 0.5, 0.2)
breite <- c(2,2,2,2)
histfj <- c(0.15, 0, 0.25, 0.1)
fex <- c(0.3, 0.3, 0.8, 1.0)

#jadi dari histogramm itu data centrepoint disuruh cari terus disuruh pilih mana data yg memungkinkan


#versuch,temperatur,brenndauer, härte fuer vollfaktoriellen versuchsplan

versuch <- c(1,2,3,4,5,6,7,8)
temperatur <- c(850,850,850,850,950,950,950,950)
brenndauer <- c(1,1,2,2,1,1,2,2)
haerte <- c(40,37,45,49,43,41,44,46)

df <- data.frame(A = versuch, B = temperatur, C = brenndauer, D = haerte)

#schaetzwert = ? matematische beschreibung der haerte in abheng der einfluss temperatur und brenndauer durch multiple lineare regression.
#hauptwirkungen und yweifach wechselwirkung berucksichtigt

#schaetywert für achsenabschnitt =?

df_plot <- ggplot(data = df, mapping = aes(x=A, z=D, group = C) +
                    geom_point()
#interaktionsplot
#ditarik 2 garis di werte haerte terus ditanya jarak itu artinza apa terus disuru itung effek sama interpret

#mittelwertvergleich

#konfidenzintervall
#deskriptiv statistik
#yufallsvariable
                

