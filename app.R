#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



###########################################################################################################################
caves <- read.csv(file="/home/grzegorz/ED/csvUTF8.csv", header=TRUE, sep=";")

#zmiana nazw kolumn
colnames(caves)[colnames(caves)=="NR_INWENT.C.50"] <- "Numer_Inwentażowy"
colnames(caves)[colnames(caves)=="REGION.C.254"] <- "Region"
colnames(caves)[colnames(caves)=="GMINA.C.100"] <- "Gmina"
colnames(caves)[colnames(caves)=="Y_1992.N.19.4"] <- "y_1992"
colnames(caves)[colnames(caves)=="X_1992.N.19.4"] <- "x_1992"
colnames(caves)[colnames(caves)=="NAZWA.C.200"] <- "Name"
colnames(caves)[colnames(caves)=="WLASCICIEL.C.254"] <- "Owner"
colnames(caves)[colnames(caves)=="DLUGOSC.N.19.4"] <- "Length"
colnames(caves)[colnames(caves)=="GLEBOKOSC.N.19.4"] <- "Depth"
colnames(caves)[colnames(caves)=="OSUWISKOWA.N.5.0"] <- "Osuwiskowa"
colnames(caves)[colnames(caves)=="PRZEWYZSZE.N.19.4"] <- "Przewyższenie"
colnames(caves)[colnames(caves)=="DENIWELACJ.N.19.4"] <- "Deniwelacja"
colnames(caves)[colnames(caves)=="ID.N.10.0"] <- "ID"

caves$Osuwiskowa <- revalue(as.character(caves$Osuwiskowa), c("1"="Tak", "0" = "Nie"))

#zamiana , na . w współrzędnych
caves$x_1992 <- gsub(",", ".", caves$x_1992)
caves$y_1992 <- gsub(",", ".", caves$y_1992)

#okreslenie liczb dziesietnych w zmiennych zmiennoprzecinkowych
options(digits=9)

#zamiana współrzędnych z danych teksotwych na numeryczne
caves$x_1992 <- as.numeric(caves$x_1992)
caves$y_1992 <- as.numeric(caves$y_1992)

