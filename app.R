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

#funkcja do przekształcania współrzędnych PUWG na WGS84
convertPuwgToWgs <- function(Xpuwg, Ypuwg){
  #Początek układu wsp. PUWG92 (długość)
  L0_stopnie=19.0; 
  m0=0.9993;
  x0=-5300000.0;
  y0= 500000.0;
  
  R0=6367449.14577; 	#promień sfery Lagrange.a
  Snorm=2.0E-6;   		#parametr normujący
  xo_prim=5765181.11148097; 		#parametr centrujucy
  
  #Wspolczynniki wielomianu
  b0=5760000;
  b1=500199.26224125;
  b2=63.88777449;
  b3=-0.82039170;
  b4=-0.13125817;
  b5=0.00101782;
  b6=0.00010778;
  
  # Wspolczynniki szeregu tryg.
  c2=0.0033565514856;
  c4=0.0000065718731;
  c6=0.0000000176466;
  c8=0.0000000000540;
  
  #Przejscie z ukladu aplikacyjnego
  Xgk=(Xpuwg-x0)/m0;
  Ygk=(Ypuwg-y0)/m0;
  
  #etap I - (Xgk, Ygk) -> (Xmerc, Ymerc)
  a = (Xgk-xo_prim)*Snorm
  b = Ygk*Snorm
  z <- complex(real = (Xgk-xo_prim)*Snorm, imaginary = Ygk*Snorm)
  Xmerc=b0+a*b1+a^2*b2-b2*b^2+a^3*b3-3*a*b^2*b3+a^4*b4-6*a^2*b^2*b4+a^5*b5-10*a^3*b^2*b5+5*a*b^4*b5+a^6*b6-15*a^4*b^2*b6+15*a^2*b^4*b6-b6*b^6
  Ymerc=b*b1+2*a*b*b2+3*a^2*b*b3-b^3*b3+4*a^3*b*b4-4*a*b^3*b4+5*a^4*b*b5-10*a^2*b^3*b5+b^5*b5+6*a^5*b*b6-20*a^3*b^3*b6+6*a*b^5*b6
  
  #etap II - Xmerc,Ymerc -> fi, delta_lambda
  alfa=Xmerc/R0;
  beta=Ymerc/R0;
  
  w=2.0*atan(exp(beta))-pi/2.0;
  fi=asin(cos(w)*sin(alfa));
  d_lambda=atan(tan(w)/cos(alfa));
  
  #etap III
  B=fi+c2*sin(2.0*fi)+c4*sin(4.0*fi)+c6*sin(6.0*fi)+c8*sin(8.0*fi);
  dL=d_lambda;
  
  #Obliczenia koncowe
  B_stopnie=B/pi*180.0;
  dL_stopnie=dL/pi*180.0;
  L_stopnie=dL_stopnie+L0_stopnie;
  
  WGS <- c(B_stopnie, L_stopnie)
  return(WGS)
}


#zamian współrzędnych w data frame
for (i in 1:length(caves$Name)) {
  coordinates <- convertPuwgToWgs(as.double(caves$y_1992[i]), as.double(caves$x_1992[i]))
  caves$x_1992[i] <- coordinates[1]
  caves$y_1992[i] <- coordinates[2]
}
