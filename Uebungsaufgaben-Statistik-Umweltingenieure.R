
rm(list = ls()) #loescht alle Variablen aus dem Workspace


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to source fiile  directory
setwd("./data") #assumes that all data files are in subfolder "data" of source director

# R-Refresher----


#R Frage 1----
daten=c("feucht","nass","feucht","nass","trocken")
reihenfolge=c("nass","feucht","trocken")
x=factor(daten,levels=reihenfolge)
y=factor(daten)
str(x)
str(y)
x==y#x und y sind gleich, nur die factors anders

#
#Sortiere die Daten: sort - Befehl sortiert
#gemaess der Reihenfolge des levels -Befehls.
#Standard: alphabetische Sortierung bei Zeichenketten
#vom Typ "factor"

x1=sort(x)# sortiert gemaess Reihenfolge
y1=sort(y) #alphabetisch sortiert

x1==y1 #was faelllt Dir auf?


#metrische Daten ("Zahlen")sind ein einfacher Datenvector
x=c(1,2,3)
str(x
)


#R-Frage 2----

#R-Frage 2a)
Kaese=read.csv("Kaese.csv") #Mac
# baum=read.csv("baum.csv") #Windows
#R-Frage 2b)
#View(Kaese)
str(Kaese)

#R-Frage 3----
Kaese$Sorte
Kaese$Naehrwert

#R-Frage 4----
attach(Kaese)
Sorte
n1=Naehrwert
n2=Kaese$Naehrwert


# Lineare Regression -----
#R-Frage 5----

baum=read.csv("baum.csv")
#View(baum)
attach(baum) #erspart $-Schreibweise
str(baum)
#par(mfrow=c(1,1)) #nur ein Bild wird nebeneinander angezeit
plot(baum$Dicke,baum$Hoehe,xlab="Dicke [cm]", ylab="Hoehe [m]")
#ist dasselbe wie
plot(Dicke,Hoehe)
plot(Dicke,Hoehe,xlab="Dicke [cm]", ylab="Hoehe [m]")



#R-Frage 6 -----

trend=lm(Hoehe~Dicke) #abhaengige Variable (abhaengige Variable f(x)=y=Hoehe ~ unabhaengige Variable x=Dicke)
abline(trend,col="red")

str(trend) #trend ist vom Datentyp "list"




# R-Frage 7----

coefficients(trend)

# Zeige, dass abline tatsaechlich die Steigungsgerade
#y=a *x +b ist:

#Auf die einzelnen Elemente der "list" Trend koennen wir mit "Name der Liste"$"Element der Liste" zugreifen
b=trend$coefficients[1] #Intercept= y-Achsenabschnitt b
a=trend$coefficients[2] #Steigung
x=0:50 #creates vector c=(0,1,2,...,50)
y=a*x+b #Trendlinie mit Steigung a und y-Achsenabschnitt b
plot(x,y,col="blue", ylim=c(0,max(baum$Hoehe)+10),xlab="x: Dicke [cm]", ylab="y: Hoehe [m]") #same as trend line below
abline(trend,col="red")




# R-Frage 8 -----


baum=read.csv("baum.csv")
#Falls die Daten vom Typ "factor" sein sollten:
#Schau Dir die Daten mit dem View - Befehl an:

#View(baum)
#Sind die Zellen mit Komma oder Semikolon getrennt?
#alternatives Einlesen

baum=read.csv("baum.csv",sep=",",dec=".") #passe eventuell an:
#Alternativen: sep="; "
#und dec=","
#Spiele mit den Variationen von sep und dec bist Du einen data.frame namens baum
#mit 2 Observablen vom Datentyp "num" hast
#
#Trick, um Daten vom Typ factor in Daten vom Datentyp numeric umzuwandeln:
#Zuerst in Daten vom Typ character verwandeln mit as.character
#und dann in Daten vom Typ as.numeric verwandeln.
#Beispiel: Falls baum$Dicke (faelschlicherweise) vom Typ "factor" waere:
#
#Dicke=as.numeric(as.character(baum$Dicke))

attach(baum) #erspart $-Schreibweise
str(baum) #hier muessen Daten vom Typ "factor" stehen.
#par(mfrow=c(1,1)) #nur ein Bild wird nebeneinander angezeit
plot(baum$Dicke,baum$Hoehe,xlab="Dicke [cm]", ylab="Hoehe [m]",xlim=c(0, 50),ylim=c(0, 25))
#xllim und ylim bestmmen den x bzw. y Achsenbereich
#hier erzwinge ich, dass beide Achsen durch 0 gehen
#schau Dir die Befehle mit ?xlim und ?ylim an

trendu=lm(baum$Hoehe~baum$Dicke+0) #Trendgerade durch Ursprung
summary(trendu)
abline(trendu,col="red")
#Vergleich Trendgerade mit y-Achsenabschnitt
trend2=lm(baum$Hoehe~baum$Dicke)
summary(trend2)
abline(trend2,col="blue")
#Offensichtlich beschreibt die blaue Trendgerade
#(mit y-Achsenabschnitt) die Daten besser,
#die Summe der Quadrate der Zufahrtsstrasse ist kleiner
#
#
#in der naechsten Aufgabe visualisieren wir die Residuen



# R-Frage 9 ----
par(mfrow = c(1, 2)) #die folgendene Plots werden 1 Zeile und 2 Spalten haben
#zwei Plots nebeneinander
plot(baum$Dicke,residuals(trendu))
plot(baum$Dicke,residuals(trend2))
#Siehst Du, dass die Residuen im Fall mit y-Achsenabschnitt (trend2), rechtes Bild
#a) viel kleiner sind und
#b) eher zufaellig aussehen.
par(mfrow = c(1, 2)) #zwei Bilder nebeneinander

#Das Residuum ist die Differenz aus y-Messwert und dem zugehoerigen y-Wert auf der Trendgeraden
#Ohne y-Achsenabschnitt
par(mfrow = c(1, 2)) #zwei Bilder nebeneinander
plot(baum$Dicke,baum$Hoehe-predict(trendu))
plot(baum$Dicke,residuals(trendu))

par(mfrow = c(1, 2)) #zwei Bilder nebeneinander
#mit y-Achsenabschnitt
plot(baum$Dicke,baum$Hoehe-predict(trend2))
#same as
plot(baum$Dicke,residuals(trend2))

par(mfrow = c(1, 1)) #wieder ein Bild

#Beispiele zum rank - Befehl:
#Beispiel 1: Krankheitstage (Beispiel im Skript)
krankheitstage=c(0,0,8,7,0,14,2,0,18,3,12,8,0,20,3,9,1,14,50) #Zahlen sind metrisch-> kein "factor" -Befehl noetig
table(krankheitstage) #table veranschaulicht wie haeufig jeder Zahlenwert auftaucht
#Rangbindung bei 0,3,8,14 Fehltagen
#Berechnung der Raenge (wie beim Sport mit Option ties="min")
rank(krankheitstage,ties="min")

#Berechnung der korrigierten Raenge (->input for Wilcoxon)
rank(krankheitstage)
#ist dasselbe wie
rank(krankheitstage,ties="average") #rechnet mittleren Rang aus


# R-Frage 10 #####
cor(baum$Dicke,baum$Hoehe)
#ist dasselbe wie
cor(baum$Dicke,baum$Hoehe,method="pearson") #pearson ist der default -Wert der cor - Funktion
#ist dasselbe wie
#r=0.68


#Der Spearman-Koeffizient braucht man in der Regel fuer ordinale Daten,
# er KANN auch fuer metrische Daten angewandt werden
#(die immer automatisch auch ordinal sind):
cor(baum$Dicke,baum$Hoehe,method="spearman")
#r=0.77
#Bemerkung: Die Standardeinstellung in R fuer "cor" ist  method="spearmann"



# Einschub #####################################
#in R kann man sich auch Funktionenen selbst schreiben - z. B. fuer den Pearsonschen Korrelationskoeffizientnen

corfunction <- function(x,y) {
  r=cov(x,y)/(sd(x)*sd(y)) #Gleichung 2.2
  return(r)
}

#Aufruf der neugeschriebenen Funktion
corfunction(baum$Dicke,baum$Hoehe) #selbes Ergebnis wie cor(baum$Dicke,baum$Hoehe,method="pearson") #

#Der Spearmansche Korrelationskoeffizient wendet Gleichugn 2.2 nicht auf die Datenpaare, sondern auf die entsprechenden
#korrigierten Raenge an
corfunction(rank(baum$Dicke),rank(baum$Hoehe))#selbes Ergebnis wie cor(baum$Dicke,baum$Hoehe,method="spearman") #
# Einschub ENDE 


# R-Frage 11 #####################################
NOx=read.csv("NOxEmissions.csv")
#View(NOx)
head(NOx) #zeigt die ersten 6 Zeilen an
attach(NOx)
par(mfrow = c(1, 1)) #wieder ein Bild
plot(LNOx,LNOxEm)
(lmNOx <- lm(LNOx ~ LNOxEm))

cor(LNOx,LNOxEm)
cor(LNOxEm, LNOx) #die Korrelation ist unabhaengig von Reihenfolge

cor(LNOx,LNOxEm,method="spearman")

trendNO=lm(LNOxEm~LNOx)
summary(trendNO)
abline(trendNO,lwd=3,col="blue") #das sieht nun sehr unbedfriedigend aus
coefficients(trendNO)
#Naechster Versuch: durch Ursprung
trendNO0=lm(LNOxEm~LNOx+0)
summary(trendNO0)
abline(trendNO0,lwd=3,col="red")



#Nichtlineare Regression #####################################
# R-Frage 12 #####################################

# Mauna Loa Atmospheric CO2 Concentration
# (weltweit laengste Messreihe der atmosphaerischen CO2-Konzentration)
# http://co2now.org/Current-CO2/CO2-Now/noaa-mauna-loa-co2-data.html
#
#Einheiten: co2: ppm (parts per million)
#t: Jahre

#Gib R an, wo sich Dein csv-File co2.csv befindet:
#sets working directory

# Einlesen> co2.csv
#alternatives Methoden zum Einesen der Daten
#co2=read.table("co2.csv",sep=",",header=TRUE)
#co2=read.delim("co2.csv",sep=",")

co2=read.csv("co2.csv")
#View(co2)
str(co2) #co2 ist data.frame mit 2 numerischen Variablen und 663 Observablen

#finde kuerzere Namen
CO2=co2$concentration
t=co2$time

#Plot der Daten

plot(t,CO2,type="l")
# lineare Regression
trend=lm(CO2~t)
coefficients(trend)
#Einzeichen der Trendgeraden
abline(trend,col="blue")
#Parameter der Geradengleichung
coef=coefficients(trend)
coef #1. Eintrag: y-Achsenabschnitt b(negative CO2 Konzentration!) , zweiter Eintrag: Steigung a=Delta CO2/Delta t)
trend$coefficients[1] #b=-2603 (negativ!)
trend$coefficients[2] #a=1.5



# f(x_i) der Trendgerade
fitwerte=predict(trend) #f(x_i)
#residu=CO2-fitwerte #same as residuals(trend)
#kuerzer
points(t,fitwerte,col="red",type='l')
# Residuen
residuals(trend)
#Residuen - Plot zeigt Muster!
#postscript("co2residuals.eps")
plot(t,residuals(trend),col='blue') #Residuen nicht gleichmaessig verteilt, sondern parabelfoermig. Deutet auf quadratische Funktion hin
#dev.off()
# Korrelation
cor(t,CO2) #cor=0.99, starke Korrelation
#cor.test(t,CO2)


# R-Frage 13 #####################################
par(mfrow=c(1,1))
t2=t^2
trend=lm(CO2~t) #lineare Regression
trend2=lm(CO2~t+t2) #quadratische Regression
#alternativ: ohne Zusatzvariable t2
#trend22=lm(CO2~ poly(t,2)) #liefert gleiches Ergebnis wie trend2:
#quadratische Regression ist Polynom 2. Grades
#im Prinzip kannst Du so ein Polynom n. Grades
#mit trend22=lm(CO2~ poly(t,n)) fitten
summary(trend2)


#b=4.498+10^4 (intercept)
#a=-46.34
#c=-1.204*10^-2
#postscript("linear_quadratic.eps")
plot(t,CO2,type="l")

#leider koennen wir jetzt die Regressionsgerade NICHT mit abline einzeichnen,
#da abline nur den y-Achsenabschnitt b und den linearen Vorfaktor a verwendet

#bringe R bei die plots uebereinander zu legen: lines statt plot, macht keine neue Figur
lines(t,predict(trend2),col="red") #Quadratische Regression

#Zum Vergleich: Zeichne nun auch noch lineare Regression in gruen ein
abline(trend,col="green") #lineare Regression
#Sind die Residuen nun auch gleichmaessig verteilt?

plot(t,residuals(trend2),col='blue') #JA!!!!


# R-Frage 14 #####################################

plot(t,CO2,type="l",col="black")
guess=list(a=1,b=1,c=1,d=1,e=1)
trend3=nls(CO2~b+a*t+c*t^2+d*sin(t*2*pi+e),start=guess)
summary(trend3)
#a=-46.43
#b=4.498*10^4
#c=0.01206
#An den Koeffizienten a,b,c aenderte sich nur wenig durch Hinzuname der Schwingung
#d=2.81
#e=-0.38

#Plote alle Trendlinien in einen plot: lineare, quadratische und quadratische +sinus Schwingung
#Was faellt Dir auf?
points(t,predict(trend3),type="l",col="blue") #mit Sinus blau
abline(trend,col="green") #linear gruen
points(t,predict(trend2),type="l",col="red") #quadratisch rot
#dev.off()
#Plot der Residueen
#postscript("allresiduals.eps")
plot(t,residuals(trend),col='green')
points(t,residuals(trend2),col='red')
points(t,residuals(trend3),col='blue')

par(mfrow=c(1,3))
hist(residuals(trend))
hist(residuals(trend2),col="green")
hist(residuals(trend3),col="red") #Test mit Histogramm, die haeufigsten Resdiuen um 0, Verteilung sollte Normalverteilung aehneln
par(mfrow=c(1,1))


rm(list = ls()) #loescht alle Variablen aus dem Workspace
par(mfrow=c(1,1)) #erzwingt ein Bild pro Plot

# R-Frage 15 #####################################
baum=read.csv("baum.csv")
plot(baum$Dicke,baum$Hoehe)
trend=lm(baum$Hoehe~baum$Dicke)
abline(trend,col="blue")
coefficients(trend)
summary(trend)
cor(baum$Dicke,baum$Hoehe)

# R-Frage 16 #####################################
#
#Mit dem Befehl nls haetten wir auch die quadratische Regression loesen koenne
#
co2=read.csv("co2.csv")
#finde kuerzere Namen
CO2=co2$concentration
t=co2$time
t2=t^2

guess=list(a=1,b=1,c=1)
trend4=nls(CO2~b+a*t+c*t^2,start=guess)

#Vegleiche mit R-Aufgabe 13:
trend2=lm(CO2~t+t2) #quadratische Regression
#kommt zum selben Resultat
#die quadratische Regression ist ein Speziall der  "nicht-lineare-Regression!

# R-Frage 17 #####################################
#Inspizieren der Daten
#View(trees)
?trees #Versuche den Abschnitt "Format" zu verstehen.
str(trees)


#Und nun das graphisches Inspizieren:
# R-Frage 18 #################### 
plot(trees)
#Wi vermutest Du eine starke Korrelation?

#R-Frage 19 -----
cor(trees)
#Starke Korrelation (r>0.7zwischen Girth und Volume
#Schwache Korrelation (r zwischen 0.4 und 0.7)zwischen Height und Girth und Height und Volume

# R-Frage 20 ###################
#View(trees)
str(trees)

#Graphische Darstellung
plot(trees$Girth,trees$Volume)

#Lineare Regression
univarlm=lm(trees$Volume~trees$Girth)#"list" der linearen Regressionheist jetzt univarlm
coefficients(univarlm)
b=univarlm$coefficients[1] #Intercept= y-Achsenabschnitt b
a=univarlm$coefficients[2] #Steigung
plot(trees$Girth,trees$Volume)
abline(univarlm,col="red")
summary(univarlm)

# Multivariate Regression ###################
# R-Frage 21 ################### 

multivarlm=lm(trees$Volume~trees$Girth+trees$Height)
multivarlm$coefficients


# R-Frage 22 ####################
summary(multivarlm)
#y-Achsenabschnitt: p=2.75e-07: hoch sinifikant ***
#Koeffizient des Umfangs (Girth): p<2*10 ^-16: hoch signifikant ***
#Koeffizient der Hoehe (Height): p<0.0145 *"nur" signifikant
#r^2: Multiple R-squared:  0.948

#Vgl: beim univariaten linearen Modell hatten wir
#Multiple R-squared:  0.9353,
#d.h. das multivariate Modell beschreibt die Daten etwas besser

## R-Frage 23 ###################
#Einlesen der Daten
NOx=read.csv("NOxEmissions.csv")
attach(NOx)
str(NOx)
#Wiederholung der linearen REgression
plot(LNOxEm,LNOx)
trendNO=lm(LNOx~LNOxEm) #RAufgabe 11: univariate lineare Regression.
abline(trendNO,col="red")
summary(trendNO) #Guetemass englisch: multiple R-squared 0.4096

#multivariate lineare Regerssion
trendNOmulti=lm(LNOx~LNOxEm+sqrtWS)
summary(trendNOmulti) #Guetemass Multiple R-squared:  0.663
#Das multivariate,lineare Modell beschreibt die Daten besser als das lineare Modell mit nur einer unabhaengigen Variablen

rm(list = ls()) #loescht alle Variablen aus dem Workspace


# Binomialverteilung -------

# R-Frage 24 ######
k=3     #Mitte des Kugelbretts: drei Erfolge, wenn unten angekommen
n=6     #Anzahl der Ereignisse in EINER Bernoulli-Kette.
p=0.5  #Wahrscheinlichkeit fuer Rechtsablenkung
#Hier: Kugel trifft sechsmal Hindernis bis unten angekommen
w_kugel_mitte=dbinom(k,n,p) #31%

wahrscheinlichkeitmitte=choose(n,k)*p^k*(1-p)^(n-k)
##R-Frage 25 ###################  
#Strom ohne Atom:   10% ja-Stimmen
#a)
k=10    # "ja"=Erfolg, es geht um eine Stichprobe mit n=100: 10% "ja"-Stimmen entspricht 10 Erfolgen
n=100   #  1 Befragter  entspricht einem Ereignis -> 100 Befragte entsprechen 100 Ereignisse
p=0.2    # Wahrscheinlichkeit des Erfolges

w_10prozent=dbinom(k,n,p) #0.3 %
dbinom(10,100,0.2)
#Ergaenzung 1
#nun befragen wir 100 mal mehr Menschen

n2=100^2
k2=0.1*n2 #10% in unserer Umfrage sind dafuer
p=0.2
w_10prozent_10000=dbinom(k2,n2,p) #6.0*10^0162 Nun ist die Wahrscheinlichkeit quasi null so daneben zu liegen

#Bemerkenswert werden bei politischen Umfragen meist nur 1000 Menschen befragt:

# Ergaeznung 2: Wahrscheinlichkeit dafuer, genau richtig zu liegen
n=1000
k2=p*n
p=0.2
#Wahrscheinlichkeit genau richtig zu liegen - auch nur 3%
w_20prozent_n1000=dbinom(p*n,n,p) #0.031=3.1%



#Ergaenzung 3:
#Umfragen koennen immer nur mit einer bestimmten Wahrscheinlichkeit das Ergebnis vorhersagen-> es ist besser einen "Bereich" anzugeben:
#z.B. so
##Wahrscheinlichkeit dafuer, ein Ergebnis gr??sser als  18 % und ist gleich oder kleiner 22 % "Ja" Stimmen zu bekommen, wenn Du 1000 Menschen befragst und Du weisst, dass in der Grundgesamt 20% fuer Strom ohne Atom sind
#genauer:


#Wahrscheinlichkeit dafuer ein Ergebnis >180 und =<220  Ja - Stimmen zu bekommen
#A)
n=1000
xk1=0.180*n
xk2=0.220*n
pbinom(xk2,n,p)-pbinom(xk1,n,p) #89%
#B):
#Variation: Wahrscheinlichkeit dafuer ein Ergebnis=>180 (inklusive 180) Ja - Stimmen) und <=220 Ja Stimmen zu erhalten
#anderer Grenzwert xk1!
n=1000
xk1=0.179*n
xk2=0.220*n
pbinom(xk2,n,p)-pbinom(xk1,n,p) #93%
#Kommentar
pbinom(xk2,n,p) #Wahrscheinlichkeit dafuer, bis zu 220  Ja Stimmen zu erhalten
pbinom(xk2,n,p) #Wahrscheinlichkeit dafuer, bis zu 179  Ja Stimmen zu erhalten
pbinom(xk2,n,p)-pbinom(xk1,n,p) #Wahrscheinlichkeit dafuer, zwischen 180 und 220 Stimmen zu erhalten




#R-Aufgabe 25 b
#Erzeuge Tabellenwerte
#Wir koennen im Funktionsaufruf von dbinom einen Vektor an die Stelle von k setzten
##b)
befuerworter_stichprobe=c(0,5,10,15,18,20,22,25,30,100) #Vektor!
dbinom(befuerworter_stichprobe,n,p) #Ergebnis: Vektor



# R-Frage 26  ###################
k=4   # 4 Erfolge ("katholisch") bei Befragung von 10 Personen
n=10  #1 Befragter entspricht einem Ereignis-> 10 Befragte entsprechen 10 Ereignissen
p=0.4 # 40% der Bevoelkerung sind katholisch

dbinom(k,n,p)


#R-Frage 27 ####################
k=280 # Grenzwert x_k: das ist die Anzahl der Plaetze, die das Flugzeug tatsaechlich hat
n=285  # Anzahl maximal verkaufte Tickets
p=0.96

#Wahrscheinlichkeit dafuer, dass mehr Passagiere als vorhandene Plaetze erscheinen:
w_ueberbuchung=1-pbinom(k,n,p)
# oder (liefert gleiches Resultat)
w_ueberbuchung=pbinom(k,n,p,lower.tail=FALSE)

#Alternativ haette man auch aeussert umstaendlich
#(und auf keinen Fall ratsam) mittels dbinom ueber alle Werte groesser als k und ist gleich kleiner
#n summieren koennen
vec=(k+1):n  #definiere Vector vec=(281,282,283,284,285)
w_ueberbuchung=sum(dbinom(vec,n,p)) #hier summieren wir dbinom fuer alle Werte im Vektor vec=(281,282,283,284,285)
#Vergleich
pbinom(k,n,p,lower.tail = FALSE) #w_ueberbuchung #selbes Resultat



## R-Frage 28 ###################
k=20 #Erfolg ist hier der Ausschuss
n=500
p=0.03

#Die Wahrscheinlichkeit mehr als k=20 Saecke Ausschuss zu produzieren, ist dann
1-pbinom(k,n,p)
# oder:
pbinom(k,n,p,lower.tail=FALSE)

# R-Frage 29 ##### 
binom.test(3,100,conf.level=0.95)
# die Intervall-Werte 0.0062 0.085
# besagen, dass man auf dem 95%-Signifikanzniveau
# auf einen Ausschuss-Anteil zwischen 0.62% und 8.52%
# schliessen darf


#R-Frage 30 #################### 
# Krawatten
n=67 # Anzahl der Teilnehmer an Stichprobe
ua=59#Anzahl der Teilnemehr, die Krawatte haesslich finden
boa=4; #Anzahl der Teilnehmer, die Krawatte schoen finden
gehtso=boa;#Anzahl der Teilnehmer, die Krawatte "geht so finden"
gamma=0.9 #Konfidenzniveau
# a) uaeaeaehh (haesslich) - Anteil:
binom.test(ua,n,conf.level=gamma)
# das Konfindenz-Intervall reicht von 79.5% bis 93.9%
#
# b)geht-so-Anteil:
binom.test(gehtso,n,conf.level=gamma)
# das Konfindenz-Intervall reicht von 2.1k% bis 13.1%
#
# c) boaaah (so schoen) -Anteil
binom.test(boa,n,conf.level=gamma)
# c) das Konfindenz-Intervall reicht von 2.1% bis 13.1%, da boa=gehtso






# R-Frage  31 ####################
x=c(168,163,160,161,163,170,169,178,164,164) #Datenvektor der Groessen
gamma=0.95
t.test(x,conf.level=gamma)   # gamma=0.95
# Das Konfidenz-Intervall reicht also bis 169.8449.
# Somit liegt 167.6 noch im Konfidenz-Intervall. Es gibt
# daher keinen Widerspruch.
#
#Merke:df steht fuer "degrees of freedom" und es gilt bei einer Stichprobe
#beim t-Test: df=n-1
# Die Obergrenze des Konfidenz-Intervalles laesst sich
# auch nach der Formel berechnen:
daten=x
mean(daten)+qt(1-(1-gamma)/2,length(daten)-1)*
  sd(daten)/sqrt(length(daten))


# Poisson-Test ----

# R-Frage  32 #################### 
# Kupferdistel
poisson.test(84,conf.level=0.95) #Konfidenzintervall ausgerechnet fuer 200*300 m^2 Flaeche
# liefert ein Konfidenz-Intervall
# von 67.00169 bis 103.99772.
#
#gesucht Konfidenzintervall fuer a 1 km^2=10^6 m^2
#1 km^2=10^6 m^2=skalierfactor*(200*300)m^2-> scalierfactor=10^6/(200*300)

scalierfactor=10^6/(200*300)
x=84;
# Jetzt alle Werte auf 1 km^2 umskalieren, d.h. mit
# scalierfactor=10^6/(200*300) multiplizieren:
lambda = x * scalierfactor   # zentraler Wert 1400
lambda_unten=67 * scalierfactor# untere Grenze 1117
lambda_oben=104 *scalierfactor   # obere Grenze 1733

#Schaetzformel darf hier eigentlich NICHT angewandt werden, da x<100
x=84 #Varianz poissonverteilter Groesse ist gleich Schaetzwert s^2=x
#sqrt(x) ist dann die Standardabweichung der poissonverteilten Groesse
z=abs(qnorm(0.025)) #z (alpha/2)
wurzel=z*sqrt(x)
lambda_unten_s=(x-wurzel)*scalierfactor #1101
lambda_oben_s=(x+wurzel) *scalierfactor #1699
lambda_s=x*scalierfactor


#wie erwartet Abweichung von poisson.test, da Stichprobe zu klein fuer Schaetzformel.



# R-Frage 33 #################
# eventuell baum.csv wieder einlesen
baum=read.csv("baum.csv")
#View(baum)
attach(baum) #erspart $-Schreibweise

#plot(Dicke,Hoehe)
plot(baum$Dicke,baum$Hoehe,xlab="Dicke [cm]", ylab="Hoehe [m]")
#lineare Regression
trend=lm(baum$Hoehe~baum$Dicke)
#Einzeichen der Trendlinie
abline(trend,col="blue")
coefficients(trend)
confint(trend,level=0.95)
#gleiches Resultat wie #gamma=0.95 ist der "default" in R und muss nicht extra eingegeben werden
confint(trend)


# R-Frage 34 ####################

plot(baum$Dicke,baum$Hoehe,xlab="Dicke [cm]", ylab="Hoehe [m]")
abline(trend,col="blue")
#predict is a generic function for predictions
#from the results of various model fitting functions (here:lm)
trumpet95=predict(trend,interval="confidence",level=0.95)
#trumpet95_short=predict(trend,interval="confidence") liefert gleiches Resultat wie trumpet95,da 0.95 immer der
#Default--Wert ist.
trumpet95
#1. Spalte: die y-Werte der Trend-Linie der linearen Regression
#2. Spalte: die y-Werte der unteren Trompeten-Kurve mit gamma=0.95
#3. Spalte: die y-Werte der oberen Trompetenkurve mit gamma=0.95
points(baum$Dicke,trumpet95[,2],type="l",lty=1,col="cyan") #untere Trompetenkurve
points(baum$Dicke,trumpet95[,3],type="l",lty=1,col="cyan") #obere Trompetenkurve
# Trompetenkurven fuer gamma=0.99
trumpet99=predict(trend,interval="confidence",level=0.99)
points(baum$Dicke,trumpet99[,2],type="l",lty=1,col="green")
points(baum$Dicke,trumpet99[,3],type="l",lty=1,col="green")

# R-Frage 35 ####################
cor.test(baum$Dicke,baum$Hoehe,level=0.95)#sample estimate entspricht cor:
cor(baum$Dicke,baum$Hoehe)
#p-value=0.02749

#R-Frage 36 ################### 
#Und nun der volle BaumDatensatz
cor.test(trees$Volume,trees$Girth,level=0.95)#sample estimate entspricht cor:
plot(trees$Girth,trees$Volume,xlab="Umfang", ylab="Volumen")
trend=lm(trees$Volume~trees$Girth)
abline(trend,col="blue")
#predict is a generic function for predictions
#from the results of various model fitting functions (here:lm)
trumpet95=predict(trend,interval="confidence",level=0.95)
#trumpet95_short=predict(trend,interval="confidence") liefert gleiches Resultat wie trumpet95,da 0.95 immer der
#Default--Wert ist.
trumpet95
#1. Spalte: die y-Werte der Trend-Linie der linearen Regression
#2. Spalte: die y-Werte der unteren Trompeten-Kurve mit gamma=0.95
#3. Spalte: die y-Werte der oberen Trompetenkurve mit gamma=0.95
points(trees$Girth,trumpet95[,2],type="l",lty=1,col="cyan") #untere Trompetenkurve
points(trees$Girth,trumpet95[,3],type="l",lty=1,col="cyan") #obere Trompetenkurve
# Trompetenkurven fuer gamma=0.99
trumpet99=predict(trend,interval="confidence",level=0.99)
points(trees$Girth,trumpet99[,2],type="l",lty=1,col="green")
points(trees$Girth,trumpet99[,3],type="l",lty=1,col="green")


#t-Test im 1-Stichprobenfall-----

mu=91 #Referenzwert
x=x=c(70,85,31)
xmean=mean(x) #62 <91: Die Daten legen nahe, einseitig mit Seitigkeit "kleiner" zu pruefen
n=length(x)
df=n-1 #degrees of freedeom df=n-1
s=sd(x)
t=(xmean-mu) *sqrt(n)/s; #t ist negativ: -1.8, da xmean<xmu

#kleiner
#
pt(t,n-1) #linker Schwanz:kleiner
t.test(x,mu=91,alternative="less")


#groesser
pt(t,n-1,lower.tail=FALSE) #rechter "Schwanz": groesser
t.test(x,mu=91,alternative="greater")

#zweiseitig
pt(t,n-1)*2 #zweimal linker Schwanz, falls t<0
t.test(x,mu=91)


#neues Beispiel: t>0
mu=50
tneu=(xmean-mu) *sqrt(n)/s; #t=0.74 ist positiv: Fuer zweiseitiges Testen: zweimal rechter Schwanz
pt(tneu,n-1,lower.tail=FALSE)*2
#Vergleich mit t-Test:
t.test(x,mu=50)
#R-Frage 37 ----- 

#Beispiel fuer Vorgehen, falls Umfang, Mittelwert und Standardabweichung
#der Stichprobe gegeben sind
n=200      #Umfang der Stichprobe
xmean=255  #Mittelwert der Stichprobe
s=60       #Standardabweichung der Stichprobe
mu=240     #Referenzwert
t=(xmean-mu)/(s/sqrt(n)) #grosser t-Wert>0

#1. H1: xmean>mu
#   H0: xmean=<mu
#2. Festlegung der Irrtumswahscheinlichkeit
alpha=0.05
#3. Berechnen des P-Wertes


#einseitige Fragestellung:groesser

#H1: Kartoffeln mit Duenger sind signifikant schwerer als Referenzwert mu=240g
pt(t,n-1,lower.tail=FALSE) #distribution function = Dichtefunktion
#berechnet die Flaeche unterhalb der t-Verteilung fuer alle Werte >t
#n-1: Freiheitsgrade

#4. P<alpha?
#P-Wert ist die Wahrscheinlichkeit der Nullhypothese
#Ist diese Wahrscheinlichkeit kleiner als unsere Irrtumswahrscheinlichkeit,
#muessen wir die Nullhypothese bei gegebenem Signifikanzniveau gamma=1-alpha ablehen

# hier: der Unterschied ist signifikant (P=0.00025<alpha=0.05),
#die Nullhypothese muss auf dem gamma=95% Signifikanzniveau verworfen werden,
#der Duenger stellt eine Verbesserung dar.


#Ueberpruefen des Resultates mit t.test

# R erlaubt uns zufaellige Werte einer Stichprobe mit Standardabweichung s und Mittelwert xmean zu wuerfeln
#hierzu muessen wir aber die library MASS intallieren:
library(MASS)
#Generiere Stichprobe mit Mittelwert overline(x) und Varianz sigma^2, Standardabweichung sigma
meansample=255;
s=60;
samplesize=200;
x = mvrnorm(n = samplesize, meansample,s^2 , tol = 1e-8, empirical = TRUE)
t.test(x,mu=240,alternative="greater")
#p-value=0.0002531

#kartoffel=write.table(x,file="kartoffel.csv",col.names=c("gewicht"),sep=",",dec=".")

######################Ende Abschnitt nicht pruefungsrelevant
#R-Frage 38 ####################  
#Joghurt
#Waehle die Irrtumswahrscheinlichkeit selber.
#Hier: Ich waehle die (Standard-)Irrtumswahrscheinlichkeit alpha=0.05.
#1.H1:Mittelwert von x ist kleiner gleich Referenzwert mu
#H0: Mittelwert von x ist groeser Referenzwert mu
#2. Waehle Irrtumswahscheinlichkeit
alpha=0.05
#3. Berechnung des p-Values
x=c(2.5,3.5,5,3.8)
t.test(x,mu=4,alternative="less")

#4. P<alpha?
# der P-Wert betraegt 0.30 und ist somit deutlich groesser als die gewaehlte Irrtumswahrscheinlichkeit alpha=0.05,
# d.h. die Stichprobe reicht nicht aus, um zu beweisen, dass die Anforderung erfuellt ist.
#b) Berechnung des t-Wertes
SE=sd(x)/sqrt(length(x))
mu=4
zaehler=(mean(x)-mu)
t=zaehler/SE #t=-0.58<0 dieser Wert wird auch vom t-test angezeigt
#Einseitiges Testen, kleiner-> linker Schwanz
df=length(x)-1 #n-1
pt(t,df) #selber p-value wie bei t.test

#Befehle im Kasten zur Erlaeuterung des 2-Stichproben t-Tests #################### 
#a) Stichprobenwerte als Zahlenwerte gegeben
x1=c(70,85,31)
x2=c(72,61,104)
#Welch Two Sample t-test: Varianzheterogenitaet
t.test(x1,x2,alternative="less")
#p-value = 0.229, R fuert den "Welch Two Sample t-test" aus, der "nur"
#Varianzheteroegnitaet fordert

#Two Sample t-test: Varianzhomogenitaet
t.test(x1,x2,alternative="less",var.equal=TRUE)
#p-value=0.228
#Unter der Annahme von Varianzhomogenitaet leicht kleinerer P-Wert ->
#Ist Varianzhomogenitaet gegeben, ist der Test "maechtiger" (oder hat mehr Power).





#R-Frage 39 #################### 
# Joghurt zum zweiten: waehle die Irrtumswahrscheinlichkeit.

#1. alpha=0.05

#2. Berechnung des P-Wertes:
#Wir wollen zeigen, dass die alte Stichprobe x1 einen (signifikant) groesseren arithmetischen Mittelwert
#liefert als die neue Stichprobe x2
#-> "alternative="greater"
x1=c(2.5,3.5,5,3.8)  # alter Test
x2=c(2,2.1,3.5,1.8)  # neuer Test
t.test(x1,x2,alternative="greater")



#3. P<alpha?
# der P=0.04< alpha=0.05, Nullhypothese verworfen,
# die neue Konstruktion ist besser als die alte

#Kommentar:
#Es steht Dir frei, welchen der zwei Datensaetze (hier: neu oder alt) du als x1 definierst.
#Waehlst du die NEUE Stichprobe als x1,
#dann moechtest Du zeigen, dass die neue Stichprobe x1 einen signifkant KLEINEREN arithmetischen Mittelwert besitzt
#als die alte Stichprobe x2.
#-> alternative="less"

x1=c(2,2.1,3.5,1.8)  # neue Stichprobe
x2=c(2.5,3.5,5,3.8)  # alter Stichprobe

t.test(x1,x2,alternative="less")

#liefert dasselbe Ergebnis wie obiges Vorgehen.



##R-Frage 40 ###################  
# Mathematiknoten t-test, 2 Stichprobenfall
#
#Wir folgen dem "Rezept" im Skript
#1,Nullhypothese: Frauen sind gleich gut oder besser als Maenner in Mathe
#Alternativhypothese: Frauen sind schlechter in Mathematik als Maenner, d.h.
#xmean1 ist signifikant kleiner als xmean2

# 2. alpha = 0.05

#3: Berechnung des t-Wertes bei Varianzhomogenitaet:
#Frauen

#Frauen
x1=c(2.3,
     2.5, 2.5,
     3.0,
     3.3,
     rep(3.5,3),
     rep(3.8,6),
     rep(4.0,8),
     rep(4.3,6),
     rep(4.5,3),
     rep(5.0,4),
     5.3,
     5.5,5.5,
     5.8,
     6.0,6.0)

#Maenner
x2=c(2.3,
     2.5, 2.5,
     2.8,
     rep(3.0,5),
     rep(3.3,2),
     rep(3.5,5),
     rep(3.8,1),
     rep(4.0,13),
     rep(4.3,8),
     rep(4.5,16),
     rep(4.8,9),
     rep(5.0,6),
     rep(5.5,4),
     5.8,
     rep(6.0,4))

#Varianzhomogenitaet
t.test(x1,x2,alternative="less",var.equal=TRUE)  #p-value = 0.2505

#Varianzheterogenitaet (weniger Power, Welch-Test)
t.test(x1,x2,alternative="less")
# Der P-Wert ist wieder 0.254 und damit groesser als alpha. Somit beweisen die Daten nicht,
# dass die Frauen schlechter sind als die Maenner



#b) Dies ist wieder der 1-Stichpgrobenfall
#Alternativ direkt aus Datenvektoren
n=length(x1)
xmean=mean(x1)
xsd=sd(x1)
ref=4
t=(xmean-ref)/xsd*sqrt(n)
pt(t,n-1,lower.tail=FALSE) #einseitiges Testen, somit kein Multiplizieren mit 2 noetig
#gleiches Resultat wie
t.test(x1, mu=4.0, alternative="greater")


#R-Frage 41 -----
#Kartoffeln graphisch
kartoffel=read.csv("kartoffel.csv")

#Moegliche Visualisierungen

boxplot(kartoffel) #Boxplot
abline(h = 240, col = "red") #makes a horizonal line at reference value

plot(kartoffel,xlab="Kartoffelgwewicht in g") #Zahlenstrahl
abline(v = 240, col = "red")#makes a vertical line at reference value


#Um ein Histogram zu machen, muessen wir Spalten und Zeilen vertauschen
#das geht mit dem Befehl "t" fuer transpose
hist(t(kartoffel))
abline(v = 240, col = "red")

#b) Fuehre den einseitigen t-Test ausgehend von den Stichprobenwerten aus
t.test(kartoffel, mu=240,alternative="greater") #gleiches Resultat wie Aufgabe 37!


#R-Frage 42 -----
#
#Kartoffel -Frage zweiseitigig
#H1: Der Stichprobenmittelwert der neuen Kartoffelsorte unterscheided sich signifikant vom Gewicht der langjaehrigen Stichprobe, dem Referenzwert
#H1: xmean ungleich mu
#H0: Kein signifikanter Unterschied zwischen Stichprobenmittelwert der neuen Kartoffelsorte unterscheided sich signifikant vom Gewicht der langjaehrigen Stichprobe
#xmean=mu
#Und nun der Test

#In der Aufgabenstellung 37 waren nur Stichprobenumfang n,Standardabweichung s
#und Mittelwert der Stichprobe sowie der Referenzwert gegeben
#
#t-Wert wie in Aufgabe 37

#Beispiel fuer Vorgehen, falls Umfang, Mittelwert und Standardabweichung
#der Stichprobe gegeben sind
n=200      #Umfang der Stichprobe
xmean=255  #Mittelwert der Stichprobe
s=60       #Standardabweichung der Stichprobe
mu=240     #Referenzwert
t=(xmean-mu)/(s/sqrt(n)) #grosser t-Wert t-Wert=3.53>0
pt(t,n-1,lower.tail=FALSE)*2 #zweiseitiges Testen -> der t-Wert verdoppelt sich
#oder #ausgehend von Stichprobenwerten: viel kuerzer!!!!
t.test(kartoffel, mu=240)

#Mathematiknoten zweiseitikg
#H1: Es gibt einen (statistisch signifikanten) Unterschied in den Mathematiknoten der Maenner und Frauen.
#H0: Es gibt keinen (statistisch signifikanten)  Unterschied in den Mathematiknoten der Maenner und Frauen.
t.test(x1,x2,var.equal=TRUE)  #p-value = 0.5=2*0.25-> wieder hat sich der P--Wert verdoppelt


# R-Frage 43 ####################
# Kirschbaeume I

#Nullhypothese: Die jaehrlichen Ertraege schwanken nicht signifikant.

# alpha = 0.05.
# a)gepaarte Stichprobe
x1=c(36,31.5,34,32.5,35,31.5,31,35.5) #Ertraege 2013
x2=c(34,35.5,33.5,36,39,35,33,39.5)   #Ertraege 2014
t.test(x1,x2,alternative="two.sided",paired=TRUE)

t.test(x1,x2,alternative="two.sided")
# Der P-Wert betraegt 0.03 und ist damit kleiner als alpha=0.05, d.h. die Stichproben zeigen,
# dass die jaehrlichen Ertragsschwankungen signifikant sind. (Allerdings ist dies noch kein Beweis,
# dass das Wetter daran schuld ist, statistische Tests liefern niemals Kausalitaeten.)
t.test(x1-x2,mu=0,alternative="two.sided")

# b) ungepaarte Stichprobe
t.test(x1,x2,alternative="two.sided",paired=FALSE)
# Diesmal ist der P-Wert 0.06, doppelt so gross wie beim paired - Fall!
# Hier: P>0.05-> die Nullhypothese kann nicht verworfen werden.

#Betrachten wir somit unterschiedliche Kirschbaeume im Feld ueber die Jahre hinweg,
#muessen wir deutlich hoehere Ertragserhoehungen messen, um eine signifikante Steigerung
# zu finden

# R-Frage  44 ###################  
#Kirschbaeume II
#eine gepaarte Stichprobe kann auf den Einstichprobenfall mit Referenzwert 0 zurueckgefuehrt werden

x1=c(36,31.5,34,32.5,35,31.5,31,35.5)
x2=c(34,35.5,33.5,36,39,35,33,39.5)
d=x1-x2 #Differenzvektor

t.test(d,mu=0,alternative="two.sided")
#Erinnerung: liefert gleiches Resultat
t.test(x1,x2,alternative="two.sided",paired=TRUE)
# Der P-Wert ist identisch mit demjenigen, den du mit dem gepaarte Verfahren fuer
# 2 Stichproben erhalten hast.

# Handskizze Frage 17 Kirschbaeume III ####################
x1=c(36,31.5,34,32.5,35,31.5,31,35.5)
x2=c(34,35.5,33.5,36,39,35,33,39.5)

d=x1-x2 #Differenzvektor
#a)
Mittelwert=mean(d)
#Mittelwert=-2.3
#b): Berechnung des Standardfehlers (Fehleranteil, der unabhaengig vom gewaehlten Signifikanzniveau ist)
#Es handelt sich um eine metrische Groesse:

SE=sd(d)/sqrt(length(d))
#1 SE =0.8
#In Handskizze trage nun Mittelwert plus minus 2 SE auf und interpretiere die  Aussage (siehe Musterloesung als pdf).

#b)Betrachte die Jahrgaenge als 2 unabhaengige Stichproben x1 und x2
# Dann:
Mittelwert_x1=mean(x1)
SE1=sd(x1)/sqrt(length(x1))

#Mittelwert x_1=33.3
#SE1=0.7

Mittelwert_x2=mean(x2)
SE2=sd(x2)/sqrt(length(x2))

#Mittelwert_x2=35.7
#SE2=0.9

#install.packages("Hmisc") #installs R package for errobars
library(Hmisc)
errbar(1, mean(x1),1,1)


rm(list = ls()) #loescht alle Variablen aus dem Workspace
 #passe diese Befehlszeile an: hier muss dasjenige Verzeichnis stehen, in dem sich Deine Excel-Datein befinden

#R-Aufgabe 45 ------
#QQ Plot Gewicht

gewicht=c(65, 62, 81, 88, 73, 75, 71, 79) #dies ist der Datenvektor, der oft x heisst:)
gewichtsorted=sort(gewicht)
#z-Werte dividing the standard normal distribution in n+1 steps
n=length(gewicht)
quantile=seq(1,n)/(n+1) #Aufteilung der Standardnormalverteilung in 8 Quantile
z=qnorm(quantile)#Bestimmung der z-Werte der Quantile

plot(z,gewichtsorted)

#Die Funktion fasst alle diese Schritte zusammen und traegt auf der x-Achse die z-Werte der Standardnormalverteilung
#auf, auf der y-Achse die nach Groesse sortierten Rohdaten


qqnorm(gewicht) #ergibt gleiches Resultat wie plot(z,gewichtsorted)
plot(z,gewichtsorted)
qqline(gewicht,col="blue")#line which passes through the first and third quartiles (all data between 25 and 75 %)


trend=lm(gewichtsorted~z)
abline(trend,col="red")
#und wie steht es nun um die Korrelation als Mass fuer die Guete?
cor(gewichtsorted,z) #0.99
abline(trend,col="red")
legend("topleft",c("Lineare Regression","qqline"), lwd=2, col=c("red","blue"),bty = "n")

#Statistische Tests auf Normalverteilung
shapiro.test(gewicht)#Nullhypothese: gewicht ist normalverteilt
#hier p=0.981 >alpha: Nullhypothese darf nicht abgelehnt werden
#Wir duerfen annehmen, dasss die Daten normalverteilt sind
ks.test(gewicht,pnorm,mean=mean(gewicht),sd=sd(gewicht)) #Nullhypothese: gewicht ist normalverteilt mit mittelwert mean(gewicht)
#und Standardabweichung sd=sd(gewicht)

#All normality tests are sensitive to sample size.
#My personal recommendation is to avoid using them unless
#you have a large number of inspections to do and you need to automate the process.



#Box F-Test ####################################################

x1=c(70,85,31); x2=c(72,61,104)
var.test(x1,x2,alternative="two.sided") #"Varianz= Standardabweichung^2"-Test
#default confidence level gamma=0.95

n1=3; s1=27.88
n2=3; s2=22.34
f=s1^2/s2^2 #Pruefgroesse F: Verhaeltniss der Varianzen
#Berechnung des P-Wertes:

2*pf(f,n1-1,n2-1,lower.tail=FALSE)
#Bemerkung
#in R:
#p"name": Cumulative density distribution of distribution "name" : Flaeche unter der Kurve
#Erinnere Dich an pbinom bei der Binomialverteilung, pnorm der Normalverteilung und pt der t-Verteilung



#Nicht pruefungsrelevant

#Plot der F-Verteilung
# Display the F - distributions with various
# degrees of freedom
#dev.off()
df1 = c(1, 2, 5, 100,300) #degrees of freedom (Freiheitsgrade) = n-1 im 1 - Stichprobenfall
df2 = c(1, 2, 2, 100,100)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df1=1,df2=1", "df1=2,df2=2", "df1=5, df2=2", "df1=df2=100","df1=300,df2=100")
x = seq(0, 4, length=100)
plot(x, df(x,df1[5],df2[5]), type="l", lty=2, xlab="f",
     ylab="Wahrscheinlichkeitsdichtefunktion", main="F - Verteilungen")

#plot t-Verteilung fuer Freiheitsgrade df
for (i in 1:5){
  lines(x, df(x,df1[i],df2[i]), lwd=2, col=colors[i])
  legend("topright", legend=labels,col=colors,lty=1)
}

abline(v=1.56,col="black",lty=2)
#Ende: Nicht pruefungsrelevant


# F-Test      ##################
##R-Frage 46 ################### 

# alpha = 0.05
n1=28; s1=6.4 #s1 muss groesser als s2 sein!
n2=35; s2=3.7
f=(s1/s2)^2  #Verhaeltnis der Varianzen
#Plot der zugehoerigen F-Verteillung (nicht pruefungsrelevant)
x <- seq(0, 4, length=100)
plot(x, df(x,n1-1,n2-1),type="l", lty=1, xlab="f",col='black',
     ylab="Wahrscheinlichkeitsdichtefunktion", main=" F-Verteilung,df1=27 und df2=34")
# Ende Plot der zugehoerigen F-Verteillung (nicht pruefungsrelevant)


#pf(f,n1-1,n2-1, lower.tail=FALSE) berechnet die Flaeche unter der F-Verteilung fuer alle Werte grosser f
P2=2*pf(f,n1-1,n2-1, lower.tail=FALSE)
#zweiseitig, darum muss mit 2 multipliziert werden
# Da P=0.0028<0.05, unterscheiden sich die beiden Stichproben
#in der Streuung signifikant



#R-Aufgabe 47 ####################################################
# Test auf Varianzhomogenitaet Joghurt
#1.
#H0: s1=s2 (Varianzhomogenitaet)
#H1: s2 ungleich s2 (Varianzheteroenitaet)
#2. alpha=0.05
#Berechnung des P-Wertes:


x1=c(2.5,3.5,5,3.8)  # alter Test
x2=c(2,2.1,3.5,1.8)  # neuer Test
var.test (x1,x2) #p=0.66
#4. p>alpha: Die Nullhypothese (s1=s2) wird nicht abgelehnt. Varianzhomogenitaet


#R-Aufgabe 48------
#Mathematik: Streuungen

#Frauen
x1=c(2.3,
     2.5, 2.5,
     3.0,
     3.3,
     rep(3.5,3),
     rep(3.8,6),
     rep(4.0,8),
     rep(4.3,6),
     rep(4.5,3),
     rep(5.0,4),
     5.3,
     5.5,5.5,
     5.8,
     6.0,6.0)

#Maenner
x2=c(2.3,
     2.5, 2.5,
     2.8,
     rep(3.0,5),
     rep(3.3,2),
     rep(3.5,5),
     rep(3.8,1),
     rep(4.0,13),
     rep(4.3,8),
     rep(4.5,16),
     rep(4.8,9),
     rep(5.0,6),
     rep(5.5,4),
     5.8,
     rep(6.0,4))
var.test(x1,x2)
#4. p>alpha: Die Nullhypothese (s1=s2) wird nicht abgelehnt. Varianzhomogenitaet

#
#Box Anteilstest, 1 Stichprobenfall ###################################################
k=40           #Erfolge
misserfolge=70
n=k+misserfolge
pdach=k/n #H1: pdach=0.36 ist signifikant kleiner als pi0=0.45
pi0=0.45
binom.test(k,n,pi0,alternative="less")


# R-Frage 49 ###################
#Pommes-Chips again....

#a)
#1.
#H1: n/k<0.05
#H0:n/k=>0.05
#2. alpha=0.01 (gamma=0.99)
#3. Berechnung von P:
n=100
k=3   #"Erfolg": schlechter Sack
pi0=0.05  #vorgegebener Wert: Maximal 5 % schlechte Saecke
binom.test(k,n,pi0,alternative="less")
#liefert P=0.26
# Damit ist P>alpha,d.h. du kannst nicht sicher sein,
# dass der Vertrag eingehalten wird. Du brauchst eine
# groessere Stichprobe

#b) Der naechste Tag
#Haetten wir 1000 Saecke kontrolliert und 30 "schlechte "gefunden, dann:
n=1000
k=30
pi0=0.05  #vorgegebener Wert: Maximal 5 % schlechte Saecke
binom.test(k,n,pi0,alternative="less")
#liefert P=0.0013<0.01, wir koennten uns somit sicher sein, den Vertrag einhalten zu koennen.



#Box Anteilstest, 2 unabhaengige Stichproben ####################################################
#alpha=0
#Nullhypothese
n1=110; k1=40; #1. Stichprobe
n2=120; k2=54; #2. Stichprobe
k=c(k1,k2)     # Vektor der Erfolge der beiden Stichproben
n=c(n1,n2)     # Datenvektor der Umfaenge der beiden Stichproben
prop.test(k,n,alternative="two.sided")

#p-value=0.23-> Stichproben unterscheiden sich nicht signifikant voneinander

# R-Frage 50 ####################
#ist die Zunahme asiatischer Beizen von 2005 zu 2014 ein wirklicher Trend?
#1. H1: k1/n1<k2/n2 (1 steht fuer 2005, 2 fuer 2014)
#2.alpha=0.025
#3. Berechnung des P-Wertes
#Jahr 2005
n1=670 # Anzahl aller Beizen
k1=27 #Anzahl asiatische Beizen
misserfolg1=n1-k1 #Anzahl nicht asiatischer Beizen
# Jahr 2014:
n2=644 # Anzahl allerBeizen
k2=54 #asiatische Beizen
misserfolg2=n2-k2  #Anzahl nicht asiatischer Beizen
k=c(k1,k2) #Reihenfolge 2005, 20014
n=c(n1,n2)
#Alternativhypothese:weniger asiatische Beizen im Jahr 2005 als 2014
#alternative="less"
prop.test(k,n,alternative="less")

#gleiches Resultat
k=c(k2,k1)
n=c(n2,n1)
#Alternativhypothese:mehr asiatische Beizen im Jahr 2014 als 2005
#alternative="greater"
prop.test(k,n,alternative="greater")
#p-value=0.0008
#die Zunahme ist somit statistisch signifikant.
pi0=0.05


#Test: H1:k1/n1  ist signifikant kleiner als pi0=0.05
#binom.test und prop.test liefern aehnliche Resultate:
prop.test(k1,n1,pi0,alternative="less")
binom.test(k1,n1,pi0,alternative="less")

##################### R-Frage 51 Bier Degustation
#Alternativhypothese:
#H1: Der Erfolgsanteil pi=11/20 ist signifikant groesser als der Vergleichswert pi0-0.5 (dem Wert, dem man durch zufaelliges Tippen erwarten wuerde)
#H0: pi=11/20 ist gleich oder kleiner als der Vergleichswert pi0-0.5
# alpha ist nicht festgelegt. Nehmen wir hier alpha=0.05
n=20  #Stichprobenumfang
k=11  #11 Teilnemer erkennen richtig, ob alkohofrei oder nicht, richtiges Erkennen =Erfolg
pi0=0.5 #



#Parameterfreie Tests #######################################

# Befehle im Kasten zur Erlaeuterung des Wilcoxon-Tests mit ordinalen Daten ##################### 

x1=c(70,85,31,36)
x2=c(71,86,35,33)
#ungepaart:
wilcox.test(x1,x2,alternative="two.sided")
#P-Wert=0.89

#gepaart
wilcox.test(x1,x2,alternative="two.sided", paired=TRUE)
#P-Wert=0.58


#default Wert fuer R des Referenzwertes mu ist 0. Somit liefert
wilcox.test(x1-x2,alternative="two.sided")
#gleiches Ergebnis
#P-Wert=0.58
#alternative="two.sided" ist auch default. Noch kuerzer
wilcox.test(x1-x2)

#1. Stichprobenfall mit Referenzwert 50
wilcox.test(x1,mu=50,alternative="two.sided")
#P-value=0.625
#gepaarter Test kann wieder auf 1 Stichprobenfall zurueckgefuehrt werden mit Referenzwert mu=0
wilcox.test(x1-x2,mu=0,alternative="two.sided")



#Wilcoxon-Test fuer nicht zahlenmaessige Daten, ordinale Daten ####################################################################################################################################################################
#Befehle im Kasten zur Erlaeuterung des Wilcoxon-Tests mit nicht-ordinalen Daten ################ 

# Wilcoxon-U-Test ############### 
x1=c("nass","nass","trocken","nass","feucht")
x2=c("trocken","feucht","trocken","nass","trocken")


#Ersetze (z.B.mit der find und replace  Funktion in R)
#"trocken"->1
#"feucht"->2
#"nass"3->3
x11=c(3,3,1,3,2)
x22=c(1,2,1,3,1)



wilcox.test(x11,x22,alternative="two.sided")
#Wilcoxon rank sum test with continuity correction
#W = 18.5, p-value = 0.2188

#Wilcoxon gepaart: Wilcoxon Vorzeichen Rangsummen Test

wilcox.test(x11,x22,alternative="two.sided",paired=TRUE)
#Output:
#V=6; p-value = 0.1736




#1 Sticprobenfall




#gepaart als Sonderfall von 1 Stichprobenfall
wilcox.test(x11-x22,mu=0,alternative="two.sided")
#oder super kurz
wilcox.test(x11-x22) #Erklaerung: mu=0 ist "default", "two.sided" ebenso


## R-Frage 52  ################### 
# 2 unabhaengige Stichproben
#H1
#H0:
#alpha=0.05
A=c(22.6,16,21.2,21.4,19.4,18.7,23.1,14.3)
B=c(17,19.2,19.1,19.6,16.8,18.4)
#a) Graphische Darstellung

boxplot(A,B,xlab ="Sorte A           Sorte B")
proteingehalt=c(A,B) #Fuhre die Resultate in einen Datenvektor zusammen
sorten=c(rep("Sorte A",length(A)),rep("Sorte B",length(B)))
#Visualize also the data points and overlay them
#meine bevorzugte Darstellung fuer kleine Datensaetze:
#Kombination von boxplot mit stripchart
stripchart(proteingehalt~sorten,vertical=TRUE,col=c("darkred","darkgreen"),ylab="",xlab="",add=TRUE)



mean(A)
mean(B)
median(A)
median(B)
#A hat hoeheren Proteingehalt. Doch ist das auch signifikant?
#Eher nicht, den der Boxplot zeigte grossen Ueberlapp der Verteilungen...
#
#

#Bohnen 
# 2 unabhaengige Stichproben
#b)
#Vergleich der Mediane
#H1: median(A) ist signifikant groesser als median(B)
wilcox.test(A,B,alternative="greater")
# P_wilcox ist somit 0.1725

#Vergleich der Mittelwerte mit Welch-t-Test (unserem Standard t-Test)
#H1: mean(A) (=Mittelwert von A) ist signifikant groesser als mean(B)
summary=t.test(A,B,alternative="greater")
#P_t=0.1662
#Der t.test liefert leicht kleineren P-Wert und ist somit aussagekraeftiger (mehr Power).
#Dies liegt daran, dass der Wilcoxon -Test die Information ueber den tatsaechlichen Abstand
#der einzelnen Messungen "wegwirft" und nur noch mit den Raengen arbeitet.

#Auf dem 95%Signifikanzniveau ist kein Unterschied zwischen den beiden Bohnensorten
#festzustellen

#Doch der t-Test erfordert  - im Gegensatzt zum Wilcoxon-Test  - Normalverteilung in beiden Stichproben.
#Bei kleinen Stichproben ist der Test nicht robust auf Verletzungen dieser Annahme
#und wir muessen ueberpruefen:
#c)
#Aber durften wir den t.test ueberhaupt machen?
#Testen auf Normalverteilung in beiden Stichproben
#Nullhypothese H0: Die Daten sind normalverteilt
shapiro.test(A) #p-value 0.4728>alpha NH kann nicht verworfen werden (erinnere Dich: die NH kann nie "bewiesen werden). Doch wir duerfen Normalverteilung voraussetzen.
ks.test(A,pnorm, mean(A),sd(A))
qqnorm(A);qqline(A)

shapiro.test(B)
ks.test(B,pnorm, mean(B),sd(B))
qqnorm(B);qqline(B)

#Ja, Normalverteilung gegeben.

#Doch welchter t-Test ist erlaubt:
#Varianzhomogenitaet
#H0:Varianhomogenitaet
var.test(A,B) #Varianzhomogenitaet nicht gegeben, da p=0.04<0.05
#-> nur Welch-Test (Varianzheterogenitaet) erfuellt alle Voraussetzungen

## R-Frage 53 ###################
# a) Der Median war letztes Jahr "zufrieden", dieses Jahr
#    "sehr zufrieden". Es scheint also eine Verbesserung
#    vorzuliegen. Der Wilcoxon - Test laesst uns nun entscheiden,
#    ob diese Verbesserung statistisch signifikant ist



# b) alpha=0.05, Wilcoxon fuer 2 unabhaengige Stichproben, nicht zahlenmaessige Daten als Ausgangspunkt
#Ersetze Worte durch Zahlen
#unzufrieden->1
#zufrieden->2
#sehr zufrieden->3

#letztes Jahr
lJ1=rep(3,65) #wiederholt ("replicates") die Zahl 3 65 mal,
#da 65 Kunden letztes Jahr zufrieden waren.
lJ2=rep(2,85)
lJ3=rep(1,4)
lJ=c(lJ1,lJ2,lJ3) # Datenvektor letztes Jahr (lJ)

#dieses Jahr
dJ1=rep(3,85)
dJ2=rep(2,65)
dJ3=rep(1,4)
dJ=c(dJ1,dJ2,dJ3) #Datenvektor diese Jahr (dJ)
#Hypothese: H1: dieses Jahr bessere Bewertungen
#H1: Der Median der Bewertungen dieses Jahr ist signifikant groesser
#als der Mecian der Bewertungen im Vorjahr

wilcox.test(dJ,lJ,alternative="greater")
#p-value=0.01433

# R-Frage 54 ####################
#Kuehlschrank Absatz
# alpha=0.05, Wilcoxon-Test im 1-Stichprobenfall
#Median: Wilcoxon-Test fuer Zahlenwerte
#H1: median(x)>10; H0: median(x) kleiner gleich 10
x=c(8,18,9,12,10,14,16,7,14,11,10,20)
wilcox.test(x,mu=10,alternative="greater")
# P=0.04127<alpha, d.h. die Verbesserung ist signifikant



# R-Frage 55 #################### 
#Cola
# alpha=0.1, 2 Stichproben gekoppelt
x1=c(20,24,28,24,20,29,19,27,20,30,18,28,26,24)
x2=c(16,26,18,17,20,21,23,22,23,20,18,21,17,26)
#Hier handelt es sich schon um ordinale (wenn auch nicht metrische Daten)
#Wir koennen den Wilcoxon - Test direkt durchfuehren.
wilcox.test(x1,x2,alternative="two.sided",paired=TRUE)
# P=0.028<alpha, d.h. es liegt ein signifikanter
# Unterschied vor.

#Alternative Berechnung: 1 Stichprobenfall mit mu=0
wilcox.test(x1-x2,mu=0,alternative="two.sided")
#P=0.028<alpha

rm(list = ls()) #loescht alle Variablen aus dem Workspace



# Chi2-Verteilung ####################################################
# Fakultativ
# Plot der Chi2-Verteilung
# Display the Chi2- distributions with various
# degrees of freedom
#dev.off()


df1 = c( 1, 2, 3, 4, 10) #degrees of freedom (Freiheitsgrade) = n-1 im 1 - Stichprobenfall

colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df1=1", "df1=2", "df1=3", "df1=4","df1=10")
x = seq(0, 20, length=100)
plot(x, dchisq(x,df1[1]), type="l", lty=2, xlab="chi^2",
     ylab="Wahrscheinlichkeitsdichtefunktion", main="Chi^2 - Verteilungen")

#plot t-Verteilung fuer Freiheitsgrade df
for (i in 1:5){
  lines(x, dchisq(x,df1[i]), lwd=2, col=colors[i])
  legend("top", legend=labels,col=colors,lty=1,bty = "n")
}

abline(v=18.5143,col="black",lty=2)

#Ende FAKULTATIV
#



#Box Chi2 am Schirmbeispiel ####################################################
mitschirm=c(30,15)
ohneschirm=c(20,55)
kontingenztafel=rbind(mitschirm,ohneschirm)

#Chi2 test
df=1
chisq.test(kontingenztafel, correct=FALSE) #correct = FALSE unterdrueckt die Yates-Korrektur
#X-squared = 18.514, df = 1, p-value = 1.686e-05
#Vergleich mit prop.test
#Definiere Erfog k: Schirm
#n: Summe aus mitschirm und ohneschirm
prop.test(mitschirm,ohneschirm+mitschirm, correct=FALSE) #prop.test und chisq.test liefern fuer 2x2 Kontingenztafel das exakt gleiche Resultat

#X-squared = 18.514, df = 1, p-value = 1.686e-05
#Unterschied ist signifikant!
#
#Doch eigentlich sollte man fuer
#df=1 die Yates-Korrektur (Standard in R)
#anwenden)
#
#mit Yates-Korrektur
chisq.test(kontingenztafel)
#X-squared = 16.905, df = 1, p-value = 3.929e-05

# FAKULTATIV####################################################
#Fakultativ Die Handrechnung des Skripts in R
Bunteni=rowSums(kontingenztafel) #Zeilensummen
Bobenj=colSums(kontingenztafel)  #Spaltensummen
#erwarteter Absolutwert ist das "Matrixprodukt" aus dem Zeilensummenvektor mit dem Spaltensumemnvektor (multipliziere jedes Element mit jedem Element und schreibe das ganze in einen array) normiert auf die Gesamtzahl der Beobachtungen
#
#Matrixprodukt in R %*%
Eij=(Bunteni%*%t(Bobenj))/sum(Bunteni)
df=(length(Bunteni)-1)*(length(Bobenj)-1) #Anzahl der Freiheitsgrade

bij=kontingenztafel
chi2_beoachtet=sum(sum((bij-Eij)^2/Eij))  #Summe ueber i und j-> sum of sum
pchisq(chi2_beoachtet,df,lower.tail=FALSE) #1.686356e-05

#fisher.test(kontingenztafel) #kann man auch fuer kleine Stichproben machen
#Ende fakultativ #############################

#Aufgaben:
#

# R-Frage 56------

#Augenfarben vs Haarfarben
#Nullhypothese: Die Augenfarbe ist fuer alle Haarfarben gleichverteilt
#H1: Es gibt einen signifikanten Unterschied
#in der Verteilung der Augenfarben in den verschieden Haarfarben
blau=c(190,40,10)
braun=c(50,220,150)
gruen=c(40,120,50)
kontingenztafel=cbind(blau,braun,gruen)
#in den Zeilen stehen jetzt die Haarfarben blond, braun und schwarz
#mit R
chisq.test(kontingenztafel)  #Hier macht R KEINE Yates-Korrektur, da df=(3-1)*(3-1)=4>1 ist
#in folgendem fakultativen Abschnitt zeigen wir, dass wir auf den gleichen
#X-squred Wert und p-Wert auch ueber die X-squared Verteilung gemaess Gleichungen 7.1 -7.4 kommen
#Versuch es nachzuvollziehen....

#X-squared =348.61,51,df=4,p-value < 2.2e-16




#Fakultativ:
#Vergleich mit Handrechung
#Berechnung der Erwartungswerte Eij
Bunteni=rowSums(kontingenztafel) #Zeilensummen
Bobenj=as.vector(colSums(kontingenztafel))#Spaltensummen
#Gleichung 7.2
#Erwartungswerte=Elementweises multiplizieren von  Zeilensumme*t(Spaltensumme)/n
Eij=(Bunteni %*%t(Bobenj))/(sum(Bunteni)) #%*% ist das "Matrizenprodukt", t ist die transponierte Spaltensumme

#Berechnung von chi2_beobachtet: Gleichung 7.1
chi2_beoachtet=sum(((kontingenztafel-Eij)^2)/Eij)
#chi2_beoabachtet=348.61 ist exakt der Wert, den auch der xi2 -Wert ergibt
#df=(3-1)*(3-1)=4 #Gleichung 7.3


#Berechnung des P--Wertes
#P-Value ist Flaeche unter der chi2 -Verteilung fuer alle Werte > chi2_beoachtet
pchisq(chi2_beoachtet,4,lower.tail=FALSE) #p-value:3.4*10^-74, R gibt bei seinen
#Ausgaben der statistischen Tests so kleine Werte gar nicht an und schreibt stattdessen p-value < 2.2e-16
# die Verteilungen unterscheiden sich signifikant
#Ende fakultativ




#R-Frage 57 #################################################


#
MP=c(50,40,10)
GE=c(40,30,90)
FFP=c(30,50,20)
matrix=rbind(MP,GE,FFP)
Alter=c("F<65","M<65","Menschen>65")
Parteien=c("MP","GE","FFP")
colors <- c("green","orange","brown")
barplot(matrix,main="Wahlverhalten nach Parteien und Altersgruppen",
        names.arg = Alter,xlab="Alter",col=colors)
legend("bottomleft", Parteien, cex = 1.3,fill = colors)

#Die Aelteren scheinen ja wirklich die Grauen Elefanten vorzuziehen.
#Die Verteilungen zwischen den Altersgruppen sehen ziemlich unaehnlich aus...

#1. H0: Die Bevoelkerungsgruppen haben nicht siginifikant verschieden (=gleich) gewaehlt
#   H1: Die Bevoelkerungsgruppen haben singifikant verschieden gewaehlt
# 2.alpha 0.01



#
#Vereinige die Spaltenvektoren in 3x3 Matrix
Stimmen=rbind(MP,GE,FFP)#Spalten: Altersgrupen
#Fuehre den chi^2 -Test aus
chisq.test(Stimmen) #teste die Zeilen der Matrix (Bevoelkerungsgruppen) gegeneinander
#X-squared = 78.75, df = 4, p-value = 3.205e-16
# Der p-Wert ist mit 3.2e-16 weit unter alpha. Es gibt also
# einen Zusammenhang zwischen Bevoelkerungsgruppen und Stimmverhalten.

#Beim Chi^2-Test ist es egal, ob die Ergebnisse zeilenweise (mit rbind) oder spaltenweise anordnets.
#Probier es aus
stimmen2=cbind(MP,GE,FFP)
chisq.test(stimmen2)
#X-squared = 78.75, df = 4, p-value = 3.205e-16
#

#Fakultativ:
#Berechnung von Xb
#erwartete Absolutwerte bei Gleichverteilung
#Wir wollen die Altersgruppen vergleichen -> die stehen in Stimmen in den Spalten
#Vergleich Schirmbeispiel: Maenner und Frauen stehen spaltenweise nebeneinander
#


#Zeilensumme
Bunteni=rowSums(Stimmen)
#Spaltensumme
Bobenj=colSums((Stimmen))
Eij=(Bunteni %*%t(Bobenj))/sum(Bunteni)

xbsquared=sum(sum(((Stimmen)-Eij)^2/Eij)) #gleiches xbsqured wie chi^2 test
#Berechnung des P-Wertes
pchisq(xbsquared,4,lower.tail=FALSE)  #p=3.204552e-16

# Ende Fakultativ:



#b)
#nun moechten wir den Unterschied genauer lokalisieren und testen die Altersgruppen gegeneinander...
#drei Moeglichkeiten:
#1. Fu65<->Mu65;
#2. Fu65<->Menschen_ueber65,
#3. Mu65<->Menschen_ueber65
#Aber auch das Zusammenfassen von Klassen:
#z. B.
# 4.Frauen und Maenner(=Menschen) unter 65 <-> Mu65
#Kreiere nun die passenden Spaltenvektoren

#1.  Frauen unter 65<->Maenner unter 65
MP=c(50,40)
GE=c(40,30)
FFP=c(30,50)
Stimmen=rbind(MP,GE,FFP)
#Fuehre den chi^2 -Test aus
chisq.test(Stimmen)
#pvlaue=0.02>alpha ->nicht signifikant mit alpha=0.01->Unterschied zwischen Maennern und Frauen unter 65
#ist auf dem 99%-Signifikanzniveau nicht siginifikant. ->Fasse Maenner und Frauen in eine Klasse zusammen
# siehe Vergleich 4

#2. Frauen unter 65<->Menschen_ueber65,
#Definiere Spaltenvektoren
MP=c(50,10)
GE=c(40,90)
FFP=c(30,20)
Stimmen=rbind(MP,GE,FFP)
#Fuehre den chi^2 -Test aus
chisq.test(Stimmen)
#p-value=4.0*10^-11 ->signifikant

#3. Maenner unter 65<->Menschen_ueber65,
#Definiere Spaltenvektoren
MP=c(40,10)
GE=c(30,90)
FFP=c(50,20)
Stimmen=rbind(MP,GE,FFP)
#Fuehre den chi^2 -Test aus
chisq.test(Stimmen)
#p-value=6.1*10^-14 ->signifikant

#4. Fasse die unter 65 jaehrigen Maenner und Frauen in 1 Klasse zusammen: Menschen_unter65
MP=c(90,10)
GE=c(70,90)
FFP=c(80,20)
#Vereinige die Spaltenvektoren in 2x3 Matrix
Stimmen=rbind(MP,GE,FFP)
#Fuehre den chi^2 -Test aus
chisq.test(Stimmen)
#p-value=5.4*10^-16

#Beim Wahlverhalten spielt das Alter viel groessere Rolle als Geschlecht !!!


# R-Frage 58  ###################
#Auberginen
#H0: Kein Einfluss der Bewaesserungsmethode auf Ertrag
#-> Nulllhypothese
#Anteil von ueberdurchschnittlich grossen Auberginen gleich
# in beiden Gruppen
# alpha 0.05
neu=c(54,80-54)
kontrolle=c(34,60-34)
Auberginen=rbind(neu,kontrolle)
#Machen wir uns ein Bild...
colors <- c("green","orange","brown")
Bewaesserung=c("Neu","normal")
Groesse=c("grosser Ertrag","normaler Ertrag")
barplot(Auberginen,main="Auberginen",
        names.arg = Bewaesserung,xlab="Bewaesserung",col=colors)
legend("bottomleft", Groesse, cex = 1.3,fill = colors)



#1. Spalte: Auberginen-Pflanzen mit hoeherem Ertrag
#2. Spalte: Augeberginen mit normalem Ertrag
chisq.test(Auberginen) #R hat (da df=1 wieder die Yates-Korrektur vorgenommen)
#p=0.26
# Da p>alpha, ist kein signifikanter Unterschied nachweisbar, Nullhypothese gilt.

#R-Frage 58 b)
#Anteilstest, 2 unabhaengige Stichproben
#Erfolgsspaltenvektor
k=c(54,34) #Erfolgsspaltenvektor
n=c(80,60)       #Gesamutumfang der Stichprobe mit neuer Bewaesserungsmethode und der Kontrollgruppe

#Wir wollen testen, ob die neue Bewaesserungsmethode zu hoeherem Ertrag fuehrt -> alternative greater
prop.test(k,n,alternative="greater")
#p-value=0.128
#kleinerer p-Wert, da Test mit mehr Power, doch immer noch nicht signifkant


#Box chi^2 -Test, 1 Stichprobenfall ###################################################################################
alpha=0.05
m=c(422,280,575)
erwarteteVerteilung=c(1,1,1)
#H0: Die Augenfarben der maennlichen Studis sind gleichverteilt
chisq.test(m,p=erwarteteVerteilung,rescale.p=TRUE)
#Alternative Formulierung
chisq.test(m,p=c(1/3,1/3,1/3)) #ohne rescale.p=TRUE muessen sich alle Wahrscheinlichkeiten auf 1 addieren
#p-value<2.2*10^-16
#Augenfarben sind nicht gleichverteilt


#Bemerkung: Natuerlich kannst Du nicht nur gegen Gleichverteilung pruefen, sondern gegen
#jede beliebige von Dir geteste Verteilung
#
#
#
# R-Frage 59:  ####################
# Kuehlschrank
# alpha 0.05
B=c(15,15,20)     # B als Abk. fuer B-eobachtete Werte
E=c(45,30,25)     # E als Abk. fuer E-rwartete Werte
chisq.test(B,p=E,rescale.p=TRUE)  #if rescale.p=TRUE:p is rescaled (if necessary) to sum to 1
# Der P-Wert ist 0.0302 und damit kleiner als alpha.
# Es hat sich also eine Veraenderung ergeben. (Dass es
# an der Werbekampagne liegt, laesst sich jedoch nicht beweisen.)

#Hier handelt es sich bei der erwarteten Verteilung um Prozentangaben
#die Eintraege von p summieren sich zu 1
E2=c(0.45,0.30,0.25)
chisq.test(B,p=E2)# gleiches Resultat


#R-Frage 60 ------
#Volkszaehlung in Kalifornien
B=c(679,51,77,190,3) #Beobachtet
#Berechnung der Erwartungswerte: Wir finden genau soviele Menschen in der Volkszaehlung, wie wir proportional erwarten wuerden.

E=c(50.7,8.6,30.6,10.8,1.3) #Erwartet
chisq.test(B,p=E,rescale.p=TRUE)
#p-value<2.2*10^-16->Unterschiede zum Erwartungswert sind signifikant
E2=E/100 #Verkaufzaheln in Prozent
#ohne rescale.p=TRUE muessen sich die Wahrscheinlihckeiten p=E2 auf 1 addieren
chisq.test(B,p=E2,rescale.p=TRUE) #selbes Resultat



# R-Frage 61 ###################  
#Wuerfel gezinkt?
# alpha 0.05 (selbst gewaehlt)
B=c(23,23,21,26,21,6)     # B als Abk. fuer B-eobachtete Haeufigkeiten
# Bei einem fairen Wuerfel sollte jede Zahl gleich haeufig auftreten
# (Gleichverteilung)
E=c(1,1,1,1,1,1)          # E als Abk. fuer E-rwartete Werte
chisq.test(B,p=E,rescale.p=TRUE) #chi squared test for given probabilites

#alternative Formulierung bei Gleichverteilung
#die Wahrscheinlichkeit fuer jede Augenzahl ist 1/6
E3=c(rep(1/6,6))
chisq.test(B,p=E3) #wieder das gleiche


#R-Frage 62----
# Mendel
B=c(161,145,294)
E=c(1,1,2)
chisq.test(B,p=E,rescale=TRUE)
#natuerlich das Gleiche wie
E2=E/sum(E) #dies sind nun Wahrscheinlichkeiten, die sich auf 1 addieren
chisq.test(B,p=E2)

rm(list = ls()) #loescht alle Variablen aus dem Workspace
#passe diese Befehlszeile an: hier muss dasjenige Verzeichnis stehen, in dem sich Deine Excel-Datein befinden

#Box Lebenswertung Weltregionen------
#1:
#Die Mittelwerte der Lebenserwartungen in den Welregionen unterscheiden sich nicht signifikant voneinander unterscheiden.
#H1: Mindestens ein Paar von Mittelwerten der Lebenserwartungen in den Regionen unterscheidet sich signifikant.


#2. alpha=0.01
#lese die Daten  ein
#Wichtig:
# - Falls Du selbst ein Excel-Sheet erstellen musst, achte darauf, dass unabhaengige und abhaengige Variable(n) in verschiedenen Zeilen stehen
# - jede Zeile: eine Wiederholung oder Messung

#Alternativ: konstruiere den data.frame direkt in R
#Haeufigkeiten der Laender in Weltregionen
eur=rep("Europa",6)
paz=rep("Pazifik/Asien",5)
afr=rep("Afrika",6)
mo=rep("Mittlerer Osten",7)
la=rep("Latein-Amerika",7)
Region=c(eur,paz,afr,mo,la) #Welt-Regionen

#Lebenserwartung in ausgewaehlten Laendern der Weltregionen
leur=c(73,74,75,72,69,65)
lpaz=c(66,75,76,73,63)
lafr=c(55,51,55,54,62,41)
lmo=c(68,65,67,62,65,69,65)
lla=c(68,59,71,69,74,67,57)
Lebenserwartung=c(leur,lpaz,lafr,lmo,lla)

#Nun der eigentliche data.frame
leberw=data.frame(Region,Lebenserwartung)



#Alternaiv: Einlesen aus csv
#Lies den File eine
leberw=read.csv2("leberw.csv")
str(leberw)



#View data frame
#View(leberw)

#1. Spalte; unabhaengige(nominale) Weltregionsnahmen mit 5 Faktor=Stufen
#2. Spalte: abhaengige Daten: Lebenserwartung


#Inspizieren wir die Daten mit einem Box-Plot
boxplot(leberw$Lebenserwartung~leberw$Region, col = "lightgray",
        xlab="Weltregion", ylab="Lebenserwartung [Jahre]")


#Alternative Darstellung
stripchart(leberw$Lebenserwartung~leberw$Region,method="jitter",vert=TRUE, add=TRUE)

#add=TRUE: Ueberlagerung von stripchart auf Boxplot


#Teste die Voraussetzungen

#Test auf Normalverteilung der Residuen: Shapiro-Test

#Nullhypothese: Normalverteilung der einzelnen Stichproben
#a)
shapiro.test(rstandard(aov(leberw$Lebenserwartung~leberw$Region)))
#Bemerkung: Ist eine Stichprobe normalverteilt, so sind die Abweichungen jedes einzelnen Messwerts um den Mittelwert der einzelnen Stichprobe ebenso normalverteilt. Diese Abweichung nennt man im Zusammenhang der ANOVA das Residuum.
#rstandard ueberprueft, ob die Residueen der ANOVA normalverteilt sind

#b)Homoskedazitaet
#Nullhypothese: Varianzhomogenitaet zwischen Faktorstufen
bartlett.test(leberw$Lebenserwartung~leberw$Region)
#Shapiro-Wilk normality test
#W = 0.9515, p-value = 0.1713
#p>0.05: Homoskedazitaet gegegeben

RegionvsLebenserwartung=aov(leberw$Lebenserwartung~leberw$Region)
summary(RegionvsLebenserwartung)
#P=1.36e-05
#post-hoc-Analyse Lebenserwartung
pairwise.t.test(Lebenserwartung,Region,p.adjust.method="holm")


#FAKULTATIV
#Mit Hilfe des Anhangs kannst Du den Output von R verstehen
#
#
#Erlaeuterung zur summary(RegionvsLebenserwartung)
#               Df Sum Sq Mean Sq F value   Pr(>F)
#leberw$Region  4 1277.7   319.4   11.78 1.36e-05 ***
#  Residuals     26  705.1    27.1

#Residuals: Werte, die zur inneren Streuung, d.h. der Varianz innerhalb der Stichproben gehoeren 
#Df= degrees of freedom
#Df der Varianz zwischen den Faktorstufen (unabhaengige Variable mit k Faktorstufen): df= Anzahl Faktorstufen-1:  df1=k-1 =5-1 =4
#Df der Varianz innerhalb der Faktorstufen:  Anzahl der Wiederholungen - Anzahl der Faktorstufen
#            df2=N-k =31-5=26

#Sum Sq = Sum of squares =SQZ
#       = Summe der Abweichungsquadrate zwischen den Faktor-Stufen
#       = SQZ= 12277.7

#SQT= Summe der Abweichungsquadrate total
#SQT=Sum Sq leberw$Region +Sum Sq Residuals
#   = 705.1+1277.7=1982.8
#   =SQI+ SQZ
#Sum Sq Residuals
#Abweichungsquadrate jeden einzelnen Messwerts vom Gruppenmittelwert
#   =SQI=SQT-SQZ=705.1


#Mean Sq = Mean Square
#Mean Sq leberw$Region: mittlere Quadratsumme der Abweichung der Faktorstufe:  MQZ
#     MQZ =  Mass f\"urVariabilitaet zwischen den Faktorstufen
#      SQZ normiert auf Freiheitsgrade der unabhaengigen Variablen
#     MQZ=SQZ/(k-1)=319.4

#Mean Sq of Residuals: mittlere Quadratsumme der Streuung der einzelnen Messerwerte um die Faktorstufenmittelwerte:  MQI
# MQI =Varianz innerhalb der Stichproben
#  MQI=SQI/(N-k)=27.1

#F value:=Quotient aus der Varianz der Faktorstufen und der Varianz innerhalb einer Stichprobe
#F=F=MQZ/MQI=319.4/11.78

#Berechnung des P-Wertes mittels des F-Tests
pf(11.78,4,26,lower.tail = FALSE) #STIMMT mit summary(aov...) ueberein
#ENDE FAKULTATIV


#R-Frage 63 ------
# Kaese
Sorte=c(rep("Gruyere",4),rep("Tilsister",5),rep("Edamer",5),
        rep("Appenzeller",3))
Naehrwert=c(1780,1690,1670,1710,1120,1180,1110,1070,1020,
            1080,990,1050,1100,1110,1630,1720,1610)
Kaese=data.frame(Sorte,Naehrwert)



# A data frame is more general than a matrix, in that different columns can have different modes (numeric, character, factor, etc.).
#View(Kaese) #1. Spalte: unabhaengige (hier: nominale) Daten: Sorte
#2. Spalte: abhaengige Daten: hier Naehrwert
str(Kaese) #str compactly displays the structure (str)
boxplot(Kaese$Naehrwert~Kaese$Sorte, col = "lightgray",
        xlab="Kaesesorte", ylab="Naehrwert[kJ/100g]")

stripchart(Kaese$Naehrwert~Kaese$Sorte,vert=TRUE, add=TRUE)

#Ueberpruefung der Voraussetzungen:
#Normalverteilung der Daten

#Nullhypothese: Daten sind normalverteilt
shapiro.test(rstandard(aov(Naehrwert~Sorte)))
#ohne attach -Befehl haetten wir schreiben muessen
shapiro.test(rstandard(aov(Kaese$Naehrwert~Kaese$Sorte)))

#Homoskedastizitaet
#Nullhpothese: Homoskedastizitaet
bartlett.test(Kaese$Naehrwert~Kaese$Sorte)
#p-value=0.97->Homoskedastizitaet erfuellt

sortevsnaehrwert=aov(Kaese$Naehrwert~Kaese$Sorte)
summary(sortevsnaehrwert)
#
#R-Frage 65-----
# Kaese post-hoc 
pairwise.t.test(Kaese$Naehrwert,Kaese$Sorte,p.adjust.method="holm")


# R-Frage 64 & 66 ----
# Nutzplanzen - Ertrag
#1. H0: Es liegt kein Unterschied im Ertrag zwischen den Sorten vor; alle Stichproben entstammen der selben Grundgesamtheit
# 2. alpha 0.05
#Sortea=c("A","A","A","B","B","B","B","C","C","C") #umstaendlich
Sorte=c(rep("A",3),rep("B",4),rep("C",3))
Ertrag=c(2.4,2.8,2.3,1.5,1.9,1.7,1.7,1.5,2.2,1.8)
Nutzpflanze=data.frame(Sorte,Ertrag)

attach(Nutzpflanze) #erspart Schreibarbeit
#View(Nutzpflanze)
str(Nutzpflanze)
boxplot(Ertrag~Sorte,xlab="Sorte", ylab="Ertrag")
stripchart(Ertrag~Sorte,vert=TRUE, add=TRUE)

#Alle Tests
#testname(abhangige_Spalte~unabhaengige_Spalte)

#Ueberpruefe die Voraussetzungen:
#Daten sind metrisch
# Pruefe Normalverteilung (kleine Stichprobe!!!)
#Nullhypothese: Normalverteilung
shapiro.test(rstandard(aov(Ertrag~Sorte)))
#p-value=0.7>0.05: Nullhypothese kann nicht abgelehnt werdenNormalverteilung gegeben
#Homoskedastizitaet
#Nullhypothese: Varianzhomogenitaet der Faktorstufen (=Homoskedastizitaet)
bartlett.test(Ertrag~Sorte)
#p-value=0.5528-> Homoskedastizitaet kann nicht abgelehnt werden

#3. 
summary(aov(Ertrag~Sorte))
#4. 
# Mit P=0.0119<0.05 ist nachgewiesen, dass signifikante Unterschiede zwischen 
# Unterschiede in den Mittelwerten zwischen mindestens einer Paarung vorliegen.
#Doch wo? Post-hoc-Test.

# 

pairwise.t.test(Ertrag,Sorte,p.adjust.method="holm")
# A unterscheidet sich signifikant von den anderen beiden.
# B und C unterscheiden sich nicht signifikant.

rm(list = ls()) #loescht alle Variablen aus dem Workspace

#Alle Aufgabennummern +2 in letzter Version des Skripts

#R-Frage 67 -------
# Kohl: Datensatz einfaktoriell
# Irrtumswahrscheinlichkeit festlegen: alpha=0.05
# (Die Spalte mit dem Feld kann man  weglassen, sie zeigtnu nur die Wiederholungen innerhalb einer Faktorstufe an)
# als csv speichern

# in RStudio einlesen --> neuer Datensatz Kohl in R

#a)
Kohl=read.csv("kohleinfaktoriell.csv",sep=",")
#View(Kohl) #IMMER ueberpruefen, ob DATA frame stimmt
str(Kohl) #Abstand vom Typ "int" -> R macht statt ANOVA lineare Regression!
#Inspiziere die Daten
#Hinweis: Wir betrachten hier das Feld nicht als Faktor, sondern nur als Mass fuer die Wiederholungen in der Faktorstufe "Abstand"

#Verwandle Abstand in den Datenyp Faktor
#Abstand ->Datenyp "int", muss mit "factor" in ordinale Daten verwandelt werden
Kohl$Abstand=as.factor(Kohl$Abstand)
str(Kohl) #Test, dass unabhaengige Faktoren beide vom Typ "factor" sind

# Daten visualisieren, z.B. mit
par(mfrow=c(1,1))
boxplot(Kohl$Kopfgewicht~Kohl$Abstand,col=rainbow(7),xlab="Abstand[cm]",
        ylab="Gewicht[g]")
#oder
stripchart(Kohl$Kopfgewicht~Kohl$Abstand,method="jitter",jitter=0.05,pch=15,col="darkgray", vert=TRUE,add=TRUE)
#optional: Zeiche auch noch die Mittelwerte der Gewichte ein!
means<- tapply(Kohl$Kopfgewicht, Kohl$Abstand, mean) #berechnet den Mittelwert jeder Faktorstufe
#optional
points(means, col="blue", pch=20)
#Testen der Voraussetzungen
#a. Daten: abhaengige Variable echt metrisch: Ja
#b. unabhaengige Variable muss in R vom Typ factor sein (ansonsten macht R eine lineare Regression)

#abhaengige Variable Gewicht
#alpha=0.05
# Test auf Normalverteilung der Faktorstufen
#Nullhypothese: Normalverteilung der Daten jeder Faktorstufe
shapiro.test(rstandard(aov(Kohl$Kopfgewicht~Kohl$Abstand))) #H0: Normalverteilung in allen Stichproben darf nicht abgelehnt werden,
# Test auf Homoskedazitaet der Faktorstufen
bartlett.test(Kohl$Kopfgewicht~Kohl$Abstand) #H0: Homosked. darf nicht abgelehnt werden
summary(aov(Kohl$Kopfgewicht~Kohl$Abstand))  #
#Wir haben einen signifikanten Unterschied-> post-hoc-Analyse
pairwise.t.test(Kohl$Kopfgewicht,Kohl$Abstand,p.adjust.method = "holm")
# Pairwise comparisons using t tests with pooled SD 
# 
# data:  Kohl$Kopfgewicht and Kohl$Abstand 
# 
# 30    38    46   
# 38 0.538 -     -    
#   46 0.143 0.534 -    
#   54 0.079 0.330 0.625



#doch wir finden bei keinem er 6 Vergleiche einen signifikanten Unterschied (alle Werte >0.05). Dies liegt daran, 
#dass das Holm-Verfahren die p-Werte umso mehr erhoeht, je mehr multiple t-Test gemacht werden. 
#Haetten wir alpha=0.1 gesetzt (und zwar vor dem Testen), dann waere zumindest der Unterschied zwischen den Abstaenden 
#30-54 signifikant. Hier 


#b)
#jetzt: was passiert, wenn ich die Umwandlung zum Faktor des abhaengigen Varialbe nicht ausfuehre?

Kohllm=read.csv("kohleinfaktoriell.csv",sep=",")
linearmodel=(lm(Kohllm$Kopfgewicht~Kohllm$Abstand)) #lineare Regression  p-value:
summary(linearmodel)  #p-value: 0.003507
plot(Kohllm$Kopfgewicht~Kohllm$Abstand,xlab="Abstand[cm]",
     ylab="Gewicht[g]")
abline(linearmodel,col="red")
summary(aov(Kohllm$Kopfgewicht~Kohllm$Abstand)) #   P=0.00351: gleicher p-Wert wie bei linearer Regression:
#Mit Daten vom Typ "integer" in abhaengiger Variable macht R eine lineare Regression beim Befehl aov!
#was auffaellt: es gibt nur noch Df=1 in der unabhaengigen Variable



# oneway test ################
#oneway.test im Lebenserwartungsbeispiel
# R-Frage 68--------
leberw=read.csv2("leberw.csv")
oneway.test(leberw$Lebenserwartung~leberw$Region) #p-value = 0.002837


leberw=read.csv2("leberw.csv")
attach(leberw)#erspart mit die $-Schreibweise
#ANOVA

summary(aov(Lebenserwartung~Region))
#p-value ANOVA p_aov 1.36*10^-5
# one way test
onewaystandarad=oneway.test(Lebenserwartung~Region,data=leberw)
#diese Darstellung kannst Du immer benutzten, so Du keinen attach-Befehl ausgefuehrt hast, gleiches Resultat wie:
oneway=oneway.test(Lebenserwartung~Region)
#p-value=0.002937

#p-value=0.002937 des oneway Test ist deutlich groesser
#als p-value mit aov p_aov 1.36*10^-5
#genauer
0.002937/1.36e-5 #ca. 216 mal groesser mit identischem Datensatz! Viel weniger Teststaerke.

#oneway test stellt weniger Voraussetzungen
#(keine Varianzhomogenitaet der Faktorstufen gefordert),
#hat aber eine geringere Teststaerke (Power)

#Kruskall-Wallis-Test reduziert die Daten auf ihre korrigierten (=mittleren) Raenge und vergleicht Mediane
kruskal=kruskal.test(Lebenserwartung~Region)

#Kruskal: p-value=0.001793
#P--Wert in gleicher Groessenordnung  wie oneway.test
0.001793/1.36e-05 #  "nur" ca. 132 groesser als ANOVA P-Wert und sogar kleiner als
#der P-Wert des oneway-Tests
0.001793/0.002937 #0.61 mal kleiner als oneway.test


#Das alles hier hat jedoch keinerlei Einfluss auf unsere Schlussinterpretation, da alle P-Werte sehr klein gegen alpha=0.05 sind.

#Nebenbemerkung:
#Aehnliches ist uns uebrigens schon beim Vergleich
#der P--Werte von
#t-test und Wilcoxon begegnet: Der Wilcoxon setzt wie der Kruskal--Wallis--Test
#keine Normalverteilung der Stichprobe voraus
#(=weniger Voraussetzungen), liefert
#aber mit den selben (metrischen) Daten einen hoeheren
#P-Wert.

#Darum: Benutze wenn moeglich immer den Test, der die hoechste statistische Power besitzt.


# R-Frage 69------------
#Parameterfreier post-hoc-Test

#Post-hoc-test muss auch parameterfrei sein, falls vorher parameterfreie Analyse durchgefuehrt wurde
#Abhaengige Variable ist metrisch-> metrische Daten sind immer auch ordinal
wilcox_kruskal=pairwise.wilcox.test(Lebenserwartung,Region,p.adjust.method="holm")
#Warnung: bei Rangbindungen koennen keine exakten Raenge berechnet werden -> ignorieren

# data:  Lebenserwartung and Region

# Afrika Asien Europa Latein-Amerika
# Asien             0.064  -     -      -
#   Europa          0.045  1.000 -      -
#   Latein-Amerika  0.064  1.000 0.663  -
#   Mittlerer Osten 0.040  0.763 0.147  1.000

#post hoc Test der ANOVA oder des oneway test
ttest_anova=pairwise.t.test(Lebenserwartung,Region,p.adjust.method="holm")
# Pairwise comparisons using t tests with pooled SD
#
# data:  Lebenserwartung and Region
#
# Afrika  Asien  Europa Latein-Amerika
# Asien             6.6e-05 -      -      -
#   Europa          1.9e-05 1.0000 -      -
#   Latein-Amerika  0.0007  0.5491 0.5121 -
#   Mittlerer Osten 0.0010  0.5278 0.4196 1.0000
#
# P value adjustment method: holm
#Bis zu drei Groessenordnungen kleinere p-values beim pairwise.t.test
#t.test ist maechtiger als wilcoxon.test!
#
#
#
# R-Frage 70-------
# Insektizide:

#Quelle: http://ww2.coastal.edu/kingw/statistics/R-tutorials/oneway.html
#Du fuehrst ein Experiment aus, in dem 6 verschiedene Insektizide auf vielen verschiedenen Versuchsfeldern getested werden
#Die abhaengige Variable war die Anzahl der gezaehlten Insekten pro Feld, nach dem das Insektizid verstreut wurden.
#Die Daten koennen einfach mit data(InectSprays) eingelesen werden.
#a) Inspiziere die Daten mit den Befehlen str(InsectSprays), View(InsectSprays)
#b)Plotte die Daten mit boxplot -> was faellt Dir auf?
#c) Fuehre nun den geeigneten Test fuer  Homoskedazitaet durch.
#d) Was erscheint Dir nun ein geeigneter Test dafuer, dass sich dier Anzahl der gezaehlten Insekten als Funktion des
#verwandten Insektizides signifikant  unterscheiden?

#Loesung
#a)
data(InsectSprays)  #Einlesen der Daten
str(InsectSprays)

#View(InsectSprays)  #Daten in geeigneter Form: 1. Spalte  abhaengige Variable "count",
#2. Spalte unabhaengige Variable "spray"

attach(InsectSprays)
#b)
boxplot(count ~ spray,col=rainbow(6),xlab="Insektizide ",
        ylab="Anzahl Insekten",main="Anzahl der Insekten pro Insektizid (Mittelwert ist blauer Punkt)")
#nicht gefragt, doch nuetzlich: Mittelwert Einzeichnen
means<- tapply(count, spray, mean) #berechnet den Mittelwert jeder Faktorstufe
points(means, col="blue", pch=20)
#Varianzen sehr unaehnlich, Varianzhomogenitaet wahrscheinlich nicht gegeben
# p-value mit bartlett.test

#c) Nullhypothese H0: Varianzhomogenitaet
shapiro.test(rstandard(aov(count~spray)))#p-value = 0.02226
#Normalverteilung in den einzelnen Stichproben nicht gegeben
#Wir duerfen kein parametrisches Verfahren anwenden
#paramterfreie Alternative zur aov -> Kruskall Wallis, vergleicht aber Mediane statt Mittelwerte
kruskal.test(count~spray) #p-value=1.51*10^-10

#parameterfreier post-hoc-Test
pairwise.wilcox.test(count,spray,p.adjust.method="holm")
#Und wo sind nun keine signifikante Unterschiede?
#zwischen A und B, B und F, C und E

#Auch der Output gibt sehr viel weniger Informationen als aov:


par(mfrow=c(1,1))          # set graphics window to plot side-by-side


#Kruskal-Wallis-Test bei ordinalen Daten:

# R-Frage 71-------

#Lebensmittel-Frische in Laeden
# Irrtumswahrscheinlichkeit festlegen: alpha=0.05
#Wandle xlsx in Excel in csv--Datei um
lebensmittel=read.csv2("lebensmittelzahlen.csv")
attach(lebensmittel)
#Inspiziere die Daten
#View(lebensmittel)
str(lebensmittel)
boxplot(Frische~Laden,main="Frische in drei verschiedenen Laeden")
stripchart(Frische~Laden,method="jitter",jitter=0.05,pch=15,col="darkgray", vert=TRUE,add=TRUE)

kruskal.test(Frische~Laden)
#kruskal.test(Frische~Laden,data=lebensmittel) #gleiches Resultat, Alternative ohne attach-befehl
#Kruskal-Wallis  p-value = 0.007814
pairwise.wilcox.test(Frische,Laden,p.adjust.method="holm")
#A       B
#B 0.732 -
#C 0.024 0.024

#Geschaeft C unterscheidet sich signifikant von Geschaeft A und B

rm(list = ls()) #loescht alle Variablen aus dem Workspace


#R-Frage 72------
#Kohl zum 2.: Zweifaktorielle Varianzzanalyse: Kopfgewich abhaengig von Faktor Abstand und
#FaktorBewaesserung
#Plots, Wechselwirkungsplots interaction.plot

df=read.csv2("kohlzweifaktoriell.csv") #df ist der Name des data.frame

#Inspiziere die Daten auf dem Bildbschirm
#View(df)
head(df) #head returns the first or last parts of a vector,matrix, table, data frame or function
str(df) #Compactly display the internal structure of an R object
#Abstand ist in R vom Datentyp "integer" doch die Faktoren der aov muessen vom Typ "factor" sein
#Somit muessen wir den as.factor - Befehl
#df$Abstand=as.factor(df$Abstand) anwenden bevor wir die ANOVA
#ausfuehren koennen

#Plots der Daten:
attach(df) #moegliche Fehlerquelle, Vorsicht, wir schreiben hier
#Plots two plots along side (1 row, two columns in graphical output)
par(mfrow=c(1,2)) #par can be used to set or query graphical parameters
plot(Kopfgewicht~Abstand)
#"Abstand" taucht auch als Spaltenname in einem anderem Data frame auf, Fehlerquelle!
plot(Kopfgewicht~Bewaesserung)
#interaction.plot(x-Achse:unabhaengige Variable 1, y-Achse: unabhaengige Variable 2,abhaengige Variable)

#optional:
#drehe die Reihenfolge um, zuerst selten, dann haeufig
df$Bewaesserung=factor(df$Bewaesserung,levels=c("selten","haeufig"))
par(mfrow=c(1,2)) #zwei Plots nebenenander
interaction.plot(df$Bewaesserung,df$Abstand,df$Kopfgewicht)
interaction.plot(df$Abstand,df$Bewaesserung,df$Kopfgewicht)
par(mfrow=c(1,1)) #zurueck zu einem Plot
# In beiden Plots laufen die Linien parallel-> wahrscheinlich keine Wechselwirkung


# R-Frage 73------
#Kohl zum 3.:
###################### ANOVA mehrfaktorielle MIT Wechselwirkung
#alpha=0.05
#Ueberpruefen der Voraussetzungen erfolgt in spaeteren Aufgaben

#Vorbereitung:
#unabhaengige Variable MUSS vom Typ factor sein, ueberpruefe mit str
df$Abstand=as.factor(df$Abstand)
str(df$Abstand)


#2 faktorielle VA MIT Wechselwirkung (WW) * zwischen den beiden Faktoren
sum_kohl_WW=summary(aov(df$Kopfgewicht~df$Abstand*df$Bewaesserung))
#                             Df Sum Sq Mean Sq F value   Pr(>F)
# df$Abstand                  3 254286   84762   10.53 0.000457 ***
# df$Bewaesserung             1 184977  184977   22.97 0.000199 ***
# df$Abstand:df$Bewaesserung  3   7729    2576    0.32 0.810863
# Residuals                  16 128853    8053
1

#p-value: Wechselwirkung Abstand:Bewaesserung: 0.81
#die Wechselwirkung spielt keine Rolle
#sah man auch am interaction plot - die Linine laufen eher parallel

#ausfuehrlichere Schreibweise: 2 faktorielle VA MIT Wechselwirkung
sum_kohl_long1=summary(aov(df$Kopfgewicht~df$Abstand+df$Bewaesserung+df$Abstand:Bewaesserung))
# +: anova mit zwei Faktoren, : beruecksichtige Wechselwirkung zwischen den Faktoren

# R-Frage 74------
# ------Kohl zum 2.: Mehrfaktoriell ANOVA OHNE Wechselwirkung
#alpha=0.05
#OHNE Wechselwirkung
zweifaktoriell_ohne_WW=summary(aov(df$Kopfgewicht~df$Abstand+df$Bewaesserung))
#p-value: Abstand-Kopfgewicht: p=0.000137: kleinere p-values als mit WW: 0.000457
#p-value: Bewaesserung-Kopfgewicht: p= 6.76e-05 : mit WW: 0.000199

#p-values der VA ohne Wechselwirkung (WW)sind viel kleiner: nur 1/3 der p-values mit WW
#mehr statistische Power!
#Wechselwirkung war unbedeutend-> Daten werden besser ohne WW beschrieben-> hoeher p-Wert


# R-Frage 75------
#Kohl zum 2.: Post-hoc--Test, mehrfaktorielle ANOVA

pairwise.t.test(df$Kopfgewicht,df$Abstand,p.adjust.method="holm")
#Signifikante Unterschiede im Mittelwert der Kopfgewichte bei Abstand 30 und 54
#p-value=0.0088<0.05
#kleinster und groesster Abstand zeigen signifikanten Unterschied im Kopfgewicht

pairwise.t.test(df$Kopfgewicht,df$Bewaesserung,p.adjust.method="holm")
#p-=value=0.0039<0.05: signifikant

#Voraussetzungen Mehrfaktorielle ANOVA

#R-Frage 76 ------
#Voraussetzungen Mehrfaktorielle ANOVA
#mit Wechselwirkung (eigentliche Fragestellung)
shapiro.test(rstandard(aov(df$Kopfgewicht~df$Abstand*df$Bewaesserung)))#0.11

#2 Faktoren ohne Wechselwirkung:
shapiro.test(rstandard(aov(df$Kopfgewicht~df$Bewaesserung+df$Abstand))) #0.068

#R-Frage 77 -------
#Voraussetzungen Mehrfaktorielle ANOVA
#Bartlett-test mehrfaktorielle ANOVA
#anders als im Skript - in jetziger R Version kann man das * nicht mehr verwenden, sondern muss folgendermassen testen:
bartlett.test(df$Kopfgewicht~interaction(df$Abstand,df$Bewaesserung))

#Vergleich Bartlett Test in einzelnen Faktorstufen
bartlett.test(df$Kopfgewicht~df$Abstand)
bartlett.test(df$Kopfgewicht~df$Bewaesserung)
# R-Frage 78---------
#Zweifaktorielle ANOVA ohne Wiederholung
# Irrtumswahrscheinlichkeit festlegen: alpha=0.05
# nematoden.csv einlesen
#nematoden=read.csv("nematoden.csv",sep=",",dec=".",header=TRUE) #dies sind Varianten, die Du ausprobieren kannst
nedf=read.csv2("nematoden.csv") #nedf ist Name des df
#View(nedf)
attach(nedf)
# Daten visualisieren, z.B. mit
par(mfrow=c(1,2)) #zwei Plots nebeneinander
interaction.plot(nedf$Boden,nedf$Methode,nedf$Nematoden) #1. Eintrag: x-Achse,unabhaegige Variable 1,  2. Eintrag: unabhaengige Variable 2, 3. Eintrag: y-Achse abhaengige Varialble
interaction.plot(nedf$Methode,nedf$Boden,nedf$Nematoden)
par(mfrow=c(1,2))
plot(Methode, Nematoden)
plot(Boden, Nematoden)
par(mfrow=c(1,1))

# fuer aov muessen die Faktoren (d.h. die unabhaengigen Variablen)
#vom Typ "factor" sein
fBoden=as.factor(Boden)     #f fuer "factor"
fMethode=as.factor(Methode)


# mehrfaktorielle ANOVA ohne Wechselwirkung mit nur einer Wiederholung
# Information fuer Wechselwirkung der Faktorstuefen fehlt
sum_nema_aov=summary(aov(Nematoden~fBoden+fMethode))
# der p-Wert von 0.0003374 fuer die Abhaengigkeit vom Boden bedeutet,
# dass die Art des Bodens signifikante Unterschiede  aufdie Anzahl der Nematoden bewirkt
# der p-Wert von 0.3249570 fuer die Abhaengigkeit von der Methode bedeutet,
# dass die keine signifikanten Unterschiede zwischen den verschiedenen
# Methoden nachweisbar sind (d.h. alle Methoden sind ungefaehr gleich gut)

# Lokalisierung der Unterschiede (post-hoc-Test):
pairwise.t.test(Nematoden,Boden,p.adjust.method="holm")
# es gibt signifikante Unterschiede zwischen:
#   Boden 1 und Boden 4 (p=0.00193)
#   Boden 2 und Boden 4 (p=0.00977)
#   Boden 5 und Boden 4 (p=0.00014)
# Es ist also einfach Boden 4, der aus der Reihe tanzt.
#
# In Bezug auf die Methode darf man keine post-hoc-Tests machen, weil die
# Varianz-Analyse hier keine signifikanten Unterschiede nachgewiesen hat.

## R-Frage 79------
# Irrtumswahrscheinlichkeit festlegen: alpha=0.05
sum_nema_kruskal_b=kruskal.test(Nematoden~fBoden) #Nematoden: integer, fBoden: Factor with 5 levels
#p-value=0.004
#liefert selbes Ergebnis wie
sum_nema_kruskal_b2=kruskal.test(Nematoden~Boden) #Kruskal-Wallis wandelt automatisch
#input in "factor" mit "levels" um und macht dann Rang-Varianz-Analyse (Vergleich der Mediane)
#Im Gegensatz zur ANOVA muss die Kategorie Boden somit nicht vorher in
#einen factor umgewandelt werden

sum_nema_kruskal_m=kruskal.test(Nematoden~fMethode)
#p-value=0.76
#liefert somit ebenso identisches Resultat wie
sum_nema_kruskal_m2=kruskal.test(Nematoden~Methode)
#p-value=0.76








