set.seed(12)               #powtarzalnośc wyników
n<-1000                    #liczba obiektów
#rozkład normalny (średnia=50, odchylenie standardowe =10)
dane<- rnorm(n,mean = 50, sd=10)
dane
średnia <- mean(dane)
odchstan <- sd(dane)
dolna_granica <- średnia -3 * odchstan
górna_granica <- średnia +3 * odchstan
#jednostki skrajne, stawiamy warunek
outliers <- dane[dane < dolna_granica | dane > górna_granica]
#Liczba jednostek skrajnych
liczba_outliers <- length(outliers)

cat("Liczba jednostek skrajnych: ", liczba_outliers, "\n")

hist(dane, breaks = 30, col = "lightyellow", main = "Histogram z wartościami skrajnymi",
     xlab = "wartości", border = "black", ylim = c(0,100),
     xlim = c(10,90))
axis(1, at = seq(10, 90, by = 10))

#Dodanie linii dla 3 odchyleń standardowych
abline(v= c(dolna_granica, górna_granica), col = "blue", lwd = 2, lty = 2)

#Dodanie punktów skrajnych
points(outliers, rep(0, length(outliers)), col = "red", pch = 16)

#points dodawanie punktów do wykresu w okreslonej pozycji
#rep(0, sum(outliers)) - wektor dłyugości równej liczbie punktów nietypowtychh, 0 współrzędna Y dla tych punktów; 
#rep() tworzy wektor, który powtarza liczbę 0 tyle razy, ile jest outliers w wektorze dane
# WARTO ZROBIĆ DO RAPORTU

#Tworzenie wykresu boxplot
boxplot(dane, main = "Boxplot z wartościami skrajnymi", col = "lightblue",
        pch = 19, boxwex = 0.5)
#dodanie outliers
points(rep(1,length(outliers)), outliers, col = "red", pch = 19, cex = 1.2)

#dodanie poziomych przerywanych linii na wykresie pokazujących +-3 odchylenia standardowe
#dolna granica (3 odchylenia poniżej średniej)
abline(h = dolna_granica, col="blue", lty = 2, lwd = 1)
#górna granicas (3 odchylenia powyżej średniej)
abline(h = górna_granica, col="blue", lty = 2, lwd = 1)


#karta kontrolna ze zdjęcia 26.03


######################wykres typu "karta kontrolna"######################################

plot(dane,type = "o", col = "blue", pch = 19,
     ylim = c(min(dane) - 1, max(dane) + 1), 
     main = "Wykrywanie wartości skrajnych",
     ylab = "Wartości", xlab = "Indeks")
abline(h = średnia, col = "black", lty = 2)
abline (h = dolna_granica, col = "red", lty = 2)
abline(h = górna_granica, col = "red", lty = 2)

#Zaznaczenie punktów przekraczających granice kontrolne
outliers <- dane < dolna_granica | dane > górna_granica
points(which(outliers), dane[outliers], col = "red", pch = 19)



############################################################################
#parametr lambda (im mniejszy, tym silniejsza asymetria)
lambda <- 1 #parametr rozkładu wykładniczegi

#generowanie danych z rozkładu wykładniczego
set.seed(123)
dane <- rexp(1000, rate = lambda) #rozkład wykładniczy
#rate - współczynnik rozkładu wykładniczego (domyślnie jest ustawoiny na 1)
# ale możesz ustawić własne lambda
dane
#wykres histogramu
hist(dane, main = "Histogram", xlab = "Wartości",
     col = "lightgreen", border = "black", breaks = 50, ylim = c(0, 200),
     xlim = c(0,8))

#rexp(n,rate=1) - liczby losowe z rozkładu wykładniczego
##rozstęp kwartylowy - RK
#Wartości uznawane za SKRAJNE leżą poza zakresem między:
#Q1-1,5xRK a Q3+1,5xRK

Q1 <- quantile(dane, 0.25, na.rm = TRUE)   #ignoruje wartości NA jeśli występują
Q3 <- quantile(dane, 0.75, na.rm = TRUE)
Me <- median(dane)
Q1
Q3
Me

#rysowanie pionowych linii przy Q1, Me i Q3
abline(v=Q1, col = "blue", lwd = 2, lty = 2) #linis przerywana dla Q1
abline(v=Me, col = "purple", lwd = 2, lty = 2)  #linia przerywana dla Me
abline(v=Q3, col = "blue", lwd = 2, lty = 2)
#lwd - grubość linii, lty - styl linii - 2 = przerywana

#dodanie wtykirt Q1, Me, Q3
text(Q1, 180, labels = "Q1", col = "blue", pos = 4, cex = 1.2)
text(Me, 180, labels = "Me", col = "purple", pos = 4, cex = 1.2)
text(Q3, 180, labels = "Q3", col = "blue", pos = 4, cex = 1.2)

#Q1 - współrzędna x, 180 - współrzędna Y
#col - kolor tekstu, pos- określa pozycję tekstu względem współ. (X,Y)
#cex - rozmiar czcionki

#pos = 1 - tekst poniżej (X,Y)
#pos = 2 - tekst na prawo od (X,Y)
#pos = 3 - tekst na lewo od (X,Y)
#pos = 4 - tekst powyżej (X,Y)


ROZSTĘP <- as.numeric(Q3-Q1)
ROZSTĘP

dolna_granica <- Q1 - 3 * ROZSTĘP
górna_granica <- Q3 + 3 * ROZSTĘP

outliers <- dane < dolna_granica | dane > górna_granica
#operacjas | (lub) łączy dwa warunki logiczne
dane[outliers]
#jeśli wartośc znajduje się poza dolną lub górną granicą - jest outlierem

#dodanie punktów dla wartości nietypowych (w kolorze czerwonym)
points(dane[outliers], rep(0, sum(outliers)), col = "red", pch = 16)


#liczenie liczby wartości nietypowych
liczba_outliers <- sum(outliers)
#obliczanie procentu danych nietypowych
procent_outliers <- (liczba_outliers / length(dane)) * 100

#wyniki
liczba_outliers
procent_outliers

######################################################################
#boxplot dla danych
boxplot(dane, main = "Boxplot z jednostkami nietypowymi",
        col = "lightblue", border = "black", horizontal = TRUE)

#dodanie linii dla dolnej i górnej granicy (outliers)
abline(v=dolna_granica, col = "red", lwd = 2, lty = 2)   # dolna granica
abline(v=górna_granica, col = "red", lwd = 2, lty = 2)   # górna granica

#dodanie etykiet do linii
text(dolna_granica, 1.2, labels = "Dolna granica", col = "red", pos = 4, cex = 1.2)
text(górna_granica, 1.2, labels = "Górna granica", col = "red", pos = 4, cex = 1.2)

#zaznaczenie wartości odastających na czerwono
points(dane[outliers], rep(1, sum(outliers)), col = "red", pch = 16)A


###################### METODA Z-SCORE   #################################


#parametry rozkładu normalnego
mean <- 70
sd <- 20
n <- 1000
#generowanie danych z rozkładu normalnego
set.seed(123)
dane_normalne <- rnorm(n, mean, sd)

#dodanie lekkiej asymetrii
dane_asymetryczne <-dane_normalne + rexp(n, rate = 0.1)


#standaryzacja danych (Z-score)
z_scores <-scale(dane_asymetryczne)

# 1. Histogram dla danych pierwotnych (asymetrycznych)
par(mfrow = c(1,2)) #układ dwóch wykresów obok siebie

hist(dane_asymetryczne, breaks = 30, col = "pink",
     main = "Rozkład asymetryczny", xlab = "wartości",
     ylim = c(0, 100), xlim = c(0, 160), xaxt = "n")
axis (1, at = seq(0, 160, by = 40))   #skoki na osi X co 40

outliers <- abs(z_scores) >3
#abs(z_scores) >3 - funkcja abs()zwraca wartośc bezwzględną Z-score
# sprawdza, które wwartości mają Z-score większy niż 3 (lub mniejszy niż -3)
#dla wartości spełniających ten warunek wynik to TRUE (outlier)

# 2. Histogram dla danych stamdaryzowanych (Z-scores)
hist(z_scores, breaks = 30, col = "lightblue",
     main = "Rozkład standaryzowany (Z-scores)", xlab = "Z-score",
     ylim = c(0, 100), xlim = c(-4, 4), xaxt = "n")
axis(1, at = seq(-4, 4, by =1))  #skoki na osi x co 1

#dodanie pionowych linii przerywanych na poziimach 3 i -3
abline(v=3, col = 'black', lty = 2)  #linia pionowa na poziomie 3
abline (v=-3, col = 'black', lty = 2)  #linia pionowa na poziomie -3


#zaznaczabnie wartości skrajnych na histogramie danych standaryzowanych
outliers_z_scores <- z_scores[outliers]
outliers_z_scores
points(outliers_z_scores, rep(0, length(outliers_z_scores)), col = 'red', pch = 16, cex = 1.5)

par(mfrow = c(1, 1))   #powrót do jednego wykresu


#liczba outliers
liczba_outliers <- sum(outliers)
liczba_outliers
#procent outliers
procent_outliers <- (liczba_outliers/n) * 100
procent_outliers


#Test Grubbsa (wykrywanie pojedynczych ekstremalnych wartości)
# metoda sprawdza, czy najbardziej ekstremalna wartość jest outlierem

install.packages("outliers")
library(outliers)

#parametry rozkładu normalnego
mean <- 0    #średnia
sd <- 1      #odchylenie standardowe
n <-1000      #liczba obiektów

#generowanie danych z rozkładu normalnego
set.seed(123)
dane_normalne <- rnorm(n, mean = mean, sd = sd)
dane_normalne
####dokończyć ze zdjęcia

######################TEST GRUBBSA####################
#metoda sprawdza, czy najbardziej eksrtemalmna wartośc jest outlierem

install.packages("outliers")
library(outliers)

#parametry rozkładu normalnego
mean <- 0
sd <- 1
n <- 1000

#generowanie danych z rozkładu normalnego
set.seed(123)
dane_normalne <- rnorm(n, mean = mean, sd = sd)
dane_normalne

#wprowadzenie asymetrii - transformacja log-normalna
dane_asymetryczne <- exp(dane_normalne)
hist(dane_asymetryczne, main = "Histogram", xlab = "wartości",
     col="lightgreen", border = "black", breaks = 10, ylim = c(0,800),
     xlim = c(0,30))

#standaryzacja danych (przekształcenie na średnią 0 i odchylenie standardowe 1)
dane_standaryzowane <- scale(dane_asymetryczne)

dane_standaryzowane

#zastosowanie testu grubbsa do wykrywania wartości nietypowych w danych standaryzowanych
test_grubbsa <- grubbs.test(dane_standaryzowane)

#wyświetlanie wu=yników testu Grubbsa
print(test_grubbsa)

#interpretacje: jesli p<0.05 - wykryto outlier
#p-value<0,05<-odrzucamy H0, w zbiorze istnieje co najmniej jedna wartość skrajna

####################

#lokalna miara LOF(Local Outlier Factor)
#ocenia, czy punkt jest znacząco bardziej odizolowany od innych
install.packages("DMwR2")
install.packages("Rlof")
library(DMwR2)
library(Rlof)

set.seed(130)
#parametry
mean <- 70
sd <- 3
n <- 1000

#generowanie danych z rozkładu normalnego
dane_normalne <- rnorm(n, mean, sd)

#dodanie asymetrii
dane_asymetryczne <- dane_normalne + rexp(n, rate = 0.01)

#rysowanie histogrammu
hist(dane_asymetryczne,
     col = "aquamarine",
     main = "Metoda LOF, k=20",
     xlab = "Wartości",
     ylab = "Częstotliwość",
     breaks = 30,
     ylim = c(0,150),   #wyłączenie automatycznej osi X
     xlim = c(50,800), xaxt = "n")
axis(1, at = seq(50, 800, by = 50))   #skoki na osi X co 50
abline(h = 0, col = "black", lwd = 1)   #lwd grubośc linii


#obliczanie wskaźnika LOF
lof_result <- lofactor(dane_asymetryczne, k = 20)

#określenie outliers na podstawie progu LOF
próg <- 2  #prosty próg dla LOF (możesz dostosować)
outliers <- which(lof_result > próg)
dane_asymetryczne[outliers]

#dodanie punktów outlier
points(dane_asymetryczne[outliers], rep(0, length(outliers)),
       col = "red", pch = 19, cex = 1.2)

plot(dane_asymetryczne, main = "Detekcja outliers metodą LOF", xlab = "Obiekt",
     ylab = "Wartośc", col = ifelse(1:n %in% outliers, "red", "black"), pch = 19)   
#%in% - jest to operator, który sprawdza, czy elementgy po lewej stronie
#(czyli elementy z wektors 1:n) są zawarte w wektorze po prawej stronie (czyli outliers)


##############INNE WIZUALIZACJE#######################

plot(lof_result, 
     type = "h",      # h - pionowe linie dla obiektów
     col = ifelse (lof_result > 2, "red", "grey"),
     main = "Wartości LOF dla poszczególnych obiektów",
     xlab = "Obiekt",
     ylab = "Wartośc LOF")
abline(h = 2, col = "red", lty = 2)  #linia odcięcia












