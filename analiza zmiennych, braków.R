install.packages("psychTools")
library(psychTools)
 
#lista zbiorów danych w pakiecie
data(package = "psychTools")
 
#zbiór danych bfi:
bfi
dim(bfi)    #wymiar zbioru bfi
#str, zapply pozwolą przyjrzeć się zmiennym:
str(bfi)    #wymiar, typ każdej zmiennej (int, factor, chr, (characer), num)
sapply(bfi, class)
View(bfi)
summary(bfi)
 
#Sprawdzanie ##
#sprawdzamy jakie wartości są w gender i education
table(bfi$gender, useNA = "ifany")   #NA są pokazane, jesli istnieją
table(bfi$gender, useNA = "always")   #pokaż braki zawsze
table(bfi$education, useNA = "ifany")
 
tablica_z_suma <- addmargins(table(bfi$gender, useNA = "ifany"))
tablica_z_suma
 
 
#ustaw cechy jako factor
 
bfi$gender <- as.factor(bfi$gender)
bfi$education <- as.factor(bfi$education)
str(bfi)
 
bfi$gender <- factor(bfi$gender, levels = c(1, 2), labels = c("Mężczyzna", "Kobieta"))
bfi$education <- factor(bfi$education, levels = c(1, 2, 3, 4, 5), labels = c("Podstawowe", "Średnie", "Średniezaw", "Licencjat", "Magister"))
str(bfi)
 
 
#############
bfi
summary(bfi)
View(bfi)
edit(bfi)
################
 
#########Braki danych########
install.packages("naniar")
library(naniar)
library(ggplot2)
library(dplyr)
library(tidyr)
 
##########Wizualizacja braków danych####
 
vis_miss(bfi)
vis_miss(bfi) + ggtitle("") + xlab("zmienne") + ylab("obserwacje")
vis_miss(bfi, cluster = TRUE)       #grupowanie zmiennych z podobnymi brakami
vis_miss(bfi, sort_miss = TRUE)     #sortowanie zmiennych według liczby braków
vis_miss(bfi, sort_miss = TRUE) +ggtitle("") + xlab("zmienne") + ylab("obserwacje")
 
gg_miss_var(bfi, show_pct = FALSE) #liczba braków w danej zmiennej
gg_miss_var(bfi, show_pct = TRUE)  #procent braków w zmiennej
 
gg_miss_var(bfi, show_pct = TRUE) + 
  ggtitle("braki danych w zmiennych zbioru bfi") + 
  xlab("zmienne") +
  ylab("Procent braków") +
  theme_classic()     #theme_minimal()
 
 
###############
install.packages("VIM")
library(VIM)
aggr(bfi)  #wykres braków danych
 
aggr(bfi,
     prop = FALSE,    #pokazuje liczbę braków, a nie proporcję
     numbers = FALSE,   #nie pokazuje proporcji wykrtytych kombinacji
     main = "Brakujące dane w zbiorze bfi",  #Tytuł wykresu
     ylab = c("Liczba braków w zmiennej", "wiersze"),   #etykiety na osi Y
     xlab = "zmienne"   #etykieta osi X
)
 
#kolumny 1:5
bfi_numeric <- bfi[,1:5]  #pierwszych 5 zmiennych A1-A5
 
aggr(bfi_numeric,
     prop = TRUE,
     numbers = TRUE,     #pokazuje proporcje wykrytych kombinacji
     col=c("yellow", "blue"),    
     main = "Brakujące dane w zbiorze bfi",
     ylab = c("Proporcja braków w zmiennej", "wiersze")
)
 
 
######Obliczamy minimum, maksimum oraz liczbę braków dla każdej zmiennej

bfi
str(bfi)
parametry <- apply(bfi, 2, function(x) c(
  min(x, na.rm = TRUE),    #minimum
  max(x, na.m = TRUE),
  sum(is.na(x))
))
parametry
 
#Dodanie nazw do wierszy (MIN, MAX, NA)
rownames(parametry) <-c("Min", "Max", "Braki danych")
parametry
 
#Sumowanie braków po wszystkich kolumnach, 3 - trzeci wiersz
liczba_brakow <- sum(as.numeric(parametry[3, ]), na.rm = TRUE)
liczba_brakow
 
#Obliczenie liczby brakujących danych w całym zbiorze bfi
liczba_brakow_c <- sum(is.na(bfi)) 
liczba_brakow_c
 
#Obliczanie liczby komórek w zbiorze danych bfi
liczba_komórek <- nrow(bfi) * ncol(bfi)
liczba_komórek
 
procent_brakow <- (liczba_brakow/liczba_komórek)*100
procent_brakow
 
##########Pełne wiersze i obiekty#########
#obliczanie liczby pełnych wierszy - bez braków danych
pełne_wiersze <- sum(complete.cases(bfi))
#wyświetlenie wyniku 
pełne_wiersze
 
bfi_bez_brakow <- bfi[complete.cases(bfi), ]  #usuwanie wierszy z brakującymi danymi
bfi_bez_brakow
dim(bfi_bez_brakow)
 
#obliczenie liczby pełnych wierszy po usunięciu braków danych
pełne_wiersze_po_usunięciu <- nrow(bfi_bez_brakow)
pełne_wiersze_po_usunięciu
 
vis_miss(bfi_bez_brakow)   #wizualizacja braków w danych
aggr(bfi_bez_brakow)   #wizualizacja braków w danych

 
##########PORÓWNYWANIE###########
#dwie mavierze bfi i bfi_bez_brakow
 
sapply(bfi, class)
sapply(bfi_bez_brakow, class)
bfi
bfi_num <- bfi[sapply(bfi, is.numeric)] #wybiera tylko kolumny numeryczne
bfi_num
bfi_bez_brakow_num <- bfi_bez_brakow[sapply(bfi_bez_brakow, is.numeric)]
bfi_bez_brakow_num
 
mediana_bfi_num <- apply(bfi_num, 2, median, na.rm = TRUE)
mediana_bfi_num
 
mediana_bfi_bez_brakow_num <- apply(bfi_bez_brakow_num, 2, median, na.rm = TRUE)
mediana_bfi_bez_brakow_num
#mediany się nie zmieniły
 
#porównanie wyników w jednym zestawieniu
mediana_porownanie <- data.framer(
  mediana_bfi_num = mediana_bfi_num,
  mediana_bfi_bez_brakow_num = mediana_bfi_bez_brakow_num)
mediana_porownanie
 
#obliczenie procentowej różnicy = przyrost względny
mediana_porownanie$procentowe_roznica <-((mediana_porownanie$mediana_bfi_bez_brakow_num - srednia_porownanie$srednis_bfi_num)/srednia_porownanie$srednis_bfi_num) = 100
 
#wyświetlanie wyników
srednia_porownanie
 
dane <- data.frame(srednia_porownanie)
dane
dane <- round(dane, 2)
dane
 
library(ggplot20)
#tworzenie wykresu słupkowego z dwiema poziomymi liniami na poziomach 5 i -5 z pogrubionymi liniami
ggplot(srednia_porownanie, aes(x = rownames (srednia_porownanie), y = procentowa_roznica)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1.5)+   #pogrubiona linia na poziomie 5
  geom_hline(yintercept = -5, linetype = "dashed", color = "red", size = 1.5)+  #pogrubiona linia na poziomie -5
  theme_minimal()+
  labs(title = "procentowa różnica między średnią BFI dla zbioru z brakami i bez braków danych", 
       theme_minimal()+
         labs(title="Procentowa różnica między srednią BFI dla zbioru z brakami i bez braków danych",
              x="Zmienne",
              y="Procentowa różnica (%)")+
         theme(axis.text.x=element_text(angle=90,hjust=1)) #obrot etykiet na osi x,
       #identyfikacja zmiennych ktore ,maja roznice wieksza niz 5% lub mniejsza niz -5%
       zmienne_problemowe <- srednia_porownanie[abs(srednia_porownanie$procentowa_roznica)>5, ],
       print(zmienne_problemowe)

#chcemy brakujące wartości zastąpić miedianami
bfi_numeric
mediana_bfi_num
 
library(dplyr)
 
bfi_num_imputowane <- bfi.num %>%
  mutate(across(everything(), ~ifelse(is.na(.), mdiana_bfi_num[cur_column()], .)))
bfi_num_imputowane
dim(bfi_num_imputowane)
#mutate(across(everything(), ....)) - operacja na każdej zmiennej
#ifelse(is.na(.), ....) - szukamy NA (braki i zastępujemy je medianą)