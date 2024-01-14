library(readxl)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shiny)
library(DT)
dane1<-"C:/Users/altru/Desktop/kandydaci_sejm_utf8.xlsx"
dane1 <- read_excel(dane1)
dane2<-"C:/Users/altru/Desktop/wykaz_list_sejm_utf8.xlsx"
dane2 <- read_excel(dane2)
dane3<-"C:/Users/altru/Desktop/komitety_sejm.xlsx"
dane3 <- read_excel(dane3)

# łączenie potrzebnych kolumn w jeden zbiór danych
dopasowanie <- match(paste(dane1$'Nr okręgu', dane1$'Nr listy'), paste(dane2$'Numer okręgu', dane2$'Nr listy'))
dane1$'Procent głosów oddanych w okręgu na partię' <- dane2$'Procent głosów oddanych w okręgu'[dopasowanie]
dopasowanie2 <- match(dane1$'Nazwa komitetu', dane3$'Komitet')
dane1$'Procentowe poparcie dla partii' <- dane3$'Procent głosów'[dopasowanie2]
dane<-dane1


#Zamiana charakteru zmiennych oraz ich nazw na skrócone w celu ułatwienia przeprowadzenia analiz
dane$`Nazwa komitetu`<-as.factor(dane$`Nazwa komitetu`)
dane$`Płeć`<-as.factor(dane$`Płeć`)
dane$`Czy przyznano mandat`<-as.factor(dane$`Czy przyznano mandat`)
#zamiana przecinków na kropki
dane$`Procent głosów oddanych w okręgu na partię` <- gsub(",", ".", dane$`Procent głosów oddanych w okręgu na partię`)
dane$`Procent głosów oddanych w okręgu na partię`<-as.numeric(dane$`Procent głosów oddanych w okręgu na partię`)
summary(dane)
#skrócone nazwy zmiennych
names(dane)[c(1, 2,4,5,16,17,18)] <- c("Okręg", "Lista", "Kandydat", "Partia", "Mandat", "pppo", "ppo")
#skrócone nazwy partii
unikalne_wartosci <- unique(dane$Partia)
print(unikalne_wartosci)
dane$Partia <- factor(dane$Partia, levels = c("KOMITET WYBORCZY BEZPARTYJNI SAMORZĄDOWCY", "KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE", "KOMITET WYBORCZY NOWA LEWICA", "KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ", "KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ", "KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI", "KOMITET WYBORCZY POLSKA JEST JEDNA", "KOMITET WYBORCZY WYBORCÓW RUCHU DOBROBYTU I POKOJU", "KOMITET WYBORCZY ANTYPARTIA", "KOMITET WYBORCZY NORMALNY KRAJ", "KOMITET WYBORCZY RUCH NAPRAWY POLSKI", "KOMITET WYBORCZY WYBORCÓW MNIEJSZOŚĆ NIEMIECKA"), labels = c("BEZ", "HO", "LEW", "PIS", "KONF", "KO", "PJJ", "DOB", "ANTY", "NOR", "RUCH", "NIEM"))
summary(dane$Partia)
#tylko wykorzystywane zmienne
dane <- dane[, c(1,2,3,4,5,6,16,17,18)]

##Przygotowanie danych dotyczących okręgów
okregi<-"C:/Users/altru/Desktop/wyniki_gl_na_listy_po_okregach_proc_sejm_utf8.xlsx"
okregi <- read_excel(okregi)
#wybór tylko potrzebnych kolumn oraz zamiana przecinków na kropki
okregi <- okregi[, c(4, 10,11,12,13,14,15,16)]
names(okregi)[c(2,3,4,5,6,7,8)] <- c("BEZ", "HO", "LEW", "PIS", "KONF", "KO", "PJJ")
zamien_przecinki <- function(x) {
  as.numeric(gsub(",", ".", x))
}
okregi <- as.data.frame(lapply(okregi, zamien_przecinki))


#przygotowanie danych dotyczących wyborów w 2019 i 2023 roku w celu porównania 
dane23<-"C:/Users/altru/Desktop/wyniki_gl_na_listy_po_okregach_sejm_utf8.xlsx"
dane23 <- read_excel(dane23)
dane23 <- dane23[, c(1,5,26,28,29,30,31,32)]

dane19<-"C:/Users/altru/Desktop/wyniki_gl_na_listy_po_okregach_sejm.xlsx"
dane19<- read_excel(dane19)
dane19 <- dane19[, c(4,24,25,27,28,30,32)]

dane19 <- as.data.frame(lapply(dane19, as.numeric))
dane23 <- as.data.frame(lapply(dane23, as.numeric))
names(dane23)[c(2,3,4,5,6,7,8)] <- c("Liczba wyborców uprawnionych do głosowania w 2023", "Liczba głosujących w 2023", "TRZECIA DROGA (2023)", "NOWA LEWICA (2023)", "PiS (2023)", "KONFEDERACJA (2023)", "KO (2023)")
names(dane19) <- c("Liczba wyborców uprawnionych do głosowania w 2019", "Liczba głosujących w 2019", "KO (2019)", "KONFEDERACJA (2019)", "PSL (2019)", "PiS (2019)", "SOJUSZ LEWICY (2019)")
dane23$Frekwencja2023<-(dane23$`Liczba głosujących w 2023`/dane23$`Liczba wyborców uprawnionych do głosowania w 2023`)*100
dane19$Frekwencja2019<-(dane19$`Liczba głosujących w 2019`/dane19$`Liczba wyborców uprawnionych do głosowania w 2019`)*100

dane23<-cbind(dane23,dane19)

dane23$PiS<-dane23$`PiS (2023)`-dane23$`PiS (2019)`
dane23$'PSL,TRZECIA DROGA'<-dane23$`TRZECIA DROGA (2023)`-dane23$`PSL (2019)`
dane23$'SOJUSZ LEWICY, NOWA LEWICA'<-dane23$`NOWA LEWICA (2023)`-dane23$`SOJUSZ LEWICY (2019)`
dane23$KO<-dane23$`KO (2023)`-dane23$`KO (2019)`
dane23$KONFEDERACJA<-dane23$`KONFEDERACJA (2023)`-dane23$`KONFEDERACJA (2019)`



#####drzewo -  tylko zmienna pozycja na liscie

drzewo1<-rpart(formula = Mandat ~ `Pozycja na liście`,
                data = dane)
rpart.plot(drzewo1, box.palette = "Oranges")

prognozy1 <- predict(drzewo1, newdata = dane, type = "prob")

dane$Prawd_drzewo1 <- prognozy1[, "Tak"]
dane_posortowane_drzewo1 <- dane[order(-dane$Prawd_drzewo1), ]
print(dane_posortowane_drzewo1[, c("Kandydat", "Mandat", "Pozycja na liście", "Prawd_drzewo1")], n=1800)
##prawdopodobieństwo dla każdego numeru na liście
numery <- aggregate(Prawd_drzewo1 ~ `Pozycja na liście`, data = dane, FUN = function(x) unique(x)[1])
numery <- tapply(dane$Prawd_drzewo1, dane$`Pozycja na liście`, FUN = function(x) unique(x)[1])
numery_dane<- data.frame(`Pozycja na liście` = names(numery), Prawd_drzewo1 = as.vector(numery))
numery_dane


#####drzewo z różnymi zmiennymi

drzewo2 <- rpart(formula = Mandat ~ Płeć + ppo+pppo + `Pozycja na liście`,
                 data = dane)

rpart.plot(drzewo2, box.palette = "Oranges")
##prawdopodobieństwo dla każdego kandydata
prognozy2 <- predict(drzewo2, newdata = dane, type = "prob")
dane$Prawd_drzewo2 <- prognozy2[, "Tak"]
dane_posortowane_drzewo2 <- dane[order(-dane$Prawd_drzewo2), ]
print(dane_posortowane_drzewo2[, c("Kandydat", "Partia", "Mandat", "Płeć", "Pozycja na liście", "pppo", "ppo", "Prawd_drzewo2")], n=300)

#model logitowy + prawdopodobieństwo

dane$Mandat1 <- ifelse(dane$Mandat == "Tak", 1, 0)

#z ppo - mniejsze zróżnicowanie
logit1 <- glm(Mandat1 ~ Płeć  +ppo + `Pozycja na liście`, data = dane, family = binomial)
summary(logit1)
szanse <- predict(logit1, newdata = dane, type = "response")
dane$Prawdopodobieństwo <- szanse
dane_posortowane <- dane[order(-dane$Prawdopodobieństwo), ]
print(dane_posortowane[, c("Kandydat", "Mandat1","Partia", "Płeć", "Pozycja na liście", "ppo", "Prawdopodobieństwo")], n=500)


#pppo - większe zróżnicowanie, bo poparcie w okręgach było zróżnicowane
logit1 <- glm(Mandat1 ~ Płeć  +pppo + `Pozycja na liście`, data = dane, family = binomial)
summary(logit1)
szanse <- predict(logit1, newdata = dane, type = "response")
dane$Prawdopodobieństwo <- szanse
dane_posortowane <- dane[order(-dane$Prawdopodobieństwo), ]
print(dane_posortowane[, c("Kandydat", "Mandat1","Partia", "Płeć", "Pozycja na liście", "pppo", "Prawdopodobieństwo")], n=500)

#######OKREGI


wynik_klastrowania <- kmeans(okregi, centers = 2)
# Przypisanie klastrów do obserwacji
przynaleznosc_do_klastrow <- wynik_klastrowania$cluster

# Wyświetlenie przynależności do klastrów
print(przynaleznosc_do_klastrow)



# numery przypadków i przypisane klastry
NumerPrzypadku <- 1:41 
Klaster <- wynik_klastrowania$cluster  

# Dodanie kolumn do istniejących danych
dane_z_klastrem <- cbind(okregi, NumerPrzypadku, Klaster)



plot(NumerPrzypadku, Klaster, pch = Klaster, col = Klaster, main = "Wizualizacja Klastrów", xlab = "Numer Przypadku", ylab = "Klaster")

# Dodanie legendy dla klastrów
legend("topright", legend = unique(Klaster), col = unique(Klaster), pch = unique(Klaster), title = "Klaster", cex = 0.8)


plot(NumerPrzypadku, Klaster, pch = Klaster, col = Klaster, main = "Wizualizacja Klastrów", xlab = "Numer Przypadku", ylab = "Klaster")

# Dodanie numerów przypadków jako etykiet na wykresie
text(NumerPrzypadku, Klaster, labels = NumerPrzypadku, pos = 3, cex = 0.8)
print(wynik_klastrowania$centers)






#####wykres różnica głosów

dane_porownanie<-dane23[,c(18,19,20,21,22)]


numer_okregu <-24
okreg<- dane_porownanie[numer_okregu, ]
numer_okregu_t <- as.data.frame(t(okreg))
names(numer_okregu_t) <- "Wartość"
numer_okregu_t$Zmienna <- rownames(numer_okregu_t)

ggplot(numer_okregu_t, aes(x = Zmienna, y = Wartość, fill = Zmienna)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Wartość, 2)), vjust = -0.5, color = "black", size = 5) + # Zwiększona wielkość czcionki
  labs(title = paste("Zmiana liczby głosów w porównaniu z wyborami w 2019 roku dla okręgu numer ", numer_okregu)) +
  theme(axis.text.x = element_blank())

kandydaci <- read_excel("/Users/altru/Desktop/kandydaci_sejm_utf8.xlsx")


# Zamieniamy przecinki na kropki w wybranych kolumnach
kandydaci <- kandydaci %>%
  mutate(across(c(`Procent głosów oddanych na listę`, `Procent głosów oddanych w okręgu`), ~ as.numeric(gsub(",", ".", .))))

# Wyświetl zmienione dane
print(kandydaci)

#Zmiana danych na dane liczbowe 

kandydaci$`Liczba głosów` <- as.numeric(kandydaci$`Liczba głosów`)
kandydaci$`Nr okręgu` <- as.numeric(kandydaci$`Nr okręgu`)
kandydaci$`Pozycja na liście` <- as.numeric(kandydaci$`Pozycja na liście`)

# Zmiana NA w kolumnie "Poparcie" na "-"
kandydaci <- kandydaci %>%
  mutate(Poparcie = ifelse(is.na(Poparcie), "-", Poparcie))


kandydaci$`Procent głosów oddanych na listę` <- as.numeric(kandydaci$`Procent głosów oddanych na listę`)

kandydaci$`Procent głosów oddanych w okręgu` <- as.numeric(kandydaci$`Procent głosów oddanych w okręgu`)

################



####SHINY
#drzewa decyzyjne
# UI - pierwszy blok shiny
ui <- fluidPage(
  titlePanel("Analiza wpływu pozycji na liście i innych zmiennych na otrzymanie mandatu poselskiego"),
  
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Analiza wpływu zmiennej: pozycja na liście", plotOutput("treePlot"), dataTableOutput("sortedData"), tableOutput("probabilityTable")),
        tabPanel("Analiza wpływu grupy zmiennych na otrzymanie mandatu poselskiego", plotOutput("treePlot2"), dataTableOutput("sortedData2"))
      )
    )
  )
)

# Server - pierwszy blok shiny
server <- function(input, output) {
  # Tworzenie drzewa dla pozycji na liście
  output$treePlot <- renderPlot({
    drzewo1 <- rpart(formula = Mandat ~ `Pozycja na liście`, data = dane)
    rpart.plot(drzewo1, box.palette = "Oranges")
  })
  
  output$sortedData <- renderDataTable({
    prognozy <- predict(drzewo1, newdata = dane, type = "prob")
    dane$Prawd_drzewo1 <- prognozy[, "Tak"]
    dane_posortowane_drzewo1 <- dane[order(-dane$Prawd_drzewo1), ]
    dane_posortowane_drzewo1[, c("Kandydat", "Mandat", "Pozycja na liście", "Prawd_drzewo")][1:1800, ]
  })
  
  output$probabilityTable <- renderTable({
    numery <- aggregate(Prawd_drzewo1 ~ `Pozycja na liście`, data = dane, FUN = function(x) unique(x)[1])
    numery_dane <- data.frame(`Pozycja na liście` = numery$`Pozycja na liście`, Prawd_drzewo1 = numery$Prawd_drzewo1)
    numery_dane
  })
  
  # Tworzenie drzewa dla innych zmiennych
  output$treePlot2 <- renderPlot({
    drzewo2 <- rpart(formula = Mandat ~ Płeć + ppo + pppo + `Pozycja na liście`, data = dane)
    rpart.plot(drzewo2, box.palette = "Oranges")
  })
  
  output$sortedData2 <- renderDataTable({
    prognozy2 <- predict(drzewo2, newdata = dane, type = "prob")
    dane$Prawd_drzewo2 <- prognozy2[, "Tak"]
    dane_posortowane_drzewo2 <- dane[order(-dane$Prawd_drzewo2), ]
    dane_posortowane_drzewo2$Prawd_drzewo2 <- round(dane_posortowane_drzewo2$Prawd_drzewo2, 4) 
    dane_posortowane_drzewo2[, c("Kandydat", "Mandat", "Płeć", "Pozycja na liście", "pppo", "ppo", "Prawd_drzewo2")]
  })
}

shinyApp(ui = ui, server = server)



#shiny modele

ui <- fluidPage(
  titlePanel("Model parametryczny - analiza wpływu różnych zmiennych na otrzymanie mandatu poselskiego"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("zmienna", "Wybierz zmienną:", choices = c("Poparcie ogólnokrajowe dla partii (ppo)", "Poparcie dla partii w okręgu (pppo)"))
    ),
    mainPanel(
      # Wyświetlanie podsumowania modelu i wyników
      verbatimTextOutput("summary_model"),
      DT::dataTableOutput("wyniki_table") 
    )
  )
)
server <- function(input, output) {
  
  # Funkcja do obliczania regresji logistycznej i wyświetlania wyników
  output$summary_model <- renderPrint({
    if (input$zmienna == "Poparcie ogólnokrajowe dla partii (ppo)") {
      logit1 <- glm(Mandat1 ~ Płeć + ppo + `Pozycja na liście`, data = dane, family = binomial)
    } else {
      logit1 <- glm(Mandat1 ~ Płeć + pppo + `Pozycja na liście`, data = dane, family = binomial)
    }
    summary(logit1)
  })
  
  output$wyniki_table <- renderDataTable({
    if (input$zmienna == "Poparcie ogólnokrajowe dla partii (ppo)") {
      dane_to_show <- dane_posortowane[, c("Kandydat", "Mandat1", "Partia", "Płeć", "Pozycja na liście", "ppo", "Prawdopodobieństwo")]
    } else {
      dane_to_show <- dane_posortowane[, c("Kandydat", "Mandat1", "Partia", "Płeć", "Pozycja na liście", "pppo", "Prawdopodobieństwo")]
    }
    dane_to_show$Prawdopodobieństwo <- round(dane_to_show$Prawdopodobieństwo, 4) # Zaokrąglenie do czterech miejsc po przecinku
    datatable(head(dane_to_show, n = 6333), options = list(order = list(list(7, 'desc'))))  
  })
}


shinyApp(ui = ui, server = server)




#shiny- podobieństwo okręgów


ui <- fluidPage(
  titlePanel("Analiza podobieństwa okręgów"),
  plotOutput("scatterplot"),
  verbatimTextOutput("clusterCenters")  
)


server <- function(input, output) {
  
  wynik_klastrowania <- reactive({
    kmeans(okregi, centers = 2)
  })
  
  output$scatterplot <- renderPlot({
    wynik <- wynik_klastrowania()
    
    NumerPrzypadku <- 1:41  
    Klaster <- wynik$cluster  
    
    plot(NumerPrzypadku, Klaster, pch = Klaster, col = Klaster, main = "Wizualizacja Klastrów", xlab = "Numer Przypadku", ylab = "Klaster", ylim = c(0, 3))
    
    axis(2, at = seq(0, 3, by = 0.5))
    
    
    legend("topright", legend = unique(Klaster), col = unique(Klaster), pch = unique(Klaster), title = "Klaster", cex = 0.8)
    
    
    text(NumerPrzypadku, Klaster, labels = NumerPrzypadku, pos = 3, cex = 1.2)
  })
  
  output$clusterCenters <- renderPrint({
    wynik <- wynik_klastrowania()
    centra_klastrów <- round(wynik$centers,2)
    centra_klastrów
  })
}


shinyApp(ui = ui, server = server)


#######shiny - różnice 2019 i 2023

generuj_wykres <- function(numer_okregu) {
  okreg <- dane_porownanie[numer_okregu, ]
  numer_okregu_t <- as.data.frame(t(okreg))
  names(numer_okregu_t) <- "Wartość"
  numer_okregu_t$Zmienna <- rownames(numer_okregu_t)
  
  ggplot(numer_okregu_t, aes(x = Zmienna, y = Wartość, fill = Zmienna)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = round(Wartość, 2)), vjust = -0.5, color = "black", size = 5) +
    labs(title = paste("Zmiana liczby głosów w porównaniu z wyborami w 2019 roku dla okręgu numer ", numer_okregu)) +
    theme(axis.text.x = element_blank())
}

# Definicja interfejsu użytkownika (UI)
ui <- fluidPage(
  titlePanel("Wizualizacja zmian liczby głosów"),
  sidebarLayout(
    sidebarPanel(
      numericInput("numer_okregu", "Wybierz numer okręgu:", min = 1, max = 41, value = 1) 
    ),
    mainPanel(
      plotOutput("wykres")
    )
  )
)

# Definicja serwera
server <- function(input, output) {
  output$wykres <- renderPlot({
    numer_okregu <- input$numer_okregu
    wykres <- generuj_wykres(numer_okregu)
    print(wykres)
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)


# podział danych "kandydaci" na okręgi

okregi_wyszukaj <- fluidPage(
  titlePanel("Przegląd Kandydatów"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("okregi_wyszukaj", "Wybierz numer okręgu:",
                  choices = unique(kandydaci$`Nr okręgu`)),
      br()
    ),
    
    mainPanel(
      tableOutput("tabela_kandydatow")
    )
  )
)


okregi_funkcja <- function(input, output) {
  
  output$tabela_kandydatow <- renderTable({
    filtered_data <- kandydaci[kandydaci$`Nr okręgu` == input$okregi_wyszukaj, ]
    return(filtered_data)
  })
  
}

shinyApp(okregi_wyszukaj, okregi_funkcja)



# Wyszukiwarka dla kandydatów, którzy otrzymali mandat, ale nie byli "jedynkami" na liście

wyszukiwarka_bez1 <- fluidPage(
  titlePanel("Wyszukiwarka kandydatów, którzy otrzymali mandat i nie byli 'jedynkami' na liście"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("partia", "Wybierz partię", choices = unique(kandydaci$`Nazwa komitetu`)),
      selectInput("nr_okregu", "Wybierz numer okręgu", choices = unique(kandydaci$`Nr okręgu`)),
      actionButton("pokaz_kandydatow", "Pokaż kandydatów")
    ),
    
    mainPanel(
      tableOutput("wyniki"),
      textOutput("brak_danych_komunikat")
    )
  )
)

# funkcja
kandydaci_bez1 <- function(input, output) {
  dane_do_wyswietlenia <- eventReactive(input$pokaz_kandydatow, {
    nr_okregu <- input$nr_okregu
    partia <- input$partia
    
    # Filtrowanie kandydatów
    wybrani_kandydaci <- subset(kandydaci, 
                                `Czy przyznano mandat` == "Tak" &
                                  `Nr okręgu` == nr_okregu &
                                  `Nazwa komitetu` == partia &
                                  `Pozycja na liście` != 1)
    
    return(wybrani_kandydaci)
  })
  
  output$wyniki <- renderTable({
    danee <- dane_do_wyswietlenia()
    
    if (nrow(danee) == 0) {
      return(NULL)
    }
    
    return(danee)
  })
  
  output$brak_danych_komunikat <- renderText({
    danee <- dane_do_wyswietlenia()
    
    if (nrow(danee) == 0) {
      return("Brak kandydatów spełniających kryteria.")
    }
    
    return(NULL)
  })
}

shinyApp(wyszukiwarka_bez1, kandydaci_bez1)



# Wyszukiwarka dla wszystkich kandydatów, którzy otrzymali mandat


wyszukiwarka_z1 <- fluidPage(
  titlePanel("Wyszukiwarka kandydatów, którzy otrzymali mandat"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("partia", "Wybierz partię", choices = unique(kandydaci$`Nazwa komitetu`)),
      selectInput("nr_okregu", "Wybierz numer okręgu", choices = unique(kandydaci$`Nr okręgu`)),
      actionButton("pokaz_kandydatow", "Pokaż kandydatów")
    ),
    
    mainPanel(
      tableOutput("wyniki"),
      textOutput("brak_danych_komunikat")
    )
  )
)

# funckja
kandydaci_z1 <- function(input, output) {
  dane_do_wyswietlenia <- eventReactive(input$pokaz_kandydatow, {
    mandat <- input$mandat
    nr_okregu <- input$nr_okregu
    partia <- input$partia
    
    # Filtrowanie kandydatów
    wybrani_kandydaci <- subset(kandydaci, 
                                `Czy przyznano mandat` == "Tak" &
                                  `Nr okręgu` == nr_okregu &
                                  `Nazwa komitetu` == partia)
    
    return(wybrani_kandydaci)
  })
  
  output$wyniki <- renderTable({
    danee <- dane_do_wyswietlenia()
    
    if (nrow(danee) == 0) {
      return(NULL)
    }
    
    return(danee)
  })
  
  output$brak_danych_komunikat <- renderText({
    danee <- dane_do_wyswietlenia()
    
    if (nrow(danee) == 0) {
      return("Brak kandydatów spełniających kryteria.")
    }
    
    return(NULL)
  })
  
}

shinyApp(wyszukiwarka_z1, kandydaci_z1)


#Zestawienie ilości osób, które dostały się do sejmu z danego miejsca na liście

# Uwzględnianie każdego numeru na liście nawet tego, z którego żaden kandydat się nie dostał 
wszystkie_numery <- 1:max(kandydaci$`Pozycja na liście`)

# Zestawienie ilości osób, które dostały się do Sejmu z danego miejsca na liście
ilosc_miejsce <- table(kandydaci$`Pozycja na liście`[kandydaci$`Czy przyznano mandat` == "Tak"])

# Kompletne zestawienie z zerami tam, gdzie brak kandydatów
kompletne_zestawienie <- data.frame(`Pozycja na liście` = wszystkie_numery,
                                    `Liczba kandydatów z mandatem` = as.numeric(ilosc_miejsce[as.character(wszystkie_numery)]))

# Uzupełnienie brakujących miejsc zerami
kompletne_zestawienie[is.na(kompletne_zestawienie$`Liczba kandydatów z mandatem`), "Liczba kandydatów z mandatem"] <- 0

# Usunięcie ostatniej kolumny
kompletne_zestawienie <- kompletne_zestawienie[, -ncol(kompletne_zestawienie)]


print(kompletne_zestawienie)


# Wykres pokazujący ilość osób, które dostały się do sejmu z danego miejsca na liście 

zestawienie_wykres <- fluidPage(
  titlePanel("Wykres liczby kandydatów z mandatem"),
  mainPanel(
    plotOutput("wykres")
  )
)

zestawienie_wykres1 <- function(input, output) {
  observe({
    # Użycie tryCatch do obsługi błędu związanego z pustą ramką danych
    tryCatch({
      # Sprawdzenie, czy są dostępne dane
      if (is.null(kompletne_zestawienie) || nrow(kompletne_zestawienie) == 0) {
        stop("Brak dostępnych danych.")
      }
      
      # Wyswietlenie struktury danych
      print(str(kompletne_zestawienie))
      
      # Uwzględnianie unikalnych numerów na liście
      wszystkie_numery <- unique(kompletne_zestawienie$`Pozycja.na.liście`)
      
      # Zestawienie ilości osób, które dostały się do Sejmu z danego miejsca na liście
      ilosc_miejsce <- table(kompletne_zestawienie$`Pozycja.na.liście`[kompletne_zestawienie$`Liczba.kandydatów.z.mandatem` != 0])
      
      # Kompletne zestawienie z zerami tam, gdzie brak kandydatów
      kompletne_zestawienie <- data.frame(`Pozycja.na.liście` = wszystkie_numery,
                                          `Liczba.kandydatów.z.mandatem` = as.numeric(ilosc_miejsce[as.character(wszystkie_numery)]))
      
      # Sprawdzenie czy kolumna jest pusta
      if (nrow(kompletne_zestawienie) > 0) {
        # Uzupełnienie brakujących miejsc zerami
        kompletne_zestawienie[is.na(kompletne_zestawienie$`Liczba.kandydatów.z.mandatem`), "Liczba.kandydatów.z.mandatem"] <- 0
      } else {
        # W przypadku pustej ramki danych, zwracamy informację o błędzie
        stop("Ramka danych jest pusta.")
      }
      
      # Usunięcie ostatniej kolumny
      kompletne_zestawienie <- kompletne_zestawienie[, -ncol(kompletne_zestawienie)]
    }, error = function(e) {
      message("Błąd: ", conditionMessage(e))
    })
  })
  
  # Utworzenie wykresu słupkowego
  output$wykres <- renderPlot({
    ggplot(kompletne_zestawienie, aes(x = as.factor(`Pozycja.na.liście`), y = `Liczba.kandydatów.z.mandatem`)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(title = "Liczba kandydatów z mandatem dla każdej pozycji na liście",
           x = "Pozycja.na.liście",
           y = "Liczba.kandydatów.z.mandatem")
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(zestawienie_wykres, zestawienie_wykres1)