### 1. Charakterystyka oprogramowania:

- **Nazwa skrócona:** AWW
- **Nazwa pełna:** Analiza zróżnicowania wyników wyborów do Sejmu 2023
- **Krótki opis ze wskazaniem celów:**
   Projekt "Analiza zróżnicowania wyników wyborów do Sejmu 2023" ma na celu przedstawienie wpływu różnych zmiennych na prawdopodobieństwo otrzymania mandatu poselskiego, analizę rezultatów wyborczych w poszczególnych okręgach wyborczych oraz analizę frekwencji w porównaniu do poprzednich wyborów.

### 2. Prawa autorskie:

- **Autorzy:**
    Marta Schulz, Aleksandra Truszkowska, Aleksandra Zientarska
- **Warunki licencyjne do oprogramowania wytworzonego przez grupę:**
    - **Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License (CC BY-NC-SA 4.0)**
        - Darmowe użytkowanie, kopiowanie, rozpowszechnianie i modyfikowanie oprogramowania są dozwolone pod warunkiem przypisania odpowiedniego uznanie autorstwa, braku użycia komercyjnego oraz udostępnienia utworów pochodnych na tych samych warunkach.

### 3. Specyfikacja wymagań:

| Identyfikator | Nazwa | Opis | Priorytet | Kategoria |
| --- | --- | --- | --- | --- |
| A1 | Załadowanie i przygotowanie danych | Program powinien umożliwiać import potrzebnych bibliotek i danych oraz ich przygotowanie do analiz.  | 1 | Funkcjonalne |
| A1.1 | Import bibliotek | Program powinien załadować potrzebne biblioteki.  | 1 | Funkcjonalne |
| A1.2 | Import danych | Program powinien importować dane wykorzystywane do analiz. | 1 | Funkcjonalne |
| A1.3 | Przygotowanie danych | Program powinien wykonywać polecenia dotyczące przygotowania i oczyszczenia danych do analizy. | 1 | Funkcjonalne |
| A2 | Analiza prawdopodobieństwa otrzymania mandatu poselskiego | Program powinien przeprowadzać analizę istotnych zmiennych oraz wykorzystywać je do prognozowania prawdopodobieństwa otrzymania mandatu poselskiego. | 1 | Funkcjonalne |
| A2.1 | Analiza zmiennych | Program  powinien wyjaśniać otrzymanie mandatu poselskiego wykorzystując do tego zmienne: płeć, pozycja na liście, poparcie ogólnokrajowe dla partii oraz poparcie dla partii w okręgu. | 1 | Funkcjonalne |
| A2.2 | Prognoza prawdopodobieństwa | Program powinien obliczać prawdopodobieństwa otrzymania mandatu dla każdego kandydata i wskazywać tych z największymi i najmniejszymi wartościami. | 1 | Funkcjonalne |
| A3 | Grupowanie podobnych okręgów | Program powinien dzielić okręgi na grupy wraz z przedstawieniem ich cech charakterystycznych oraz wizualizować wyniki.  | 1 | Funkcjonalne |
| A3.1 | Podział okręgów na grupy | Program powinien przeprowadzać analizę dzielącą okręgi na grupy podobne pod względem wyników wyborów. | 1 | Funkcjonalne |
| A3.2 | Cechy charakterystyczne grup | Program powinien wyświetlać charakterystyki utworzonych grup okręgów i wskazywać czym te okręgi się różnią od siebie. | 1 | Funkcjonalne |
| A3.3 | Wizualizacja grup | Program powinien wizualizować wyróżnione grupy na wykresie. | 1 | Funkcjonalne |
| A4 | Analiza wzrostu frekwencji | Program powinien wizualizować różnicę liczby głosów dla poszczególnych partii (według każdego okręgu wyborczego) porównując wyniki z roku 2019 z wyborami w 2023 roku.  | 1 | Funkcjonalne |
| A5 | Analiza kandydatów pod względem pozycji na liście  | Program powinien wyszukiwać osoby, które dostały mandat w zależności od okręgu i partii politycznej oraz wyświetlać liczbę osób, które otrzymały mandat z danego miejsca na liście.  | 1 | Funkcjonalne |
| A5.1 | Podział kandydatów na okręgi  | Program powinien dzielić zbiór danych „kandydaci” na okręgi.  | 1 | Funkcjonalne |
| A5.2 | Wyszukiwanie kandydatów otrzymujących mandat | Program powinien wyszukiwać kandydatów, którzy otrzymali mandat dla dowolnego okręgu i dowolnej partii.  | 1 | Funkcjonalne |
| A5.3 | Wizualizacja osób z mandatem  | Program powinien wyświetlać wykres obrazujący liczbę osób, które otrzymały mandat z danego miejsca na liście.  | 1 | Funkcjonalne |
| A6 | Przejrzystość  | Graficzny interfejs użytkownika stworzony przy wykorzystaniu Shiny powinien być przejrzysty i zapewniać prostotę i swobodę użytkowania oprogramowania. | 2 | Niefunkcjonalne |
| A7 | Dostępność | Program powinien być dostępny 24h/7 z odpowiedzią kilku sekund przy zwiększonym obciążeniu. | 2 | Niefunkcjonalne |

### 4. Architektura systemu/oprogramowania:

- **Architektura rozwoju – stos technologiczny:**
    - github
    - RStudio version 4.2.2
    - R packages: 
        - Shiny
        - DT
        - dplyr
        - rpart
        - ggplot2
        - tidyr
        - readxl 
        - tidyverse
    
- **Architektura uruchomieniowa – stos technologiczny:**
    - Shiny
    - R packages: 
        - DT
        - dplyr
        - rpart
        - ggplot2
        - tidyr
        - readxl 
        - tidyverse
    

### 5. Testy:

#### a. Scenariusz testów:

tbd

#### b. Sprawozdanie z wykonania scenariuszy testów:

tbd

