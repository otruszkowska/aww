### 1. Charakterystyka oprogramowania:

- **Nazwa skrócona:** AWW
- **Nazwa pełna:** Analiza zróżnicowania wyników wyborów do Sejmu 2023
- **Krótki opis ze wskazaniem celów:**
    tbd

### 2. Prawa autorskie:

- **Autorzy:**
    Marta Schulz, Aleksandra Truszkowska, Aleksandra Zientarska
- **Warunki licencyjne do oprogramowania wytworzonego przez grupę:**
    - **Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License (CC BY-NC-SA 4.0)**
        - Darmowe użytkowanie, kopiowanie, rozpowszechnianie i modyfikowanie oprogramowania są dozwolone pod warunkiem przypisania odpowiedniego uznanie autorstwa, braku użycia komercyjnego oraz udostępnienia utworów pochodnych na tych samych warunkach.

### 3. Specyfikacja wymagań:

| Identyfikator | Nazwa | Opis | Priorytet | Kategoria |
| --- | --- | --- | --- | --- |
| A1 | Załadowanie i przygotowanie danych | System umożliwia import potrzebnych bibliotek, wykorzystywanych danych oraz ich przygotowania do analiz.  | 1 | Funkcjonalne |
| A1.1 | Import bibliotek | System umożliwia załadowanie potrzebnych bibliotek.  | 1 | Funkcjonalne |
| A1.2 | Import danych | System importuje dane wykorzystywane do analiz. | 1 | Funkcjonalne |
| A1.3 | Przygotowanie danych | System wykonuje niezbędne polecenia dotyczące przygotowania danych do analizy. | 1 | Funkcjonalne |
| A2 | Analiza prawdopodobieństwa otrzymania mandatu poselskiego | System umożliwia przeprowadzenie analizy istotnych zmiennych oraz wykorzystanie ich do prognozowania prawdopodobieństwa otrzymania mandatu poselskiego. | 1 | Funkcjonalne |
| A2.1 | Analiza zmiennych | System  wyjaśnia otrzymanie mandatu poselskiego wykorzystując do tego wskazane zmienne, wyróżniając te, które miały wpływ na ostateczny wynik. | 1 | Funkcjonalne |
| A2.2 | Prognoza prawdopodobieństwa | System oblicza prawdopodobieństwa otrzymania mandatu dla każdego kandydata i wskazuje tych z największymi i najmniejszymi wartościami. | 1 | Funkcjonalne |
| A3 | Grupowanie podobnych okręgów | System umożliwia podział okręgów na grupy wraz z przedstawieniem ich cech charakteryztycznych oraz wizualizację wyników.  | 1 | Funkcjonalne |
| A3.1 | Podział okręgów na grupy | System przeprowadza analizę dzielącą okręgi na grupy podobne pod względem wyników wyborów. | 1 | Funkcjonalne |
| A3.2 | Cechy charakterystyczne grup | System wyświetla charakterystyki utworzonych grup okręgów i wskazuje czym te okręgi się różnią od siebie. | 1 | Funkcjonalne |
| A3.3 | Wizualizacja grup | System wizualizuje wyróżnione grupy na wykresie. | 1 | Funkcjonalne |
| A4 | Analiza wzrostu frekwencji | System pozwala na wizualizację różnicy liczby głosów dla poszczególnych partii (według każdego okręgu wyborczego) porównując wyniki z roku 2019 z wyborami w 2023 roku.  | 1 | Funkcjonalne |

### 4. Architektura systemu/oprogramowania:

- **Architektura rozwoju – stos technologiczny:**
    - RStudio version 4.2.2
- **Architektura uruchomieniowa – stos technologiczny:**
    - R packages: 
        - Shiny
        - DT
        - dplyr
        - rpart
        - ggplot2
        - tidyr
    

### 5. Testy:

#### a. Scenariusz testów:

tbd

#### b. Sprawozdanie z wykonania scenariuszy testów:

tbd

