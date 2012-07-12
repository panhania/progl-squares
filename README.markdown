Zadanie KWADRATY
================
Łukasz Hanuszczak


Wstęp
-----

Oddaję 3 rozwiązania: `squares_a.hs`, `squares_b.hs`, `squares_c.hs` - każde jest bardziej skomplikowane od swojego poprzednika, z lepszymi optymalizacjami. Część wejścia i wyjścia w każdym rozwiązaniu jest identyczna (w rozwiązaniu C dochodzi jeszcze tylko wyłuskanie w jednym miejscu odpowiedzi z pary), różnią się tylko algorytmem. Jeżeli oddanie 3 rozwiązań jest w jakiś sposób nielegalne (tzn. oddaję je tylko po to aby pokazać rozwój rozwiązania i ułatwić jego zrozumienie) to oczywiście pod uwagę powinno zostać wzięte tylko rozwiązanie `squares_c.hs`.

Testowane po skompilowaniu w GHC z flagą `-O`, bez niej program działa znacznie wolniej (wolałem pisać kod bardziej ładny niż bardziej optymalny, licząc na to że kompilator go odpowiednio wyprostuje). Szczegóły dotyczące testowania znajdują się na dole tego pliku, testy są w folderze `test`.

Pierwszym parametrem programu musi być lokalizacja pliku tekstowego, z którego będą wczytywane dane. Drugi parametr jest opcjonalny - uruchomienie z flagą `-step` powoduje że program zatrzymuje się po wyświetleniu jednego rozwiązania i czeka na wciśnięcie entera z wyświetleniem kolejnych (starałem się imitować to co robi Prolog).


Koncepcja
---------

Zakładam, że osoba czytająca zna treść zadania. Idea jest ta sama, którą zastosowałem podczas rozwiązywania zadania w Prologu. Za całość odpowiada funkcja `answers` zwracająca listę wszystkich rozwiązań. Bierzemy któryś z punktów na planszy i generujemy dla niego poprawnie zaczepione kwadraty. Robię to poprzez "rozciąganie" kwadratu najpierw na północny zachód, potem północny wschód, potem południowy wschód i na końcu na południowy zachód (kończąc rozciąganie w danym kierunku w momencie kiedy "ramka" wyjdzie poza planszę). Odpowiada za to funkcja `expand`. Teraz przydaloby się zostawić tylko te kwadraty, które zawierają określoną liczbę liczb (sic!) wewnątrz siebie. Do tego służy funkcja `count` - zwraca właśnie tę żądaną liczbę liczb dla danego kwadratu. Dostajemy zatem wszystkie poprawne kwadraty kończące się w zadanym punkcie. W między czasie generujemy rekurencyjnie rozwiązania dla pozostałych punktów. Aby otrzymać "większe" rozwiązanie musimy wziąć któreś z tych wygenerowanych rozwiązań i sprawdzić czy aktualny kwadrat do niego pasuje. Odpowiada za to funkcja `fit`. Jeżeli pasuje - to fajnie, mamy nowe rozwiązanie. Jak nie pasuje - to trudno, widocznie tak miało być. Oczywiście zbieramy wszystkie pasujące do siebie układy dostając pełną listę rozwiązań.


Rozwiązanie A
-------------

Jest to jawne przepisanie powyższego pomysłu, bez zbędnych optymalizacji.
    
    * `valid` testuje kolizję między dwoma kwadratami
    * `fit` sprawdza czy dany kwadrat nie koliduje z listą kwadratów przechodząc całą listę liniowo (przy użyciu funkcji `map`)
    * `count` bierze kwadrat oraz wejściową listę zwracająć liczbę liczb w środku kwadratu przechodząc całą listę liniowo i robiąc test zawierania
    * `expand` jest dokładnie wyjaśnione powyżej
    * `answers` znajduje wszystkie rozwiązania zadanej zagadki, zapisane przy pomocy monadycznej notacji z `do`


Rozwiązanie B
-------------

Jest to głównie optymalizacja związana z funkcją `count`. Łatwo zauważyć, że jest ona używana na każdym wygenerowanym kwadracie. Każdy test zawiera liniowo dużo czasu - każdy punkt jest sprawdzany czy nie siedzi wewnątrz kwadratu. Można z tej straszliwej liniowej złożoności zejść do odpowiadania na zapytania w czasie stałym. Ponieważ punkty na mapie nigdy nie zmieniają położenia, na samym początku algorytmu można wywołać preprocesing. Użyłem tutaj dość znanego triku: mamy tablicę liczb i chcemy w czasie stałym odpowiadać jaka jest suma na danym przedziale (tutaj w wersji dwuwymiarowej). Fachowo to się nazywa chyba "sumami częściowymi". Używam tutaj po prostu haskellowych tablic, jeżeli ktoś umie się nimi posługiwać to zaimplementowanie wspomnianego schematu nie jest problemem.

Druga optymalizacja związana jest z funkcją `expand`. Zamiast generować wszystkie mieszczące się na planszy kwadraty, a potem filtrować z nich te które zawierają w sobie odpowiednią liczbę liczb sprawdzane jest to na bieżąco. W momencie w którym pierwszy kwadrat ma więcej liczb niż wymagana dalsze "rozciąganie ramki" nie ma już sensu - wiadomo, że każdy większy kwadrat może mieć tylko więcej liczb wewnątrz siebie. Wprowadzenie tej optymalizacji jest stosunkowo łatwe - wystarczy użyć prostego `takeWhile` na "pełnej liście". Ponieważ Haskell jest leniwy to niepasujący ogon po prostu nigdy nie zostanie wygenerowany.


Rozwiązanie C
-------------

Tutaj dokładam jeszce jedną optymalizację do poprzedniego pomysłu. Wcześniej rozwiązanie było po prostu reprezentowane poprzez listę wygenerowanych kwadratów. Sprawdzanie czy nowy kwadrat pasuje do rozwiązania wymagało liniowego przejścia całej tej listy i sprawdzenia kolizji. Tutaj zamiast samej listy kwadratów (która jest potrzebna do wypisania odpowiedzi) trzymam także dwie mapy przedziałów poziomych i przedziałów pionowych.

Przypuśćmy że mamy kwadrat zaczepiony w punkcie `(x, y)` o boku długości `d`. Składa się on z czterech odcinków: poziomych `(x, y)-(x + d, y)` i `(x, y + d)-(x + d, y + d)` oraz pionowych `(x, y)-(x, y + d)` i `(x + d, y)-(x + d, y + d)`. Mając zatem mapę poziomych przedziałów na wartościach `y` i wartości `y + d` dodaję przedział `(x, x + d)` (przedziały trzymam w postaci pary `(początek, koniec)`), natomiast do mapy pionowych przedziałów na wartościach `x` i `x + d` dodaję przedział `(y, y + d)`.

Mając te dwie mapy jestem w stanie znacznie szybciej odpowiadać na pytanie czy kwadrat koliduje z układem na planszy. Jeżeli jest on zaczepiony w punkcie `(x, y)` i ma bok długości `d` to sprawdzić kolizję przedziału `(y, y + d)` w mapie pionowej na współrzędnych `x` oraz `x + d` i odrotne na mapie przedziałów poziomych.

Mapa trzyma listę przedziałów, więc nadal jest to w pesymistycznym przypadku rozwiązanie liniowe (a przez użycie mapy generalnie złożoność pesymistyczna jest chyba nawet gorsza niż była). Zawsze to jednak spora optymalizacja dla "niebezczelnych" testów. Rozwiązanie to jednak można jeszcze bardziej wyśrubować - zamiast list wewnątrz tych map można użyć drzewa przedziałowego. Wtedy raz na zawsze zeszłoby się z liniowej złożoności. Jednak pisanie drzewa przedziałowego umożliwiającego zapytania przedział-przedział nie należy do najprostszych, więc odpuściłem sobie tę wątpliwą przyjemność. Potężna biblioteka standardowa Haskella co prawda udostępnia strukturę `IntervalTree`, jednak nie udostępnia ona wszystkich potrzebnych mi operacji.


Testy
-----

Do sprawdzania poprawności i wydajności używałem:

    * testu z treści zadania (`t0`)
    * testu z magazynu "Wiedza i Życie" (`t1`)
    * jednego testu własnego zwracającego 4 odpowiedzi (`t2`)
    * własnego testu do mierzenia wydajności (`t3`)

`t3` jest zaprojektowany tak, aby znalezienie pierwszego rozwiązania było natychmiastowe, natomiast znalezienie pozostałych już stanowiło wyzwanie. Oczywiście wszystkie te testy w wersji podstawowej są banalne i nawet najmniej optymalne rozwiązanie bezproblemowo sobie z nimi radzi. Dlatego do każdego testu dochodzi jeszcze współczynnik `xN` gdzie N mówi ile razy plansza została "pomnożona". I tak np. dla `t0x1` (dokładnie test z treści zadania) mamy `t0x2` gdzie plansza ma dwa razy większe wymiary, natomiast każdy kwadrat z `t0x1` ma swoje cztery odpowiedniki (przesunięcia na skopiowanych planszach) na `t0x2`. `t0x3` ma tych odpowiedników już 9 itd. Dokładne wyniki testów znajdują się w pliku `bechmark.pdf`.
