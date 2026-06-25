2026-06-26

EN
--

Asgrnxrnx
---------

Asgrnxrnx is a simple tool designed for users in Poland. The program
creates a new RINEX file without redundant observation types from a
ASG-EUPOS RINEX 3.04 observation file.


PL
--

Asgrnxrnx
---------

Asgrnxrnx to proste narzędzie wiersza poleceń, które z pliku
obserwacyjnego RINEX 3.04 systemu ASG‑EUPOS tworzy nowy
plik RINEX bez nadmiarowych typów obserwacji.


Opis działania programu:

Program wyszukuje typy obserwacji, które nie zawierą żadnych danych
liczbowych i tworzy nowy plik bez tych typów. Wynikiem działania jest
nowy, poprawiony plik RINEX.  Plik wejściowy nie jest modyfikowany.


Pobieranie programu
-------------------

Jeśli chcesz tylko uruchomić program w systemie Windows, pobierz
gotowy plik asgrnxrnx.exe z Releases.

Nie trzeba pobierać innych plików.


Przykład użycia
---------------

W wierszu poleceń:
```
asgrnxrnx XXXX001M.26o
```


Kompilacja kodu źródłowego (opcjonalnie)
--------------------------

Jeśli chcesz skompilować program samodzielnie w systemie MS Windows,
to trzeba pobrać i zainstalować kompilator języka haskell o nazwie
ghc. Następnie w wierszu poleceń:
```
ghc -O2 AsgRnxRnx.hs -o asgrnxrnx.exe
```
Powstałe plik *.hi *.o można usunąć.
