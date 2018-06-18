# It's alive!

Bot działa na https://twitter.com/rstatspl

## Zbieranie tweetów z tag(ów)

Z shella odpalić należy

```
Rscript capture_stream.R 22:15 POLSEG
```

lub dla meczy równoległych - dwa tagi

```
Rscript capture_stream.R 22:15 RUSEGY POLSEG
```

### Znaczenie parametrów:

* args[1] - godzina zakończenia pobierania danych
* args[2] - pierwszy tag, z którego są pobierane tweety; dla parametru xxxyyy pobierane będą tweety z #xxxyyy oraz #yyyxxx
* args[3] - drugi tag, z którego są pobierane tweety; przydatne przy meczach równoległych. Dla pierwszego tagu = #aaabbb i drugiego tagu = #cccddd tweety pobierane będą w sumie dla czterech tagów: #aaabbb, #bbbaaa, #cccddd oraz #dddccc


## Analiza i publikowanie tweetów

Z shella:

```
Rscript analyze_stream.R 1 POL SEG
```

### Znaczenie parametrów:

* args[1] - czy opublikować tweeta? 0 = nie, 1 = tak
* args[2] - kod ISO3 pierwszej drużyny (gospodarza)
* args[3] - kod ISO3 drugiej drużyny (gościa)

Uwaga - kolejność kodów drużyn ma znaczenie dla pobierania wyniku. 


## Analiza brzydkich wyrazów i wzmianek osób:

```
Rscript polskie_mecze.R 1 pl POL SEG
```

### Znaczenie parametrów:

* args[1] - czy opublikować tweeta? 0 = nie, 1 = tak
* args[2] - jakiego słownika brzyskich słów użyć? pl = polskiego, eng (lub coś innego) = angielski
* args[3] - kod ISO3 pierwszej drużyny (gospodarza)
* args[4] - kod ISO3 drugiej drużyny (gościa)

Przy użyciu nie-polskiego słownika nie wszystkie wykresy się generują - ze względu na słownik osób ograniczony tylko do polskiegj drużyny.

