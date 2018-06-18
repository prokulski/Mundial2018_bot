# zbieranie tweetów z tag(ów)

z shella odpalić należy

```
Rscript capture_stream.R 22:15 POLSEG
```

lub dla meczy równoległych - dwa tagi

```
Rscript capture_stream.R 22:15 RUSEGY POLSEG
```

i to się będzie kręcić od momentu uruchomienia do 22:15 bieżącego dnia. Warto odpalić z użyciem `screen`a.
Brane są pod uwagę też tagi symetryczne - czyli dla POLSEG zbirany jest też SEGPOL.


# Analiza i publikowanie tweetów:

```
Rscript analyze_stream.R 1 POL SEG
```

Bot działa na https://twitter.com/rstatspl
