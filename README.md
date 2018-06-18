# zbieranie tweetów

z shella odpalić należy

```
Rscript capture_stream.R 22:15 POLSEG
```

lub dla meczy równoległych

```
Rscript capture_stream.R 22:15 RUSEGY POLSEG
```

i to się będzie kręcić do 20:00. Warto odpalić z użyciem `screen`a


Analiza i publikowanie tweetów:

```
Rscript analyze_stream.R 1 POL SEG
```

Bot działa (jak się go włączy) na https://twitter.com/rstatspl
