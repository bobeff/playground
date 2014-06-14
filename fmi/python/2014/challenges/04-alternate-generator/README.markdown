# Алтерниращ генератор

Да се напише генератор `alternate`, който приема __произволен брой аргументи__, всеки от които връща обект итератор при извикването си.
`alternate` се очаква да генерира обекти като взима последователно първите елементи от върнатите от аргументите итератори в същия ред, в който са подадени, после вторите, третите и т.н.

### Примери

```
>>> mirror_count = alternate(lambda: itertools.count(1, 1), lambda: itertools.count(-1, -1))
>>> next(mirror_count)
1
>>> next(mirror_count)
-1
>>> next(mirror_count)
2
>>> next(mirror_count)
-2
>>> next(mirror_count)
3
>>> next(mirror_count)
-3
>>> next(mirror_count)
4
```
