# Wordle


## Casos de Uso

Para correr el wordle via terminal:

Por default -> daily con día actual.


Con ARG palabra:

```bash
cabal run wordle-tp -- --palabra (insert palabra 5 letras)
```

Si la palabra es inválida te avisa por consola.
Si usas el argumento --palabra, pero no indicas ninguna se juega random.


Con ARG daily:

```bash
cabal run wordle-tp -- --daily
cabal run wordle-tp -- --daily YYYY-MM-DD
```

Si la fecha es inválida jugas el día actual.


Con ARG random:

```bash
cabal run wordle-tp -- --random
```



## Test

Para correr test (solo de match hay) via terminal por favor utilizar:ç

```bash
cabal test --test-show-details=always
```

Si los tests dan correctos los podemos ver sin ir a logs... Mejor!

## Aclaración guardadoEstado -> solo Juego

Al intentar guarda el estado completo con el juego + los datos del estado, el ToJSON funcionaba comúnmente, pero el FromJSON y read (cargado de ese file) no.
Se intento definir las instancias manualmente aunque sin resultados...
Por eso se decidió guardar tan solo el Juego (que si se derivaba, guardaba y cargaba correctamente) y recalcular "descartadas" de Estado.
El resto que incluía Estado por demás, estaba disponible (diccionario por ejemplo) y por otro lado , el intento actual y las alertas no son necesarias ni guardar ni recalcular (son en vivo).