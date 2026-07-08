# Codebook

## Identificativi

| Variabile  | Descrizione                              |
|------------|------------------------------------------|
| `name`     | Nome della città                         |
| `city`     | ID della città                           |
| `jid`      | Identificativo univoco della città       |
| `statenam` | Stato (abbreviazione a 2 lettere)        |
| `state`    | Codice dello stato (2 cifre, non-FIPS)   |
| `year`     | Anno (4 cifre)                           |

## Criminalità (FBI UCR CUS)

| Variabile  | Descrizione                              |
|------------|------------------------------------------|
| `murder`   | Omicidi e omicidi colposi non colpevoli  |
| `rape`     | Stupri                                   |
| `robbery`  | Rapine                                   |
| `assault`  | Aggressioni aggravate                    |
| `burglary` | Furti con scasso                         |
| `larceny`  | Furti semplici                           |
| `auto`     | Furti di veicoli a motore                |

## Polizia (FBI UCR CUS)

| Variabile  | Descrizione                              |
|------------|------------------------------------------|
| `sworn`    | Numero di agenti giurati                 |
| `civil`    | Numero di impiegati civili               |

## Elezioni e variabili politiche

| Variabile  | Descrizione                                          |
|------------|------------------------------------------------------|
| `elecyear` | Indicatore anno elettorale (sindaco)                 |
| `mayor`    | Indicatore anno elettorale (sindaco)                 |
| `governor` | Indicatore anno elettorale (governatore)             |
| `term2`    | Indicatore mandato sindacale di 2 anni               |
| `term3`    | Indicatore mandato sindacale di 3 anni               |
| `term4`    | Indicatore mandato sindacale di 4 anni               |
| `termlim`  | Governatore non ricandidabile (Anne Case)            |
| `date_wa`  | Data prossime elezioni (World Almanac)               |
| `date_my`  | Data prossime elezioni (Municipal Yearbook)          |
| `web`      | Date elettorali validate via web                     |

## Demografia

| Variabile   | Descrizione                                                   |
|-------------|---------------------------------------------------------------|
| `citypop`   | Popolazione della città (FBI UCR, Arrests tape)               |
| `citybla`   | Quota popolazione nera nella città (interpolata)              |
| `cityfemh`  | Quota nuclei con capofamiglia donna nella città (interpolata) |
| `a0_5`      | Quota popolazione SMSA di età 0–4                             |
| `a5_9`      | Quota popolazione SMSA di età 5–9                             |
| `a10_14`    | Quota popolazione SMSA di età 10–14                           |
| `a15_19`    | Quota popolazione SMSA di età 15–19                           |
| `a20_24`    | Quota popolazione SMSA di età 20–24                           |
| `a25_29`    | Quota popolazione SMSA di età 25–29                           |

## Variabili economiche

| Variabile   | Descrizione                                          |
|-------------|------------------------------------------------------|
| `rincpc`    | Reddito reale pro capite statale                     |
| `econgrow`  | Crescita economica: Δlog(reddito reale pro capite)   |
| `unemp`     | Tasso di disoccupazione statale                      |
| `price`     | IPC (Economic Report of the President)               |

## Spesa pubblica statale e locale

| Variabile   | Descrizione                                                    |
|-------------|----------------------------------------------------------------|
| `sta_educ`  | Spesa reale pro capite per istruzione (stato + enti locali)   |
| `sta_welf`  | Spesa reale pro capite per welfare (stato + enti locali)      |

---

**Nota:** Le variabili `jid`, `mayor`, `date_wa`, `date_my` e `web` provengono da McCrary (2002). Le restanti da Levitt (1997).
