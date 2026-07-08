# Codebook

## Identifiers

| Variable   | Description                              |
|------------|------------------------------------------|
| `name`     | City name                         |
| `city`     | City ID                           |
| `jid`      | Unique city identifier       |
| `statenam` | State (2-letter abbreviation)            |
| `state`    | State code (2 digits, non-FIPS)          |
| `year`     | Year (4 digits)                          |

## Crime (FBI UCR CUS)

| Variable   | Description                              |
|------------|------------------------------------------|
| `murder`   | Murders and non-negligent manslaughters  |
| `rape`     | Rapes                                    |
| `robbery`  | Robberies                                |
| `assault`  | Aggravated assaults                      |
| `burglary` | Burglaries                               |
| `larceny`  | Larcenies                                |
| `auto`     | Motor vehicle thefts                     |

## Police (FBI UCR CUS)

| Variable   | Description                              |
|------------|------------------------------------------|
| `sworn`    | Number of sworn officers                 |
| `civil`    | Number of civilian employees             |

## Elections and political variables

| Variable   | Description                                          |
|------------|------------------------------------------------------|
| `elecyear` | Election year indicator (mayor)                      |
| `mayor`    | Election year indicator (mayor)                      |
| `governor` | Election year indicator (governor)                   |
| `term2`    | 2-year mayoral term indicator                        |
| `term3`    | 3-year mayoral term indicator                        |
| `term4`    | 4-year mayoral term indicator                        |
| `termlim`  | Governor cannot run for reelection (Anne Case)       |
| `date_wa`  | Date of next election (World Almanac)                |
| `date_my`  | Date of next election (Municipal Yearbook)           |
| `web`      | Election dates validated via web                     |

## Demographics

| Variable    | Description                                                   |
|-------------|---------------------------------------------------------------|
| `citypop`   | City population (FBI UCR, Arrests tape)               |
| `citybla`   | Share of Black population in the city (interpolated)          |
| `cityfemh`  | Share of female-headed households in the city (interpolated)  |
| `a0_5`      | SMSA population share aged 0–4                                |
| `a5_9`      | SMSA population share aged 5–9                                |
| `a10_14`    | SMSA population share aged 10–14                              |
| `a15_19`    | SMSA population share aged 15–19                              |
| `a20_24`    | SMSA population share aged 20–24                              |
| `a25_29`    | SMSA population share aged 25–29                              |

## Economic variables

| Variable    | Description                                          |
|-------------|------------------------------------------------------|
| `rincpc`    | State real income per capita                         |
| `econgrow`  | Economic growth: Δlog(real income per capita)        |
| `unemp`     | State unemployment rate                              |
| `price`     | CPI (Economic Report of the President)               |

## State and local government spending

| Variable    | Description                                                    |
|-------------|----------------------------------------------------------------|
| `sta_educ`  | Real per capita spending on education (state + local governments) |
| `sta_welf`  | Real per capita spending on welfare (state + local governments)   |

---

**Note:** The variables `jid`, `mayor`, `date_wa`, `date_my`, and `web` come from McCrary (2002). The remaining variables come from Levitt (1997).
