library(Quandl)
gdp = Quandl("FRED/USARGDPR", type="ts", start_date = "1980-1-1", collapse = "annual")
int = Quandl("FRED/DTB3", sort="asc", type='ts', collapse = "annual")
cons = Quandl("WORLDBANK/USA_NE_CON_PRVT_PP_CD", 'ts', sort='asc')

dlogcons = diff(log(cons))
int=window(int, 1981, 2012)
int
cons
plot(int, dlogcons)
lm(int ~ dlogcons)

# 3 month Fred rate
