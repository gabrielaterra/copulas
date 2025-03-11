library(quantmod)
library(PerformanceAnalytics)
library(copula)
library(dplyr)
library(ggplot2)
library(MASS)
library(copula)
library(VineCopula)
library(lattice)
library(tidyr)
library(purrr)
library(scatterplot3d)
library(grid)
set.seed(235)

# Downloading the relevant data 
ativos <- c("^GSPC", "^STOXX50E", "^N225", "EEM", "^BVSP", "000001.SS",  # Ações
            "EURUSD=X", "USDJPY=X", "GBPUSD=X", "USDBRL=X", "USDCNY=X", "USDINR=X",  # Moedas
            "BZ=F", "CL=F", "GC=F", "SI=F", "ZS=F", "ZC=F", "KC=F")  # Commodities

bonds_fred <- c("DGS10")  # US Treasury 10Y

dados <- list()
bonds <- list()

# Adjusted Prices Yahoo Finance
for (ativo in ativos) {
  dados[[ativo]] <- getSymbols(ativo, src = "yahoo", from = "2010-01-01", auto.assign = FALSE)
}
precos <- do.call(merge, lapply(dados, function(x) Cl(x)))
colnames(precos) <- ativos

precos_df <- data.frame(Date = index(precos), coredata(precos))
precos_df$Date <- as.Date(precos_df$Date)
precos_df <- na.omit(precos_df)

if (!"Date" %in% colnames(precos_df)) {
  precos_df <- data.frame(Date = index(precos_df), coredata(precos_df))
}
for (bond in bonds_fred) {
  bonds[[bond]] <- getSymbols(bond, src = "FRED", from = "2010-01-01", auto.assign = FALSE)
}

bonds_df <- data.frame(Date = index(bonds[[1]]), coredata(bonds[[1]]))
colnames(bonds_df) <- c("Date", bonds_fred)


bonds_df$Date <- as.Date(bonds_df$Date)
bonds_df <- na.omit(bonds_df)

bonds_df <- bonds_df %>% filter(Date %in% precos_df$Date)
precos_df <- precos_df %>% filter(Date %in% bonds_df$Date)

# Merge pelo Date
dados_completos <- full_join(precos_df, bonds_df, by = "Date")
dados_completos <- na.omit(dados_completos)
dados_completos[,-1] <- sapply(dados_completos[,-1], function(x) as.numeric(as.character(x)))

retornos_df <- dados_completos %>%
  arrange(Date) %>%
  mutate(across(-Date, ~ log(.x / lag(.x)))) %>%
  na.omit()

# Defining COVID periods
retornos_df <- retornos_df %>%
  mutate(Period = case_when(
    Date < as.Date("2020-01-01") ~ "Pre-COVID",
    Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31") ~ "COVID",
    Date > as.Date("2021-12-31") ~ "Post-COVID"
  ))

#### ---- Creating the pairs ----

asset_pairs <- tribble(
  ~Asset1, ~Asset2,
  "X.GSPC", "X.STOXX50E",
  "X.GSPC", "X.N225",
  "X.GSPC", "X.BVSP",
  "EURUSD.X", "USDJPY.X",
  "EURUSD.X", "GBPUSD.X",
  "EURUSD.X", "USDCNY.X",
  "DGS10", "X.GSPC",
  "DGS10", "X.STOXX50E",
  "DGS10", "USDBRL.X",
  "BZ.F", "CL.F",
  "GC.F", "DGS10",
  "GC.F", "SI.F",
  "ZS.F", "ZC.F",
  "ZS.F", "KC.F",
  "ZC.F", "KC.F"
)
