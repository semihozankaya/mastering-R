library(scales)
library(binancer)
library(data.table)
library(jsonlite)
library(checkmate)
library(httr)

## Constants
BITCOINS <- 0.42

## Helper Functions

forint <- function(x) {
  dollar(x, prefix = '', suffix = ' HUF')
}

get_usdhuf <- function(retried = 0) {
  tryCatch({
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 250, upper = 400)
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf(retried = retried + 1)
  })
  log_info('1 USD = {forint(usdhuf)}')
  usdhuf
}

get_bitcoin_price <- function(retried = 0) {
  tryCatch({
    btcusdt <- binance_coins_prices()[symbol == 'BTC', usd]
    assert_number(btcusdt, lower = 1000)
  },
  error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_bitcoin_price(retried = retried + 1)
  })
  log_info('The current Bitcoin price is {dollar(btcusdt)}')
  btcusdt
}

##### Production ######

forint(get_bitcoin_price() * get_usdhuf() * BITCOINS)
