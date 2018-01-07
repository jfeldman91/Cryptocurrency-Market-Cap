# Cryptocurrency Market Cap

library(coinmarketcapr)
library(ggplot2)
library(treemap)

# Convert character variables to numeric variables

market_today <- get_marketcap_ticker_all()
market_today$price_usd <- as.numeric(market_today$price_usd)
market_today$available_supply <- as.numeric(market_today$available_supply)
market_today$market_cap_usd <- as.numeric(market_today$market_cap_usd)

# Add logarithmic variables

market_today$log_price_usd <- log(market_today$price_usd)
market_today$log_available_supply <- log(market_today$available_supply)

# Bubble chart

ggplot(data = market_today, aes(x = log_available_supply,
                                y = log_price_usd,
                                size = market_cap_usd,
                                fill = log(market_cap_usd),
                                color = log(market_cap_usd),
                                label = name)) +
  geom_point(shape = 21) + 
  geom_smooth(method = "lm", se = FALSE, color = "Black") +
  labs(x = "Log of Available Supply",
       y = "Log of Price (USD)",
       size = "Market Cap (USD)",
       fill = "Log of Market Cap (USD)") +
  scale_size_area(max_size = 20) +
  theme(legend.position = "none") +
  annotate(geom = "text",
           x = market_today$log_available_supply[market_today$name == "Bitcoin"],
           y = market_today$log_price_usd[market_today$name == "Bitcoin"],
           label = "Bitcoin [BTC]",
           fontface = "bold") +
  annotate(geom = "text",
           x = market_today$log_available_supply[market_today$name == "Ethereum"],
           y = market_today$log_price_usd[market_today$name == "Ethereum"],
           label = "Ethereum [ETH]",
           fontface = "bold") +
  annotate(geom = "text",
           x = market_today$log_available_supply[market_today$name == "Ripple"],
           y = market_today$log_price_usd[market_today$name == "Ripple"],
           label = "Ripple [XRP]",
           fontface = "bold") +
  annotate(geom = "text",
          x = market_today$log_available_supply[market_today$name == "Bitcoin Cash"],
          y = market_today$log_price_usd[market_today$name == "Bitcoin Cash"],
          label = "Bitcoin Cash [BCH]",
          fontface = "bold") +
  annotate(geom = "text",
           x = market_today$log_available_supply[market_today$name == "Litecoin"],
           y = market_today$log_price_usd[market_today$name == "Litecoin"],
           label = "Litecoin [LTC]",
           fontface = "bold")

# Treemap

df1 <- na.omit(market_today[,c('name','market_cap_usd')])
df1$market_cap_usd <- as.numeric(df1$market_cap_usd)
df1$formatted_market_cap <- paste0(df1$name,'\n','$',
                                   format(round(df1$market_cap_usd / 1e9, 1), trim = TRUE), "bn")

treemap(df1,
        index = 'formatted_market_cap',
        vSize = 'market_cap_usd',
        vColor = 'market_cap_usd',
        title = 'Cryptocurrency Market Cap',
        fontsize.labels = 10,
        lowerbound.cex.labels= .1,
        type = "dens",
        palette = "Blues")
