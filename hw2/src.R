library(tidyverse)

ALL3 <- read.csv('data/ALL3.csv') %>%
    subset(ProductDescription != '  UN Special Code') %>%
    subset(select = c(
        'ProductDescription',
        'TradeFlowCode',
        'TradeValue.in.1000.USD',
        'Year'
    ))
ALL5 <- read.csv('data/ALL5.csv') %>%
    subset(ProductDescription != '  UN Special Code') %>%
    subset(select = c(
        'ProductDescription',
        'TradeFlowCode',
        'TradeValue.in.1000.USD',
        'Year'
    ))

industries.ALL3 <- unique(ALL3$ProductDescription)
industries.ALL5 <- unique(ALL5$ProductDescription)
years <- unique(ALL5$Year)

ALL3.imports <- ALL3 %>% filter(TradeFlowCode == 5)
ALL5.imports <- ALL5 %>% filter(TradeFlowCode == 5)
ALL3.exports <- ALL3 %>% filter(TradeFlowCode == 6)
ALL5.exports <- ALL5 %>% filter(TradeFlowCode == 6)

trade.ALL3 <- data.frame('year' = double(),
                         'prod' = character(),
                         'ratio' = double())

trade.ALL5 <- trade.ALL3

time.1 <- system.time({
    for (i in years) {
        for (j in industries.ALL3) {
            imports <- ALL3.imports %>%
                filter(Year == i,
                       ProductDescription == j)
            
            if (dim(imports)[1] == 0) {
                imports <- imports %>%
                    add_row(
                        ProductDescription = j,
                        TradeFlowCode = 5,
                        TradeValue.in.1000.USD = 0,
                        Year = i
                    )
            }
            
            exports <- ALL3.exports %>%
                filter(Year == i,
                       ProductDescription == j)
            
            if (dim(exports)[1] == 0) {
                exports <- exports %>%
                    add_row(
                        ProductDescription = j,
                        TradeFlowCode = 6,
                        TradeValue.in.1000.USD = 0,
                        Year = i
                    )
            }
            
            diff <- abs(imports$TradeValue.in.1000.USD -
                            exports$TradeValue.in.1000.USD)
            
            sum <- imports$TradeValue.in.1000.USD +
                exports$TradeValue.in.1000.USD
            
            ratio <- diff / sum
            
            trade.ALL3 <- trade.ALL3 %>%
                add_row(year = i,
                        prod = j,
                        ratio = ratio)
            
            # cat(i, j, '\n')
        }
    }
})

time.2 <- system.time({
    for (i in years) {
        for (j in industries.ALL5) {
            imports <- ALL5.imports %>%
                filter(Year == i,
                       ProductDescription == j)
            
            if (dim(imports)[1] == 0) {
                imports <- imports %>%
                    add_row(
                        ProductDescription = j,
                        TradeFlowCode = 5,
                        TradeValue.in.1000.USD = 0,
                        Year = i
                    )
            }
            
            exports <- ALL5.exports %>%
                filter(Year == i,
                       ProductDescription == j)
            
            if (dim(exports)[1] == 0) {
                exports <- exports %>%
                    add_row(
                        ProductDescription = j,
                        TradeFlowCode = 6,
                        TradeValue.in.1000.USD = 0,
                        Year = i
                    )
            }
            
            diff <- abs(exports$TradeValue.in.1000.USD -
                            imports$TradeValue.in.1000.USD)
            
            sum <- exports$TradeValue.in.1000.USD +
                imports$TradeValue.in.1000.USD
            
            ratio <- diff / sum
            
            trade.ALL5 <- trade.ALL5 %>%
                add_row(year = i,
                        prod = j,
                        ratio = ratio)
            
            # cat(i, j, '\n')
        }
    }
})

# time.1
# time.2

trade.ALL3 <- na.omit(trade.ALL3)
trade.ALL5 <- na.omit(trade.ALL5)

gl.ALL3 <- aggregate(. ~ year, trade.ALL3[-2], mean)
gl.ALL3$index <- 1 - gl.ALL3$ratio

gl.ALL5 <- aggregate(. ~ year, trade.ALL5[-2], mean)
gl.ALL5$index <- 1 - gl.ALL5$ratio

# colours <- c('Three-digit SITC' = '#1e40ca',
#              'Five-digit SITC' = '#00a2ed')
# 
# ggplot() +
#     geom_line(aes(
#         x = gl.ALL3$year,
#         y = gl.ALL3$index,
#         colour = 'Three-digit SITC'
#     )) +
#     geom_line(aes(
#         x = gl.ALL5$year,
#         y = gl.ALL5$index,
#         colour = 'Five-digit SITC'
#     )) +
#     theme_minimal() +
#     labs(title = 'Grubel and Lloyd Index Weighted Average',
#          x = 'year',
#          y = NULL) +
#     scale_colour_manual(name = '', values = colours) +
#     scale_x_continuous(breaks = seq(1990, 2010, 2))
