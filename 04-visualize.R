
## ----, include = FALSE---------------------------------------------------
source("chunk_options.R")
opts_chunk$set(fig.path = "figure/04-viz-R-", fig.height = 5, fig.width = 10)


## ----, echo = FALSE------------------------------------------------------
health <- read.csv(
    file = "health.csv",
    header = TRUE,
    stringsAsFactors = FALSE)

health$presence <- factor(health$presence, labels = c("No", "Yes"))
health$parity <- cut(health$lactation,
                     breaks = c(0, 1, max(health$lactation)),
                     labels = c("1", "2+"))

prod <- read.csv(
    file = "prod.csv",
    header = TRUE,
    stringsAsFactors = FALSE
    )

library(reshape2)
prod.long <- melt(
    data = prod,
    id = "unique",
    measure.vars = 2:11,
    variable.name = "test",
    value.name = "milk"
    )

prod.long2 <- melt(
    data = prod,
    id = "unique",
    measure.vars = 12:21,
    variable.name = "test",
    value.name = "dim"
    )

health.wide <- dcast(
    data = health,
    formula = unique + age + lactation + parity ~ disease,
    value.var = "presence"
    )

library(stringr)
health.wide$id <- str_split_fixed(health.wide$unique, "-", 2)
health.wide$herd <- health.wide$id[, 1]
health.wide$cow <- health.wide$id[, 2]

health.prod <- merge(health.wide, prod, by = "unique")

library(plyr)
prod.long <- mutate(prod.long, test = sub("milk.", "", test))
prod.long2 <- mutate(prod.long2, test = sub("dim.", "", test))

prod.long <- join(prod.long, prod.long2, by = c("unique", "test"))

prod.long <- arrange(prod.long, unique, test)


## ----plot1---------------------------------------------------------------
plot(milk ~ dim, data = prod.long)


## ------------------------------------------------------------------------
trend <- lm(milk ~ dim, data = prod.long)


## ------------------------------------------------------------------------
plot(milk ~ dim, data = prod.long)
abline(trend)


## ----, warning=FALSE, message=FALSE--------------------------------------
library(ggplot2)
qplot(dim, milk, data = prod.long, geom = "point")


## ----, warning=FALSE-----------------------------------------------------
dairy <- merge(prod.long, health.wide, by = "unique")
ggplot(dairy, aes(x = dim, y = milk)) +
    geom_point(aes(color = parity)) +
    geom_smooth(method = 'lm')


## ----, warning=FALSE-----------------------------------------------------
ggplot(dairy, aes(x = dim, y = milk)) +
    geom_point(aes(color = parity, shape = mf)) +
    geom_smooth(method = 'lm')


## ----layer1, warning=FALSE-----------------------------------------------
layer_point <- geom_point(
    mapping = aes(x = dim, y = milk, color = parity),
    data = dairy,
    size = 3
)
ggplot() + layer_point


## ----pig_boxplot, echo = FALSE, warning = FALSE--------------------------
ggplot(dairy, aes(mf, milk)) +
    geom_boxplot() +
    geom_jitter(aes(colour = parity), alpha = .5) +
    stat_summary(fun.y = mean, geom = "point", shape = 3, size = 4)    


## ----facet-wrap, warning = FALSE-----------------------------------------
ggplot(dairy, aes(x = dim, y = milk)) +
    geom_point(aes(color = parity)) +
    geom_smooth(method = 'lm') +
    facet_wrap(~ mf)


## ----facet-grid, warning = FALSE-----------------------------------------
ggplot(dairy, aes(x = dim, y = milk)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    facet_grid(mf ~ parity)

