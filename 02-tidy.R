
## ----, include = FALSE---------------------------------------------------
source("chunk_options.R")
opts_chunk$set(fig.path = "figure/02-tidy-R-", fig.height = 5, fig.width = 10)


## ----, echo=FALSE--------------------------------------------------------
health <- read.csv(
    file = "health.csv",
    header = TRUE,
    stringsAsFactors = FALSE)

health$presence <- factor(health$presence, labels = c("No", "Yes"))
health$parity <- cut(health$lactation,
                     breaks = c(0, 1, max(health$lactation)),
                     labels = c("1", "2+"))


## ----import-prod---------------------------------------------------------
prod <- read.csv(
    file = "prod.csv",
    header = TRUE,
    stringsAsFactors = FALSE
    )


## ----melt----------------------------------------------------------------
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

# combining melt and dcast
prod.l1 <- melt(prod, id = "unique")
library(stringr)
prod.l1 <- transform(prod.l1, month = str_replace(variable, "^.*\\.", ""),
                    variable = str_replace(variable, "\\..*$", ""))
prod.l2 <- dcast(prod.l1, unique + month ~ variable)


## ----cast----------------------------------------------------------------
health.wide <- dcast(
    data = health,
    formula = unique + age + lactation + parity ~ disease,
    value.var = "presence"
    )


## ----herd----------------------------------------------------------------
library(stringr)
health.wide$id <- str_split_fixed(health.wide$unique, "-", 2)
health.wide$herd <- health.wide$id[, 1]
health.wide$cow <- health.wide$id[, 2]


## ----merge---------------------------------------------------------------
health.prod <- merge(health.wide, prod, by = "unique")


## ----mutate--------------------------------------------------------------
library(plyr)
prod.long <- mutate(prod.long, test = sub("milk.", "", test))
prod.long2 <- mutate(prod.long2, test = sub("dim.", "", test))

prod.long <- join(prod.long, prod.long2, by = c("unique", "test"))

prod.long <- arrange(prod.long, unique, test)


