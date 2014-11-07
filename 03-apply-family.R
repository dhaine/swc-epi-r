
## ----, include = FALSE---------------------------------------------------
source("chunk_options.R")
opts_chunk$set(fig.path = "figure/03-apply-R-")


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

prod.l1 <- melt(prod, id = "unique")
library(stringr)
prod.l1 <- transform(prod.l1, month = str_replace(variable, "^.*\\.", ""),
                    variable = str_replace(variable, "\\..*$", ""))
prod.l2 <- dcast(prod.l1, unique + month ~ variable)

health.wide <- dcast(
    data = health,
    formula = unique + age + lactation + parity ~ disease,
    value.var = "presence"
    )

health.wide$id <- str_split_fixed(health.wide$unique, "-", 2)
health.wide$herd <- health.wide$id[, 1]
health.wide$cow <- health.wide$id[, 2]

health.prod <- merge(health.wide, prod, by = "unique")

library(plyr)
prod.long <- mutate(prod.long, test = sub("milk.", "", test))
prod.long2 <- mutate(prod.long2, test = sub("dim.", "", test))

prod.long <- join(prod.long, prod.long2, by = c("unique", "test"))

prod.long <- arrange(prod.long, unique, test)


## ----, eval = FALSE------------------------------------------------------
## apply
## by
## lapply
## tapply
## sapply


## ------------------------------------------------------------------------
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
m
# 1 is the row index
# 2 is the column index
apply(m, 1, sum)  # row totals
apply(m, 2, sum)  # column totals
apply(m, 1, mean)
apply(m, 2, mean)


## ------------------------------------------------------------------------
by(prod.long$milk, prod.long[, "test"], summary)


## ------------------------------------------------------------------------
by(prod.long$milk, prod.long[, "test"], function(x) mean(x, na.rm = TRUE))


## ------------------------------------------------------------------------
tapply(health.wide$age, health.wide$lactation, mean)


## ------------------------------------------------------------------------
my_list <- list(a = 1:10, b = 2:20)
my_list
lapply(my_list, mean)


## ------------------------------------------------------------------------
my_list
x <- sapply(my_list, mean)
x
class(x)


## ------------------------------------------------------------------------
replicate(10, rnorm(10))
replicate(10, rnorm(10), simplify = TRUE)


## ------------------------------------------------------------------------
list_1 <- list(a = c(1:10), b = c(11:20))
list_1
list_2 <- list(c = c(21:30), d = c(31:40))
list_2
mapply(sum, list_1$a, list_1$b, list_2$c, list_2$d)


## ------------------------------------------------------------------------
aggregate(age ~ lactation + da, data = health.wide, FUN = mean)


## ------------------------------------------------------------------------
t1 <- table(health.wide$parity)
t1
t2 <- table(health.wide$parity, health.wide$da)
t2

prop.table(t1)
prop.table(t2)
prop.table(t2, margin = 2)


## ------------------------------------------------------------------------
tab <- rbind(c(200, 133), c(200, 80))
rownames(tab) <- c("Gloves", "No gloves")
colnames(tab) <- c("Mastitis", "No mastitis")

row.tot <- apply(tab, 1, sum)
risk <- tab[, "Mastitis"] / row.tot
risk.ratio <- risk / risk[2]
odds <- risk / (1 - risk)
odds.ratio <- odds / odds[2]
rbind(risk, risk.ratio, odds, odds.ratio)


## ------------------------------------------------------------------------
lact <- ddply(health.wide,
              .(herd, parity),
              summarize,
              mean.age = mean(age)
              )


