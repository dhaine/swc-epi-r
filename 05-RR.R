
## ----, include = FALSE---------------------------------------------------
source("chunk_options.R")
opts_chunk$set(fig.path = "figure/05-RR-R-", fig.height = 5, fig.width = 10)


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

dairy <- merge(prod.long, health.wide, by = "unique")


## ----, eval = FALSE------------------------------------------------------
## plot(1:10)
## dev.copy(png, file = "myPlot.png")
## dev.off()


## ----, eval = FALSE------------------------------------------------------
## pdf(file = "myPlot.pdf")
## plot(1:10)
## dev.off()


## ----, eval = FALSE------------------------------------------------------
## ggsave("myPlot.pdf")


## ----, eval = FALSE------------------------------------------------------
## source('myScript.R')


## ----, eval = FALSE------------------------------------------------------
dairy.sum <- summary(dairy)
sink("summary.log")
cat("My summary stats", fill = TRUE)
show(dairy.sum)
sink()


## ----, eval = FALSE------------------------------------------------------
dairy.sum <- summary(dairy)
capture.output({dairy.sum}, file = "summary.txt")


## ----, eval = FALSE------------------------------------------------------
library(epicalc)
with(health.wide, cc(da, parity, 
                     decimal = 2, 
                     graph = FALSE, 
                     original = TRUE,
                     design = "cohort", 
                     alpha = .05, 
                     fisher.or = FALSE, 
                     exact.ci.or = TRUE))
## 
## with(health.wide, mhor(da, parity, mf,
##                design = "cohort",
##                decimal = 2,
##                graph = FALSE))


## ----, eval = FALSE------------------------------------------------------
#' Displaced Abomasum

#+ echo=FALSE
health <- read.csv(
   file = "health.csv",
   header = TRUE,
   stringsAsFactors = FALSE)
health$presence <- factor(health$presence, labels = c("No", "Yes"))
health$parity <- cut(health$lactation,
                    breaks = c(0, 1, max(health$lactation)),
                    labels = c("1", "2+"))
library(reshape2)
health.wide <- dcast(
   data = health,
   formula = unique + age + lactation + parity ~ disease,
   value.var = "presence"
   )
library(stringr)
health.wide$id <- str_split_fixed(health.wide$unique, "-", 2)
health.wide$herd <- health.wide$id[, 1]
health.wide$cow <- health.wide$id[, 2]
health.wide$da <- with(health.wide, ifelse(da == "No", 0, 1))

#' The results of the logistic regression are the following:

#+ log, echo=FALSE
mod1 <- glm(da ~ parity,
            family = binomial("logit"),
            data = health.wide)  # "logit" can be omitted as it is the default

#+ log-sum
summary(mod1)


## ----, eval = FALSE------------------------------------------------------
install.packages("knitr", dependencies = TRUE)


## ----, eval = FALSE------------------------------------------------------
library(knitr)
knit("file.Rmd")


## ----, eval = FALSE------------------------------------------------------
## The mean is `r mean(1:5)`.


## ----, eval = FALSE------------------------------------------------------
library(ggplot2)
p <- qplot(carat, price, data = diamonds) + geom_hex()
p
# no need to explicitly print(p)


## ----master, eval = FALSE------------------------------------------------
source("load.R")
source("tidy.R")
source("analyze.R")


## ----load, eval = FALSE--------------------------------------------------
# load
health <- read.csv(
   file = "health.csv",
   header = TRUE,
   stringsAsFactors = FALSE)

health$parity <- cut(health$lactation,
                    breaks = c(0, 1, max(health$lactation)),
                    labels = c("1", "2+"))


## ----tidy, eval = FALSE--------------------------------------------------
# tidy
library(reshape2)
health.wide <- dcast(
   data = health,
   formula = unique + age + lactation + parity ~ disease,
   value.var = "presence"
   )


## ----analyze, eval = FALSE-----------------------------------------------
# analyze
library(epicalc)
with(health.wide, cc(da, parity,
  decimal = 2,
  graph = FALSE,
  original = TRUE,
  design = "cohort",
  alpha = .05,
  fisher.or = FALSE,
  exact.ci.or = TRUE))

