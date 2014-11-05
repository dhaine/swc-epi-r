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
