library(epicalc)
with(health.wide, cc(da, parity,
   decimal = 2,
   graph = FALSE, 
   original = TRUE,
   design = "cohort",
   alpha = .05,
   fisher.or = FALSE, 
   exact.ci.or = TRUE))

with(health.wide, mhor(da, parity, mf,
               design = "cohort",
               decimal = 2,
               graph = FALSE))
