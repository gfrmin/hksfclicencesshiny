library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)

sfclicences <- stream_in(file(list.files(pattern = "*.jl$")))

sfclicences8 <- sfclicences[sapply(sfclicences$jsoninfo, ncol) == 8,]
sfclicences8$jsoninfo <- lapply(sfclicences8$jsoninfo, flatten)
sfclicences8 <- unnest(sfclicences8)
sfclicences8[,names(which(sapply(sfclicences8, function(x) {sum(!is.na(x))}) == 0))] <- NULL
sfclicences8 <- unnest(sfclicences8)

sfclicences <- sfclicences8 %>% select(ceref, name, prinCeName, prinCeRef, regulatedActivity.actType, effectiveDate, endDate, lcRole) %>% mutate(lcRole = if_else(lcRole == "RE", "Representative", "Responsible Officer")) %>% mutate(effectiveDate = as.Date(effectiveDate, format = "%b %e, %Y %I:%M:%S"), endDate = as.Date(endDate, format = "%b %e, %Y %I:%M:%S")) 

write.csv(sfclicences, "sfclicences.csv", row.names = FALSE)
