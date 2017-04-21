library(data.table)
library(dplyr)
library(jsonlite)
library(tidyr)

sfclicences <- stream_in(file(list.files(pattern = "*.jl$")))

sfclicences9 <- sfclicences[sapply(sfclicences$jsoninfo, ncol) == 9,]
sfclicences9 <- unnest(sfclicences9)

sfclicences8 <- sfclicences[sapply(sfclicences$jsoninfo, ncol) == 8,]
sfclicences8$jsoninfo <- lapply(sfclicences8$jsoninfo, flatten)
sfclicences8 <- unnest(sfclicences8)
sfclicences8[,names(which(sapply(sfclicences8, function(x) {sum(!is.na(x))}) == 0))] <- NULL
sfclicences8 <- unnest(sfclicences8)

sfclicences8 <- sfclicences8 %>% select(ceref, name, prinCeName, regulatedActivity.actType, effectiveDate, endDate, lcRole)
sfclicences9 <- sfclicences9 %>% select(ceref, name, fullName, actType, effDate) %>% mutate(endDate = NA, lcRole = "EO") 

hkma <- stream_in(file("hkma.json"))

sfclicences <- rbindlist(list(sfclicences8, sfclicences9))


sfclicencestidy <- sfclicences %>% mutate(licence = "✔") %>% mutate(effectiveDate = as.Date(effectiveDate, format = "%b %e, %Y %I:%M:%S"), endDate = as.Date(endDate, format = "%b %e, %Y %I:%M:%S")) %>% spread(regulatedActivity.actType, licence, fill = "") 

write.csv(sfclicencestidy, "hksfclicences.csv", row.names = FALSE)
