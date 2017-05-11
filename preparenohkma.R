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

sfclicencestidy <- sfclicences %>% mutate(licence = "!") %>% spread(regulatedActivity.actType, licence, fill = "") 

sfclicencescompressed <- sfclicencestidy %>% group_by(ceref, name, prinCeName, prinCeRef) %>% summarise(effectiveDate = min(effectiveDate), endDate = max(endDate), lcRole = ifelse(any(lcRole == "Responsible Officer"), "Representative", "Responsible Officer"), `1` = ifelse(any(`1` == "!"), "!", ""), `2` = ifelse(any(`2` == "!"), "!", ""), `3` = ifelse(any(`3` == "!"), "!", ""), `4` = ifelse(any(`4` == "!"), "!", ""), `5` = ifelse(any(`5` == "!"), "!", ""), `6` = ifelse(any(`6` == "!"), "!", ""), `7` = ifelse(any(`7` == "!"), "!", ""), `8` = ifelse(any(`8` == "!"), "!", ""), `9` = ifelse(any(`9` == "!"), "!", ""), `10` = ifelse(any(`10` == "!"), "!", "")) %>% arrange(ceref) %>% group_by(ceref, name, effectiveDate, endDate) %>% summarise(`1` = ifelse(any(`1` == "!"), "!", ""), `2` = ifelse(any(`2` == "!"), "!", ""), `3` = ifelse(any(`3` == "!"), "!", ""), `4` = ifelse(any(`4` == "!"), "!", ""), `5` = ifelse(any(`5` == "!"), "!", ""), `6` = ifelse(any(`6` == "!"), "!", ""), `7` = ifelse(any(`7` == "!"), "!", ""), `8` = ifelse(any(`8` == "!"), "!", ""), `9` = ifelse(any(`9` == "!"), "!", ""), `10` = ifelse(any(`10` == "!"), "!", ""))

# write.csv(sfclicencestidy, "sfclicences.csv", row.names = FALSE)
