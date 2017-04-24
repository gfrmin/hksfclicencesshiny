library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)

sfclicences <- stream_in(file(list.files(pattern = "*.jl$")))

sfclicences9 <- sfclicences[sapply(sfclicences$jsoninfo, ncol) == 9,]
sfclicences9 <- unnest(sfclicences9)

sfclicences8 <- sfclicences[sapply(sfclicences$jsoninfo, ncol) == 8,]
sfclicences8$jsoninfo <- lapply(sfclicences8$jsoninfo, flatten)
sfclicences8 <- unnest(sfclicences8)
sfclicences8[,names(which(sapply(sfclicences8, function(x) {sum(!is.na(x))}) == 0))] <- NULL
sfclicences8 <- unnest(sfclicences8)

sfclicences8 <- sfclicences8 %>% select(ceref, name, prinCeName, regulatedActivity.actType, effectiveDate, endDate, lcRole) %>% mutate(lcRole = if_else(lcRole == "RE", "Representative", "Responsible Officer"))
sfclicences9 <- sfclicences9 %>% select(ceref, name, fullName, actType, effDate) %>% mutate(endDate = NA, lcRole = "Executive Officer") 
colnames(sfclicences9) <- colnames(sfclicences8)

sfclicences <- bind_rows(list(sfclicences8, sfclicences9)) %>% mutate(effectiveDate = as.Date(effectiveDate, format = "%b %e, %Y %I:%M:%S"), endDate = as.Date(endDate, format = "%b %e, %Y %I:%M:%S")) 

hkma <- stream_in(file("hkma.json")) 
hkma <- hkma %>% filter(grepl("^[A-Z][A-Z, '.-]+ \\([A-Z0-9]+\\)", name)) # data validation
hkma$ceref <- str_match(hkma$name, "\\(([^\\)]*?)\\)$")[,2] # extract ceref
hkma$name <- str_match(hkma$name, "(.*?) \\(([^\\)]*?)\\)$")[,2] # delete ceref from name
hkma$acttype <- as.numeric(str_match(hkma[,"acttype"], "Type (\\d+):")[,2])
hkma <- hkma %>% mutate(startdate = as.Date(startdate, format = "%d/%m/%Y"), enddate = as.Date(enddate, format = "%d/%m/%Y")) 
hkma <- hkma %>% select(ceref, name, instname, acttype, startdate, enddate, role)
colnames(hkma) <- colnames(sfclicences)
hkma <- unique(hkma)

sfclicences <- bind_rows(sfclicences, hkma)

sfclicencestidy <- sfclicences %>% mutate(licence = "✔") %>% spread(regulatedActivity.actType, licence, fill = "") 

write.csv(sfclicencestidy, "hksfclicences.csv", row.names = FALSE)
