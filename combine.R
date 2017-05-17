library(dplyr)
library(tidyr)
library(readr)

sfclicences <- read_csv("sfclicences.csv")

sfclicencesexperience <- sfclicences %>% select(ceref, name, regulatedActivity.actType, effectiveDate, endDate) %>% mutate(endDate = ifelse(is.na(endDate), Sys.Date(), endDate)) %>% group_by(ceref, name, regulatedActivity.actType) %>% arrange(effectiveDate, endDate) %>% mutate(idx = c(0, cumsum(lead(effectiveDate)-1 > cummax(endDate))[-n()])) %>% group_by(idx, add = TRUE) %>% summarise(effectiveDate = as.Date(first(effectiveDate), "1970-01-01"), endDate = as.Date(last(endDate), "1970-01-01")) %>% group_by(ceref, name, regulatedActivity.actType) %>% arrange(idx) %>% mutate(timeinlicence = endDate - effectiveDate) %>% summarise(timeinlicence = sum(timeinlicence), timesincelastlicence = Sys.Date() - max(endDate)) %>% gather(variable, value, timeinlicence, timesincelastlicence) %>% unite(temp, variable, regulatedActivity.actType) %>% spread(temp, value)

sfclicencestidy <- sfclicences %>% mutate(licence = "!") %>% spread(regulatedActivity.actType, licence, fill = "") %>% mutate(endDate = ifelse(is.na(endDate), Sys.Date(), endDate)) # spread those licences

sfclicencesjobtime <- sfclicencestidy  %>% group_by(ceref, name) %>% arrange(effectiveDate, endDate) %>% mutate(idx = c(0, cumsum(lead(effectiveDate)-1 > cummax(endDate))[-n()])) %>% group_by(ceref, name, idx) %>% summarise(effectiveDate = as.Date(first(effectiveDate), "1970-01-01"), endDate = as.Date(last(endDate), "1970-01-01")) %>% group_by(ceref, name) %>% arrange(idx) %>% mutate(timeinjob = endDate - effectiveDate, timebeforenextjob = lead(effectiveDate) - endDate) %>% summarise(numberofjobs = n(), meantimeinjob = mean(timeinjob), meantimebeforenextjob = mean(timebeforenextjob, na.rm = TRUE), timesincelastjob = Sys.Date() - max(endDate), timeinlatestjob = last(timeinjob))

sfcroletime <- sfclicencestidy %>% group_by(ceref, name, lcRole) %>% arrange(effectiveDate, endDate) %>% mutate(idx = c(0, cumsum(lead(effectiveDate)-1 > cummax(endDate))[-n()])) %>% group_by(ceref, name, lcRole, idx) %>% summarise(effectiveDate = as.Date(first(effectiveDate), "1970-01-01"), endDate = as.Date(last(endDate), "1970-01-01")) %>% group_by(ceref, name, lcRole) %>% arrange(idx) %>% mutate(timeinrole = endDate - effectiveDate) %>% summarise(timeinlatestrole = last(timeinrole), timeinrole = sum(timeinrole), timesincelastrole = Sys.Date() - max(endDate)) %>% gather(variable, value, timeinrole, timesincelastrole, timeinlatestrole) %>% unite(temp, variable, lcRole) %>% spread(temp, value)

sfcall <- sfclicencesexperience %>% inner_join(sfclicencesjobtime) %>% inner_join(sfcroletime)

write_csv(sfcall, path = "sfcall.csv")
