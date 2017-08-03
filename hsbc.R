library(tidyverse)
library(stringr)
library(lubridate)

sfclicences <- read_csv("sfclicences.csv") 
# mindate <- min(sfclicences$effectiveDate)
sfclicences$endDate[is.na(sfclicences$endDate)] <- Sys.Date()

hsbcprinces <- sfclicences %>% pull(prinCeName) %>% str_subset("HSBC") %>% unique
hsbcceref <- sfclicences %>% filter(prinCeName %in% hsbcprinces) %>% pull(ceref) %>% unique
hsbclicences <- sfclicences %>% filter(ceref %in% hsbcceref)
write_csv(hsbclicences, path = "hsbclicences.csv")

hsbclicencesclean <- hsbclicences %>% select(ceref, name, prinCeName, effectiveDate, endDate) %>% mutate(prinCeName = ifelse(str_detect(prinCeName, "HSBC"), 1, 0)) %>% rename(hsbc = prinCeName) %>% distinct
hsbclicencesexperience <- hsbclicencesclean  %>% arrange(ceref, name, effectiveDate, endDate) %>% group_by(ceref, name) %>% filter(n() > 1) %>% mutate(jobnumber = 1 + c(0, cumsum(lead(as.numeric(effectiveDate))-1 > cummax(as.numeric(endDate)))[-n()])) %>% group_by(jobnumber, hsbc, add = TRUE) %>% summarise(effectiveDate = first(effectiveDate), endDate = last(endDate)) %>% group_by(ceref, name) %>% filter(max(jobnumber) > 1)
