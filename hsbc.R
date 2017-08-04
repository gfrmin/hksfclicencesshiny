library(tidyverse)
library(stringr)
library(lubridate)

sfclicences <- read_csv("sfclicences.csv") 
mindate <- min(sfclicences$effectiveDate)
sfclicences$endDate[is.na(sfclicences$endDate)] <- Sys.Date()

hsbcprinces <- sfclicences %>% pull(prinCeName) %>% str_subset("HSBC|Halbis") %>% unique
hsbcceref <- sfclicences %>% filter(prinCeName %in% hsbcprinces) %>% pull(ceref) %>% unique
hsbclicences <- sfclicences %>% filter(ceref %in% hsbcceref)
write_csv(hsbclicences, path = "hsbclicences.csv")

hsbclicencesclean <- hsbclicences %>% select(ceref, name, prinCeName, effectiveDate, endDate) %>% mutate(prinCeName = ifelse(str_detect(prinCeName, "HSBC|Halbis"), "hsbc", "nothsbc")) %>% rename(hsbc = prinCeName) %>% distinct
hsbclicencesexperience <- hsbclicencesclean  %>% arrange(ceref, name, effectiveDate, endDate) %>% group_by(ceref, name) %>% filter(n() > 1) %>% mutate(jobnumber = 1 + c(0, cumsum(lead(as.numeric(effectiveDate))-1 > cummax(as.numeric(endDate)))[-n()])) %>% group_by(jobnumber, hsbc, add = TRUE) %>% summarise(effectiveDate = first(effectiveDate), endDate = last(endDate)) %>% group_by(ceref, name) %>% filter(max(jobnumber) > 1) 

hsbcfirstceref <- hsbclicencesexperience %>% filter(jobnumber == 1 & hsbc == "hsbc") %>% pull(ceref) %>% unique
hsbcearlyceref <- hsbclicencesexperience %>% filter(effectiveDate == mindate) %>% pull(ceref) %>% unique

hsbclicencesnotfirstjob <- hsbclicencesexperience %>% filter(!ceref %in% union(hsbcfirstceref, hsbcearlyceref))

hsbclicenceslastjob <- hsbclicencesnotfirstjob %>% filter(hsbc == "hsbc") %>% group_by(ceref) %>% summarise(lasthsbcjob = max(jobnumber))

hsbclicencesnotfirstjobhsbclastjob <- hsbclicencesnotfirstjob %>% inner_join(hsbclicenceslastjob) %>% filter(!jobnumber > lasthsbcjob) %>% group_by(ceref) %>% filter(sum(hsbc == "hsbc") == 1) # only worked for HSBC once

hsbclicencesnotfirstjobhsbclastjobquitceref  <- hsbclicencesnotfirstjobhsbclastjob %>% filter(endDate == Sys.Date()) %>% pull(ceref) %>% unique

hsbclicencesnotfirstjobhsbclastjobquit <- hsbclicencesnotfirstjobhsbclastjob %>% filter(!ceref %in% hsbclicencesnotfirstjobhsbclastjobquitceref) 

hsbclicencesnotfirstjobhsbclastjobnumberofjobs <- hsbclicencesnotfirstjobhsbclastjobquit %>% group_by(ceref) %>% summarise(numberofnonhsbcjobs = n() - 1)

hsbclicencesnotfirstjobhsbclastjobquittidy <- hsbclicencesnotfirstjobhsbclastjobquit %>% mutate(daysinjob = endDate - effectiveDate) %>% inner_join(hsbclicencesnotfirstjobhsbclastjobnumberofjobs) %>% group_by(ceref, name, numberofnonhsbcjobs, hsbc) %>% summarise(avgdaysinjob = mean(daysinjob)) %>% spread(hsbc, avgdaysinjob)

ggplot(hsbclicencesnotfirstjobhsbclastjobquittidy %>% filter(numberofnonhsbcjobs > 2)) + geom_point(aes(x = nothsbc, y = hsbc, colour = factor(numberofnonhsbcjobs)))