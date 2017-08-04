library(tidyverse)
library(stringr)
library(lubridate)

sfclicences <- read_csv("sfclicences.csv") 
mindate <- min(sfclicences$effectiveDate)
sfclicences$endDate[is.na(sfclicences$endDate)] <- Sys.Date()

sfclicencesclean <- sfclicences %>% select(ceref, name, prinCeName, effectiveDate, endDate) %>% distinct
sfclicencesexperience <- sfclicencesclean  %>% arrange(ceref, name, effectiveDate, endDate) %>% group_by(ceref, name) %>% filter(n() > 1) %>% mutate(jobnumber = 1 + c(0, cumsum(lead(as.numeric(effectiveDate))-1 > cummax(as.numeric(endDate)))[-n()])) %>% group_by(jobnumber, add = TRUE) %>% summarise(effectiveDate = first(effectiveDate), endDate = last(endDate)) %>% group_by(ceref, name) %>% filter(max(jobnumber) > 1) 

sfclicencesearlyceref <- sfclicencesexperience %>% filter(effectiveDate == mindate) %>% pull(ceref) %>% unique

sfclicencesnotfirstjob <- sfclicencesexperience %>% filter(!ceref %in% sfclicencesearlyceref)

sfclicencesnotfirstjobquitceref  <- sfclicencesnotfirstjob %>% filter(endDate == Sys.Date()) %>% pull(ceref) %>% unique

sfclicencesnotfirstjobquit <- sfclicencesnotfirstjob %>% filter(!ceref %in% sfclicencesnotfirstjobquitceref) 

sfclicencesnotfirstjobquitnumberofjobs <- sfclicencesnotfirstjobquit %>% group_by(ceref) %>% summarise(numberofjobs = n())

sfclicencesnotfirstjobquittidy <- sfclicencesnotfirstjobquit %>% mutate(daysinjob = as.numeric(endDate - effectiveDate)) %>% inner_join(sfclicencesnotfirstjobquitnumberofjobs) %>% mutate(lastjob = ifelse(jobnumber == numberofjobs, "lastjob", "notlastjob")) %>% group_by(ceref, name, numberofjobs, lastjob) %>% summarise(avgdaysinjob = mean(daysinjob)) %>% spread(lastjob, avgdaysinjob) 

sfclicencesnotfirstjobquittidynooutlierstidy <- sfclicencesnotfirstjobquittidy %>% filter(lastjob > 15) %>% filter(notlastjob > 15)

ggplot(sfclicencesnotfirstjobquittidynooutlierstidy %>% filter(numberofjobs > 7)) + geom_point(aes(x = notlastjob, y = lastjob, colour = factor(numberofjobs)))
