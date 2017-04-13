library(jsonlite)
library(tidyr)

hksfclicences <- stream_in(file(list.files(pattern = "*.jl")))

write.csv(hksfclicences, file = "hksfclicences.csv", row.names = FALSE, na = "")
