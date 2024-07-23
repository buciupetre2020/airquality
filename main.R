library(httr)
library(jsonlite)

bounds <- "13.68,46.94,36.10,55.30"

statii <- httr::content(httr::POST("https://api.waqi.info/mapq2/bounds",
                                   body = list(bounds=bounds),
                                   encode = "multipart"))$data

jsonlite::write_json(statii, paste0("data/", "statii", Sys.Date(), ".json"))

ids <- sapply(statii, function(x) gsub("[A-Z]+", "", x$id))

#get station information
info <- sapply(ids, function(id) content(GET(paste0("https://airnet.waqi.info/airnet/feed/hourly-cached/", id)))$data) 
write_json(info, paste0("data/", "info", Sys.Date(), ".json"))






