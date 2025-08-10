library(httr)
library(jsonlite)
library(data.table)
library(collapse)

proceseaza <- function(statii) {
  ids <- names(statii)
  
  rez <- rbindlist(lapply(seq_along(statii), function(i) {
    id <- ids[i]
    statie <- statii[[i]]
    
    campuri <- names(statie)[lengths(statie) > 0]
    
    dt_camp <- rbindlist(lapply(campuri, function(camp) {
      if (length(statie[[camp]]) == 0) return(NULL)
      rbindlist(statie[[camp]], use.names = FALSE) |>
        fselect(1:2) |>
        setnames(c("time", "value")) |>
        fmutate(field = camp)
    }), use.names = TRUE, fill = TRUE)
    
    if (nrow(dt_camp)) dt_camp[, id := id]
    dt_camp
  }), use.names = TRUE, fill = TRUE) 
  
  return(rez |> 
    fmutate(across(time:value, \(x) unlist(x))) |> 
    na_omit() |> roworder(time) |> 
    fmutate(id = as.integer(id)))
}

informatii <- function(x){
rbindlist(x, fill=T) |> 
  fmutate(geo=unlist(geo)) |> 
  fmutate(what = rep(c('lat', 'lon'), times=length(x))) |> 
  dcast(idx + utime + name ~ what, value.var='geo') |>
  fmutate(idx = as.integer(gsub('[A-Z]+', "", idx))) |>
  frename(id=idx)
}



bounds <- "25.80,44.30,26.35,44.60"

#apel către waqi
labels <- httr::content(httr::POST("https://api.waqi.info/mapq2/bounds",
                                   body = list(bounds=bounds),
                                   encode = "multipart"))$data

#pune ca csv curățat
labels <- informatii(labels)

fwrite(labels, paste0("data/info/", "labels", Sys.Date(), ".csv"))


#acum măsurătorile stațiilor
ids <- gsub("[A-Z]+", "", labels$id)

statii <- sapply(ids, function(id) content(GET(paste0("https://airnet.waqi.info/airnet/feed/hourly-cached/", id)))$data) 
statii <- proceseaza(statii)
fwrite(statii, paste0("data/statii/", "statii", Sys.Date(), ".csv"))


                 






