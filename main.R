library(httr)
library(jsonlite)
library(data.table)
library(collapse)
library(ggplot2)
library(lubridate)
library(hrbrthemes)


#pentru fourier features
transforma <- function(x, k=24, f=sin) {
  x <- {as.integer(x)/3600} - min(as.integer(x)/3600) 
  f(2 * pi * x/k) 
}


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

#curatam valori problematice, salvam ultimele 72 ore
statii <- statii |> 
  fmutate(time = lubridate::parse_date_time(time, orders='ymdHMS') + lubridate::hours(2)) |> 
  fgroup_by(field, id) |> roworder(time) |>
  fmutate(pos = as.integer(time)/3600) |> fungroup() |>
  fsubset(pos > max(pos) - 72) |> fselect(-pos) |> roworder(field, id, time)
                 
                 
fwrite(statii, paste0("data/statii/", "statii", Sys.Date(), ".csv"))

zile <- list.files('data/statii/', pattern='*csv')
zile <- zile[{length(zile)-5}:length(zile)]

statii_last <- rbindlist(lapply(paste0("data/statii/", zile), fread)) |> funique()
                 
grafic <- statii_last |> fsubset(field %in% c('met.t', 'met.p', 'met.h', 'pm25', 'co2', 'ch2o')) |>
  fgroup_by(id, field) |>
  fmutate(medie = fmean(value)) |>
  fungroup() |> 
  ggplot(aes(x=time, y=value, group=id)) + 
  geom_line() + 
  geom_smooth(method='lm', aes(x=time, y=value), se=FALSE, colour='red3', linewidth=1,
            formula = y ~ transforma(x, k=24, f=sin) + transforma(x, k=24, f=cos) + 
                          transforma(x, k=8, sin) + transforma(x, k=8, cos) +
                          as.factor(lubridate::day(x)), 
            inherit.aes = FALSE) + scale_y_log10() +
  facet_wrap(~field, ncol=3, scales='free') +
  labs(title = paste0('Poluare București, ', Sys.Date()), x='Oră', y='Valoare') +
  hrbrthemes::theme_ipsum_es()

link <- paste0("data/grafic_", Sys.Date(), ".png")

#scrie graficul
png(link, res=300, width=3200, height=2000)
print(grafic)
dev.off()


payload <- list(
  text = "📷 Poluarea azi în București",
  attachments = list(
    list(
      fallback = "GitHub Actions + AQICN API",
      image_url = paste0("https://raw.githubusercontent.com/buciupetre2020/airquality/refs/heads/main/", link)
    )
  )
)

POST(
  url = 'https://hooks.slack.com/services/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',
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)






