UnlikelyTools::set_wd("unlikelyvolcano.github.io")


library(rgdal)
library(purrr)
library(plyr)
library(RColorBrewer)
library(animation)

jhu_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/"
daily_base <- paste0(jhu_url, "csse_covid_19_daily_reports/%s.csv")

td <- as.integer(Sys.Date() - as.Date("2020-03-21"))
if (Sys.time() < paste(Sys.Date(), "20:30:00 EDT")) {
  tx <- td - 1
} else {
  tx <- td
}

day <- c()
daily <- list()
for (i in 1:(tx)) {
  day[i] <- format.Date(Sys.Date() - td + i, "%m-%d-%Y")
  daily_url <- sprintf(daily_base, day[i])
  temp <- read.csv(daily_url, stringsAsFactors = F)
  temp <- temp[temp$Country_Region %in% "US", ]
  temp$FIPS <- sprintf("%05d", temp$FIPS)
  temp <- temp[!duplicated(temp$FIPS), c("FIPS", "Confirmed")]
  names(temp) <- c("FIPS", paste(names(temp)[-1], day[i], sep = "_"))
  daily[[day[i]]] <- temp
}

combined <- reduce(daily, join)

combined[, -1] <- apply(combined[, -1], 2, function(x) {ifelse(x > 0, log(x), 0)})

for (i in 3:ncol(combined)) {
  combined[is.na(combined[, i]),  i] <- combined[is.na(combined[ , i]), i - 1]
  combined[combined[ , i] < combined[ , i - 1], i] <- combined[combined[ , i] < combined[ , i - 1], i - 1]
}

brks <-
  scales::cbreaks(c(min(unlist(combined[, -1])), max(unlist(combined[, -1]))))$breaks

combined[, -1] <-
  apply(combined[, -1], 2, function(x) {
    cut(x, breaks = brks, labels = brewer.pal(4, "Reds"))
  })


us_map <- readRDS("data/us_counties.RDS")


us_cases <- merge(us_map, combined, by.x = "GEOID", by.y = "FIPS")
x <- c(43, 72, 52, 78, 74, 69, 76, 79, 95, 70, 86, 67, 89, 68, 71, 14, 66, 07, 64, 60, 03, 84, 15, 02)
us_cases <- us_cases[!as.numeric(us_cases$STATEFP) %in% x,]

saveGIF({
  for (d in day) {
    plot(
      us_cases,
      col = us_cases[[paste0("Confirmed_", d)]],
      border = "gray87",
      sub = d,
      cex.sub = 4
    )
  }
}, movie.name = "spread.gif", ani.width = 2000, ani.height = 1200, interval = 0.25)
