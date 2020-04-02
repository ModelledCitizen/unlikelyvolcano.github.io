UnlikelyTools::set_wd("unlikelyvolcano.github.io")

library(leaflet)
library(rgdal)
library(rgeos)

jhu_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/"
jhu_daily_base <- paste0(jhu_url, "csse_covid_19_daily_reports/%s.csv")
tryCatch({
  jhu_update_date <- format.Date(Sys.Date(), "%d %B %Y")
  jhu_daily_url <- sprintf(jhu_daily_base, format.Date(Sys.Date(), "%m-%d-%Y"))
  jhu_daily_update <- read.csv(jhu_daily_url, stringsAsFactors = F)
},
error = function(cond) {
  jhu_update_date <<- format.Date(Sys.Date() - 1, "%d %B %Y")
  jhu_daily_url <<- sprintf(jhu_daily_base, format.Date(Sys.Date() - 1, "%m-%d-%Y"))
  jhu_daily_update <<- read.csv(jhu_daily_url, stringsAsFactors = F)
}, finally = {})
jhu_us_live <- jhu_daily_update[jhu_daily_update$Country_Region %in% "US",]
jhu_us_mat <- jhu_us_live[!duplicated(jhu_us_live$FIPS), ]
jhu_us_mat$FIPS <- sprintf("%05d", jhu_us_mat$FIPS)
jhu_us_map <- readRDS("data/us_counties.RDS")
jhu_us_map <- merge(jhu_us_map, jhu_us_mat, by.x = "GEOID", by.y = "FIPS")
rm(jhu_us_mat)


ctp_st_live <- read.csv("https://covidtracking.com/api/states.csv", stringsAsFactors = F)
ctp_st_live$fips <- sprintf("%02d", ctp_st_live$fips)
ctp_st_map <- readRDS("data/us_states.RDS")
ctp_st_map <- merge(ctp_st_map, ctp_st_live, by.x = "STATEFP", by.y = "fips")


dmn_confirmed <-
  c(min(log(jhu_us_map$Confirmed[jhu_us_map$Confirmed > 0]), na.rm = T),
    max(log(jhu_us_map$Confirmed[jhu_us_map$Confirmed > 0]), na.rm = T))
pal_confirmed <-
  colorNumeric(palette = "Reds",
               domain = dmn_confirmed,
               na.color = "white")

dmn_positive <-
  c(min(log(ctp_st_map$positive[ctp_st_map$positive > 0]), na.rm = T),
    max(log(ctp_st_map$positive[ctp_st_map$positive > 0]), na.rm = T))
pal_positive <-
  colorNumeric(palette = "Reds",
               domain = dmn_positive,
               na.color = "white")

dmn_total <-
  c(min(log(ctp_st_map$totalTestResults[ctp_st_map$totalTestResults > 0]), na.rm = T),
    max(log(ctp_st_map$totalTestResults[ctp_st_map$totalTestResults > 0]), na.rm = T))
pal_total <-
  colorNumeric(palette = "Reds",
               domain = dmn_total,
               na.color = "white")

lab_counties <- paste0(
  "<strong>",
  jhu_us_map$NAMELSAD,
  ", ",
  vapply(as.numeric(jhu_us_map$STATEFP), function(x)
    acs::fips.state$STATE_NAME[acs::fips.state$STATE == x], "character"),
  "</strong><br>",
  prettyNum(jhu_us_map$Confirmed , big.mark = ","),
  " cases<br>",
  prettyNum(jhu_us_map$Deaths, big.mark = ","),
  " deaths"
) %>%
  lapply(htmltools::HTML)

lab_states <- paste0(
  "<strong>",
  ctp_st_map$NAME,
  "</strong><br>",
  prettyNum(ctp_st_map$totalTestResults, big.mark = ","),
  " tests<br>",
  prettyNum(ctp_st_map$positive, big.mark = ","),
  " positive<br>",
  prettyNum(ctp_st_map$negative, big.mark = ","),
  " negative"
) %>%
  lapply(htmltools::HTML)

lab_circle <- paste0(
  "<strong>",
  jhu_us_map$Admin2,
  ifelse(jhu_us_map$Admin2 == "", "", ", "),
  jhu_us_map$Province_State,
  "</strong><br>",
  prettyNum(jhu_us_map$Confirmed , big.mark = ","),
  " cases<br>",
  prettyNum(jhu_us_map$Deaths, big.mark = ","),
  " deaths"
) %>%
  lapply(htmltools::HTML)


leaflet(jhu_us_map) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(
    fillColor = ~ pal_confirmed(log(Confirmed)),
    weight = 1,
    opacity = 1,
    color = ~ pal_confirmed(log(Confirmed)),
    fillOpacity = 1,
    smoothFactor = 0.5,
    highlight = highlightOptions(
      weight = 1,
      color = "black",
      bringToFront = T,
      sendToBack = T
    ),
    label = lab_counties,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "Counties"
  ) %>%
  addPolygons(
    data = ctp_st_map,
    fillColor = ~ pal_positive(log(positive)),
    weight = 1,
    opacity = 1,
    color = ~ pal_positive(log(positive)),
    fillOpacity = 1,
    smoothFactor = 0.5,
    highlight = highlightOptions(
      weight = 1,
      color = "black",
      bringToFront = T,
      sendToBack = T
    ),
    label = lab_states,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "States: Cases"
  ) %>%
  addPolygons(
    data = ctp_st_map,
    fillColor = ~ pal_total(log(totalTestResults)),
    weight = 1,
    opacity = 1,
    color = ~ pal_total(log(totalTestResults)),
    fillOpacity = 1,
    smoothFactor = 0.5,
    highlight = highlightOptions(
      weight = 1,
      color = "black",
      bringToFront = T,
      sendToBack = T
    ),
    label = lab_states,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "States: Tests"
  ) %>%
  addCircleMarkers(
    lng = jhu_us_map$Long_,
    lat = jhu_us_map$Lat,
    label = lab_circle,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    radius = log(jhu_us_map$Confirmed),
    weight = 10,
    fillColor = "red",
    stroke = F,
    group = "Markers"
  ) %>%
  addLayersControl(
    baseGroups = c("Counties", "States: Cases", "States: Tests", "Markers"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = F, autoZIndex = T)
  )  %>%
  setView(lat = 37.8333333,
          lng = -98.585522,
          zoom = 4) %>%
  addEasyButton(easyButton(
    icon = "fa-external-link",
    title = "Open map in new tab",
    onClick = JS("function(btn, map){ window.open('https://unlikelyvolcano.com/us-map.html') }")
  )) %>%
  addEasyButton(easyButton(
    icon = "fa-film",
    title = "See GIF of map over time",
    onClick = JS("function(btn, map){ window.open('https://unlikelyvolcano.com/spread.gif') }")
  )) %>%
  htmlwidgets::saveWidget("us-map.html")