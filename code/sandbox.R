UnlikelyTools::set_wd("unlikelyvolcano.github.io")

library(readxl)

p1 <- read_xls("POP/POP01.xls")
p2 <- read_xls("POP/POP02.xls")
p3 <- read_xls("POP/POP03.xls")



library(rvest)


pa.counties <-
  read_html("https://www.health.pa.gov/topics/disease/coronavirus/Pages/Cases.aspx") %>%
  html_nodes("#ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField > div > div > table") %>%
  html_table() %>%
  (function(x) {x[[1]]}) %>%
  (function(x) {colnames(x) <- x[1,]; x[-1,]})




read_html("https://public.tableau.com/profile/cityofphiladelphia#!/vizhome/COP-COVID-19/Stacked") %>%
  html_nodes("#title7713620505763405234_17874758422068147630 > div.tab-textRegion-boundary > div > span > div:nth-child(3) > span") %>%
  html_text()

%>%
  (function(x) {x[[1]]}) %>%
  (function(x) {colnames(x) <- x[1,]; x[-1,]})


