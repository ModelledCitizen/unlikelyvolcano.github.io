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




"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"

