UnlikelyTools::set_wd("unlikelyvolcano.github.io")

# library(readxl)
#
# p1 <- read_xls("POP/POP01.xls")
# p2 <- read_xls("POP/POP02.xls")
# p3 <- read_xls("POP/POP03.xls")


nyt_counties <-
  read.csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
    stringsAsFactors = F
  )
nyt_counties$fips <- sprintf("%05d", nyt_counties$fips)

nyt_states <-
  read.csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
    stringsAsFactors = F
  )
nyt_states$fips <- sprintf("%05d", nyt_states$fips)


philly_jhu[1, ] <-
  vapply(philly_jhu[1, ], prettyNum, "chr", big.mark = ",")
kable(
  philly_jhu,
  col.names = c("Cases", "Active", "Recovered", "Deaths"),
  row.names = F,
  align = "ccccc",
  digits = 3
) %>%
  kable_styling() %>%
  row_spec(1, bold = T) %>%
  column_spec(1, color = "darkorange") %>%
  column_spec(2, color = "goldenrod") %>%
  column_spec(3, color = "yellowgreen") %>%
  column_spec(4, color = "firebrick")