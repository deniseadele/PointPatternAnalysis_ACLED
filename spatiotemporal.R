# 3D plot
dataxts <- NULL
country <- unique(ACLED_SA$country)
for (l in 1:length(country)) {
  SAcountry <- data[ACLED_SA$country == country[l], ]
  dd <- xts(
    SAcountry[, "fatalities"],
    as.Date(paste0(SAcountry$eve, "-01-01"))
  )
  dataxts <- cbind(dataxts, dd)
}