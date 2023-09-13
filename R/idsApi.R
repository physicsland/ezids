#' Api connect to api.regression.fit to fetch datasets
#' ELo 202205 GWU DATS
#' version 1.0
#' @param table The name of table on DB
#' @return dataframe
#' @examples
#' api_rfit( 'gapminder' )
#' table names can be in this list ("AAPL_daily", "AAPL_full", "AStudentRecord", "bank", "BaseballHeightWeight", "BikeShare", "Credit", "Dats_grades", "DC_py", "Diabetes", "Diet6wk", "faithful", "framinghamHD", "gapminder", "GOOG_daily", "GOOG_full", "gradAdmit", "GSS_demographics", "GSS_demographics_xlsx", "Happy", "housing", "MSFT_daily", "MSFT_full", "nfl2008_fga", "nybirths", "pima", Pizza, "Titanic", "USDANutrient", "VideoGames", "vlbw");
#' updated list can also be found at https://api.regression.fit/endpt.json
#' @export
api_rfit <- function(table) {
  url = "http://api.regression.fit/endpt.json"
  apikey = "K35wHcKuwXuhHTaz7zY42rCje"
  res = httr::GET(url,query = list(apikey=apikey, table=table)) # typically getting a json object
  df = type.convert(
    jsonlite::fromJSON(rawToChar(res$content)),
    na.strings = "NA",
    as.is = TRUE,
    dec = "."
  )
  return(df)
}
