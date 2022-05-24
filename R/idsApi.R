#' Api connect to api.regression.fit to fetch datasets
#' ELo 202205 GWU DATS
#' version 1.0
#' @param table The name of table on DB
#' @param index If there is a default column as row index
#' @return dataframe
#' @examples
#' api_rfit( 'gapminder' )
#' table names can be in this list ('AStudentRecord','BikeShare','Dats_grades','Diet6wk','Happy','gapminder','gradAdmit','nfl2008_fga','Pizza','Titanic','USDANutrient','GSS_demographics','AAPL_daily','AAPL_full','GOOG_full','GOOG_daily','MSFT_daily','MSFT_full','GSS_demographics_xlsx','World1','World2');
#' @export
api_rfit <- function(table) {
  url = 'http://api.regression.fit/endpt.json'
  apikey = 'K35wHcKuwXuhHTaz7zY42rCje'
  res = httr::GET(url,query = list(apikey=apikey, table=table)) # typically getting a json object
  df = jsonlite::fromJSON(rawToChar(res$content))
  return(df)
}
