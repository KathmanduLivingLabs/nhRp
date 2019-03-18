#' @title get_crosstabs()
#'
#' @description For a given location code, and theme(s), this function fetches aggregated data at a theme level.
#'
#' @param themes,location_code
#'
#' @return dataframe
#'
#' @examples get_crosstabs(themes="indv_age,bldg_technical_solution", location_code="1201")
#'
#' @export get_crosstabs

get_crosstabs <- function(location_code, themes) {
  param_key_location <- "location_code="
  param_key_theme <- "dimensions="

  json_response <- jsonlite::fromJSON(
    paste0("https://eq2015.npc.gov.np/api/crosstabs?",
           param_key_location,
           location_code,
           "&",
           param_key_theme,
           themes)
  )

  df <- as.data.frame(json_response$data)
  df <- dplyr::select(df, -dplyr::one_of('population'), dplyr::one_of('population'))
  df <- dplyr::arrange(df, desc(population))

    # Return resultant dataframe, with population column at the end
  return (df)
}

