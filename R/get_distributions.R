#' @title get_distribution()
#'
#' @description This function generates a dataset of population/household/building distributions for different values of a given theme, for a particular district. The depth parameter will specify the geographical level of the resulting data frame (0=district-level, 1=municipality-level, 2=ward-level)
#'
#' @param theme,location_code,depth
#'
#' @return dataframe
#'
#' @examples get_distribution(theme="age_indv", location_code="12", depth="1")
#'
#' @export get_distribution

get_distribution <- function(location_code, theme, depth) {

  if(stringr::str_detect(theme, ",")) {
    stop(paste0("More than one theme provided: ",theme,". This function only supports one theme at a time."))
  }

  param_key_location <- "district_id="
  param_key_depth <- "geography_level="
  param_key_theme <- "dimensions="

  json_response <- jsonlite::fromJSON(
    paste0("https://eq2015.npc.gov.np/api/distributions?",
           param_key_location,
           location_code,
           "&",
           param_key_theme,
           theme,
           "&",
           param_key_depth,
           depth
    )
  )

  # Return resultant dataframe, with population column at the end
  df <- as.data.frame(json_response$data[theme])
  names(df) = sub(paste0("*",theme,"."),"",names(df))

  return (df)
}

