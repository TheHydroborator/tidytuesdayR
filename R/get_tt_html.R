#' @title Get TidyTuesday URL and HTML
#' @param git_url url to tidytuesday files
#' @importFrom xml2 read_html
get_tt_html <- function(git_url) {

  tt_html <- try(xml2::read_html(
    file.path("https://github.com/rfordatascience/tidytuesday/tree/master",git_url)),
    silent = TRUE)
  if (inherits(tt_html, "try-error")) {
    stop(tt_html[1])
  } else {
    return(tt_html)
  }

}
