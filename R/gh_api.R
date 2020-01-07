
#' Access Tidytuesday Repository though the github api
#'
#' @param x path to folder
#'
#' @importFrom purrr map_chr
#' @importFrom jsonlite read_json

tt_gh_api <- function(path){

  # get latest commit
  base_api <- "https://api.github.com/repos/rfordatascience/tidytuesday/git/refs/heads/master"
  latest_commit <- jsonlite::read_json(base_api)$object$url

  # get tree
  tree_url <- jsonlite::read_json(latest_commit)$tree$url
  base_tree <- jsonlite::read_json(tree_url)$tree
  names(base_tree) <- purrr::map_chr(base_tree,`[[`,"path")

  path <- strsplit(path,"/")[[1]]

  gh_api_json <- tt_gh_tree(path, base_tree)

  if ( gh_api_result(gh_api_json, "success") ) {
    if ( gh_api_result(gh_api_json, "folder") ) {
      return( purrr::map_chr(gh_api_json$tree,`[[`,"path") )

    } else if( gh_api_result(gh_api_json, "file") ) {
      return( rawToChar(jsonlite::base64_dec(gh_api_json$content)) )
    }

  }else{
    return( gh_api_json )
  }

}

tt_gh_tree <- function(path,tree){

  if ( length(path) == 1 ){
    if( path %in% names(tree) ) {

      gh_api_json <- jsonlite::read_json(tree[[path]]$url)

      return_type <- ifelse("tree" %in% names(gh_api_json),
                            "folder","file")
      structure(
        .Data = gh_api_json,
        class = c("gh_api","success",return_type)
      )

    }else{
      structure(
        .Data = path,
        class = c("gh_api","gh_not_valid_path")
      )
    }
  }else{
    if( path[[1]] %in% names(tree) ){

      tree_url <- tree[[path[[1]]]]$url
      folder_tree <- jsonlite::read_json(tree_url)$tree
      names(folder_tree) <- purrr::map_chr(folder_tree,`[[`,"path")
      tt_gh_tree(path[-1],folder_tree)

    }else{
      structure(
        .Data = paste(path,collapse = "/"),
        class = c("gh_api","gh_not_valid_path")
      )
    }
  }

}

gh_api_result <- function( x, type = c("success","gh_not_found","gh_not_valid_path","folder","file")){
   "gh_api" %in% class(x) &
    match.arg(type) %in% class(x)
}

