#' @export
inspect_click_script <- function() {
  includeScript(system.file("js/inspect_click.js", package = "tidystats"))
}

#' @export
copy_to_clipboard_script <- function() {
  includeScript(system.file("js/copy_to_clipboard.js", package = "tidystats"))
}

#' @export
css_style <- function() {
  includeCSS(system.file("css/style.css", package = "tidystats"))
}