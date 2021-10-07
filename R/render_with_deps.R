#' @author Alec L. Robitaille
#' From https://github.com/robitalec/targets-parameterized-bookdown
render_with_deps <- function(index, deps) {
	bookdown::render_book(index)
}
