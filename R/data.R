#' Jmol-style element colour map
#'
#' A named character vector mapping element symbols to their conventional
#' Jmol colours. Used for colouring atoms in 3D molecular visualisations.
#'
#' @format A named character vector of hexadecimal colour codes, where names
#'   are element symbols (e.g., `"C"`, `"O"`, `"N"`, `"H"`).
#' @examples
#' element_colours["O"]  # "#FF0D0D"
#' names(element_colours)[1:5]
#' @usage data(element_colours)
#' @source Jmol element colour convention.
#' @keywords datasets
"element_colours"
