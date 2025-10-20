#' Generate Jmol-style element colour palette
#'
#' Returns a named character vector mapping element symbols to their
#' conventional Jmol colours. These colours are commonly used for atom
#' colouring in molecular visualisations and correspond to the
#' \code{element_colours} dataset included in this package.
#'
#' @return A named character vector of hexadecimal colour codes, where names
#'   are element symbols (e.g., `"C"`, `"O"`, `"N"`, `"H"`).
#' @details
#' The returned colour palette follows the Jmol convention for element
#' colours, widely adopted in chemistry visualisation tools.
#'
#' @seealso [element_colours]
#' @examples
#' pal <- pal_atoms()
#' pal["C"]   # Grey
#' pal["O"]   # Red
#' head(pal)
#'
#' # Identical to the built-in data object:
#' identical(pal, element_colours)
#'
#' @export
pal_atoms <- function(){
  # Common element colours (Jmol convention, with quoted names)
  c(
    "H"  = "#FFFFFF",  # White
    "C"  = "#909090",  # Grey
    "N"  = "#3050F8",  # Blue
    "O"  = "#FF0D0D",  # Red
    "F"  = "#90E050",  # Green
    "Cl" = "#1FF01F",  # Green
    "Br" = "#A62929",  # Dark red
    "I"  = "#940094",  # Violet
    "He" = "#D9FFFF",  # Cyan
    "Ne" = "#B3E3F5",  # Light blue
    "Ar" = "#80D1E3",  # Light cyan
    "Xe" = "#94FFFF",  # Pale blue
    "Kr" = "#5CB8D1",  # Light blue
    "P"  = "#FF8000",  # Orange
    "S"  = "#FFFF30",  # Yellow
    "B"  = "#FFB5B5",  # Pink
    "Li" = "#CC80FF",  # Purple
    "Na" = "#AB5CF2",  # Purple
    "K"  = "#8F40D4",  # Violet
    "Ca" = "#3DFF00",  # Green
    "Fe" = "#E06633",  # Brownish orange
    "Zn" = "#7D80B0",  # Blue-grey
    "Cu" = "#C88033",  # Orange
    "Ni" = "#50D050",  # Green
    "Mg" = "#8AFF00",  # Light green
    "Al" = "#BFA6A6",  # Light grey
    "Si" = "#F0C8A0",  # Tan
    "Mn" = "#9C7AC7",  # Purple
    "Cr" = "#8A99C7",  # Blue-grey
    "Co" = "#F090A0",  # Pink-red
    "Se" = "#FFA100",  # Orange
    "Ti" = "#BFC2C7",  # Silver
    "Au" = "#FFD123",  # Gold
    "Hg" = "#B8B8D0",  # Silver-blue
    "Pb" = "#575961"   # Dark grey
  )
}
