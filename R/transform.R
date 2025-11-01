
prepare_atoms_for_plotting <- function(atoms, atom_colour_type, colour_map = pal_atoms(), missing = "black", strip_numbers = TRUE, highlight = NULL, highlight_colour = "pink") {
  atom_names <- atoms[[atom_colour_type]]
  atom_ids <- atoms[["eleno"]]
  if(strip_numbers){
    atom_names <- gsub(x=atom_names, pattern = "[0-9]", replacement = "")
  }
  atoms[["..colour"]] <- colour_map[match(atom_names, names(colour_map))]
  atoms[["..colour"]] <- ifelse(is.na(atoms[["..colour"]]), missing, atoms[["..colour"]])

  # Add highlights
  atoms[["..colour"]] <- ifelse(atom_ids %in% highlight, highlight_colour, atoms[["..colour"]])


  return(atoms)
}


normalise <- function(x){x / sqrt(sum(x^2))}


slide_segment <- function(p0, p1, d) {
  v <- p1 - p0
  u <- v / sqrt(sum(v^2))
  delta <- d * u
  list(p0 = p0 + delta, p1 = p1 + delta)
}
