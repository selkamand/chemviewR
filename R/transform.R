  enrich_bonds_with_xyz_position <- function(bonds, atoms, origin = "origin", target = "target", atom_id = "eleno") {
    df_atoms_minimal <- atoms[, c(atom_id, "x", "y", "z"), drop = FALSE]
    df_atoms_minimal_end <- df_atoms_minimal
    colnames(df_atoms_minimal_end) <- c(atom_id, "xend", "yend", "zend")

    bonds |>
      dplyr::left_join(df_atoms_minimal, by = list(x = origin, y = atom_id)) |>
      dplyr::left_join(df_atoms_minimal_end, by = list(x = target, y = atom_id))
  }


prepare_atoms_for_plotting <- function(atoms, col_name, col_id, colour_map = element_colours, missing = "black", strip_numbers = TRUE, highlight = NULL, highlight_colour = "pink") {
  atom_names <- atoms[[col_name]]
  atom_ids <- atoms[[col_id]]
  if(strip_numbers){
    atom_names <- gsub(x=atom_names, pattern = "[0-9]", replacement = "")
  }
  atoms[["..colour"]] <- colour_map[match(atom_names, names(colour_map))]
  atoms[["..colour"]] <- ifelse(is.na(atoms[["..colour"]]), missing, atoms[["..colour"]])

  # Add highlights
  atoms[["..colour"]] <- ifelse(atom_ids %in% highlight, highlight_colour, atoms[["..colour"]])


  return(atoms)
}

to_interleaved <- function(df, coord = c("x", "y", "z"), end_suffix = "end") {
  # Columns assumed to be coordinates
  start_cols <- coord
  end_cols <- paste0(coord, end_suffix)

  # Safety check
  missing_cols <- setdiff(c(start_cols, end_cols), names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  n <- nrow(df)

  # Point labels
  point <- rep(c("start", "end"), times = n)

  # Build interleaved coordinates
  xyz_mat <- t(cbind(df[start_cols], df[end_cols]))
  xyz_mat <- matrix(xyz_mat, ncol = length(coord), byrow = TRUE)
  xyz_df <- as.data.frame(xyz_mat, stringsAsFactors = FALSE)
  names(xyz_df) <- coord

  # Keep and replicate non-coordinate (meta) columns
  meta_cols <- setdiff(names(df), c(start_cols, end_cols))
  meta_df <- if (length(meta_cols)) {
    df[rep(seq_len(n), each = 2), meta_cols, drop = FALSE]
  } else {
    NULL
  }

  # Assemble output (segment info + meta + coords)
  out <- data.frame(
    point = point,
    meta_df,
    xyz_df,
    row.names = NULL,
    check.names = FALSE
  )

  out
}

# pal_elements <- function(){
#   # Common element colours (Jmol convention, with quoted names)
#   element_colours <- c(
#     "H"  = "#FFFFFF",  # White
#     "C"  = "#909090",  # Grey
#     "N"  = "#3050F8",  # Blue
#     "O"  = "#FF0D0D",  # Red
#     "F"  = "#90E050",  # Green
#     "Cl" = "#1FF01F",  # Green
#     "Br" = "#A62929",  # Dark red
#     "I"  = "#940094",  # Violet
#     "He" = "#D9FFFF",  # Cyan
#     "Ne" = "#B3E3F5",  # Light blue
#     "Ar" = "#80D1E3",  # Light cyan
#     "Xe" = "#94FFFF",  # Pale blue
#     "Kr" = "#5CB8D1",  # Light blue
#     "P"  = "#FF8000",  # Orange
#     "S"  = "#FFFF30",  # Yellow
#     "B"  = "#FFB5B5",  # Pink
#     "Li" = "#CC80FF",  # Purple
#     "Na" = "#AB5CF2",  # Purple
#     "K"  = "#8F40D4",  # Violet
#     "Ca" = "#3DFF00",  # Green
#     "Fe" = "#E06633",  # Brownish orange
#     "Zn" = "#7D80B0",  # Blue-grey
#     "Cu" = "#C88033",  # Orange
#     "Ni" = "#50D050",  # Green
#     "Mg" = "#8AFF00",  # Light green
#     "Al" = "#BFA6A6",  # Light grey
#     "Si" = "#F0C8A0",  # Tan
#     "Mn" = "#9C7AC7",  # Purple
#     "Cr" = "#8A99C7",  # Blue-grey
#     "Co" = "#F090A0",  # Pink-red
#     "Se" = "#FFA100",  # Orange
#     "Ti" = "#BFC2C7",  # Silver
#     "Au" = "#FFD123",  # Gold
#     "Hg" = "#B8B8D0",  # Silver-blue
#     "Pb" = "#575961"   # Dark grey
#   )
#   return(element_colours)
# }

normalise <- function(x){x / sqrt(sum(x^2))}


slide_segment <- function(p0, p1, d) {
  v <- p1 - p0
  u <- v / sqrt(sum(v^2))
  delta <- d * u
  list(p0 = p0 + delta, p1 = p1 + delta)
}
