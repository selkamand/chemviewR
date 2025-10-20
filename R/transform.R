#' Enrich bond data with atom coordinates
#'
#' Joins atomic coordinate columns (`x`, `y`, `z`) to a bond table so that each
#' bond row includes both start (`x`, `y`, `z`) and end (`xend`, `yend`, `zend`)
#' positions corresponding to its origin and target atoms.
#'
#' @param bonds Data frame of bonds containing columns that reference atom IDs.
#' @param atoms Data frame of atoms containing coordinates `x`, `y`, `z` and an
#'   atom identifier column.
#' @param origin Name of the column in `bonds` giving the origin atom ID.
#' @param target Name of the column in `bonds` giving the target atom ID.
#' @param atom_id Name of the column in `atoms` giving the atom ID used to match
#'   against `origin` and `target`.
#'
#' @return A data frame identical to `bonds` but with six additional coordinate
#'   columns: `x`, `y`, `z` (for origin atoms) and `xend`, `yend`, `zend` (for
#'   target atoms).
#'
#' @examples
#' atoms <- data.frame(
#'   eleno = 1:2,
#'   x = c(0, 1), y = c(0, 1), z = c(0, 1)
#' )
#' bonds <- data.frame(origin = 1, target = 2)
#' enrich_bonds_with_xyz_position(bonds, atoms)
#'
#' @export
enrich_bonds_with_xyz_position <- function(bonds, atoms, origin = "origin", target = "target", atom_id = "eleno") {
  df_atoms_minimal <- atoms[, c(atom_id, "x", "y", "z"), drop = FALSE]
  df_atoms_minimal_end <- df_atoms_minimal
  colnames(df_atoms_minimal_end) <- c(atom_id, "xend", "yend", "zend")

  bonds |>
    dplyr::left_join(df_atoms_minimal, by = list(x = origin, y = atom_id)) |>
    dplyr::left_join(df_atoms_minimal_end, by = list(x = target, y = atom_id))
}


prepare_atoms_for_plotting <- function(atoms, col_name, col_id, colour_map = pal_atoms(), missing = "black", strip_numbers = TRUE, highlight = NULL, highlight_colour = "pink") {
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



normalise <- function(x){x / sqrt(sum(x^2))}


slide_segment <- function(p0, p1, d) {
  v <- p1 - p0
  u <- v / sqrt(sum(v^2))
  delta <- d * u
  list(p0 = p0 + delta, p1 = p1 + delta)
}
