#' Plot atoms and bonds in 3D with rgl
#'
#' Renders a ball–stick scene from atom and bond tables using **rgl**.
#' Each atom is drawn as a sphere and each bond as a segment connecting
#' referenced atom coordinates. Labels can be drawn with different display
#' modes via `label_mode`.
#'
#' @param atoms Data frame of atoms with numeric columns `x`, `y`, `z`, an ID
#'   column (named by `col_atom_id`), and a name/element column (named by
#'   `col_atom_name`).
#' @param bonds Data frame of bonds with two columns that reference atom IDs:
#'   origin (named by `col_bond_origin`) and target (named by `col_bond_target`).
#' @param col_atom_id Character scalar naming the atom ID column in `atoms`.
#' @param col_atom_name Character scalar naming the atom element/name column
#'   in `atoms`.
#' @param highlight Optional selection of atoms to highlight. Supply values of
#'   `col_atom_id` (e.g., numeric IDs) or a logical vector aligned with `atoms`.
#'   When provided, these atoms are colored using `highlight_colour`.
#' @param highlight_colour Single color used for atoms in `highlight`; overrides
#'   the normal color mapping for those atoms (default: `"pink"`).
#' @param col_atom_colour Column name (in `atoms`) or a vector used to assign
#'   atom colors. Defaults to `col_atom_name`, which is mapped via
#'   `colour_map_atom`.
#' @param clear_scene Logical; if `TRUE`, clears the existing rgl scene first.
#' @param colour_map_atom Named character vector mapping element/name → color
#'   (e.g., Jmol-style colors). Used by `prepare_atoms_for_plotting()`.
#' @param strip_numbers Logical; if `TRUE`, digits are stripped from the
#'   `col_atom_name` values before color lookup (useful for labels like `"C12"` → `"C"`).
#' @param missing Color to use for atoms not found in `colour_map_atom`.
#' @param bond_width Numeric line width for bonds.
#' @param axes Logical; draw labeled axes box if `TRUE`.
#' @param colour_axis Axis and label color.
#' @param label_mode One of `c("none", "no_atoms", "transparent")`. Controls
#'   how labels and atoms are displayed:
#'   - `"none"`: draw atoms (opaque by default) and no labels;
#'   - `"no_atoms"`: draw only labels (no atom spheres);
#'   - `"transparent"`: draw labels and semi-transparent atoms (see
#'     `atom_alpha_when_labelled` and `atom_shininess`).
#' @param col_label Column name (in `atoms`) whose values are shown as labels
#'   when `label_mode != "none"`. Defaults to `col_atom_name`.
#' @param atom_alpha Atom opacity when `label_mode` is not `"transparent"`.
#' @param atom_alpha_when_labelled Atom opacity used when `label_mode = "transparent"`.
#' @param atom_radius Sphere radius used for atoms.
#' @param atom_shininess Numeric shininess for atom spheres; forcibly set to `0` for a
#'   flat (non-specular) look when `label_mode = "transparent"`.
#' @param grid Logical; draw orthogonal reference grids if `TRUE`.
#' @param grid_n Integer; number of grid lines per drawn axis when `grid = TRUE`.
#' @param aspect Numeric length-3 vector for aspect ratio (currently not used).
#' @param col_bond_origin Name of column in bonds datatframe describing the atom_id of the first element involved in bond
#' @param col_bond_target Name of column in bonds datatframe describing the atom_id of the second element involved in bond
#' @param colour_bg Colour of scene background
#' @param label_cex Label character expansion value.
#' @param label_colour colour of atom labels
#' @param bond_alpha Opacity of bond segments.
#'
#' @details
#' Bonds are enriched with atom coordinates via
#' `enrich_bonds_with_xyz_position()` and converted to an interleaved start/end
#' format by `to_interleaved()` (columns `x,y,z` and `xend,yend,zend` become
#' alternating rows suitable for [rgl::segments3d()]).
#'
#' Labels are drawn at the atom coordinates using [rgl::texts3d()]. Depth test
#' is disabled for labels to ensure visibility over spheres (see code comments).
#'
#' @return (Invisibly) a list with two elements:
#' \describe{
#'   \item{atoms}{Vector of RGL object IDs for atom spheres (or `NULL` if none).}
#'   \item{bonds}{Vector of RGL object IDs for bond segments.}
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal toy example (two atoms connected by one bond)
#' atoms <- data.frame(
#'   eleno = c(1, 2),
#'   elena = c("C", "O"),
#'   x = c(0, 1), y = c(0, 0), z = c(0, 0)
#' )
#' bonds <- data.frame(origin = 1, target = 2)
#'
#' plotrgl(
#'   atoms, bonds,
#'   col_atom_id = "eleno",
#'   col_atom_name = "elena",
#'   bond_width = 3,
#'   axes = TRUE, grid = TRUE,
#'   label_mode = "transparent"
#' )
#' }
#'
#' @seealso prepare_atoms_for_plotting, enrich_bonds_with_xyz_position, to_interleaved
#' @export
plotrgl <- function(
    atoms,
    bonds,
    col_bond_origin = "origin",
    col_bond_target = "target",
    col_atom_id = "eleno",
    col_atom_name = "elena",
    highlight = NULL, # Which element numbers to highlight
    highlight_colour = "pink",
    col_atom_colour = col_atom_name,
    clear_scene = TRUE,
    colour_map_atom = pal_atoms(),
    missing = "grey",
    strip_numbers = TRUE,
    bond_width = 2,
    axes = TRUE,
    colour_bg = "black",
    colour_axis = "red",
    label_mode = c("none", "no_atoms", "transparent"),
    col_label = col_atom_name,
    label_cex = 1,
    label_colour = "#F0F8E6",
    atom_alpha = 1,
    atom_alpha_when_labelled = 0.1,
    atom_radius = 0.3,
    atom_shininess = 100,
    bond_alpha = 1,
    grid = FALSE,
    grid_n = 10,
    aspect = c(1, 1, 1)
) {
  # ---- Validate inputs -------------------------------------------------------
  assertions::assert_dataframe(atoms)
  assertions::assert_dataframe(bonds)
  assertions::assert_names_include(
    atoms, names = c(col_atom_id, col_atom_name, "x", "y", "z")
  )
  assertions::assert_names_include(
    bonds, names = c(col_bond_origin, col_bond_target)
  )
  label_mode <- rlang::arg_match(label_mode)

  # ---- Prepare bonds: attach coordinates and interleave for segments3d -------
  bonds_enriched <- enrich_bonds_with_xyz_position(
    bonds, atoms,
    origin = col_bond_origin, target = col_bond_target, atom_id = col_atom_id
  )
  # rgl::segments3d uses alternating rows as start/end of each segment
  bonds_interleaved <- to_interleaved(bonds_enriched)

  # ---- Prepare atoms: compute per-atom color column '..colour' ---------------
  atoms_enriched <- prepare_atoms_for_plotting(
    atoms = atoms,
    col_name = col_atom_colour,
    col_id = col_atom_id,
    colour_map = colour_map_atom,
    strip_numbers = strip_numbers,
    highlight = highlight,
    highlight_colour = highlight_colour,
    missing = missing
  )

  # ---- Scene setup -----------------------------------------------------------
  if (clear_scene) {
    rgl::clear3d(type = "all")
  }

  rgl::bg3d(color = colour_bg)
  rgl::light3d(specular = "white", diffuse = "white", ambient = "gray20")

  # ---- Draw atoms (or not), depending on label_mode --------------------------
  # - "none":        draw opaque (or user-specified) atoms only
  # - "no_atoms":    skip spheres, labels only
  # - "transparent": draw spheres with reduced alpha and zero shininess
  if (label_mode != "no_atoms") {
    alpha <- if (label_mode == "transparent") atom_alpha_when_labelled else atom_alpha
    shininess <- if (label_mode == "transparent") 0 else atom_shininess

    # Spheres are drawn in one call for efficiency.
    sphere_ids <- with(
      atoms_enriched,
      rgl::spheres3d(
        x = x, y = y, z = z,
        color = ..colour,
        radius = atom_radius,
        lit = TRUE,
        shininess = shininess,
        alpha = alpha
      )
    )
  } else {
    sphere_ids <- NULL
  }

  # ---- Draw labels depending on label_mode -----------------------------------
  if (label_mode != "none") {
    labels <- atoms_enriched[[col_label]]

    # Draw labels at atom positions. We give them an emissive color to "pop"
    # and disable depth test/writes so they stay visible over spheres.
    label_id <- with(
      atoms_enriched,
      rgl::texts3d(
        x, y, z,
        texts = labels,
        color = label_colour,
        emission = "white",
        specular = "white",
        lit = FALSE,
        shininess=1,
        alpha = 1,
        depth_test = "always",
        depth_mask = FALSE,
        cex = label_cex
      )
    )
  }

  # ---- Draw bonds ------------------------------------------------------------
  bond_ids <- with(
    bonds_interleaved,
    rgl::segments3d(
      x = x, y = y, z = z,
      add = TRUE,
      color = "grey",
      lwd = bond_width,
      lit = FALSE,
      alpha = bond_alpha
    )
  )

  # ---- Axes and optional grids ----------------------------------------------
  if (axes) {
    rgl::axes3d(
      label = TRUE, box = TRUE, nticks = 5, expand = 2,
      color = colour_axis, lit = FALSE
    )
    rgl::title3d(xlab = "X", ylab = "Y", zlab = "Z", color = colour_axis)
  }

  # if (!missing(aspect)) rgl::aspect3d(aspect[1], aspect[2], aspect[3]) # reserved
  if (grid) {
    rgl::grid3d(c("x", "y+", "z+"), n = grid_n)
  }

  # ---- Return IDs for downstream updates ------------------------------------
  rgl_ids <- list(atoms = sphere_ids, bonds = bond_ids)
  invisible(rgl_ids)
}



#' Add a plane and optional normal arrows to an rgl scene
#'
#' Draws a plane defined by a normal vector and offset using `rgl::planes3d()`,
#' with optional visualisation of the plane's normal via `rgl::arrow3d()`.
#'
#' @param normal Numeric length-3 vector giving the plane's normal direction.
#' @param offset Numeric scalar giving the plane's offset (distance from the origin
#'   along the normal).
#' @param color Colour of the plane surface and normal arrows.
#' @param lit Logical; if `TRUE`, the plane and arrows are affected by scene lighting.
#' @param alpha Numeric in `[0, 1]` giving plane transparency.
#' @param show_normal Logical; if `TRUE`, draws one or more arrows indicating
#'   the plane normal direction.
#'
#' @return Called for side effects; returns `NULL` invisibly.
#' @seealso [add_plane_by_position_and_normal()]
#' @export
add_plane <- function(normal, offset=0, color="pink", lit=FALSE, alpha = 0.5, show_normal = TRUE){
  stopifnot(length(normal) == 3)
  nlen <- sqrt(sum(normal^2))
  if (nlen == 0) stop("normal must be non-zero")
  normal <- normalise(normal)

  # new_arrow_pos = slide_segment(p0 = c(0, 0, 0), p1 = normal, d = -offset)

  if(show_normal){
    rgl::arrow3d(
      # p0 = rep(0, times = length(normal)),
      # p1 = normal,
      p0 = translate_position_in_direction(c(0, 0, 0), direction = normal, magnitude = -offset),
      p1 = translate_position_in_direction(normal, normal, magnitude = -offset),
      color=color, alpha = 1, lit=lit, type = "rotation", theta = pi/6,
    )
    rgl::arrow3d(
      p0 = rep(0, times = length(normal)),
      p1 = normal,
      color="blue", alpha = 1, lit=lit, type = "rotation", theta = pi/6,
    )
  }

  rgl::planes3d(normal, d = offset, p1 = normal, lit = lit, color = color, alpha = alpha)
}


#' Add a plane defined by position and normal
#'
#' Computes the plane offset from a point and normal, then draws it with
#' [add_plane()].
#'
#' @param plane_normal Numeric length-3 vector giving the plane normal.
#' @param plane_position Numeric length-3 vector specifying a point on the plane.
#' @return Called for side effects; returns `NULL` invisibly.
#' @seealso [add_plane()]
#' @export
add_plane_by_position_and_normal <- function(plane_normal, plane_position){
  plane_normal <- normalise(plane_normal)
  # rgl::arrow3d(p0=plane_position, p1=plane_position + plane_normal, lit=FALSE, color = "green", type="rotation")
  offset = compute_offset_for_plane_by_position(plane_normal, plane_position)

  add_plane(plane_normal, offset=offset, color = "orange")
  # rgl::spheres3d(x = plane_position, lit=FALSE, color = "purple", radius=0.1)
}



compute_offset_for_plane_by_position <- function(plane_normal, target){
  # -sqrt(sum(target^2))
  -(plane_normal %*% target)/magnitude(plane_normal)
}


translate_position_in_direction <- function(position, direction, magnitude){
  translation_vector = normalise(direction) * as.numeric(magnitude)
  position + translation_vector
}

pad_limits <- function(v, factor = 2, extra = 0) {
  r <- range(v); c <- mean(r); h <- diff(r)/2
  if (h == 0) h <- 1  # avoid zero-size bbox
  c + c(-1, 1) * (h * factor + extra)
}

magnitude <- function(x){
 sqrt(sum(x^2))
}

