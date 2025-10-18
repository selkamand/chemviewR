#' Plot atoms and bonds in 3D with rgl
#'
#' Renders a simple ball–stick scene from atom and bond tables using **rgl**.
#' Each atom is drawn as a sphere, and each bond as a segment connecting the
#' referenced atom coordinates.
#'
#' @param atoms Data frame of atoms with columns `x`, `y`, `z`, an ID column
#'   (given by `col_atom_id`), and a name/element column (given by `col_atom_name`).
#' @param bonds Data frame of bonds with two columns that reference atom IDs:
#'   origin (given by `col_bond_origin`) and target (given by `col_bond_target`).
#' @param col_bond_origin, col_bond_target Character scalars naming the bond
#'   columns that reference atom IDs.
#' @param col_atom_id Character scalar naming the atom ID column in `atoms`.
#' @param col_atom_name Character scalar naming the atom element/name column in `atoms`.
#' @param highlight Optional selection of atoms to highlight. Supply values of
#'   `col_atom_id` (e.g., numeric IDs) or a logical vector aligned with `atoms`.
#'   When provided, these atoms are drawn using `highlight_colour`.
#' @param highlight_colour Single colour used for atoms in `highlight`; overrides
#'   the normal colour mapping for those atoms (default: `"pink"`).
#' @param col_atom_colour Column name or vector used to assign atom colours.
#'   Defaults to `col_atom_name`, which is mapped via `colour_map_atom`.
#' @param clear_scene Logical; if `TRUE`, clears the existing rgl scene first.
#' @param colour_map_atom Named character vector mapping element/name → colour
#'   (e.g., Jmol-style colours). Used by `prepare_atoms_for_plotting()`.
#' @param strip_numbers Logical; if `TRUE`, digits are stripped from `col_atom_name`
#'   before colour lookup (useful for labels like `"C12"` → `"C"`).
#' @param bond_width Numeric line width for bonds.
#' @param axes Logical; draw labelled axes box if `TRUE`.
#' @param bg3d Background colour passed to `rgl::bg3d()`. **Note:** current
#'   implementation sets black unconditionally.
#' @param colour_axis Axis and label colour.
#' @param grid Logical; draw orthogonal reference grids if `TRUE`.
#' @param grid_n Integer; number of grid lines per drawn axis when `grid = TRUE`.
#' @param aspect Numeric length-3 vector for aspect ratio. **Note:** not used yet.
#'
#' @details
#' Internally, bonds are enriched with atom coordinates via
#' `enrich_bonds_with_xyz_position()` and converted to an interleaved start/end
#' format by `to_interleaved()` (columns `x,y,z` and `xend,yend,zend` become
#' alternating rows suitable for `rgl::segments3d()`).
#'
#' @return Called for its side effects (renders to the rgl device); returns `NULL` invisibly.
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
#'   axes = TRUE, grid = TRUE
#' )
#' }
#'
#' @seealso [to_interleaved()], [enrich_bonds_with_xyz_position()],
#'   [prepare_atoms_for_plotting()], and the **rgl** functions
#'   [rgl::spheres3d()], [rgl::segments3d()], [rgl::axes3d()], [rgl::grid3d()].
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
    colour_map_atom = element_colours,
    missing = "grey",
    strip_numbers = TRUE,
    bond_width = 2,
    axes = TRUE,
    bg3d = "black",
    colour_axis = "red",
    grid = FALSE,
    grid_n = 10,
    aspect=c(1,1,1)
  ) {
  # Assertions
  assertions::assert_dataframe(atoms)
  assertions::assert_dataframe(bonds)
  assertions::assert_names_include(atoms, names = c(col_atom_id, col_atom_name, "x", "y", "z"))
  assertions::assert_names_include(bonds, names = c(col_bond_origin, col_bond_target))

  # Add start/end positions to bonds dataframe
  bonds_enriched <- enrich_bonds_with_xyz_position(bonds, atoms, origin = col_bond_origin, target = col_bond_target, atom_id = col_atom_id)

  # Interleave Bonds (rgl requires that segments are defined by xyz columns and 2 neighbouring rows are taken as start and end of each segment
  bonds_interleaved <- to_interleaved(bonds_enriched)

  # Add '..colour' column describing colour
  atoms_enriched <- prepare_atoms_for_plotting(atoms = atoms, col_name = col_atom_colour, col_id = col_atom_id, colour_map = colour_map_atom, strip_numbers = strip_numbers, highlight=highlight, highlight_colour = highlight_colour, missing=missing)

  # Open new window
  # rgl::open3d()
  if(clear_scene){
    rgl::clear3d(type = "all")
  }

  rgl::bg3d(color = "black")
  rgl::light3d(specular = "white", diffuse = "white", ambient = "gray20")

  with(atoms_enriched, {
    rgl::spheres3d(x = x, y = y, z = z, color=..colour, radius=0.3, lit=TRUE,  shininess = 10, alpha = 1)
  })

  with(bonds_interleaved, {
    rgl::segments3d(x = x, y = y, z = z, add=TRUE, color = "grey", lwd = bond_width, lit=FALSE)
    # rgl::cylinder3d(radius = 0.1, center = )
  })


  if(axes){
    rgl::axes3d(
      label=TRUE,
      box=TRUE,
      nticks = 5,
      expand = 2,
      color = colour_axis,
      lit=FALSE,
    )
    rgl::title3d(xlab = "X", ylab = "Y", zlab = "Z", color=colour_axis)
  }

  #rgl::aspect3d(1,1,1)
  # Add reference grid
  if(grid){
    rgl::grid3d(c("x", "y+", "z+"), n=grid_n)
  }
}

# add_cube <- function(){
#   rgl::translate3d(rgl::cube3d(col = "green"), 3, 0, 0, lit=FALSE)
# }

# Still need to figure out how to move arrow based on offset

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

