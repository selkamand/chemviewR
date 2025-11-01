#' Plot atoms and bonds in 3D with rgl
#'
#' Renders a ball–stick view of a \code{Molecule3D} using **rgl**. Atoms are drawn
#' as spheres, bonds as line segments, and optional labels can be shown with
#' different display modes via \code{label_mode}.
#'
#' @param molecule A \code{Molecule3D} object (see \code{structures::read_mol2()}).
#' @param highlight Optional selection of atoms to highlight. Either values of
#'   \code{eleno} (atom IDs) or a logical vector aligned with \code{molecule@atoms}.
#' @param highlight_colour Single colour for highlighted atoms (default \code{"pink"}).
#' @param atom_colour_type One of \code{c("element","elena","eleno")}; selects which
#'   column drives per-atom colours.
#' @param clear_scene Logical; if \code{TRUE}, clears the current rgl scene first.
#' @param colour_map_atom Named character vector mapping key \eqn{\rightarrow} colour
#'   (e.g., Jmol-style palette) used by \code{prepare_atoms_for_plotting()}.
#' @param missing Colour used when a key is not found in \code{colour_map_atom}.
#' @param strip_numbers Logical; if \code{TRUE}, digits are stripped from labels
#'   before colour lookup (e.g., \code{"C12"} \eqn{\rightarrow} \code{"C"}).
#' @param bond_width Numeric line width for bonds.
#' @param axes Logical; draw axes box and axis labels.
#' @param colour_bg Background colour for the scene.
#' @param colour_axis Axis/label colour.
#' @param label_mode One of \code{c("none","no_atoms","transparent")} controlling
#'   atom/label display:
#'   \itemize{
#'     \item \code{"none"}: spheres only, no labels;
#'     \item \code{"no_atoms"}: labels only (no spheres);
#'     \item \code{"transparent"}: labels and semi-transparent, non-specular spheres.
#'   }
#' @param label One of \code{c("element","elena","eleno")}; column in \code{atoms}
#'   used for text labels when \code{label_mode != "none"}.
#' @param label_cex Character expansion for labels.
#' @param label_colour Label colour; if \code{NULL}, labels inherit the per-atom
#'   colour (\code{..colour}).
#' @param atom_alpha Sphere opacity when \code{label_mode != "transparent"}.
#' @param atom_alpha_when_labelled Sphere opacity when \code{label_mode == "transparent"}.
#' @param atom_radius Sphere radius.
#' @param atom_shininess Sphere shininess; forced to \code{0} when
#'   \code{label_mode == "transparent"} for a flat look.
#' @param bond_alpha Opacity of bond segments.
#' @param grid Logical; draw orthogonal reference grids.
#' @param grid_n Integer; number of grid lines per axis when \code{grid = TRUE}.
#' @param show_anchor Render a wireframe octahedron wherever the anchor is
#' @param anchor_scale Control size of the anchor (number)
#' @param anchor_colour Colour of the anchor (string)
#' @param aspect Length-3 numeric vector for aspect ratio (reserved; not currently applied).
#' @param userMatrix Optional 4×4 user view matrix (e.g., from \code{rgl::par3d("userMatrix")})
#'   to set the camera/view.
#'
#' @details
#' Bonds are expanded with atom coordinates using \code{enrich_bonds_with_xyz_position()}
#' and interleaved by \code{to_interleaved()} into alternating start/end rows for
#' \code{rgl::segments3d()}. Labels are drawn with \code{rgl::texts3d()} and use
#' depth settings that keep them visible over spheres.
#'
#' @return (Invisibly) a list:
#' \describe{
#'   \item{atoms}{Integer vector of rgl object IDs for atom spheres (or \code{NULL} if not drawn).}
#'   \item{bonds}{Integer vector of rgl object IDs for bond segments.}
#' }
#'
#' @examples
#' \dontrun{
#' mol <- structures::read_mol2(system.file(package = "chemviewR", "benzene.mol2"))
#' plotrgl(mol, axes = TRUE, grid = TRUE)
#'
#' # Labels only
#' plotrgl(mol, label_mode = "no_atoms", label = "elena")
#'
#' # Reuse a saved camera/view
#' M <- rgl::par3d("userMatrix")
#' plotrgl(mol, userMatrix = M)
#' }
#'
#' @seealso prepare_atoms_for_plotting, enrich_bonds_with_xyz_position, to_interleaved
#' @export
plotrgl <- function(
    molecule,
    highlight = NULL, # Which element numbers to highlight
    highlight_colour = "pink",
    atom_colour_type = c("element", "elena", "eleno"),
    clear_scene = TRUE,
    colour_map_atom = pal_atoms(),
    missing = "grey",
    strip_numbers = TRUE,
    bond_width = 2,
    axes = FALSE,
    colour_bg = "black",
    colour_axis = "red",
    label_mode = c("none", "no_atoms", "transparent"),
    label = c("element", "elena", "eleno"),
    label_cex = 1,
    label_colour = "#F0F8E6", # If null will inherit colour from atom_colour_type/colour_map_atom
    atom_alpha = 1,
    atom_alpha_when_labelled = 0.1,
    atom_radius = 0.3,
    atom_shininess = 100,
    bond_alpha = 1,
    grid = FALSE,
    grid_n = 10,
    aspect = c(1, 1, 1),
    userMatrix = NULL,
    show_anchor = FALSE,
    anchor_scale = 0.3,
    anchor_colour = "pink"
) {
  # ---- Validate inputs -------------------------------------------------------
  assertions::assert_class(molecule, class = "structures::Molecule3D")
  atom_colour_type <- rlang::arg_match(atom_colour_type)
  label_mode <- rlang::arg_match(label_mode)
  label <- rlang::arg_match(label)

  # ---- Fetch Atom -------
  bonds_interleaved <- molecule@bond_positions_interleaved
  atoms <- molecule@atoms

  # ---- Prepare atoms: compute per-atom color column '..colour' ---------------
  atoms_enriched <- prepare_atoms_for_plotting(
    atoms = atoms,
    colour_map = colour_map_atom,
    missing = missing,
    atom_colour_type = atom_colour_type,
    strip_numbers = strip_numbers,
    highlight = highlight,
    highlight_colour = highlight_colour
  )


  # ---- Scene setup -----------------------------------------------------------
  if (clear_scene) {
    rgl::clear3d(type = "all")
  }

  rgl::bg3d(color = colour_bg)
  rgl::light3d(specular = "white", diffuse = "white", ambient = "gray20")


  # Camera Setup ------------------------------------------------------------
  if(!is.null(userMatrix)){
    rgl::par3d(userMatrix = userMatrix)
  }

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
    labels <- atoms_enriched[[label]]

    # Draw labels at atom positions. We give them an emissive color to "pop"
    # and disable depth test/writes so they stay visible over spheres.
    label_id <- with(
      atoms_enriched,
      rgl::texts3d(
        x, y, z,
        texts = labels,
        color = if(!is.null(label_colour)) label_colour else ..colour,
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

  # ---- Draw Anchor  ----------------------------------------------
  if(show_anchor){
    anchor <- molecule@anchor
    anchor_shape <- rgl::octahedron3d(col = anchor_colour)
    anchor_shape <- rgl::translate3d(anchor_shape, anchor[1], anchor[2], anchor[3])
    anchor_shape <- rgl::scale3d(anchor_shape, x = anchor_scale, anchor_scale, anchor_scale)
    # Render anchor
    rgl::wire3d(anchor_shape, lit=FALSE)
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

