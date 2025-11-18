# Plot shapes

#' Plot a 3D Shape using rgl
#'
#' Renders a [`structures::Shape`] object in an interactive rgl scene,
#' including vertices, labelled vertex names, edges, and (optionally)
#' face centroids. Colours, lighting, and point sizes can be customised.
#'
#' @param shape A [`structures::Shape`] object containing vertices,
#'   edges (`segments_interleaved`), and optionally face centroids.
#' @param clear_scene Logical; if `TRUE` (default), clears the existing
#'   rgl scene before drawing.
#' @param lit Logical; whether rgl should apply lighting effects.
#' @param vertex_size Numeric; size of vertex points.
#' @param colour_vertices Colour for vertices and their labels.
#' @param colour_edges Colour for edges.
#' @param colour_centroids Colour for face centroids.
#' @param centroid_size Numeric; point size for centroids.
#' @param colour_bg Background colour for the rgl scene.
#'
#' @return Invisibly returns `shape`, as a side effect plots the object in rgl.
#'
#' @examples
#' \dontrun{
#' shp <- structures::ShapeCube()  # example shape
#' plot_shapes(shp)
#' }
#'
#' @export
plot_shapes <- function(shape, clear_scene = TRUE, lit = FALSE, vertex_size = 10, colour_vertices = "red", colour_edges = "pink", colour_centroids = "white", centroid_size = 10, colour_bg = "black"){
 assertions::assert_class(shape, "structures::Shape")

  if(clear_scene){
    rgl::clear3d(type = "all")
  }
  # Prepare Scene
  rgl::bg3d(color = colour_bg)

  # Add vertices
  with(
    shape@vertices,
    rgl::points3d(x=x, y=y, z=z, lit = lit, color = colour_vertices, size=vertex_size),
  )

  # Add vertex_text
  with(
    shape@vertices,
    rgl::text3d(x=x, y=y, z=z, texts = name, lit = lit, color = colour_vertices, size=vertex_size),
  )

  # Add faces
  if(length(shape@faces) > 0){
    with(
      shape@face_centroids,
      rgl::points3d(x=x, y=y, z=z, lit = lit, color = colour_centroids, size=centroid_size),
    )
  }

  # Add edges
  with(
    shape@segments_interleaved,
    rgl::segments3d(x=x, y=y, z=z, lit = lit, color = colour_edges)
   )
}
