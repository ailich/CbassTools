#' Calculates the Angular Field of View
#'
#' Calculates the angular field of view (in degrees) based on camera and lens properties. Can calculate horizontal, vertical, and diagonal angular field of view in either air or seawater. If d is unknown, it can be approximated by pix and pixel_size. If d is known, either aspect_ratio or pix can be used.
#' @param d length of diagonal of camera sensor (must be same units as f)
#' @param f focal length of lens (must be same units as d or pixel size)
#' @param medium "air" or "seawater"
#' @param type Type of field of view. Either "horiz", "vert", or "diag" (diag does not need pix or aspect ratio)
#' @param pix width and height of frame in number of pixels c(width, height)
#' @param aspect_ratio aspect ratio e.g. c(16,9)
#' @param pixel_size pixel size of camera sensor (must be same units as f)
#' @param n_air index of refraction for air (default = 1.000277)
#' @param n_sea index of refraction for seawater (default = 4/3)
#' @export

calc_AFOV<- function(d=NULL, f=NULL, medium, type, pix=NULL, aspect_ratio=NULL, pixel_size, n_air = 1.000277, n_sea = 4/3){
  if (is.null(d)) {
    w<- pix[1] * pixel_size
    v<- pix[2] * pixel_size
    d<- sqrt(w^2 + v^2)
  } else{
      if (is.null(aspect_ratio)){aspect_ratio <- pix}
      w<- sqrt(((d^2 * aspect_ratio[1]^2)/(aspect_ratio[1]^2 + aspect_ratio[2]^2)))
      v<- sqrt(((d^2 * aspect_ratio[2]^2)/(aspect_ratio[1]^2 + aspect_ratio[2]^2)))
  }
  D_AFOV_air<- 2 * atan(d/(2*f))
  H_AFOV_air<- 2 * atan(w/(2*f))
  V_AFOV_air<- 2 * atan(v/(2*f))

  D_AFOV_sea<- 2 * asin(sin((D_AFOV_air/2))*(n_air/n_sea))
  H_AFOV_sea<- 2 * asin(sin((H_AFOV_air/2))*(n_air/n_sea))
  V_AFOV_sea<- 2 * asin(sin((V_AFOV_air/2))*(n_air/n_sea))

  if (medium=="air" & type == "diag") {return(degrees(D_AFOV_air))}
  if (medium=="air" & type == "horiz") {return(degrees(H_AFOV_air))}
  if (medium=="air" & type == "vert") {return(degrees(V_AFOV_air))}
  if (medium=="seawater" & type == "diag") {return(degrees(D_AFOV_sea))}
  if (medium=="seawater" & type == "horiz") {return(degrees(H_AFOV_sea))}
  if (medium=="seawater" & type == "vert") {return(degrees(V_AFOV_sea))}
  }





