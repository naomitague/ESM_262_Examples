
#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at 
#' a given speed
#' @param cdrag coefficient due to drag default=0.3 
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

autopower = function(V, m, A, cdrag=0.3, crolling=0.015,pair=1.2,g=9.8) {
  P = crolling*m*g*V + 1/2*A*pair*cdrag*V**3
  return(P)
}
