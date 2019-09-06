f_to_k2 <- function(temp_f){
  kelvin <- ((temp_f - 32)*(5/9)) + 273.15
  return(kelvin)
}