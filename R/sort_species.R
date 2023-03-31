#' Sort Species List
#'
#' Sorts species names alphabetically but puts spp first in their respective groups and No IDs last
#' @param species_list vector of unique species names
#' @export
sort_species<- function(species_list){
  new_species_list<- gsub(pattern = "_spp$", replacement = "_0spp", x = species_list)
  new_species_list<- sort(new_species_list)
  new_species_list<- gsub(pattern = "_0spp$", replacement = "_spp", x = new_species_list)
  noid_idx<- grep(pattern = "noid",x = new_species_list)
  new_species_list<- c(new_species_list, new_species_list[noid_idx])
  if (length(noid_idx)>0) {new_species_list<- new_species_list[-noid_idx]}
  return(new_species_list)
}
