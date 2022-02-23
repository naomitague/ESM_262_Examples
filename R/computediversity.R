#' Describe diversity based on a list of species 
#' 
#' Compute a species diversity index
#' @param species list of species (names, or code) 
#' @return list with the following items
#' \describe{
#' \item{num}{ Number of distinct species}
#' \item{simpson}{Value of simpson diversity index}
#' \item{dominant}{Name of the most frequently occuring species}
#' }
#' @examples
#' computediversity(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

computediversity = function(species) {


species = as.factor(species)

# use simple simpson form
tmp = (summary(species)/sum(summary(species))) ** 2
diversity = 1.0-sum(tmp)

# number of species
nspecies = length(summary(species))

# which is the most frequent
tmp = which.max(summary(species))
dominant = names(summary(species)[tmp])

# output from function
return(list(num=nspecies, simpson=diversity, dominant=dominant))
}


