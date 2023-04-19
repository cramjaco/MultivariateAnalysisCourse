# Extra processing for networks.

# Here's a function to use the smallest non zero value
smallest_non_zero <- function(vec){
  # start with a vecotr named `vec`
  # This is only going to work if there are no negative values, so lets test for that
  if(min(vec)<0) stop("This function can't handle negative numbers, but vec has at least one negative")
  # first, just take the elements that aren't zero
  all_non_zeros <- vec[which(vec > 0)]
  smz <- min(all_non_zeros)
  smz
}

smz_arisa <- smallest_non_zero(arisa_community_mtx)
smz_arisa

arisa_community_clr <- decostand(arisa_community_mtx, method = "clr", pseudocount = smz_arisa)
