pos2i = function(x, pdim) {
  if(any(nchar(x)>3))
    stop("Position IDs must contain 3 characters maximally")
  let = substr(x, 1, 1)
  num = substr(x, 2, 3)
  let = match(let, LETTERS)
  num = as.integer(num)
  inv = is.na(let) | (let>pdim["nrow"]) | is.na(num) | (num>pdim["ncol"])
  if(any(inv))
    stop("Invalid position IDs in 'x'")
  
  return((let-1)*pdim["ncol"]+num)
}

