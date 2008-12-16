oneRowPerId = function(x, ids) {
  stopifnot(all(x[,1] %in% ids))
  x = x[ apply(x[,-1,drop=FALSE], 1, function(y) any(y!="")), ]
  d = lapply(2:ncol(x), function(i) {
    r  = character(length(ids))
    v  = sapply(split(x[,i], x[,1]), unique)
    v  = sapply(v, paste, collapse=", ")
   # mt = match(names(v), ids)
   # r[mt] = v
    mt = match(ids, names(v))
    r[!is.na(mt)] = v[mt[!is.na(mt)]]
    r[r==""] = NA
    return(I(r))
  })
  names(d) = colnames(x)[2:ncol(x)]
  do.call(data.frame, d)
}
