decode = function(x, internal) {
  # we need to make sure that split does not change the order in the results
  mysplit =  function(v, f) split(v, factor(f, levels=unique(f)))
  r = mysplit(x$r, internal$ids.r)
  z = mysplit(x$z, internal$ids.z)
  d = Map(function(val.name, vals) vals[[val.name]], x$d, internal$values)
  d = mysplit(d, internal$ids.d)
  d = Map(function(par, val) if(par$length==1) val[[1]] else val, internal$ps.d$pars, d)
  c(r, z, d)[internal$decode.order]
}

