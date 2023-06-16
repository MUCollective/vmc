
get_type = function(data) {
  data.unique = unique(data)
  data.is.number = is.numeric(data)
  if (data.is.number && length(data.unique) > 9) {
    return("quantitative")
  }
  if (data.is.number) {
    return("ordinal")
  }
  return("nominal")
}
