
#' Sets details for a new person
#'
#' @param name the name of the person
#' @param nickname a nickname for the person
#' @export
set_person <- function(name, nickname) {
  ppl <- ppl_read()
  ppl <- dplyr::bind_rows(
    ppl,
    tibble::tibble(
      name = name,
      nickname = nickname
    ))
  ppl_write(ppl)
}


# if name is a nickname, substitute with the real one
real_name <- function(name) {

  # if there aren't any names, return early
  if(length(name) == 0) {return(name)}

  # read the nicknames and substitute
  ppl <- ppl_read()
  for(i in 1:length(name)) {
    if(name[i] %in% ppl$nickname) {
      name[i] <- ppl$name[ppl$nickname == name[i]]
    } else {
      warning("'", name[i], "' is not a known nickname", call. = FALSE)
    }
  }
  return(name)
}
