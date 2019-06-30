
# read people data from CSV file if it exists
ppl_read <- function() {
  if(file.exists(ppl_file())) {
    return(suppressMessages(readr::read_csv(ppl_file())))
  }
  return(list())
}

# write people data to CSV file
ppl_write <- function(ppl) {
  readr::write_csv(ppl, ppl_file())
}

#' Add a new person
#'
#' @param name the name of the person
#' @param nickname a nickname for the person
#' @export
people_add <- function(name, nickname) {
  ppl <- ppl_read()
  ppl <- dplyr::bind_rows(
    ppl,
    tibble::tibble(
      name = name,
      nickname = nickname
    ))
  ppl_write(ppl)
}



#' List all known people
#'
#' @export
people_list <- function() {
  ppl_read()
}

# if name is a nickname, substitute with the real one
real_name <- function(name) {
  if(length(name) == 0) {
    return(name)
  }
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
