# helper functions specific to the people data

# where is the CSV for the names stored?
ppl_file <- function() {
  file.path(workbch_gethome(), "workbch_people.csv")
}

# read people data from CSV file if it exists
ppl_read <- function() {
  if(file.exists(ppl_file())) {
    return(suppressMessages(readr::read_csv(ppl_file())))
  }
  return(
    tibble::tibble(
      fullname = character(0),
      nickname = character(0),
      default = logical(0))
  )
}

# write people data to CSV file
ppl_write <- function(ppl) {
  readr::write_csv(ppl, ppl_file())
}

# get the default person (error if none exists)
ppl_get_default <- function(strict = TRUE) {
  ppl <- ppl_read()
  def <- ppl$fullname[ppl$default == TRUE]
  if(strict & length(def) == 0) {
    stop("'owner' argument must be specified if no default person is set",
         call. = FALSE)
  }
  return(def)
}

# if name is a nickname, substitute with the real one
ppl_get_fullname <- function(nickname) {

  # if there aren't any names, return early
  if(length(nickname) == 0) {return(nickname)}

  # read the nicknames and substitute
  ppl <- ppl_read()

  # search for the name
  fullname <- character(length(nickname))
  for(i in 1:length(nickname)) {

    # if it's a known nickname, substitute the full
    if(nickname[i] %in% ppl$nickname) {
      fullname[i] <- ppl$fullname[ppl$nickname == nickname[i]]

    # if not, return the nickname itself...
    } else {
      fullname[i] <- nickname[i]

      # but throw warnings...
      if(nickname[i] %in% ppl$fullname) {
        nn <- ppl$nickname[ppl$fullname == nickname[i]]
        warning("'", nickname[i], "' has a known nick name '", nn, "'", call. = FALSE)
      } else{
        warning("'", nickname[i], "' is not a known nick name", call. = FALSE)
      }
    }
  }
  return(fullname)
}
