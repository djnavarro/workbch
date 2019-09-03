# helper functions specific to the people data


# read and write ----------------------------------------------------------

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
      nickname = character(0)
    )
  )
}

# write people data to CSV file
ppl_write <- function(ppl) {
  readr::write_csv(ppl, ppl_file())
}



# clean up, default, nickname ---------------------------------------------

ppl_parseowner <- function(owner) {
  owner <- owner %||% ppl_defaultowner()
  owner <- ppl_fullname(owner)
}

# get the default person (error if none exists)
ppl_defaultowner <- function(strict = TRUE) {
  stop("'owner' argument must be specified", call. = FALSE)
}

# if name is a nickname, substitute with the real one
ppl_fullname <- function(nickname) {

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
      } else{
        warning("'", nickname[i], "' is not a known nick name", call. = FALSE)
      }
    }
  }
  return(fullname)
}



# check functions ---------------------------------------------------------

ppl_checkteam <- function(owner, team) {
  if(!(owner %in% team)) {
    warning("'", owner, "' is not on the team for this job", call. = FALSE)
  }
}
