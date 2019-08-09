
#' Sets details for a new person
#'
#' @param fullname the full name of the person
#' @param nickname a short name for the person
#' @param make_default should this person be set as the "default"
#' @export
set_person <- function(fullname, nickname, make_default = FALSE) {

  verify_nickname(nickname)
  verify_fullname(fullname)
  verify_makedefault(make_default)

  ppl <- ppl_read()

  # remove an old default person if need be
  if(make_default == TRUE) {
    old_def <- which(ppl$default == TRUE)
    ppl$default[old_def] <- FALSE
  }

  # see if the nickname exists and/or the full name exists
  fn_ind <- which(ppl$fullname == fullname)
  nn_ind <- which(ppl$nickname == nickname)

  # if neither name exists, add a new person
  if(length(fn_ind) == 0 & length(nn_ind) == 0) {

    message("added '", fullname, "' with nickname '", nickname, "'")

    ppl <- dplyr::bind_rows(
      ppl,
      tibble::tibble(
        fullname = fullname,
        nickname = nickname,
        default = make_default
      ))
    ppl_write(ppl)
    return(invisible(ppl))

  }

  # if the nickname exists and not the fullname
  if(length(fn_ind) == 0 & length(nn_ind) > 0) {

    oldname <- ppl$fullname[nn_ind]
    message("changed full name of '", nickname, "' from '",
            oldname , "' to '", fullname, "'")

    # update the people file
    ppl$fullname[nn_ind] <- fullname
    ppl$default[nn_ind] <- make_default
    ppl_write(ppl)

    # update the jobs file
    jobs <- job_read()
    jobs <- purrr::map(jobs, function(.x) {
      if(.x$owner == oldname) {.x$owner <- fullname}
      .x$team <- gsub(oldname, fullname, .x$team, fixed = TRUE)
      return(.x)
    })
    job_write(jobs)

    return(invisible(ppl))
  }

  # if the fullname exists and not the nickname
  if(length(fn_ind) > 0 & length(nn_ind) == 0) {

    message("changed nickname of '", fullname, "' from '",
            ppl$nickname[fn_ind], "' to '", nickname, "'")
    ppl$nickname[fn_ind] <- nickname
    ppl$default[fn_ind] <- make_default
    ppl_write(ppl)
    return(invisible(ppl))

  }

  # if both exist and are the same index do nothing except possibly
  # update the default status
  if(identical(fn_ind, nn_ind)) {
    message("'", fullname, "' already has nickname '", nickname, "'")
    ppl$default[fn_ind] <- make_default
    ppl_write(ppl)
    return(invisible(ppl))
  }

  # if both exist but map to different people do nothing
  message("'", fullname, "' already has nickname '", ppl$nickname[fn_ind], "'")
  message("'", ppl$fullname[nn_ind], "' already has nickname '", nickname, "'")
  return(invisible(ppl))

}


