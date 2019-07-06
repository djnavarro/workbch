new_note <- function(name, note, id = NA, date = as.character(Sys.Date())) {
  tibble::tibble(
    name = name,  # name of the job it is linked to
    note = note,  # the message text
    id = id,      # unique identifier
    date = date   # the time stamp
  )
}

#' Append a note to a job
#'
#' @param name the job to which the note should be added
#' @param note the text of the note
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' job_add_note("myjob", "susan wanted me to notify her when done")
#' }
job_add_note <- function(name, note) {
  jobs <- job_read()
  jb <- jobs[[name]]

  if(is.null(dim(jb$notes))) {
    nt <- new_note(name, note, id = 1)
  } else if(nrow(jb$notes) == 0) {
    nt <- new_note(name, note, id = 1)
  } else {
    id <- max(jb$notes$id) + 1
    nt <- new_note(name, note, id = id)
  }

  jb$notes <- dplyr::bind_rows(jb$notes, nt)
  jb$notes <- dplyr::arrange(jb$notes, dplyr::desc(date), dplyr::desc(id))
  jobs[[name]] <- jb
  job_write(jobs)
}
