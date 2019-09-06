# to avoid the 'no visible binding' note during check:
utils::globalVariables(
  c("priority", "status", "owner", "jobname", "staged", "unstaged", "untracked",
    "ahead", "behind", "path", "hidden", "jobs", "tag")
)



# not exported because I'm not sure what to do with this
job_tags <- function() {

  jobs <- job_read()

  # vector containing each instance of a tag
  all_tags <- purrr::map(jobs, ~ .x$tags)
  all_tags <- unlist(all_tags)

  # tabulate and enframe
  freq_tags <- table(all_tags)
  tag_tbl <- tibble::tibble(
    tag = names(freq_tags),
    jobs = unname(freq_tags)
  )

  # arrange
  tag_tbl <- dplyr::arrange(tag_tbl, dplyr::desc(jobs), tag)

  return(as_wkbch_tbl(tag_tbl))
}

