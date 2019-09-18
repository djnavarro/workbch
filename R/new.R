
new_job <- function(jobname, description, owner, status = NULL,
                    priority = NULL, tags = NULL, path = NULL, urls = NULL,
                    idstring = NULL, verify = TRUE, sentinel = TRUE) {

  # replace nulls with default values
  status   <- status    %||%  "active"
  priority <- priority  %||%  1
  tags     <- tags      %||%  character(0)
  path     <- path      %||%  NA_character_
  urls     <- urls      %||%  empty_url()
  idstring <- idstring  %||%  idstring()

  # verify unless explicitly told not to
  if(verify) {
    verify_jobname(jobname)
    verify_description(description)
    verify_owner(owner)
    verify_status(status)
    verify_character(tags)
    verify_path(path)
    verify_priority(priority)
    ## missing: verify_urls(urls)
  }

  # construct object
  obj <- list(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    priority = priority,
    tags = tags,
    path = path,
    urls = urls,
    idstring = idstring
  )

  # add sentinel file
  if(dir.exists(obj$path) & sentinel == TRUE) {
    write_sentinel(obj$path, obj$jobname, obj$idstring)
  }

  return(obj)
}


new_url <- function(site, link, verify = TRUE) {

  # verify unless explicitly told not to
  if(verify) {
    verify_site(site)
    verify_link(link)
  }

  # construct object
  tibble::tibble(
    site = site,
    link = link
  )
}

