# constructor functions


# constructors for new objects with default values ------------------------

new_job <- function(jobname, description, owner, status = NULL,
                    team = NULL, priority = NULL,
                    tags = NULL, path = NULL, urls = NULL, verify = TRUE) {

  # replace nulls with default values
  status <- status %||% "active"
  team <- team %||% character(0)
  priority <- priority %||% 1
  tags <- tags %||% character(0)
  path <- path %||% NA_character_
  urls <- urls %||% empty_url()

  # verify unless explicitly told not to
  if(verify) {
    verify_jobname(jobname)
    verify_description(description)
    verify_owner(owner)
    verify_status(status)
    verify_character(team)
    verify_character(tags)
    verify_path(path)
    verify_priority(priority)
    ## missing: verify_urls(urls)
  }

  # tidy names
  owner <- ppl_parseowner(owner)
  team <- ppl_fullname(team)
  if(!(owner %in% team)) { team <- c(owner, team) }

  # construct object
  list(
    jobname = jobname,
    description = description,
    owner = owner,
    status = status,
    team = team,
    priority = priority,
    tags = tags,
    path = path,
    urls = urls
  )
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



# constructors for empty objects ------------------------------------------

# these sometimes skip the verification step because in normal usage the
# user is expected to specify a string of length one (enforced in the
# verification functions) but strictly speaking the empty object violates
# this by using strings of length 0

empty_url <- function() {
  new_url(site = character(0), link = character(0), verify = FALSE)
}


empty_job <- function() {
  new_job(jobname = character(0), description = character(0),
          owner = character(0), status = character(0), team = character(0),
          priority = numeric(0), tags = character(0),
          path = character(0), urls = empty_url())
}
