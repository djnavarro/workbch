# to avoid the 'no visible binding' note during check:
utils::globalVariables(
  c("priority", "status", "owner", "jobname", "staged", "unstaged", "untracked",
    "ahead", "behind", "path", "hidden", "jobs", "tag")
)

# # startup messages
# .onAttach <- function(libname, pkgname){
#   packageStartupMessage(
#     paste0("    workbch.home: '", getOption("workbch.home"), "'")
#   )
# }

