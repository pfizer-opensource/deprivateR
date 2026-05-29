#' Set Census API Key
#'
#' @description A wrapper around \code{tidycensus::census_api_key()} that
#'     checks for a Census API key using a cascading lookup: (1) the explicit
#'     \code{key} argument, (2) the \code{CENSUS_API_KEY} environment variable,
#'     (3) an interactive prompt. If no key can be resolved in a non-interactive
#'     session, an informative error is raised.
#'
#' @param key A character scalar; a Census API key. If \code{NULL} (default),
#'     the function checks the \code{CENSUS_API_KEY} environment variable and,
#'     if that is also unset, prompts interactively.
#' @param install A logical scalar; if \code{TRUE}, the key is written to
#'     \code{.Renviron} for use in future sessions (via
#'     \code{tidycensus::census_api_key(install = TRUE)}). Defaults to
#'     \code{FALSE}, which sets the key for the current session only.
#' @param overwrite A logical scalar; if \code{TRUE} and \code{install = TRUE},
#'     an existing key in \code{.Renviron} will be overwritten. Defaults to
#'     \code{FALSE}.
#'
#' @details The lookup order is:
#' \enumerate{
#'   \item If \code{key} is provided (non-\code{NULL}), it is used directly.
#'   \item If \code{key} is \code{NULL}, the function checks
#'     \code{Sys.getenv("CENSUS_API_KEY")}. If that is set (non-empty), it is
#'     used.
#'   \item If neither is available and the session is interactive, the user is
#'     prompted to enter their key with setup instructions.
#'   \item If the session is non-interactive and no key is found, an error
#'     is raised with instructions for setting the environment variable.
#' }
#'
#' @return The Census API key (invisibly), as returned by
#'     \code{tidycensus::census_api_key()}.
#'
#' @examples
#' \dontrun{
#'   # Use an explicit key (session only)
#'   dep_set_api_key(key = "your_key_here")
#'
#'   # Install key to .Renviron for future sessions
#'   dep_set_api_key(key = "your_key_here", install = TRUE)
#'
#'   # Rely on CENSUS_API_KEY env var or interactive prompt
#'   dep_set_api_key()
#' }
#'
#' @export
dep_set_api_key <- function(key = NULL, install = FALSE, overwrite = FALSE) {

  # 1. Explicit key argument

  if (!is.null(key)) {
    resolved_key <- key
  } else {
    # 2. Check environment variable
    env_key <- Sys.getenv("CENSUS_API_KEY", unset = "")

    if (nzchar(env_key)) {
      resolved_key <- env_key
      cli::cli_inform("Using Census API key found in {.envvar CENSUS_API_KEY} environment variable.")
    } else {
      # 3. Interactive prompt or error
      if (interactive()) {
        cli::cli_inform(c(
          "No Census API key found.",
          "i" = "You can obtain a key at {.url https://api.census.gov/data/key_signup.html}.",
          "i" = "To avoid this prompt in the future, use {.code dep_set_api_key(key = \"your_key\", install = TRUE)} to save the key to your {.file .Renviron}."
        ))
        resolved_key <- readline(prompt = "Enter your Census API key: ")

        if (!nzchar(resolved_key)) {
          cli::cli_abort("No key entered. A Census API key is required.")
        }
      } else {
        cli::cli_abort(c(
          "No Census API key found.",
          "x" = "Set the {.envvar CENSUS_API_KEY} environment variable before running in non-interactive mode.",
          "i" = "Obtain a key at {.url https://api.census.gov/data/key_signup.html}.",
          "i" = "Set it with {.code Sys.setenv(CENSUS_API_KEY = \"your_key\")} or add {.code CENSUS_API_KEY=your_key} to your {.file .Renviron}."
        ))
      }
    }
  }

  # Validate key format (basic check — Census keys are 40-character hex strings)
  if (!grepl("^[a-f0-9]{40}$", resolved_key)) {
    cli::cli_warn(c(
      "The provided key does not match the expected Census API key format.",
      "i" = "Census API keys are typically 40-character hexadecimal strings.",
      "i" = "Proceeding anyway, but API calls may fail if the key is invalid."
    ))
  }

  # Set the key via tidycensus
  tidycensus::census_api_key(resolved_key, install = install, overwrite = overwrite)
}
