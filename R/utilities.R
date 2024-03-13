#' Map No/Yes to 0/1
#'
#' @param x "No" or "Yes" (case sensitive)
#'
#' @return 0 for "No", 1 for "Yes", NA otherwise
#'
#' @examples
#' ClarityDictionaryIngester:::map_no_yes_to_0_1("No")
#'
#' ClarityDictionaryIngester:::map_no_yes_to_0_1("Yes")
#'
#' ClarityDictionaryIngester:::map_no_yes_to_0_1("Maybe")
map_no_yes_to_0_1 <- function(x) {
  dplyr::case_when(
    x == "No" ~ 0,
    x == "Yes" ~ 1,
    TRUE ~ NA_integer_
  )
}

#' Map "Not Exported"/"Exported" to 0/1
#'
#' @param x "Not Exported" or "Exported" (case sensitive)
#'
#' @return 0 for "Not Exported", 1 for "Exported", NA otherwise
#'
#' @examples
#' ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Not Exported")
#'
#' ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Exported")
#'
#' ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Imported")
map_not_exported_exported_to_0_1 <- function(x) {
  dplyr::case_when(
    x == "Not Exported" ~ 0,
    x == "Exported" ~ 1,
    TRUE ~ NA_integer_
  )
}

#' Verify WUSTL Login Page
#'
#' @param b chromote session
#'
#' @return logical indicating presence of WUSTL login page
is_wustl_login_page <- function(b) {
  js <- "$('title').text()"
  b$Runtime$evaluate(js)$result$value == "Secure Login"
}

#' Do WUSTL Login
#'
#' @param b chromote session
#' @param user WUSTL Key username
#' @param pass WUSTL Key password
do_wustl_login <- function(
    b,
    user = Sys.getenv("WUSTL_KEY_USER"),
    pass = Sys.getenv("WUSTL_KEY_PASS")) {
  Sys.sleep(5)

  js <- sprintf('$("#ucWUSTLKeyLogin_txtUsername").val("%s")', user)
  b$Runtime$evaluate(js)

  js <- sprintf('$("#ucWUSTLKeyLogin_txtPassword").val("%s")', pass)
  b$Runtime$evaluate(js)

  js <- '$("#ucWUSTLKeyLogin_btnLogin").trigger("click")'
  b$Runtime$evaluate(js)

  Sys.sleep(5)
}

inject_jquery <- function(b) {
  js <- paste(
    'var script = document.createElement("script");',
    'script.src = "https://code.jquery.com/jquery-3.7.1.min.js";',
    'document.getElementsByTagName("head")[0].appendChild(script);',
    sep = "\n"
  )
  b$Runtime$evaluate(js)
}

#' Navigate to Table Page
#'
#' @param b chromote session
#' @param table name of table
navigate_to_table <- function(b, table) {
  url <- sprintf(
    "https://datahandbook.epic.com/ClarityDictionary/Details?tblName=%s",
    table
  )
  b$Page$navigate(url)
  b$Page$loadEventFired()

  inject_jquery(b)
}
