#' Create Table zc_ny
#'
#' @param con database connection
#'
#' @return returns TRUE, invisibly
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' create_table_zc_ny(con)
#'
#' DBI::dbDisconnect(con)
#' }
create_table_zc_ny <- function(con) {
  DBI::dbWriteTable(
    con,
    DBI::Id(
      catalog = "sandbox",
      schema  = "wilcox_lab",
      table   = "clarity_dictionary_zc_ny"
    ),
    dplyr::tibble(
      id = 0:1,
      name = c("No", "Yes"),
      dl_load_ts = Sys.time()
    ),
    overwrite = TRUE
  )
}

#' Create Table zc_ehi_status
#'
#' @param con database connection
#'
#' @return returns TRUE, invisibly
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' create_table_zc_ehi_status(con)
#'
#' DBI::dbDisconnect(con)
#' }
create_table_zc_ehi_status <- function(con) {
  DBI::dbWriteTable(
    con,
    DBI::Id(
      catalog = "sandbox",
      schema  = "wilcox_lab",
      table   = "clarity_dictionary_zc_ehi_status"
    ),
    dplyr::tibble(
      id = 0:1,
      name = c("Not Exported", "Exported"),
      dl_load_ts = Sys.time()
    ),
  )
}

#' Create Table
#'
#' @param con database connection
#' @param table name of table to create
#'
#' @return TRUE
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' create_table(con, "column")
#'
#' create_table(con, "column_ini_item_bridge")
#'
#' create_table(con, "description")
#'
#' create_table(con, "ini_item")
#'
#' create_table(con, "table")
#'
#' create_table(con, "type")
#'
#' DBI::dbDisconnect(con)
#' }
create_table <- function(
    con,
    table = c(
      "column",
      "column_ini_item_bridge",
      "description",
      "ini_item",
      "table",
      "type"
    )) {
  table <- match.arg(table)

  structure <- dplyr::case_when(
    table == "column" ~
      paste(
        "id DOUBLE",
        "table_id DOUBLE",
        "table_row DOUBLE",
        "name VARCHAR(255)",
        "type_c DOUBLE",
        "deprecated_c DOUBLE",
        "discontinued_c DOUBLE",
        "preserved_c DOUBLE",
        "character_replacement_c DOUBLE",
        "ehi_status_c DOUBLE",
        "description_id DOUBLE",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    table == "column_ini_item_bridge" ~
      paste(
        "column_id DOUBLE",
        "ini_item_id DOUBLE",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    table == "description" ~
      paste(
        "id DOUBLE",
        "line DOUBLE",
        "description VARCHAR(255)",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    table == "ini_item" ~
      paste(
        "id DOUBLE",
        "ini VARCHAR(255)",
        "item VARCHAR(255)",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    table == "table" ~
      paste(
        "id DOUBLE",
        "name VARCHAR(255)",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    table == "type" ~
      paste(
        "id DOUBLE",
        "name VARCHAR(255)",
        "dl_load_ts TIMESTAMP",
        collapse = ", "
      ),
    TRUE ~
      NA_character_
  )

  table <- sprintf("sandbox.wilcox_lab.clarity_dictionary_%s", table)
  statement <- sprintf("CREATE TABLE %s (%s);", table, structure)
  invisible(DBI::dbExecute(con, statement) == 0)
}

#' Drop Table
#'
#' @param con database connection
#' @param table name of table to drop
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' drop_table(con, "column")
#'
#' drop_table(con, "column_ini_item_bridge")
#'
#' drop_table(con, "description")
#'
#' drop_table(con, "ini_item")
#'
#' drop_table(con, "table")
#'
#' drop_table(con, "type")
#'
#' drop_table(con, "zc_ehi_status")
#'
#' drop_table(con, "zc_ny")
#'
#' DBI::dbDisconnect(con)
#' }
drop_table <- function(
    con,
    table = c(
      "column",
      "column_ini_item_bridge",
      "description",
      "ini_item",
      "table",
      "type",
      "zc_ehi_status",
      "zc_ny"
    )) {
  table <- match.arg(table)
  table <- sprintf("sandbox.wilcox_lab.clarity_dictionary_%s", table)
  statement <- sprintf("DROP TABLE %s;", table)
  DBI::dbExecute(con, statement)
}

#' Select New Id (Primary Key)
#'
#' @param con database connection
#' @param table name of the table for which to select the next available id
#' (primary key)
#'
#' @return the next available id (primary key) for the table
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' select_new_id(con, "column")
#'
#' select_new_id(con, "description")
#'
#' select_new_id(con, "ini_item")
#'
#' select_new_id(con, "table")
#'
#' select_new_id(con, "type")
#'
#' DBI::dbDisconnect(con)
#' }
select_new_id <- function(
    con,
    table = c(
      "column",
      "description",
      "ini_item",
      "table",
      "type"
    )) {
  table <- match.arg(table)

  statement <- sprintf(
    paste(
      "SELECT ",
      "  MAX(id) AS max_id ",
      "FROM ",
      "  sandbox.wilcox_lab.clarity_dictionary_%s; ",
      collapse = "\n"
    ),
    table
  )

  response <- DBI::dbGetQuery(con, statement)

  if (is.na(response$max_id)) {
    1
  } else {
    response$max_id + 1
  }
}

#' Select Existing Id (Primary Key)
#'
#' @param con database connection
#' @param table name of table for which to select existing id
#' @param x one-row dataframe containing ini and item columns for table
#' ini_item or name column for other tables
#'
#' @return id (primary key) of the existing record or NA if not found.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' x <- tibble(id = NA_integer, ini = "EPT", item = ".1")
#' select_existing_id(con, "ini_item", x)
#'
#' y <- tibble(id = NA_integer_, name = "PAT_ID")
#' select_existing_id(con, "table", y)
#'
#' z <- tibble(id = NA_integer_, name = "VARCHAR(255)")
#' select_existing_id(con, "type", z)
#'
#' DBI::dbDisconnect(con)
#' }
select_existing_id <- function(con, table = c("ini_item", "table", "type"), x) {
  table <- match.arg(table)

  where_clause <- switch(table,
    "ini_item" = sprintf("ini = '%s' AND item = '%s'", x$ini, x$item),
    "table"    = sprintf("name = '%s'", x$name),
    "type"     = sprintf("name = '%s'", x$name),
    NA_character_
  )

  table <- sprintf("sandbox.wilcox_lab.clarity_dictionary_%s", table)
  statement <- sprintf("SELECT id FROM %s WHERE %s;", table, where_clause)
  response <- DBI::dbGetQuery(con, statement)

  if (nrow(response) == 0) {
    NA_integer_
  } else {
    response$id
  }
}

#' Select or Append to a Table
#'
#' @param con database connection
#' @param table table name for which to search for or append the record
#' @param x one-row dataframe representing a record to find by id or append in
#' the table
#'
#' @return id (primary key) of the existing or appended record
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' x <- tibble(id = NA_integer, ini = "EPT", item = ".1")
#' select_or_append(con, "ini_item", x)
#'
#' y <- tibble(id = NA_integer_, name = "PAT_ID")
#' select_or_append(con, "table", y)
#'
#' z <- tibble(id = NA_integer_, name = "VARCHAR(255)")
#' select_or_append(con, "type", z)
#'
#' DBI::dbDisconnect(con)
#' }
select_or_append <- function(
    con,
    table = c(
      "ini_item",
      "table",
      "type"
    ),
    x) {
  table <- match.arg(table)

  existing_id <- select_existing_id(con, table, x)

  if (is.na(existing_id)) {
    x$id <- select_new_id(con, table)
    x$dl_load_ts <- Sys.time()
    DBI::dbWriteTable(
      con,
      DBI::Id(
        catalog = "sandbox",
        schema  = "wilcox_lab",
        table   = sprintf("clarity_dictionary_%s", table)
      ),
      x,
      append = TRUE
    )

    x$id
  } else {
    existing_id
  }
}

#' Select Clarity Tables
#'
#' @param con database connection
#'
#' @return dataframe of table names that may or may not have dictionary
#' documentation ingested but should
select_clarity_tables <- function(con) {
  statement <-
    'SHOW TABLES FROM cleansed.epic_clarity LIKE "^clarity_orgfilter_.*"'
  DBI::dbGetQuery(
    con,
    statement
  ) %>%
    dplyr::transmute(
      name = toupper(sub("clarity_orgfilter_", "", .data["tableName"]))
    )
}

#' Select Ingested Tables
#'
#' @param con database connection
#'
#' @return dataframe of table names that appear in the dictionary table table.
#' Note, the last table may not have been fully ingested if an error occurred.
select_ingested_tables <- function(con) {
  statement <- "SELECT name FROM sandbox.wilcox_lab.clarity_dictionary_table;"
  DBI::dbGetQuery(con, statement)
}
