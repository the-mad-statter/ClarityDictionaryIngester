#' Clarity Dictionary Ingest
#'
#' @param tables table names to process
#' @param b chromote session
#' @param con database connection
#'
#' @return dataframe representing a logfile including columns table (name),
#' column_index (index of the column in the table), column_name, time_start
#' (before reading), time_inter (after read and before write),
#' time_end (after write), read (success or error message), write
#' (success or error message)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## 0. Setup
#' # - Ensure Windows OS ODBC Data Sources is configured with unexpired token
#' # - may have to wait for cluster startup on connect
#'
#' ## 1. Connect to Database
#' con <- clarity_dictionary_database_connect("Omicron Persi 8")
#'
#' ## 2. Prepare Database (if needed)
#' clarity_dictionary_drop_all(con)
#' clarity_dictionary_init(con)
#'
#' ## 3. Select Tables for Processing
#' tables_to_ingest <- clarity_dictionary_select_tables_to_ingest(con)
#'
#' ## 4. Setup Browser
#' b <- clarity_dictionary_chromote_session_open()
#'
#' ## 5. manual login
#'
#' ## 6. Ingest
#' clarity_dictionary_ingest(tables_to_ingest, b, con)
#'
#' ## 7. On Error
#' # Could drop the last table ingested and all associated records:
#' tables_to_ingest <-
#'   clarity_dictionary_revert_last_table(con, tables_to_ingest)
#'
#' # Could also revert the entire database to a known good timestamp:
#' clarity_dictionary_revert_all(con, "2024-03-07T14:19:08Z")
#'
#' ## 8. Clean Up
#' clarity_dictionary_chromote_session_close(b)
#' clarity_dictionary_database_disconnect(con)
#'
#' ## 9. Query full dictionary
#' clarity_dictionary_select_all(con)
#' }
clarity_dictionary_ingest <- function(tables, b, con) {
  purrr::map_dfr(
    seq_along(tables),
    function(table_i, tables = tables, table_n = length(tables)) {
      table <- tables[table_i]

      navigate_to_table(b, table)

      if (is_wustl_login_page(b)) {
        do_wustl_login(b)
      }

      ingest_column_information_table(b, con, table, table_i, table_n)
    }
  )
}

# nolint start: line_length_linter.

#' Clarity Dictionary Select All
#'
#' @param con database connection
#' @param cat cat the query instead of running it
#'
#' @return dataframe of entire clarity dictionary
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' clarity_dictionary_select_all(con)
#'
#' DBI::dbDisconnect(con)
#' }
clarity_dictionary_select_all <- function(con, cat = FALSE) {
  q <- paste(
    "SELECT ",
    "  table.name AS table ",
    "  , column.table_row ",
    "  , column.name ",
    "  , ini_item.ini ",
    "  , ini_item.item ",
    "  , type.name AS type",
    "  , zc_ny_deprecated.name AS deprecated ",
    "  , zc_ny_discontinued.name AS discontinued ",
    "  , zc_ny_preserved.name AS preserved ",
    "  , zc_ny_character_replacement.name AS character_replacement ",
    "  , zc_ehi_status.name AS ehi_status ",
    "  , column.description_id ",
    "  , description.line ",
    "  , description.description ",
    "FROM ",
    "  sandbox.wilcox_lab.clarity_dictionary_column AS column ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_table AS table ",
    "  ON ",
    "    column.table_id = table.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_type AS type ",
    "  ON ",
    "    column.type_c = type.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_column_ini_item_bridge AS column_ini_item_bridge ",
    "  ON ",
    "    column.id = column_ini_item_bridge.column_id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_ini_item AS ini_item ",
    "  ON ",
    "    column_ini_item_bridge.ini_item_id = ini_item.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_deprecated ",
    "  ON ",
    "    column.deprecated_c = zc_ny_deprecated.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_discontinued ",
    "  ON ",
    "    column.deprecated_c = zc_ny_discontinued.id ",
    "LEFT JOIN ",
    " sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_preserved ",
    "  ON ",
    "    column.deprecated_c = zc_ny_preserved.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_character_replacement ",
    "  ON ",
    "    column.deprecated_c = zc_ny_character_replacement.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_zc_ehi_status AS zc_ehi_status ",
    "  ON ",
    "    column.ehi_status_c = zc_ehi_status.id ",
    "LEFT JOIN ",
    "  sandbox.wilcox_lab.clarity_dictionary_description AS description ",
    "  ON ",
    "    column.description_id = description.id ",
    "ORDER BY ",
    "  table.name, column.table_row, ini_item.ini, ini_item.item, description.line; ",
    collapse = "\n"
  )

  if (cat) {
    cat(q)
  } else {
    DBI::dbGetQuery(con, q)
  }
}

# nolint end

#' Clarity Dictionary Revert All
#'
#' @param con database connection
#' @param datetime datetime for which to revert
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' clarity_dictionary_revert_all(con, "2024-03-07T14:19:08Z")
#'
#' DBI::dbDisconnect(con)
#' }
clarity_dictionary_revert_all <- function(con, datetime) {
  c(
    "column",
    "column_ini_item_bridge",
    "description",
    "ini_item",
    "table",
    "type",
    "zc_ehi_status",
    "zc_ny"
  ) %>%
    sprintf(fmt = "sandbox.wilcox_lab.clarity_dictionary_%s") %>%
    purrr::map_int(
      ~ {
        DBI::dbExecute(
          con,
          sprintf("DELETE FROM %s WHERE dl_load_ts > '%s';", .x, datetime)
        )
      }
    )
}

#' Clarity Dictionary Drop All
#'
#' @param con database connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' clarity_dictionary_drop_all(con)
#'
#' DBI::dbDisconnect(con)
#' }
clarity_dictionary_drop_all <- function(con) {
  c(
    "column",
    "column_ini_item_bridge",
    "description",
    "ini_item",
    "table",
    "type",
    "zc_ehi_status",
    "zc_ny"
  ) %>%
    purrr::map_int(~ drop_table(con, .x))
}

#' Clarity Dictionary Initialize
#'
#' @param con database connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' clarity_dictionary_init(con)
#'
#' DBI::dbDisconnect(con)
#' }
clarity_dictionary_init <- function(con) {
  c(
    c(
      "column",
      "column_ini_item_bridge",
      "description",
      "ini_item",
      "table",
      "type"
    ) %>%
      purrr::map_int(~ create_table(con, .x)),
    create_table_zc_ehi_status(con),
    create_table_zc_ny(con)
  )
}

#' Clarity Dictionary Revert Last Table
#'
#' @param con database connection
#' @param x vector of table names being processed
#'
#' @return table names that still need to be processed
#' @export
#'
#' @examples
#' \dontrun{
#' clarity_table_names_remaining <-
#'   clarity_dictionary_revert_last_table(
#'     con,
#'     clarity_table_names_remaining
#'   )
#' }
clarity_dictionary_revert_last_table <- function(con, x) {
  statement <- paste(
    "SELECT ",
    "  * ",
    "FROM ",
    "  sandbox.wilcox_lab.clarity_dictionary_table ",
    "ORDER BY ",
    "  dl_load_ts DESC ",
    "LIMIT 2;",
    collapse = "\n"
  )
  result <- DBI::dbGetQuery(con, statement)
  result_drop_table <- result %>%
    dplyr::filter(.data[["dl_load_ts"]] == max(.data[["dl_load_ts"]]))
  result_keep_table <- result %>%
    dplyr::filter(.data[["dl_load_ts"]] == min(.data[["dl_load_ts"]]))

  message(sprintf(
    "Reverting database to timestamp: %s...",
    result_keep_table$dl_load_ts
  ))
  clarity_dictionary_revert_all(con, result_keep_table$dl_load_ts)
  message(sprintf(
    "  deleted all records associated with table %s.",
    result_drop_table$name
  ))

  from <- which(x == result_drop_table$name)
  if (length(from) == 0) {
    # x was determined with dropped table partially ingested
    # but the partially ingested table has now been deleted
    # so add it back to the list for processing
    c(result_drop_table$name, x)
  } else {
    to <- length(x)
    x[from:to]
  }
}

#' Clarity Dictionary Select Tables to Ingest
#'
#' @param con database connection
#'
#' @return vector of table names requiring ingest
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   drv = odbc::odbc(),
#'   dsn = "Databricks"
#' )
#'
#' tables_to_ingest <-
#'   clarity_dictionary_select_tables_to_ingest(con)
#'
#' DBI::dbDisconnect(con)
#' }
clarity_dictionary_select_tables_to_ingest <- function(con) {
  select_clarity_tables(con) %>%
    dplyr::anti_join(
      select_ingested_tables(con),
      by = "name"
    ) %>%
    dplyr::pull(.data[["name"]])
}

#' Clarity Dictionary Chromote Session Open
#'
#' @return the chromote session
#' @export
#'
#' @examples
#' \dontrun{
#' b <- clarity_dictionary_chromote_session_open()
#' }
clarity_dictionary_chromote_session_open <- function() {
  b <- chromote::ChromoteSession$new()
  b$view()
  b$Page$navigate("https://datahandbook.epic.com/ClarityDictionary")
  b
}

#' Clarity Dictionary Chromote Session Close
#'
#' @param b chromote session
#'
#' @export
#'
#' @examples
#' \dontrun{
#' b <- clarity_dictionary_chromote_session_open()
#'
#' clarity_dictionary_chromote_session_close(b)
#' }
clarity_dictionary_chromote_session_close <- function(b) {
  b$parent$close()
}

#' Clarity Dictionary Database Connect
#'
#' @param dsn name of dsn entry
#' @param drv driver
#' @param ... additional arguements passed to DBI::dbConnect()
#'
#' @return a database connection
#' @export
#'
#' @examples
#' \dontrun{
#' con <- clarity_dictionary_database_connect(dsn = "Omicron Persi 8")
#' }
clarity_dictionary_database_connect <-
  function(dsn = "Databricks", drv = odbc::odbc(), ...) {
    DBI::dbConnect(drv = drv, dsn = dsn, ...)
  }

#' Clarity Dictionary Database Disconnect
#'
#' @param con database connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- clarity_dictionary_database_connect(dsn = "Omicron Persi 8")
#'
#' clarity_dictionary_database_disconnect(con)
#' }
clarity_dictionary_database_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}
