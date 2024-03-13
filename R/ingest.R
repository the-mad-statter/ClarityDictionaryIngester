#' Scrape Column Information
#'
#' @param b chromote session
#' @param tr_i index of the initial html row for the column
#' @param column_i index of the column in the clarity table
#' @param column_n total columns in the clarity table
#' @param table clarity table name
#' @param table_i index of the current clarity table being processed
#' @param table_n total number of clarity tables to be processed
#'
#' @return a list of dataframes to pass to write_column_information() for
#' writing to the database (table, type, ini_item, description, and column)
scrape_column_information <- function(
    b,
    tr_i,
    column_i,
    column_n,
    table,
    table_i,
    table_n) {
  # select html of a specific child <tr> of table containing column information
  fmt <- paste(
    '$("#____Column-Information____").parents().eq(2)',
    'next().children("tbody").children().eq(%i).html()',
    sep = "."
  )

  js_1 <- sprintf(fmt, tr_i)
  row_1 <- b$Runtime$evaluate(js_1)$result$value %>%
    rvest::read_html() %>%
    rvest::html_elements("body > td")

  # nolint start: commented_code_linter, line_length_linter.

  ## selects children <td>s like these three examples:
  #
  ## Example 1 (0 ini-item pairs):
  # {xml_nodeset (11)}
  #  [1] <td class="T1Head" style="padding: 5px;">2</td>
  #  [2] <td class="T1Head">LINE</td>
  #  [3] <td><table class="SubList"></table></td>
  #  [4] <td><table class="SubList"></table></td>
  #  [5] <td>INTEGER</td>
  #  [6] <td>No</td>
  #  [7] <td>No</td>
  #  [8] <td>No</td>
  #  [9] <td>No</td>
  # [10] <td>Exported</td>
  # [11] <td width="90%"></td>
  #
  ## Example 2 (1 ini-item pairs):
  # {xml_nodeset (11)}
  #  [1] <td class="T1Head" style="padding: 5px;">1</td>
  #  [2] <td class="T1Head">PAT_ENC_CSN_ID</td>
  #  [3] <td><table class="SubList"><tbody><tr>\n<td>EPT</td>\n</tr></tbody></table></td>
  #  [4] <td><table class="SubList"><tbody><tr>\n<td>8</td>\n</tr></tbody></table></td>
  #  [5] <td>NUMERIC (18,0) </td>
  #  [6] <td>No</td>
  #  [7] <td>No</td>
  #  [8] <td>No</td>
  #  [9] <td>No</td>
  # [10] <td>Exported</td>
  # [11] <td width="90%"></td>
  #
  ## Example 3 (2 ini-item pairs):
  # {xml_nodeset (11)}
  #  [1] <td class="T1Head" style="padding: 5px;">15</td>
  #  [2] <td class="T1Head">BIRTH_DATE</td>
  #  [3] <td><table class="SubList"><tbody>\n<tr>\n<td>EPT</td>\n</tr>\n<tr>\n<td>EPT</td>\n</tr>\n</tbody></table></td>
  #  [4] <td><table class="SubList"><tbody>\n<tr>\n<td>110</td>\n</tr>\n<tr>\n<td>111</td>\n</tr>\n</tbody></table></td>
  #  [5] <td>DATETIME (Local) </td>
  #  [6] <td>No</td>
  #  [7] <td>No</td>
  #  [8] <td>No</td>
  #  [9] <td>No</td>
  # [10] <td>Exported</td>
  # [11] <td width="90%"></td>

  # nolint end

  js_2 <- sprintf(fmt, tr_i + 1)
  row_2 <- b$Runtime$evaluate(js_2)$result$value %>%
    rvest::read_html()

  column <- rvest::html_text(row_1[2])

  message(
    sprintf(
      "Processing Table: %s (%s of %s); Column: %s (%i of %i)...",
      table,
      table_i,
      table_n,
      column,
      column_i,
      column_n
    )
  )

  # 1. table
  table <- dplyr::tibble(
    id = NA_integer_,
    name = table
  )

  # 2. type
  type <- dplyr::tibble(
    id = NA_integer_,
    name = rvest::html_text(row_1[5]) %>%
      trimws()
  )

  # 3. ini_item (0 or more rows)
  css <- "table > tbody > tr"
  ini_item <- dplyr::tibble(
    id = NA_integer_,
    ini = rvest::html_elements(row_1[3], css) %>%
      rvest::html_text() %>%
      trimws(),
    item = rvest::html_elements(row_1[4], css) %>%
      rvest::html_text() %>%
      trimws()
  )

  # 4. description
  full_desc <- row_2 %>%
    rvest::html_text() %>%
    trimws()
  varchar_limit <- 254
  v <- seq(1, nchar(full_desc), varchar_limit)
  split_desc <- substring(full_desc, v, v + varchar_limit - 1)

  description <- dplyr::tibble(
    id          = NA_integer_,
    line        = seq_along(split_desc),
    description = split_desc
  )

  # 5. column
  column <- dplyr::tibble(
    id = NA_integer_,
    table_id = NA_integer_,
    table_row = column_i,
    name = column,
    type_c = NA_integer_,
    deprecated_c = map_no_yes_to_0_1(rvest::html_text(row_1[6])),
    discontinued_c = map_no_yes_to_0_1(rvest::html_text(row_1[7])),
    preserved_c = map_no_yes_to_0_1(rvest::html_text(row_1[8])),
    character_replacement_c = map_no_yes_to_0_1(rvest::html_text(row_1[9])),
    ehi_status_c = map_not_exported_exported_to_0_1(
      rvest::html_text(row_1[10])
    ),
    description_id = NA_integer_
  )

  list(
    table = table,
    type = type,
    ini_item = ini_item,
    description = description,
    column = column
  )
}

#' Write Column Information
#'
#' @param con database connection
#' @param data list of dataframes from scrape_column_information() to write to
#' the database
write_column_information <- function(con, data) {
  # 1. table
  table_id <- select_or_append(con, "table", data$table)

  # 2. type
  type_c <- select_or_append(con, "type", data$type)

  # 3. ini_item (ini_item_id will be 0 or more length integer vector)
  ini_item_id <- purrr::pmap_int(data$ini_item, function(...) {
    select_or_append(con, "ini_item", dplyr::tibble(...))
  })

  # 4. description
  description_id <- select_new_id(con, "description")
  data$description$id <- description_id
  data$description$dl_load_ts <- Sys.time()
  DBI::dbWriteTable(
    con,
    DBI::Id(
      catalog = "sandbox",
      schema  = "wilcox_lab",
      table   = "clarity_dictionary_description"
    ),
    data$description,
    append = TRUE
  )

  # 5. column
  column_id <- select_new_id(con, "column")
  data$column$id <- column_id
  data$column$table_id <- table_id
  data$column$type_c <- type_c
  data$column$description_id <- description_id
  data$column$dl_load_ts <- Sys.time()
  DBI::dbWriteTable(
    con,
    DBI::Id(
      catalog = "sandbox",
      schema  = "wilcox_lab",
      table   = "clarity_dictionary_column"
    ),
    data$column,
    append = TRUE
  )

  # 6. ini_item_bridge
  if (length(ini_item_id) > 0) {
    DBI::dbWriteTable(
      con,
      DBI::Id(
        catalog = "sandbox",
        schema  = "wilcox_lab",
        table   = "clarity_dictionary_column_ini_item_bridge"
      ),
      dplyr::tibble(
        column_id = column_id,
        ini_item_id = ini_item_id,
        dl_load_ts = Sys.time()
      ),
      append = TRUE
    )
  }
}

#' Ingest Column Information Table
#'
#' @param b chromote session
#' @param con database connection
#' @param table name of the table
#' @param table_i index of the table being processed out of table_n
#' @param table_n total number of tables being processed
#'
#' @return dataframe representing a logfile including columns table (name),
#' column_index (index of the column in the table), column_name, time_start
#' (before reading), time_inter (after read and before write),
#' time_end (after write), read (success or error message), write
#' (success or error message)
ingest_column_information_table <- function(b,
                                            con,
                                            table,
                                            table_i,
                                            table_n) {
  js <- paste(
    '$("#____Column-Information____").parents().eq(2)',
    'next().children("tbody").children().length',
    sep = "."
  )
  n_tr <- b$Runtime$evaluate(js)$result$value
  init_trs <- seq(1, n_tr - 1, 3)

  purrr::map_dfr(
    seq_along(init_trs),
    function(column_i,
             b = b,
             init_trs = init_trs,
             column_n = length(init_trs),
             table = table,
             table_i = table_i,
             table_n = table_n,
             con = con) {
      time_start <- Sys.time()

      read_result <- tryCatch(
        {
          data <- scrape_column_information(
            b,
            init_trs[column_i],
            column_i,
            column_n,
            table,
            table_i,
            table_n
          )
          "Success"
        },
        error = function(e) conditionMessage(e),
        warning = function(e) conditionMessage(e)
      )

      time_inter <- Sys.time()

      write_result <- tryCatch(
        {
          write_column_information(con, data)
          "Success"
        },
        error = function(e) conditionMessage(e),
        warning = function(e) conditionMessage(e)
      )

      time_end <- Sys.time()

      # ensure a value in the case of bad read
      column_name <- ifelse(
        read_result == "Success",
        data$column$name,
        NA_character_
      )

      results <- dplyr::tibble(
        table        = table,
        column_index = column_i,
        column_name  = column_name,
        time_start   = time_start,
        time_inter   = time_inter,
        time_end     = time_end,
        read         = read_result,
        write        = write_result
      )

      readr::write_csv(results, "clarity_dictionary_log.csv", append = TRUE)

      results
    }
  )
}
