# clarity_dictionary_select_all_sql()

    Code
      clarity_dictionary_select_all_sql()
    Output
      [1] "SELECT \n  table.name AS table \n  , column.table_row \n  , column.name \n  , ini_item.ini \n  , ini_item.item \n  , type.name AS type\n  , zc_ny_deprecated.name AS deprecated \n  , zc_ny_discontinued.name AS discontinued \n  , zc_ny_preserved.name AS preserved \n  , zc_ny_character_replacement.name AS character_replacement \n  , zc_ehi_status.name AS ehi_status \n  , column.description_id \n  , description.line \n  , description.description \nFROM \n  sandbox.wilcox_lab.clarity_dictionary_column AS column \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_table AS table \n  ON \n    column.table_id = table.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_type AS type \n  ON \n    column.type_c = type.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_column_ini_item_bridge AS column_ini_item_bridge \n  ON \n    column.id = column_ini_item_bridge.column_id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_ini_item AS ini_item \n  ON \n    column_ini_item_bridge.ini_item_id = ini_item.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_deprecated \n  ON \n    column.deprecated_c = zc_ny_deprecated.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_discontinued \n  ON \n    column.deprecated_c = zc_ny_discontinued.id \nLEFT JOIN \n sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_preserved \n  ON \n    column.deprecated_c = zc_ny_preserved.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_zc_ny zc_ny_character_replacement \n  ON \n    column.deprecated_c = zc_ny_character_replacement.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_zc_ehi_status AS zc_ehi_status \n  ON \n    column.ehi_status_c = zc_ehi_status.id \nLEFT JOIN \n  sandbox.wilcox_lab.clarity_dictionary_description AS description \n  ON \n    column.description_id = description.id \nORDER BY \n  table.name, column.table_row, ini_item.ini, ini_item.item, description.line; "
