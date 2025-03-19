#' Clean and Process Microbiome Data
#'
#' Processes microbiome and metadata files (e.g., 16S rRNA sequencing data) to produce an analysis-ready dataset.
#' Supports CSV, TXT, and 'Excel' file formats. This function validates file formats, reads the data,
#' and merges the datasets by the common column 'SampleID'. If a 'Taxonomy' column exists, the data
#' are filtered to include only rows matching the provided taxonomic level.
#'
#' @param microbiome_data A string specifying the path to the microbiome data file.
#' @param metadata A string specifying the path to the metadata file.
#' @param level A string indicating the taxonomic level for filtering the data (e.g., "genus").
#'
#' @return A data frame containing the cleaned and merged dataset.
#'
#' @examples
#' \donttest{
#'   # Example usage (ensure that 'inst/extdata' contains the appropriate files,
#'   # or modify this example to use your own data)
#'   microbiome_data <- system.file("extdata", "microbiome.csv", package = "mbX")
#'   metadata <- system.file("extdata", "metadata.csv", package = "mbX")
#'   if (nzchar(microbiome_data) && nzchar(metadata)) {
#'     cleaned_data <- ezclean(microbiome_data, metadata, "genus")
#'     head(cleaned_data)
#'   } else {
#'     message("Sample data files not found.")
#'   }
#' }
#'
#' @importFrom stats aggregate
#' @importFrom utils read.delim read.table
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @import dplyr
#' @import tidyr
#' @export
ezclean <- function(microbiome_data, metadata, level = "d") {
  # Check the file extension for microbiome_data
  microbiome_ext <- tools::file_ext(microbiome_data)
  if (!microbiome_ext %in% c("csv", "xls", "xlsx")) {
    return("The file is not csv, xls, or xlsx format. Please check the file type for microbiome data.")
  }
  
  # Check the file extension for metadata
  metadata_ext <- tools::file_ext(metadata)
  if (metadata_ext == "txt") {
    metadata_df <- utils::read.delim(metadata, sep = "\t", header = TRUE, check.names = FALSE)
  } else if (metadata_ext == "csv") {
    metadata_df <- read.csv(metadata, header = TRUE, check.names = FALSE)
  } else if (metadata_ext %in% c("xls", "xlsx")) {
    metadata_df <- readxl::read_excel(metadata, col_names = TRUE)
    # Optionally, convert to data.frame if needed:
    metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
  } else {
    return("Please check the file format of metadata.")
  }
  
  
  
  
  # Checking the first header of metadata
  valid_headers <- c("id", "sampleid", "sample id", "sample-id", "featureid", "feature id", "feature-id")
  # Ensure that the header is trimmed of any leading or trailing whitespace and converted to lower case
  if (!(tolower(trimws(names(metadata_df)[1])) %in% valid_headers)) {
    return("Please check the first header of the metadata for file format correction.")
  }
  
  
  
  # Read microbiome data
  if (microbiome_ext == "txt") {
    microbiome_df <- read.delim(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext == "csv") {
    microbiome_df <- read.csv(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext %in% c("xls", "xlsx")) {
    # Convert tibble to data.frame to preserve names as-is.
    microbiome_df <- as.data.frame(readxl::read_excel(microbiome_data, skip = 1), check.names = FALSE)
  } else {
    return("The microbiome file is not in a supported format. Please use txt, csv, xls, or xlsx.")
  }
  
  # Define the levels map with all variations
  levels_map <- c(
    "domain" = "d__", "Domain" = "d__", "DOMAIN" = "d__", "D" = "d__", "d" = "d__",
    "kingdom" = "d__", "Kingdom" = "d__", "KINGDOM" = "d__", "K" = "d__", "k" = "d__",
    "phylum" = "p__", "Phylum" = "p__", "PHYLUM" = "p__", "P" = "p__", "p" = "p__",
    "class" = "c__", "Class" = "c__", "CLASS" = "c__", "C" = "c__", "c" = "c__",
    "order" = "o__", "Order" = "o__", "ORDER" = "o__", "O" = "o__", "o" = "o__",
    "family" = "f__", "Family" = "f__", "FAMILY" = "f__", "F" = "f__", "f" = "f__",
    "genera" = "g__", "genus" = "g__", "Genera" = "g__", "GENERA" = "g__", "G" = "g__", "g" = "g__",
    "species" = "s__", "Species" = "s__", "SPECIES" = "s__", "S" = "s__", "s" = "s__"
  )
  
  # Convert input level parameter to the correct taxonomy prefix
  level_key <- levels_map[tolower(level)]
  if (is.null(level_key)) {
    return(sprintf("The level value should be one of the following: domain, phylum, class, order, family, genera, species or their abbreviations."))
  }
  level_value <- level_key
  
  
  # Remove columns in microbiome data that are not in metadata headers (common columns)
  common_cols <- intersect(colnames(microbiome_df), colnames(metadata_df))
  just_microbiome_df <- microbiome_df[ , !(colnames(microbiome_df) %in% common_cols), drop = FALSE]
  just_metadata_df <- metadata_df[ , (colnames(metadata_df) %in% common_cols), drop = FALSE]
  
  
  # Save the files
  write.xlsx(just_microbiome_df, "just_microbiome.xlsx")
  write.xlsx(just_metadata_df, "just_metadata.xlsx")
  
  # Transpose just_microbiome_df and save as microbiome_ezy-1.xlsx
  # Transpose just_microbiome_df exactly as in Excel's paste special transpose
  # ---- Updated Transposition Block Start ----
  # Combine the header row with the data to mimic Excel's full-range transpose
  df_to_transpose <- rbind(names(just_microbiome_df), just_microbiome_df)
  # Convert all values to character (so that Excel sees exactly what you expect)
  df_to_transpose <- data.frame(lapply(df_to_transpose, as.character), stringsAsFactors = FALSE)
  # Transpose the entire dataset (including the header row)
  transposed_microbiome <- t(df_to_transpose)
  # Convert the transposed matrix back to a data frame
  transposed_microbiome_df <- as.data.frame(transposed_microbiome, stringsAsFactors = FALSE)
  # Write the transposed data to Excel without row names and without the auto-generated column names
  write.xlsx(transposed_microbiome_df, "microbiome_ezy_1.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Updated Transposition Block End ----
  
  
  
  
  #CONVERTING THE ENTIRE TEXT INTO NUMERICAL AND TEXT, AS MEZY_1 EXCEL IS ALL TEXT VALUED
  # ---- Additional Conversion Block Start ----
  # Read the previously generated Excel file (all values as text)
  df_ezy1 <- openxlsx::read.xlsx("microbiome_ezy_1.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row <- df_ezy1[1, ]
  data_rows  <- df_ezy1[-1, ]
  
  # Convert all columns except the first in data_rows to numeric
  data_rows[, -1] <- lapply(data_rows[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  
  # Write the header row as text.
  # Convert header_row (a one-row data frame) to a vector and transpose it
  header_vec <- as.vector(unlist(header_row))
  openxlsx::writeData(wb, sheet = "Sheet1", x = t(header_vec),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  # The first column remains text while columns 2:end are numeric
  openxlsx::writeData(wb, sheet = "Sheet1", x = data_rows,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_2.xlsx
  openxlsx::saveWorkbook(wb, "microbiome_ezy_2.xlsx", overwrite = TRUE)
  # ---- Additional Conversion Block End ----
  
  ####ADD A NEW BLANK COLUMN AS TAXA AS THE SECOND COLUMN
  
  # ---- Additional Blank Column Block Start ----
  # Read the file microbiome_ezy_2.xlsx (all values are as saved)
  df_ezy2 <- openxlsx::read.xlsx("microbiome_ezy_2.xlsx", colNames = FALSE)
  
  # Convert the data frame to a matrix for easy column insertion
  m_ezy2 <- as.matrix(df_ezy2)
  
  # Create a new matrix by binding:
  # - The first column of m_ezy2,
  # - A new column (with a blank value for each row, except the header),
  # - The remaining columns of m_ezy2.
  new_matrix <- cbind(m_ezy2[, 1, drop = FALSE],
                      rep("", nrow(m_ezy2)),
                      m_ezy2[, -1, drop = FALSE])
  
  # Set the header (first row, second column) to "Taxa"
  new_matrix[1, 2] <- "Taxa"
  
  # Write the new matrix as microbiome_ezy_3.xlsx
  openxlsx::write.xlsx(as.data.frame(new_matrix, stringsAsFactors = FALSE),
                       "microbiome_ezy_3.xlsx",
                       rowNames = FALSE, colNames = FALSE)
  # ---- Additional Blank Column Block End ----
  
  
  
  
  
  
  
  ####REPLACING THE LEVEL VALUE WITH THE @
  
  # Read the microbiome_ezy_3.xlsx file without any modifications
  df_ezy3 <- openxlsx::read.xlsx("microbiome_ezy_3.xlsx", colNames = FALSE)
  
  # Save the data as microbiome_ezy_4.xlsx
  openxlsx::write.xlsx(df_ezy3, "microbiome_ezy_4.xlsx", rowNames = FALSE, colNames = FALSE)
  
  
  ####GRABBING THE TAXA NAME FROM THE FIRST COLUMN AND PUTTING IT INTO SECOND COL "TAXA" 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---- Additional Extraction and Paste Block Start ----
  # Read the microbiome_ezy_4.xlsx file (all data, including header)
  df_ezy4 <- openxlsx::read.xlsx("microbiome_ezy_4.xlsx", colNames = FALSE)
  
  # Define the taxonomic level order (highest to lowest)
  levels_order <- c("d__", "p__", "c__", "o__", "f__", "g__", "s__")
  
  # Define a mapping from abbreviated level markers to full names
  level_mapping <- c("d__" = "domain", "p__" = "phylum", "c__" = "class", 
                     "o__" = "order", "f__" = "family", "g__" = "genus", "s__" = "species")
  
  # Assume level_value is defined (e.g., "s__") and is one of the above.
  # For example:
  # level_value <- "s__"
  
  blank_counter <- 0  # to count how many blanks (unidentified) occur
  
  # Process only if there is more than just the header row
  if (nrow(df_ezy4) > 1) {
    # For each data row (excluding the header)
    extracted_text <- sapply(df_ezy4[-1, 1], function(x) {
      # Only attempt extraction if the target marker exists in the string
      if (grepl(level_value, x)) {
        pattern_target <- paste0(".*", level_value, "\\s*([^;]*)(;.*)?$")
        candidate <- trimws(sub(pattern_target, "\\1", x))
      } else {
        candidate <- ""
      }
      
      if (candidate != "") {
        # If extraction for level_value is nonblank, return it
        return(candidate)
      } else {
        # If blank, search for a previous (higher) level in the defined order
        target_idx <- match(level_value, levels_order)
        found_value <- ""
        found_marker <- ""
        if (!is.na(target_idx) && target_idx > 1) {
          # Look upward from the immediate preceding marker down to "d__"
          for (i in seq(from = target_idx - 1, to = 1)) {
            if (grepl(levels_order[i], x)) {
              pattern_prev <- paste0(".*", levels_order[i], "\\s*([^;]*)(;.*)?$")
              candidate_prev <- trimws(sub(pattern_prev, "\\1", x))
              if (candidate_prev != "") {
                found_value <- candidate_prev
                found_marker <- levels_order[i]
                break  # stop at the first nonblank found
              }
            }
          }
        }
        if (found_value != "") {
          blank_counter <<- blank_counter + 1
          return(paste0("unidentified_", level_mapping[level_value], "_", blank_counter,
                        "_at_", found_value, "_", level_mapping[found_marker]))
        } else {
          return("")
        }
      }
    })
    
    # Paste the extracted (or constructed) text into the second column for data rows
    df_ezy4[-1, 2] <- extracted_text
  }
  
  # Save the updated data as microbiome_ezy_5.xlsx without extra row or column names
  openxlsx::write.xlsx(df_ezy4, "microbiome_ezy_5.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Extraction and Paste Block End ----
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### DELETE THE FIRST COLUMN THE "INDEX" COLUMN WITH THE LONG NAME OF THE TAXA
  
  # ---- Additional Delete First Column Block Start ----
  # Read the microbiome_ezy_5.xlsx file (all data, including header)
  df_ezy5 <- openxlsx::read.xlsx("microbiome_ezy_5.xlsx", colNames = FALSE)
  
  # Delete the first column entirely
  df_ezy5 <- df_ezy5[, -1, drop = FALSE]
  
  # Save the updated data as microbiome_ezy_6.xlsx without extra row or column names
  openxlsx::write.xlsx(df_ezy5, "microbiome_ezy_6.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Delete First Column Block End ----
  
  
  
  ####AGAIN CONVERTING THE THINGS INTO NUMERICALS
  #####
  # ---- Additional Numeric Conversion Block for ezy_7 Start ----
  # Read the microbiome_ezy_6.xlsx file (all values as text)
  df_ezy6 <- openxlsx::read.xlsx("microbiome_ezy_6.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row_6 <- df_ezy6[1, ]
  data_rows_6  <- df_ezy6[-1, ]
  
  # Convert all columns except the first in data_rows_6 to numeric
  data_rows_6[, -1] <- lapply(data_rows_6[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "Sheet1")
  
  # Write the header row as text.
  header_vec_6 <- as.vector(unlist(header_row_6))
  openxlsx::writeData(wb2, sheet = "Sheet1", x = t(header_vec_6),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb2, sheet = "Sheet1", x = data_rows_6,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_7.xlsx
  openxlsx::saveWorkbook(wb2, "microbiome_ezy_7.xlsx", overwrite = TRUE)
  # ---- Additional Numeric Conversion Block for ezy_7 End ----
  
  ###HANDLING THE SAME TAXA NAME AND THE BLANKS 
  
  # ---- Additional Aggregation Block Start ----
  # Read the microbiome_ezy_7.xlsx file (all data, including the header row)
  df_ezy7 <- openxlsx::read.xlsx("microbiome_ezy_7.xlsx", colNames = FALSE)
  
  # Separate the header row (first row) and the data rows (remaining rows)
  header_row_7 <- df_ezy7[1, ]
  data_rows_7  <- df_ezy7[-1, ]
  
  # Ensure the first column is character and all other columns are numeric
  data_rows_7[, 1] <- as.character(data_rows_7[, 1])
  data_rows_7[, -1] <- lapply(data_rows_7[, -1, drop = FALSE], function(x) as.numeric(x))
  
  # Define replacement names based on the level_value
  other_names <- list(
    "d__" = "Other_domains",
    "p__" = "Other_phyla",
    "c__" = "Other_classes",
    "o__" = "Other_orders",
    "f__" = "Other_families",
    "g__" = "Other_genera",
    "s__" = "Other_species"
  )
  
  # Determine the appropriate replacement name using the level_value
  replacement_name <- other_names[[level_value]]
  
  # Replace rows with missing taxa information (NA, blank, or "#VALUE") with the determined replacement_name
  data_rows_7[, 1] <- ifelse(is.na(data_rows_7[, 1]) | data_rows_7[, 1] == "" | data_rows_7[, 1] == "#VALUE",
                             replacement_name, data_rows_7[, 1])
  
  # Aggregate the data by the taxa (first) column:
  # For rows with the exact same taxa value, sum the numeric columns column-wise.
  aggregated_df <- aggregate(data_rows_7[, -1],
                             by = list(Taxa = data_rows_7[, 1]),
                             FUN = sum, na.rm = TRUE)
  
  
  
  
  
  # Set the column names of the aggregated data using the original header row.
  # Convert the header row to a vector.
  new_header <- as.character(unlist(header_row_7))
  # Ensure the first header is "Taxa" (or any appropriate label)
  new_header[1] <- "Taxa"
  colnames(aggregated_df) <- new_header
  
  # Create a new workbook and add a worksheet
  wb3 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb3, "Sheet1")
  
  # Write the aggregated data (including header) starting at row 1
  openxlsx::writeData(wb3, sheet = "Sheet1", x = aggregated_df,
                      startRow = 1, startCol = 1, colNames = TRUE)
  
  # Save the new workbook as microbiome_ezy_8.xlsx
  openxlsx::saveWorkbook(wb3, "microbiome_ezy_8.xlsx", overwrite = TRUE)
  # ---- Additional Aggregation Block End ----
  
  
  
  
  
  
  ####NOW CONVERTING THE NUMBERS TO PERCENTAGES 
  # ---- Additional Column-wise Percentage Block Start ----
  # Read the microbiome_ezy_8.xlsx file with header information
  microbiome_ezy8 <- openxlsx::read.xlsx("microbiome_ezy_8.xlsx", colNames = TRUE)
  microbiome_ezy8 <- as.data.frame(microbiome_ezy8)
  
  # Extract the taxa column (first column) and numeric columns (columns 2 onward)
  taxa_column <- microbiome_ezy8[, 1]
  numeric_columns <- microbiome_ezy8[, -1, drop = FALSE]
  
  # Ensure all numeric columns are properly converted to numeric
  numeric_columns[] <- lapply(numeric_columns, as.numeric)
  
  # Calculate the column-wise percentages:
  # Divide each element by the column sum (ignoring NA's) and multiply by 100
  percentage_columns <- sweep(numeric_columns, 2, colSums(numeric_columns, na.rm = TRUE), "/") * 100
  
  # Combine the taxa column with the calculated percentage columns
  microbiome_ezy9 <- cbind(taxa_column, percentage_columns)
  
  # Save the resulting data frame as microbiome_ezy_9.xlsx without row names
  openxlsx::write.xlsx(microbiome_ezy9, "microbiome_ezy_9.xlsx", rowNames = FALSE)
  # ---- Additional Column-wise Percentage Block End ----
  
  
  
  
  
  ####TRANSPOSING THE DATA 
  
  # ---- Additional Transposition for ezy_9 to ezy_10 Block Start ----
  # Read the microbiome_ezy_9.xlsx file with header information
  df_ezy9 <- openxlsx::read.xlsx("microbiome_ezy_9.xlsx", colNames = TRUE)
  
  # Combine the header row with the data to mimic Excel's full-range transpose
  df_to_transpose_9 <- rbind(names(df_ezy9), df_ezy9)
  
  # Convert all values to character (so that Excel sees exactly what you expect)
  df_to_transpose_9 <- data.frame(lapply(df_to_transpose_9, as.character), stringsAsFactors = FALSE)
  
  # Transpose the entire dataset (including the header row)
  transposed_ezy9 <- t(df_to_transpose_9)
  
  # Convert the transposed matrix back to a data frame
  transposed_ezy9_df <- as.data.frame(transposed_ezy9, stringsAsFactors = FALSE)
  
  # Write the transposed data to Excel without row names and without auto-generated column names
  openxlsx::write.xlsx(transposed_ezy9_df, "microbiome_ezy_10.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Transposition for ezy_9 to ezy_10 Block End ----
  
  
  
  
  
  #####GETTING THE ORIGINAL HEADER FOR THE FIRST COLUMN LIKE SAMPLE-ID
  
  # ---- Additional Header Replacement Block Start ----
  # Extract the header of the first column from the metadata file
  metadata_first_header <- names(metadata_df)[1]
  
  # Read the microbiome_ezy_10.xlsx file; since it was written without column names,
  # the first row actually contains the header information.
  df_ezy10 <- openxlsx::read.xlsx("microbiome_ezy_10.xlsx", colNames = FALSE)
  
  # Extract the first row as the current header and convert to a character vector
  new_header <- as.character(unlist(df_ezy10[1, ]))
  
  # Replace the first element with the metadata header
  new_header[1] <- metadata_first_header
  
  # Remove the first row (which contained the original header) from the data
  df_ezy10_data <- df_ezy10[-1, ]
  
  # Assign the new header to the data
  colnames(df_ezy10_data) <- new_header
  
  # Save the updated data as microbiome_ezy_11.xlsx with proper column names
  openxlsx::write.xlsx(df_ezy10_data, "microbiome_ezy_11.xlsx", rowNames = FALSE, colNames = TRUE)
  # ---- Additional Header Replacement Block End ----
  
  
  
  
  
  
  #### SETTING THE NUMERIC AS ALL VALUES ARE TEXT AGAIN
  # ---- Additional Numeric Conversion Block for ezy_12 Start ----
  # Read the microbiome_ezy_11.xlsx file (all values as text)
  df_ezy11 <- openxlsx::read.xlsx("microbiome_ezy_11.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row_11 <- df_ezy11[1, ]
  data_rows_11  <- df_ezy11[-1, ]
  
  # Convert all columns except the first in data_rows_11 to numeric
  data_rows_11[, -1] <- lapply(data_rows_11[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb12 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb12, "Sheet1")
  
  # Write the header row as text.
  header_vec_11 <- as.vector(unlist(header_row_11))
  openxlsx::writeData(wb12, sheet = "Sheet1", x = t(header_vec_11),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb12, sheet = "Sheet1", x = data_rows_11,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_12.xlsx
  openxlsx::saveWorkbook(wb12, "microbiome_ezy_12.xlsx", overwrite = TRUE)
  # ---- Additional Numeric Conversion Block for ezy_12 End ----
  
  
  
  
  
  
  
  
  #ADDING THE METEDATACOLUMNS TO THE CLEANED DATA SET
  # ---- Additional Metadata Merge Block Start ----
  # Read the microbiome_ezy_12.xlsx file (with header)
  df_ezy12 <- openxlsx::read.xlsx("microbiome_ezy_12.xlsx", colNames = TRUE)
  
  # Use the metadata_df that was loaded earlier in the function.
  # Assume that the first column in metadata_df is the key to match.
  # Extract the remaining metadata columns (i.e., all except the first)
  meta_remaining <- metadata_df[, -1, drop = FALSE]
  
  # Extract the key values from metadata_df and df_ezy12
  meta_key <- metadata_df[[1]]
  df_key   <- df_ezy12[[1]]
  
  # For each row in df_ezy12, find the matching row in metadata_df
  # and extract the corresponding remaining metadata columns.
  appended_metadata <- meta_remaining[match(df_key, meta_key), , drop = FALSE]
  
  # Combine the microbiome_ezy_12 data with the appended metadata columns
  df_ezy13 <- cbind(df_ezy12, appended_metadata)
  
  # Save the combined data as microbiome_ezy_13.xlsx with headers
  openxlsx::write.xlsx(df_ezy13, "microbiome_ezy_13.xlsx", rowNames = FALSE)
  # ---- Additional Metadata Merge Block End ----
  
  
  
  
  #I WILL DELETE ALL THE INTERMEDIATE FILES, KEEPING THEM FOR NOW FOR THE DEBUGGING PURPOSE, FOR THE FINAL EZCLEAN THERE WILL ONLY BE MICROBIOME_EZY_13.XLSX
  # THE FINAL FILE NAME WILL BE NAMED AS MICROBIOME_CLEANED_DATA.XLSX
  
  
  # Return the modified level value for further steps
  #return(list(just_microbiome_path = "just_microbiome.xlsx",
  #just_metadata_path = "just_metadata.xlsx",
  #just_microbiome_transposed = "microbiome_ezy_1.xlsx",
  #df_ezy2= "microbiome_ezy_2.xlsx",
  #df_ezy3= "microbiome_ezy_3.xlsx",
  #df_ezy4= "microbiome_ezy_4.xlsx",
  #df_ezy5= "microbiome_ezy_5.xlsx",
  #df_ezy6= "microbiome_ezy_6.xlsx",
  #df_ezy7= "microbiome_ezy_7.xlsx",
  #df_ezy8= "microbiome_ezy_8.xlsx",
  #df_ezy9= "microbiome_ezy_9.xlsx",
  #df_ezy10= "microbiome_ezy_10.xlsx",
  #df_ezy11= "microbiome_ezy_11.xlsx",
  #df_ezy12= "microbiome_ezy_12.xlsx",
  #df_ezy13= "microbiome_ezy_13.xlsx",
  #level = level_value))
  
  
  
  
  #####REMOVING THE INTERMEDIATE FILES IF ANY WRITTEN IN THE CURRENT DIRECTORY
  
  # List of intermediate files to be removed (excluding the final file "microbiome_ezy_13.xlsx")
  intermediate_files <- c(
    "just_microbiome.xlsx",
    "just_metadata.xlsx",
    "microbiome_ezy_1.xlsx",
    "microbiome_ezy_2.xlsx",
    "microbiome_ezy_3.xlsx",
    "microbiome_ezy_4.xlsx",
    "microbiome_ezy_5.xlsx",
    "microbiome_ezy_6.xlsx",
    "microbiome_ezy_7.xlsx",
    "microbiome_ezy_8.xlsx",
    "microbiome_ezy_9.xlsx",
    "microbiome_ezy_10.xlsx",
    "microbiome_ezy_11.xlsx",
    "microbiome_ezy_12.xlsx"
  )
  
  # Remove each intermediate file if it exists
  sapply(intermediate_files, function(f) {
    if (file.exists(f)) file.remove(f)
  })
  #############
  
  
  
  
  
  
  
  
  ####FINAL NAME RETURN
  
  
  # ---- Final Return Block ----
  final_names <- list(
    "d__" = "mbX_cleaned_domains_or_kingdom.xlsx",
    "p__" = "mbX_cleaned_phylum.xlsx",
    "c__" = "mbX_cleaned_classes.xlsx",
    "o__" = "mbX_cleaned_orders.xlsx",
    "f__" = "mbX_cleaned_families.xlsx",
    "g__" = "mbX_cleaned_genera.xlsx",
    "s__" = "mbX_cleaned_species.xlsx"
  )
  
  final_file_name <- final_names[[level_value]]
  file.rename("microbiome_ezy_13.xlsx", final_file_name)
  
  return(final_file_name)
  cat("new ho hai")
}



#ezclean("level-7.csv", "metadata.txt", "f")
#ezclean("level-7.csv", "metadata.txt", "g")
#ezclean("level-7.csv", "metadata.txt", "s")





#' Visualize Microbiome Data
#'
#' Generates publication-ready visualizations for microbiome data. This function first processes
#' the microbiome and metadata files using ezclean(), then creates a bar plot using ggplot2.
#' Supported file formats are CSV, TXT, and 'Excel'. Note: Only one of the parameters top_taxa or threshold
#' should be provided.
#'
#' @param microbiome_data A string specifying the path to the microbiome data file.
#' @param metadata A string specifying the path to the metadata file.
#' @param level A string indicating the taxonomic level for filtering the data (e.g., "genus").
#' @param selected_metadata A string specifying the metadata column used for grouping.
#' @param top_taxa An optional numeric value indicating the number of top taxa to keep. Use this OR
#'        threshold, but not both.
#' @param threshold An optional numeric value indicating the minimum threshold value; taxa below this
#'        threshold will be grouped into an "Other" category.
#'
#' @return A ggplot object containing the visualization.
#'
#' @examples
#' \donttest{
#'   # Example usage (ensure that 'inst/extdata' contains the appropriate files):
#'   microbiome_data <- system.file("extdata", "microbiome.csv", package = "mbX")
#'   metadata <- system.file("extdata", "metadata.csv", package = "mbX")
#'   plot_obj <- ezviz(microbiome_data, metadata, "genus", "sample_type", top_taxa = 20)
#'   print(plot_obj)
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @export
ezviz <- function(microbiome_data, metadata, level, selected_metadata, top_taxa = NULL, threshold = NULL) {
  # Check the file extension for microbiome_data
  microbiome_ext <- tools::file_ext(microbiome_data)
  if (!microbiome_ext %in% c("csv", "xls", "xlsx")) {
    return("The file is not csv, xls, or xlsx format. Please check the file type for microbiome data.")
  }
  #memory.limit(size = 50)
  # Check the file extension for metadata
  metadata_ext <- tools::file_ext(metadata)
  if (metadata_ext == "txt") {
    metadata_df <- read.delim(metadata, sep = "\t", header = TRUE, check.names = FALSE)
  } else if (metadata_ext == "csv") {
    metadata_df <- read.csv(metadata, header = TRUE, check.names = FALSE)
  } else if (metadata_ext %in% c("xls", "xlsx")) {
    metadata_df <- readxl::read_excel(metadata, col_names = TRUE)
    # Optionally, convert to data.frame if needed:
    metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
  } else {
    return("Please check the file format of metadata.")
  }
  
  
  
  
  
  ##removING WARNING 
  #old_warn <- getOption("warn")
  #options(warn = -1)
  #on.exit(options(warn = old_warn))
  
  
  
  
  # Check the first header of metadata
  valid_headers <- c("id", "sampleid", "sample id", "sample-id", 
                     "featureid", "feature id", "feature-id")
  if (!(tolower(trimws(names(metadata_df)[1])) %in% valid_headers)) {
    return("Please check the first header of the metadata for file format correction.")
  }
  
  # Check if the selected_metadata is a valid categorical column in metadata_df
  if (!(selected_metadata %in% colnames(metadata_df)) ||
      !(is.factor(metadata_df[[selected_metadata]]) || is.character(metadata_df[[selected_metadata]]))) {
    return("The selected metadata is either not in the metadata or not a categorical value")
  }
  
  # Read microbiome data
  # Read microbiome data based on file extension
  if (microbiome_ext == "txt") {
    microbiome_df <- read.delim(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext == "csv") {
    microbiome_df <- read.csv(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext %in% c("xls", "xlsx")) {
    # Convert tibble to data.frame to preserve names as-is.
    microbiome_df <- as.data.frame(readxl::read_excel(microbiome_data, skip = 1), check.names = FALSE)
  } else {
    return("The microbiome file is not in a supported format. Please use txt, csv, xls, or xlsx.")
  }
  
  
  
  
  # Define the levels map with variations
  levels_map <- c(
    "domain"  = "d__", "Domain"  = "d__", "DOMAIN"  = "d__", "D" = "d__", "d" = "d__",
    "kingdom" = "d__", "Kingdom" = "d__", "KINGDOM" = "d__", "K" = "d__", "k" = "d__",
    "phylum"  = "p__", "Phylum"  = "p__", "PHYLUM"  = "p__", "P" = "p__", "p" = "p__",
    "class"   = "c__", "Class"   = "c__", "CLASS"   = "c__", "C" = "c__", "c" = "c__",
    "order"   = "o__", "Order"   = "o__", "ORDER"   = "o__", "O" = "o__", "o" = "o__",
    "family"  = "f__", "Family"  = "f__", "FAMILY"  = "f__", "F" = "f__", "f" = "f__",
    "genera"  = "g__", "Genera"  = "g__", "GENERA"  = "g__", "G" = "g__", "g" = "g__",
    "species" = "s__", "Species" = "s__", "SPECIES" = "s__", "S" = "s__", "s" = "s__"
  )
  
  # Get the corresponding level code using the lower-case version of level
  level_code <- levels_map[tolower(level)]
  if (is.na(level_code)) {
    return("Invalid taxonomic level provided.")
  }
  
  # Ensure that only one of top_taxa or threshold is provided
  if (!is.null(top_taxa) && !is.null(threshold)) {
    return("Only one of the parameter can be selected between top_taxa and threshold")
  }
  
  # Determine the threshold value based on the provided parameter
  if (!is.null(threshold)) {
    threshold_value <- threshold
  } else if (!is.null(top_taxa)) {
    threshold_value <- top_taxa
  } else {
    threshold_value <- NULL
  }
  
  ##### File Handling: Run ezclean and use its output #####
  
  # Run the ezclean function using the first three parameters.
  # It should return one of the following fixed file names, e.g. "mbX_cleaned_families.xlsx"
  cleaned_file_ezviz <- ezclean(microbiome_data, metadata, level)
  message("ezclean returned:", cleaned_file_ezviz, "\n")
  
  # Check if the file exists
  if (!file.exists(cleaned_file_ezviz)) {
    stop("The file returned by ezclean does not exist: ", cleaned_file_ezviz)
  }
  
  # Read the Excel file returned by ezclean using openxlsx
  # Read the Excel file returned by ezclean using openxlsx
  cleaned_data <- openxlsx::read.xlsx(cleaned_file_ezviz, colNames = TRUE, check.names = FALSE)
  
  # Option 1: Prevent conversion by ensuring check.names = FALSE everywhere (preferred)
  
  # OR, Option 2: Conditionally replace dots with spaces:
  correct_names <- names(metadata_df)
  for(i in seq_along(names(cleaned_data))) {
    orig_name <- names(cleaned_data)[i]
    if (!(orig_name %in% correct_names)) {
      candidate <- gsub("\\.", " ", orig_name)
      if (candidate %in% correct_names) {
        names(cleaned_data)[i] <- candidate
      }
    }
  }
  
  # Debug: Print out the names of the columns in cleaned_data
  #cat("Columns in cleaned_data:\n", paste(names(cleaned_data), collapse = ", "), "\n")
  
  # Identify metadata columns in cleaned_data that are also present in metadata_df and remove
  # Identify the metadata columns that exist in the cleaned data:
  metadata_cols_in_cleaned <- intersect(names(metadata_df), names(cleaned_data))
  
  # Check that the selected_metadata is indeed present in the cleaned data:
  if (!(selected_metadata %in% metadata_cols_in_cleaned)) {
    stop("The selected metadata column '", selected_metadata, "' was not found in the cleaned data.")
  }
  
  # Determine which metadata columns (that are present) should be removed
  metadata_to_remove <- setdiff(metadata_cols_in_cleaned, selected_metadata)
  
  # Subset the cleaned_data to keep only non-metadata columns plus the selected_metadata column
  subset_data <- cleaned_data[, !(names(cleaned_data) %in% metadata_to_remove), drop = FALSE]
  
  
  # Write the resulting subset_data to "mbX_cleaning_1.xlsx" using openxlsx
  openxlsx::write.xlsx(subset_data, file = "mbX_cleaning_1.xlsx", rowNames = FALSE, colNames = TRUE)
  #cat("Output file 'mbX_cleaning_1.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######NOW I WILL take average along the colUMN SEGREGATED BY THE CATEGORIES IN THE SELECTED_METADATA
  
  
  
  # Read in the "mbX_cleaning_1.xlsx" file
  # Read in the "mbX_cleaning_1.xlsx" file
  data_cleaning_mbX_1 <- openxlsx::read.xlsx("mbX_cleaning_1.xlsx", colNames = TRUE)
  
  # Filter out rows where the selected_metadata column is empty, NA, "Na", "#VALUE!", or "#NAME?"
  data_cleaning_mbX_1 <- data_cleaning_mbX_1 %>%
    filter(!(is.na(.data[[selected_metadata]]) |
               .data[[selected_metadata]] %in% c("", "Na", "NA", "#VALUE!", "#NAME?")))
  
  # Group the data by the selected_metadata column and calculate the mean for each numeric column
  data_summary_mbX_2 <- data_cleaning_mbX_1 %>%
    group_by(across(all_of(selected_metadata))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Write the summarized data to "mbX_cleaning_2.xlsx" in the working directory
  openxlsx::write.xlsx(data_summary_mbX_2, file = "mbX_cleaning_2.xlsx", rowNames = FALSE, colNames = TRUE)
  
  #cat("Output file 'mbX_cleaning_2.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###now the thiNGS ARE AVERAGED
  ######## I WILL TRANSPOSE THEM to get the OTHER_TAXA THING BEYOND THE THRESHOLD OR THE TOP TAXA
  
  
  
  
  # Read in the "mbX_cleaning_2.xlsx" file with headers
  data_cleaning_mbX_2 <- openxlsx::read.xlsx("mbX_cleaning_2.xlsx", colNames = TRUE)
  
  # Convert the data frame to a matrix and transpose it
  data_matrix_mbX_2 <- as.matrix(data_cleaning_mbX_2)
  transposed_matrix_mbX_3 <- t(data_matrix_mbX_2)
  
  # Convert the transposed matrix back to a data frame.
  # Note: We set rowNames = FALSE when writing to avoid an extra index column.
  data_transposed_mbX_3 <- as.data.frame(transposed_matrix_mbX_3, stringsAsFactors = FALSE, check.names = FALSE )
  
  # Write the transposed data to "mbX_cleaning_3.xlsx" in the working directory without row names.
  openxlsx::write.xlsx(data_transposed_mbX_3, file = "mbX_cleaning_3.xlsx", rowNames = TRUE)
  
  #cat("Output file 'mbX_cleaning_3.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  ### NOW THE CLEANING_3 IS THE TRANSPOSED BUT HAS ONE EXTRA ROW WITH THE index
  ### so here we delete THE FIRST ROW 
  
  # Read in the "mbX_cleaning_3.xlsx" file without using a header
  raw_data_mbX_3 <- openxlsx::read.xlsx("mbX_cleaning_3.xlsx", colNames = FALSE)
  
  # Debug: show dimensions of the raw data
  #cat("Dimensions of raw data:", dim(raw_data_mbX_3), "\n")
  
  # Remove the very first row (assumed to be the unwanted index row)
  raw_data_no_index <- raw_data_mbX_3[-1, ]
  
  # Now, the first row of raw_data_no_index is the actual header.
  header_row <- raw_data_no_index[1, ]
  data_without_index <- raw_data_no_index[-1, ]
  
  # Assign the header row as column names
  colnames(data_without_index) <- header_row
  
  # Write the cleaned data to "mbX_cleaning_4.xlsx" in the working directory
  openxlsx::write.xlsx(data_without_index, file = "mbX_cleaning_4.xlsx", rowNames = FALSE)
  #cat("Output file 'mbX_cleaning_4.xlsx' has been created in the working directory.\n")
  #######33333333DONE TILL THIS
  
  
  
  
  
  
  
  
  
  
  
  #######NOW THE ISSUE IS EVERYTHING IS TEXT 
  
  # Read in the "mbX_cleaning_4.xlsx" file without treating any row as header
  data_cleaning_mbX_4 <- openxlsx::read.xlsx("mbX_cleaning_4.xlsx", colNames = FALSE)
  
  # Separate the header row (first row) and the data rows (remaining rows)
  header_row_mbX_4 <- data_cleaning_mbX_4[1, ]
  data_rows_mbX_4  <- data_cleaning_mbX_4[-1, ]
  
  # Convert all columns except the first in data_rows_mbX_4 to numeric
  data_rows_mbX_4[, -1] <- lapply(data_rows_mbX_4[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  
  # Write the header row as text.
  # We first convert the header row to a vector and then transpose it so that it is written as a row.
  header_vec_mbX_4 <- as.vector(unlist(header_row_mbX_4))
  openxlsx::writeData(wb, sheet = "Sheet1", x = t(header_vec_mbX_4),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb, sheet = "Sheet1", x = data_rows_mbX_4,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as "mbX_cleaning_5.xlsx"
  openxlsx::saveWorkbook(wb, "mbX_cleaning_5.xlsx", overwrite = TRUE)
  
  #cat("Output file 'mbX_cleaning_5.xlsx' has been created in the working directory.\n")
  
  
  
  #####NOW THE BIG PART GETTING THAT OTHER_TAXA EITHER FROM THE THRESHOLD OR THE TOP_TAXA 
  
  
  # Read in "mbX_cleaning_5.xlsx"
  df_clean5 <- openxlsx::read.xlsx("mbX_cleaning_5.xlsx", colNames = TRUE)
  
  # Assume the first column contains taxon names and the remaining columns are numeric.
  # Compute the row averages (ignoring the first column)
  numeric_matrix <- as.matrix(df_clean5[, -1])
  row_avg <- rowMeans(numeric_matrix, na.rm = TRUE)
  
  # Attach the computed averages as a new column (for selection/sorting only)
  df_clean5$RowAvg <- row_avg
  
  # Sort the data frame in descending order by the row average
  df_sorted <- df_clean5[order(-df_clean5$RowAvg), ]
  
  # We'll remove the RowAvg column later.
  # Define the mapping for the aggregated "Other" row names
  other_name_map <- list(
    "d__" = "Other_domains",
    "p__" = "Other_phyla",
    "c__" = "Other_classes",
    "o__" = "Other_orders",
    "f__" = "Other_families",
    "g__" = "Other_genera",
    "s__" = "Other_species"
  )
  
  # Retrieve the proper "Other" name based on level_code
  other_row_name <- other_name_map[[ level_code ]]
  
  # Initialize final data frame variable
  final_df <- NULL
  
  if (!is.null(threshold)) {
    # --- THRESHOLD LOGIC ---
    # Keep rows with average >= threshold
    rows_keep <- df_sorted[df_sorted$RowAvg >= threshold, ]
    # Rows with average below threshold will be aggregated
    rows_agg <- df_sorted[df_sorted$RowAvg < threshold, ]
    
    if (nrow(rows_agg) > 0) {
      # Sum the numeric values (columns 2 to ncol-1, since last column is RowAvg)
      agg_values <- colSums(as.matrix(rows_agg[, 2:(ncol(rows_agg)-1)]), na.rm = TRUE)
      # Create a new row: first column is the Other row name; numeric columns are the aggregated sums.
      agg_row <- data.frame(matrix(ncol = ncol(df_sorted), nrow = 1), stringsAsFactors = FALSE)
      colnames(agg_row) <- colnames(df_sorted)
      agg_row[1, 1] <- other_row_name
      # Fill aggregated numeric values into columns 2 to (ncol-1)
      agg_row[1, 2:(ncol(df_sorted)-1)] <- agg_values
      # For RowAvg column, we can leave as NA (or compute a new average if desired)
      agg_row[1, ncol(df_sorted)] <- NA
      # Combine the kept rows with the aggregated row.
      final_df <- rbind(rows_keep, agg_row)
    } else {
      final_df <- df_sorted
    }
    
  } else if (!is.null(top_taxa)) {
    # --- TOP_TAXA LOGIC ---
    # Select top 'top_taxa' rows based on average
    if (nrow(df_sorted) > top_taxa) {
      rows_keep <- df_sorted[1:top_taxa, ]
      rows_agg <- df_sorted[(top_taxa + 1):nrow(df_sorted), ]
      
      if (nrow(rows_agg) > 0) {
        agg_values <- colSums(as.matrix(rows_agg[, 2:(ncol(rows_agg)-1)]), na.rm = TRUE)
        agg_row <- data.frame(matrix(ncol = ncol(df_sorted), nrow = 1), stringsAsFactors = FALSE)
        colnames(agg_row) <- colnames(df_sorted)
        agg_row[1, 1] <- other_row_name
        agg_row[1, 2:(ncol(df_sorted)-1)] <- agg_values
        agg_row[1, ncol(df_sorted)] <- NA
        final_df <- rbind(rows_keep, agg_row)
      } else {
        final_df <- df_sorted
      }
    } else {
      final_df <- df_sorted
    }
  }
  
  # Remove the helper RowAvg column before writing the output
  final_df$RowAvg <- NULL
  
  # Write the final data frame to "mbX_cleaning_6.xlsx"
  # Define a mapping for visualization data file names based on level_code
  # Define a mapping for the visualization data file names based on level_code
  vizDataNames <- list(
    "d__" = "mbX_vizualization_data_domains.xlsx",
    "p__" = "mbX_vizualization_data_phyla.xlsx",
    "c__" = "mbX_vizualization_data_classes.xlsx",
    "o__" = "mbX_vizualization_data_orders.xlsx",
    "f__" = "mbX_vizualization_data_families.xlsx",
    "g__" = "mbX_vizualization_data_genera.xlsx",
    "s__" = "mbX_vizualization_data_species.xlsx"
  )
  
  # Determine the output file name based on level_code
  outputVizFile <- vizDataNames[[ level_code ]]
  
  # Write the final data frame to the dynamic file name
  openxlsx::write.xlsx(final_df, file = outputVizFile, rowNames = FALSE)
  #cat("Visualization data file written to: ", file.path(getwd(), outputVizFile), "\n")
  
  # --- Later, when reading the file for plotting ---
  
  # Check if the file exists before reading
  if (!file.exists(outputVizFile)) {
    stop("The file ", outputVizFile, " does not exist in the working directory: ", getwd())
  }
  df_clean6 <- openxlsx::read.xlsx(outputVizFile, colNames = TRUE)
  
  
  
  
  ###LEN FOR THE ROTATION OF THE TEXT IN THE X AXIS
  # Compute maximum header length (excluding the first column header)
  headers_to_check <- names(df_clean6)[-1]
  headers_clean <- gsub("\\s+", "", headers_to_check)  # Remove all whitespace
  max_header_length <- max(nchar(headers_clean), na.rm = TRUE)
  #cat("Maximum header length (excluding the first header) is:", max_header_length, "\n")
  
  # Set axis text parameters based on max_header_length
  if (max_header_length > 9) {
    axis_x_angle <- 65
    axis_x_hjust <- 1.0
  } else {
    axis_x_angle <- 0
    axis_x_hjust <- 0.5
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## THE PLOTTIIINNNGNGNGNGNNGNG
  
  
  
  # Load required packages
  
  
  # --- Dynamic Plotting Code Block using custom colors ---
  
  # Read in the final cleaned data
  
  # Define the mapping for taxonomic levels
  level_names <- list(
    "d__" = "Domains",
    "p__" = "Phyla",
    "c__" = "Classes",
    "o__" = "Orders",
    "f__" = "Families",
    "g__" = "Genera",
    "s__" = "Species"
  )
  
  rm(data)
  gc()
  
  # Assume that the variable 'level_code' is already defined (e.g., "f__" or "g__")
  # Retrieve the proper descriptive name for the level from the mapping
  taxon_descriptor <- level_names[[ level_code ]]
  if (is.null(taxon_descriptor)) {
    stop("Invalid level_code provided.")
  }
  
  # Build dynamic titles for the plot and legend
  plot_title   <- paste("Relative Abundance of Microbial", taxon_descriptor)
  legend_title <- paste("Microorganism", taxon_descriptor)
  
  # Reshape the data from wide to long format
  # The first column is taxon names; all remaining columns represent samples.
  df_long <- df_clean6 %>%
    pivot_longer(
      cols = -1,                      # leave the first column untouched
      names_to = "Sample",
      values_to = "Abundance"
    )
  
  # Ensure the taxon column is treated as a factor (preserving the original order)
  taxa_col <- names(df_clean6)[1]
  df_long[[taxa_col]] <- factor(df_long[[taxa_col]], levels = df_clean6[[taxa_col]])
  
  # Define the two base palettes
  tab10 <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  set3 <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
            "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")
  
  # We want 200 colors in total. That means 100 values from each palette.
  tab10_rep <- rep(tab10, length.out = 100)
  set3_rep  <- rep(set3, length.out = 100)
  
  # Interleave the two repeated palettes
  custom_colors <- as.vector(rbind(tab10_rep, set3_rep))
  
  # Calculate the number of distinct taxa (for distinct colors)
  n_colors <- length(unique(df_long[[taxa_col]]))
  
  # If needed, subset custom_colors to match the number of taxa
  if(n_colors > length(custom_colors)){
    warning("Number of taxa exceeds custom color length; colors will be recycled.")
    palette_colors <- rep(custom_colors, length.out = n_colors)
  } else {
    palette_colors <- custom_colors[1:n_colors]
  }
  
  # Create the stacked bar plot using the custom colors
  p <- ggplot(df_long, aes(x = Sample, y = Abundance, fill = .data[[taxa_col]])) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = palette_colors) +
    guides(fill = guide_legend(ncol = 1)) +
    labs(
      title = plot_title,
      x = selected_metadata,
      y = "Relative Abundance (%)",
      fill = legend_title
    ) +
    theme_bw() +
    theme(
      plot.title  = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text.x = element_text(angle = axis_x_angle, hjust = axis_x_hjust, size = 12),
      legend.position = "right",
      legend.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 16),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  
  
  # Define output filename dynamically
  final_names_viz <- list(
    "d__" = "mbX_viz_domains_or_kingdom",
    "p__" = "mbX_viz_phylum",
    "c__" = "mbX_viz_classes",
    "o__" = "mbX_viz_orders",
    "f__" = "mbX_viz_families",
    "g__" = "mbX_viz_genera",
    "s__" = "mbX_viz_species"
  )
  output_plot_filename <- paste0(final_names_viz[[ level_code ]], ".pdf")
  
  
  # Save the plot with DPI = 1800 and dimensions adjusted to avoid cropping
  # Calculate the dimensions of df_clean6
  df_clean6_dim <- dim(df_clean6)
  df_clean6_row <- df_clean6_dim[1]
  df_clean6_column <- df_clean6_dim[2]
  
  # Compute plot width:
  # Minimum width is 12; if there are more than 4 columns, add 0.7 for each additional column.
  if (df_clean6_column > 4) {
    plot_width <- 12 + 0.7 * (df_clean6_column - 4)
  } else {
    plot_width <- 12
  }
  
  # Compute plot height:
  # Minimum height is 15; if there are more than 55 rows, add 0.35 for each additional row.
  if (df_clean6_row > 55) {
    plot_height <- 15 + 0.35 * (df_clean6_row - 55)
  } else {
    plot_height <- 15
  }
  
  #cat("Calculated plot width:", plot_width, "and height:", plot_height, "\n")
  
  # Save the plot with dynamic dimensions
  ggsave(
    filename = output_plot_filename,
    plot     = p,
    width    = plot_width,
    height   = plot_height,
    dpi      = 1200
  )
  
  #cat("Output plot '", output_plot_filename, "' has been created.\n")
  
  # Delete temporary cleaning files before closing the function
  ####DELETE TEMP FILE FOR DEBUGGING HERE
  temp_files <- c("mbX_cleaning_1.xlsx", "mbX_cleaning_2.xlsx", 
                  "mbX_cleaning_3.xlsx", "mbX_cleaning_4.xlsx", 
                  "mbX_cleaning_5.xlsx")
  for(file in temp_files) {
    if(file.exists(file)) {
      file.remove(file)
      #cat("Deleted file:", file, "\n")
    } else {
      #cat("File not found (already deleted or never created):", file, "\n")
    }
  }
  message("Done with the vizualization, cite us!")
  
}
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("SampleID", "Taxonomy", "Value", "data", "Sample", "Abundance"))
}


#ezviz("level-7.csv", "metadata.txt", "f", "sample_type", top_taxa = 20, threshold = 11) 
#ezviz("level-7.csv", "metadata.txt", "s", "sample_type2", 30) 
#ezviz("level-7.csv", "metadata.txt", "f", "sample_type2", threshold = 1) 





