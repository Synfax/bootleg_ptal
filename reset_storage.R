
reset_storage <- function() {

  if(!dir.exists('stop_isochrones/')) {
    dir.create('stop_isochrones/')
  }

  if(!dir.exists('sf_output/')) {
    dir.create('sf_output/')
  }

  delete_files_in_folders <- function(folder_paths) {
    # Loop through each folder
    for (folder in folder_paths) {
      # Check if the folder exists
      if (dir.exists(folder)) {
        # Get all files in the folder
        files <- list.files(folder, full.names = TRUE, recursive = FALSE)

        # Delete each file
        if (length(files) > 0) {
          message("Deleting ", length(files), " files from ", folder)
          file.remove(files)
        } else {
          message("No files found in ", folder)
        }
      } else {
        warning("Folder does not exist: ", folder)
      }
    }
  }

  # Example usage:
  folders <- c("stop_isochrones/")
  delete_files_in_folders(folders)

}
