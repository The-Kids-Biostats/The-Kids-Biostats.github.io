# Define the base URL for your site
base_url <- "https://the-kids-biostats.github.io/your-blog"

# Path to the RSS feed file
rss_file <- "docs/index.xml"

# Function to fix relative paths in the RSS feed
fix_relative_paths <- function(file_path, base_url) {
  if (!file.exists(file_path)) {
    stop(paste("Error: File", file_path, "does not exist."))
  }
  
  # Read the RSS file
  rss_content <- readLines(file_path, encoding = "UTF-8")
  
  # Replace relative paths with absolute paths
  rss_content_fixed <- gsub('src="/', paste0('src="', base_url, '/'), rss_content)
  
  # Write the modified content back to the file
  writeLines(rss_content_fixed, file_path, useBytes = TRUE)
  
  cat("Relative paths in RSS feed have been fixed.\n")
}

# Run the function
fix_relative_paths(rss_file, base_url)
