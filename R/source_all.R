# Source all function files in the package's R/ folder, excluding this script.

this_file <- "source_all.R"  # name of this script, update if renamed

r_dir <- here::here("R")

r_files <- list.files(
  path       = r_dir,
  pattern    = "\\.[Rr]$",
  full.names = TRUE
)

r_files <- sort(r_files[basename(r_files) != this_file])

for (f in r_files) {
  message("Sourcing: ", basename(f))
  source(f, local = FALSE)
}

message(length(r_files), " files sourced from ", r_dir)