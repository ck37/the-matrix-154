sink() # Close any open log file first.
# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

# Load our main functions.
source("function_library.R")

# Directory to load
directory = "inbound/Training/"

# These are the outcome label and the subdirectory name.
doc_subdirectories = list(child="Child_0", history="History_1", religion="Religion_2", science="Science_3")

system.time({
  docs = import_text_documents(directory, doc_subdirectories)
  # These are not books at all, just Gutenberg junk text.
  # We are disabling some of these temporarily so that the imported data is consistent with the power features.
  # TODO: re-enable these if we have time to update the power features.
  #bad_child = c("813.txt")
  #docs$child = docs$child[!names(docs$child) %in% bad_child]
  #bad_history = c("819953.txt")
  #docs$history = docs$history[!names(docs$history) %in% bad_history]
  #bad_science = c("829559.txt", "832040.txt", "832041.txt", "837448.txt", "840969.txt", "841622.txt",
  #                "842181.txt", "842182.txt", "949809.txt") 
  
  bad_science = c("837448.txt", "841622.txt", "842181.txt", "949809.txt") 
  docs$science = docs$science[!names(docs$science) %in% bad_science]
})

sapply(docs, FUN=length)
sum(sapply(docs, FUN=length))
  
# Save our results, which is also a bit slow. Should be about 194MB in file size.
save(docs, file="data/imported-text-docs.Rdata")

# Clean up.
rm(directory, doc_subdirectories, docs, bad_science)
# TODO: restore this rm() once we re-enable the vectors above.
# rm(bad_history, bad_child)

#########################################
# Cleanup

gc()

# Review script execution time.
if (exists("script_timer")) {
  cat("Script execution time:", round((proc.time() - script_timer)[3] / 60, 0), "minutes.\n")
  rm(script_timer)
}

# Stop logging.
sink()