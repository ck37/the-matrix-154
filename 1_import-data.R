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
})
  
# Save our results, which is also a bit slow. Should be about 194MB in file size.
save(docs, file="data/imported-text-docs.Rdata")

# Clean up.
rm(directory, doc_subdirectories, docs)

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