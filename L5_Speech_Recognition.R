# =====================================================

# LESSON 5: Speech Recognition
# Author: Rathina Grace Monica / Adjunct Lecturer

# =====================================================


# Install required packages
install.packages(c("audio", "wrassp", "googleLanguageR"))

# Load the libraries
library(audio)
library(wrassp)
library(googleLanguageR)

# 1. SETUP & AUTHENTICATION 
library(googleLanguageR)

# Set credentials (
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "C:/Users/rathi/R_Portfolio/speechtotext-r-project-28f9a0465652.json")
gl_auth(json_file = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))

# 2. THE CLEAN DEMO 
target_file <- "demo_audio.wav" 

if(file.exists(target_file)) {
  cat("Clean file found! Sending to Google...\n")
  
  result <- gl_speech(
    audio_source = target_file,
    encoding = "LINEAR16",  # This works ONLY with uncompressed WAV
    sampleRateHertz = 16000,
    languageCode = "en-US"
  )
  
  cat("\n--- GOOGLE API TRANSCRIPTION ---\n")
  print(result$transcript$transcript[1])
} else {
  cat("Please ensure 'demo_audio.wav' is in your folder.")
}


# Experiment 2 Flexible R Function
# 1. EXECUTION 
target_file <- "demo_audio.wav"

# Call the function (using the function you already defined)
experiment_results <- transcribe_flexible(
  audio_source = target_file,
  sample_rate = 16000
)

# 2. CLEAN DISPLAY RESULTS 
cat("\n--- EXPERIMENT 2 RESULTS ---\n")

# Extract values safely
transcript_text <- experiment_results$transcript$transcript[1]
# We use as.numeric to fix the 'binary operator' error
confidence_val <- as.numeric(experiment_results$transcript$confidence[1])

if(!is.na(transcript_text)) {
  cat("TRANSCRIPT:\n", transcript_text, "\n")
  
# Only print confidence if it's a valid number
  if(!is.na(confidence_val)) {
    cat("\nCONFIDENCE SCORE:", round(confidence_val * 100, 2), "%\n")
  }
} else {
  cat("The API processed the file but no speech was recognized.\n")
}

# Experiment 3
# Different Settings (Flac & British English)
# --- 1. THE UNIVERSAL FUNCTION ---
transcribe_flexible <- function(audio_source, 
                                encoding = "LINEAR16",
                                sample_rate = 16000,
                                language = "en-US") {
  
  cat("Sending request to Google Cloud...\n")
  
  # We removed channel_count to avoid 'unused argument' errors
  result <- gl_speech(
    audio_source = audio_source,
    encoding = encoding,
    sampleRateHertz = sample_rate,
    languageCode = language
  )
  
  return(result)
}

# --- 2. EXECUTION FOR EXPERIMENT 3 ---
target_file_3 <- "interview_recording.flac"

if(file.exists(target_file_3)) {
  result_3 <- transcribe_flexible(
    audio_source = target_file_3,
    encoding = "FLAC",
    sample_rate = 48000, # Matches your file's specific header
    language = "en-GB"   # British English detection
  )
  
  # --- 3. DISPLAY RESULTS ---
  cat("\n--- EXPERIMENT 3: FLAC & UK ENGLISH ---\n")
  
  if(!is.null(result_3$transcript)) {
    transcript_text <- result_3$transcript$transcript[1]
    # Keep as.numeric to ensure the math works
    conf <- as.numeric(result_3$transcript$confidence[1])
    
    cat("TRANSCRIPT:\n", transcript_text, "\n")
    cat("\nCONFIDENCE SCORE:", round(conf * 100, 2), "%\n")
  }
}


# EXPERIMENT 4: PRACTICAL ANALYTICAL WORKFLOWS 

# 1. RE-AUTHENTICATION 
# Set credentials (
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "C:/Users/rathi/R_Portfolio/speechtotext-r-project-28f9a0465652.json")
gl_auth(json_file = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))


# 2. BATCH PROCESSING FUNCTION 
process_interviews <- function(interview_dir, output_csv = "transcripts.csv") {
  
# Identify all .wav files in the folder (including demo_audio.wav)
  audio_files <- list.files(interview_dir, pattern = "\\.wav$", full.names = TRUE)
  
  results <- data.frame(
    filename = character(),
    transcript = character(),
    confidence = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (file in audio_files) {
    cat(sprintf("Processing: %s...\n", basename(file)))
    
# Send to Google API
    result <- gl_speech(file, languageCode = "en-US")
    
# Store output safely
    results <- rbind(results, data.frame(
      filename = basename(file),
      transcript = result$transcript$transcript[1],
      confidence = as.numeric(result$transcript$confidence[1])
    ))
  }
  
  write.csv(results, output_csv, row.names = FALSE)
  cat("\nSuccess! Results saved to:", output_csv, "\n")
  return(results)
}

# 3. VOICE-CONTROLLED DATA QUERIES 
voice_query_demo <- function(data_frame, command_audio_file) {
  
  cat("\nProcessing voice command from file:", command_audio_file, "\n")
  
  result <- gl_speech(command_audio_file, languageCode = "en-US")
  command <- tolower(result$transcript$transcript[1])
  
  cat("Detected Voice Command:", command, "\n")
  
  if (grepl("show", command) || grepl("top", command)) {
    cat("Action: Displaying top records...\n")
    return(head(data_frame, 5))
    
  } else if (grepl("summarize", command) || grepl("summary", command)) {
    cat("Action: Generating data summary...\n")
    return(summary(data_frame))
    
  } else {
    cat("Warning: No matching command found in the audio.\n")
    return(head(data_frame, 3))
  }
}

# 4. EXECUTION 
# Run the batch process
cat("--- Starting Batch Processing Simulation ---\n")
batch_data <- process_interviews(getwd())
print(batch_data)

# Run the voice query test
if(file.exists("demo_audio.wav")) {
  cat("\n--- Starting Voice-Controlled Query Simulation ---\n")
  query_result <- voice_query_demo(iris, "demo_audio.wav")
  print(query_result)
}

# Experiment 5: Domain-Specific Accuracy (Speech Context)
# EXPERIMENT 5: ADDING SPEECH CONTEXT HINTS 

transcribe_with_context <- function(audio_file, phrases = NULL) {
  
  cat("Sending request with technical context hints...\n")
  
# In your version, speechContexts is often passed as a separate argument
# or within a list provided to the 'context' parameter.
  
# We will use the most direct method for your package version:
  result <- gl_speech(
    audio_file, 
    languageCode = "en-US",
    sampleRateHertz = 16000,
    encoding = "LINEAR16",
    speechContexts = list(phrases = phrases) # Passing phrases directly
  )
  
  return(result$transcript$transcript[1])
}

# 2. DEFINE YOUR "DICTIONARY"
tech_vocabulary <- c("Data Science", "API", "R programming", "Google Speech", "MIS")

# 3. RUN THE TRANSCRIPTION
if(file.exists("demo_audio.wav")) {
  improved_transcript <- transcribe_with_context("demo_audio.wav", tech_vocabulary)
  
  cat("\n--- CONTEXT-AWARE TRANSCRIPT ---\n")
  cat(improved_transcript, "\n")
}

# Experiment 6: Robust Error Handling & Retry Logic
#  PRODUCTION-READY ERROR HANDLING ---

safe_transcribe <- function(audio_file, max_retries = 3, language = "en-US") {
  attempt <- 1
  
  while (attempt <= max_retries) {
    # tryCatch acts as a "safety net" to prevent the script from stopping on an error
    result_list <- tryCatch({
      cat(sprintf("Attempt %d: Sending request to Google Cloud...\n", attempt))
      
      res <- gl_speech(
        audio_file,
        languageCode = language,
        encoding = "LINEAR16",
        sampleRateHertz = 16000
      )
      
      # If successful, extract and return data immediately
      if (!is.null(res$transcript) && nrow(res$transcript) > 0) {
        return(list(
          success = TRUE,
          transcript = res$transcript$transcript[1],
          confidence = as.numeric(res$transcript$confidence[1])
        ))
      }
      
    }, error = function(e) {
      # This part runs ONLY if an error (like "Invalid Token") occurs
      cat(sprintf("!! Attempt %d failed: %s\n", attempt, e$message))
      
      if (attempt < max_retries) {
        cat("Waiting 2 seconds before next attempt...\n")
        Sys.sleep(2) 
      }
      return(NULL) # Signal that this specific attempt failed
    })
    
    # If we got a successful result back, the function would have returned already.
    # If we are here, the attempt failed, so we increment and loop.
    attempt <- attempt + 1
  }
  
  return(list(success = FALSE, error = "Maximum retries reached without success."))
}

# --- 2. EXECUTION ---
if(file.exists("demo_audio.wav")) {
  cat("--- Starting Robust Transcription Test ---\n")
  final_result <- safe_transcribe("demo_audio.wav")
  
  if(final_result$success) {
    cat("\nSUCCESS after robust processing:\n")
    cat("Transcript:", final_result$transcript, "\n")
  } else {
    cat("\nFAILURE:", final_result$error, "\n")
  }
}

