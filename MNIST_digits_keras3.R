# =============================================================
# EXERCISE 2: DEEP LEARNING WITH KERAS 3 (MNIST DIGITS)
# Goal: Classify handwritten digits (0-9) using a Neural Network
# Author: Rathina Grace Monica, Adjunct Lecturer
# =============================================================

# --- STEP 1: Environment Bridge ---
# This ensures R ignores the restricted Windows/OneDrive paths 
# and uses our healthy local environment.
library(reticulate)
Sys.setenv(RETICULATE_PYTHON_ENV = "C:/r-keras-env")
use_virtualenv("C:/r-keras-env", required = TRUE)

library(keras3)

# --- STEP 2: Load and Prepare Data ---
# Downloading the MNIST dataset (70,000 images of handwritten digits)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Data Pre-processing:
# 1. Flatten images (28x28 matrix -> 784 pixel vector)
# 2. Normalize (Divide by 255 to get values between 0 and 1)
x_train <- array_reshape(x_train, c(nrow(x_train), 784)) / 255
x_test <- array_reshape(x_test, c(nrow(x_test), 784)) / 255

# 3. One-Hot Encoding: Convert labels (0-9) into categorical vectors
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# --- STEP 3: Build the Model (SLIDE 21) ---
# Sequential model with one hidden layer (256 neurons) and one output layer (10 neurons)
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 10, activation = 'softmax')

# --- STEP 4: Compile ---
# Defining how the model measures error and improves
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

# --- STEP 5: Train (The Demonstration) ---
# Running 5 iterations (epochs). RStudio will show a live progress plot.
cat("\n--- STARTING TRAINING ---\n")
history <- model %>% fit(
  x_train, y_train, 
  epochs = 5, 
  batch_size = 128,
  validation_split = 0.2
)

# Extract the final training accuracy from the history object
train_acc <- tail(history$metrics$accuracy, 1)

cat("\n--- ACCURACY COMPARISON ---\n")
cat("Training Accuracy: ", round(train_acc * 100, 2), "%\n")


# --- STEP 6: Final Evaluation ---
# Testing the model on 10,000 images it has never seen before
results <- model %>% evaluate(x_test, y_test)

cat("\n--- FINAL EVALUATION RESULTS ---\n")
cat("Test Loss: ", results[["loss"]], "\n")
cat("Test Accuracy: ", round(results[["accuracy"]] * 100, 2), "%\n")


# --- EXPERIMENT 2: TWO HIDDEN LAYERS (Slide 22) ---
# We are increasing complexity to see if accuracy improves

model_2 <- keras_model_sequential() %>%
  layer_dense(units = 1024, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 1024, activation = 'relu') %>% # Second Hidden Layer
  layer_dense(units = 10, activation = 'softmax')

model_2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

cat("\n--- TRAINING EXPERIMENT 2 (Deep Network) ---\n")
history_2 <- model_2 %>% fit(
  x_train, y_train, 
  epochs = 5, 
  batch_size = 128,
  validation_split = 0.2
)

# Evaluation for Experiment 2
results_2 <- model_2 %>% evaluate(x_test, y_test)
train_acc_2 <- tail(history_2$metrics$accuracy, 1)

cat("\n--- EXPERIMENT 2 RESULTS ---\n")
cat("Training Accuracy: ", round(train_acc_2 * 100, 2), "%\n")
cat("Test Accuracy:     ", round(results_2[["accuracy"]] * 100, 2), "%\n")

# ---  Train with Visualization (TensorBoard) ---
cat("\n--- STARTING TRAINING WITH TENSORBOARD ---\n")

# 1. Define log location
log_path <- "logs/run_1"

# 2. Add the callback to the fit function
history <- model %>% fit(
  x_train, y_train, 
  epochs = 5, 
  batch_size = 128,
  validation_split = 0.2,
  callbacks = list(
    callback_tensorboard(log_dir = log_path)
  )
)

# 3. Launch the Dashboard 
tensorboard(log_path)