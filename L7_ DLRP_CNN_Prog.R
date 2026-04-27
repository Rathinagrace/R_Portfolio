# =====================================================
# LESSON 7: Convolutional Neural Networks

# Author: Rathina Grace Monica / Adjunct Lecturer
# =====================================================

library(reticulate)
library(keras3)

# 1. Define the local folder we found in the terminal
local_env <- "C:/Users/rathi/AppData/Local/r-miniconda/envs/keras-env"

# 2. Force the installation specifically into THIS local folder
# This bypasses the OneDrive folder seen in your logs
py_install("keras", envname = local_env, pip = TRUE, python_version = "3.10")

# 3. Tell R to use this specific engine now
use_python(paste0(local_env, "/python.exe"), required = TRUE)

# 4. Load the CIFAR-10 data
cifar10 <- dataset_cifar10()

# Assign training and testing data
x_train <- cifar10$train$x
y_train <- cifar10$train$y
x_test  <- cifar10$test$x
y_test  <- cifar10$test$y

# Success Check for the Slide
print(dim(x_train))

# =====================================================
# LESSON 7: Convolutional Neural Networks
# Slide: Configure/Preprocess the data
# Author: Rathina Grace Monica / Adjunct Lecturer
# =====================================================

# 1. Rescale: Normalize pixel values from [0, 255] to [0, 1]
# This aids in faster convergence during model training.
x_train <- x_train / 255
x_test <- x_test / 255

# 2. Categorize labels: Convert integer labels to one-hot encoded vectors
# This is required for multi-class classification (10 classes for CIFAR-10).
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# --- Verification for the Class ---
# The target labels should now be a matrix with 10 columns
print(dim(y_train))

# =====================================================
# LESSON 7: Convolutional Neural Networks
# Slide: Use Functions to Create a New Convolution Layer
# Author: Rathina Grace Monica / Adjunct Lecturer
# =====================================================

# Initialize a sequential model
model <- keras_model_sequential()

# Add a convolution and pooling layer
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", 
                input_shape = c(32, 32, 3)) %>%
  
  # First Conv layer
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  
  # Second Conv layer
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  # Add the remaining layers: Flatten the 3D output to 1D
  layer_flatten() %>%
  
  # Fully connected layer
  layer_dense(units = 128, activation = "relu") %>%
  
  # Output layer with 10 units for CIFAR-10 (Softmax for probability)
  layer_dense(units = 10, activation = "softmax")

# --- Verification for the Class ---
# This prints the summary table showing parameters and shapes
summary(model)


# =====================================================
# LESSON 7: Compile and Train
# =====================================================

# 1. Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# 2. Fit the model (Training)
# Note: For a live demo, we keep epochs low (e.g., 5) to save time.
history <- model %>% fit(
  x_train, y_train,
  epochs = 5,
  batch_size = 64,
  validation_split = 0.2
)

# 3. Plot the learning progress
plot(history)