display_digit <- function(images, predicted_labels = NULL, true_labels = NULL, max_val = 150) {
  # Function: display_digit
  # Purpose: Display grayscale digits stored as column vectors in a matrix.
  #
  # Args:
  #   images           - A 256 x n matrix, where each column corresponds to a 16x16 grayscale image.
  #   predicted_labels - Optional: A length-n vector of predicted labels to display above each image.
  #   true_labels      - Optional: A length-n vector of true labels for comparison with predicted_labels.
  #                      If predicted_labels[i] != true_labels[i], the true label will be displayed below the image.
  #   max_val          - Maximum pixel intensity for grayscale scaling. If set to NA, intensity is scaled adaptively.
  #
  # Returns:
  #   Displays the images with optional labels.
  
  # Number of images
  num_images <- ncol(images)
  
  # Default grayscale intensity range
  min_val <- 0
  
  # Determine layout for plotting
  if (num_images == 1) {
    images <- matrix(images, ncol = 1)  # Ensure images is a matrix if a single column vector is provided
    rows <- 1
    cols <- 1
  } else {
    cols <- ceiling(sqrt(num_images) * 1.75)  # Adjust layout for better visualization
    rows <- ceiling(num_images / cols)
  }
  
  # Adaptive intensity scaling if max_val is NA
  if (is.na(max_val)) {
    min_val <- min(images) * 0.9
    max_val <- max(images) * 0.9
  }
  
  # Set up plot layout and margins
  par(mfrow = c(rows, cols), 
      mai = c(0.1, 0.2, 0.1, 0.2),  # Margin size
      mar = c(0.1, 0.2, 0.1, 0.2))  # Plot margins
  
  # Loop through and plot each image
  for (i in 1:num_images) {
    # Set the title based on predicted and true labels
    title_text <- if (!is.null(predicted_labels)) predicted_labels[i] else ""
    if (!is.null(true_labels) && true_labels[i] != predicted_labels[i]) {
      title_text <- paste0(title_text, " (", true_labels[i], ")")
    }
    
    # Reshape and plot the image
    image(
      1:16, 
      1:16, 
      t(matrix(images[, i], nrow = 16, ncol = 16)[16:1, ]),  # Reshape and flip image
      col = grey.colors(255),  # Grayscale
      zlim = c(min_val, max_val),  # Intensity range
      useRaster = TRUE,
      asp = 1, 
      xaxt = 'n', 
      yaxt = 'n', 
      bty = 'n'  # Remove axis and box
    )
    title(title_text, line = -1)  # Add title above image
  }
}
