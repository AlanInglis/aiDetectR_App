# global.R
library(shiny)
library(reticulate)

## 1) lazy loader with wheel installation
torch        <- NULL
transformers <- NULL
PIL_Image    <- NULL
processor    <- NULL
model        <- NULL
device       <- NULL

load_py_model <- function() {
  if (!is.null(model)) return(invisible())
  if (!py_module_available("torch")) {
    py_install(
      packages = c(
        "numpy==1.26.4",
        "torch==2.2.2+cpu",
        "torchvision",
        "torchaudio",
        "transformers",
        "pillow",
        "accelerate"
      ),
      pip              = TRUE,
      pip_install_opts = "--extra-index-url https://download.pytorch.org/whl/cpu"
    )
  }
  torch        <<- import("torch")
  transformers <<- import("transformers")
  PIL_Image    <<- import("PIL.Image")
  hf_model     <- "Ateeqq/ai-vs-human-image-detector"
  processor    <<- transformers$AutoImageProcessor$from_pretrained(hf_model)
  model        <<- transformers$AutoModelForImageClassification$from_pretrained(hf_model)
  device       <<- torch$device(
    if (torch$cuda$is_available()) "cuda"
    else if (torch$backends$mps$is_available()) "mps"
    else "cpu"
  )
  model$to(device)$eval()
}

## 2) your detect function
detect_ai_image <- function(image_path, threshold = 0.5) {
  load_py_model()
  img <- PIL_Image$open(image_path)$convert("RGB")
  batch      <- processor(img, return_tensors = "pt")
  pixel_vals <- batch$pixel_values$to(device)
  out <- with(torch$no_grad(), model(pixel_values = pixel_vals))
  probs  <- torch$softmax(out$logits, as.integer(1))$cpu()$numpy()[1, ]
  labels <- unlist(model$config$id2label, use.names = FALSE)
  pred   <- which.max(probs)
  list(
    label      = labels[pred],
    confidence = round(probs[pred], 4) * 100,
    probs      = setNames(round(probs, 4) * 100, labels),
    flagged    = (labels[pred] == "ai") && probs[pred] >= threshold
  )
}