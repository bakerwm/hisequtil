#' helper functions


#' Check numbers of overlapx functions
#' @param x Input vector. numbers
#' @param call vector. for error report, see [rlang::caller_env()]
#' @returns logical
#'
#' @importFrom rlang caller_env
#' @importFrom cli cli_abort
#'
#' @examples
#' check_int(x = c(1:10))
#'
#' @export
check_int <- function(x, call = rlang::caller_env()) {
  if (is.numeric(x)) {
    if (all(x >= 0)) {
      if (all(floor(x) == x)) {
        TRUE
      } else {
        message("Only integers accepted, not float")
        FALSE
      }
    } else {
      message("Only positive integers, not negative values")
      FALSE
    }
  } else {
    cli::cli_abort(
      "{.arg {arg}} must be a integer vector, not {.obj_type_friendly {x}}.",
      call = call
    )
    FALSE
  }
}


#' save plot as pdf
#' @param x graph vector. Could be ggplot or grob
#' @param file character. Path to the pdf file
#' @param overwrite bool. Overwrite exists files, default: FALSE
#' @param save_rds bool. Save plot object as RDS, default: FALSE
#' @param call vector. for error report, see [rlang::caller_env()]
#' @param ... arguments for [export::graph2pdf] function, such as `width`,
#'  `height`, `font`, `bg`, ...
#' @returns NULL. return NULL
#'
#' @importFrom rlang caller_env
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom export graph2bitmap graph2vector
#' @importFrom purrr list_modify
#' @importFrom grDevices dev.off pdf
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' p <- plot(1:10)
#' save_plot(p, "demo.png")
#' save_plot(p, "demo.pdf", width = 4, height = 3, bg = "white")
#' }
#'
#' @export
save_plot <- function(x, file, overwrite = FALSE, save_rds = FALSE,
                      call = rlang::caller_env(), ...) {
  #----------------------------------------------------------------------------#
  # 1. Check arguments
  if (!any(c(is(x, "grob"), is(x, "ggplot")))) {
    cli::cli_abort(
      "{.arg {arg}} must be a ggplot or grob, not {.obj_type_friendly {x}}." #,
      # call = call
    )
    return(NULL)
  }
  if (!is.character(file)) {
    cli::cli_abort(
      "{.arg {file}} must be a character, not {.obj_type_friendly {file}}.",
      call = call
    )
    return(NULL)
  }
  # determine device
  fext <- toupper(tools::file_ext(file))
  if (fext %in% c("PDF", "SVG", "EPS")) {
    device <- export::graph2vector
  } else if (fext %in% c("PNG", "TIF", "JPG")) {
    device <- export::graph2bitmap
  } else {
    message("unknown file type, expect [pdf, png]")
    return(NULL)
  }
  #----------------------------------------------------------------------------#
  # 2. Save plot
  args <- list(
    x      = x,
    file   = file,
    type   = fext,
    width  = 3,
    height = 3.4,
    dpi    = 300,
    font   = "Arial",
    bg     = "transparent"
  )
  args <- purrr::list_modify(args, ...)
  # save only supported arguments
  sup_args <- names(formals(device))
  un_args  <- setdiff(names(args), sup_args)
  if (length(un_args) > 0) {
    for (i in un_args) {
      args[[i]] <- NULL # remove unsupported args
    }
  }
  if (file.exists(file) && ! isTRUE(overwrite)) {
    message(glue::glue("file exists: {file}"))
  } else {
    pdf(NULL) # prevent generating empty file: "Rplot.pdf"
    message(glue::glue(
      "export pdf size, width={args$width}, height={args$height}"
    ))
    tryCatch(
      {do.call(device, args)},
      error=function(cond) {
        message("Failed to run `export::graph2pdf()`")
        message(cond)
        return(NULL)
      }
    )
    dev.off() # close empty pdf
    if (isTRUE(save_rds)) {
      rds <- gsub("\\.[a-z]+$", ".rds", file, perl = TRUE)
      saveRDS(x, file = rds)
    }
  }
}

























