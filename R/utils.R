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


#' calculate the overlaps between each group
#' @param x list. list of items
#' @param call vector. for error report, see [rlang::caller_env()]
#' @returns vector of overlaps between each group
#'
#' @importFrom rlang caller_env
#' @importFrom cli cli_abort
#' @importFrom glue glue
#'
#' @examples
#' x <- list(x = 1:10, y = 5:20)
#' cal_overlap(x)
#'
#' @export
cal_overlap <- function(x, call = rlang::caller_env()) {
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {x}} must be a list, not {.obj_type_friendly {x}}.",
      call = call
    )
    return(NULL)
  }
  if (length(x) > 4 | length(x) == 0) {
    message(glue::glue("{length(x)} items found in x, expect [2-4] groups"))
    return(NULL)
  }
  if (length(x) == 2) {
    # check names
    if (is.null(names(x))) {
      names(x) <- LETTERS[1:2]
    }
    n12 <- length(intersect(x[[1]], x[[2]]))
    n1  <- length(x[[1]]) - n12
    n2  <- length(x[[2]]) - n12
    out <- list(
      labels = names(x),
      n1     = n1,
      n2     = n2,
      n12    = n12
    )
  } else if(length(x) == 3) {
    # check names
    if (is.null(names(x))) {
      names(x) <- LETTERS[1:3]
    }
    n123 <- length(Reduce(intersect, x))
    n12  <- length(intersect(x[[1]], x[[2]])) - n123
    n13  <- length(intersect(x[[1]], x[[3]])) - n123
    n23  <- length(intersect(x[[2]], x[[3]])) - n123
    n1   <- length(x[[1]]) - n12 - n13 - n123
    n2   <- length(x[[2]]) - n12 - n23 - n123
    n3   <- length(x[[3]]) - n13 - n23 - n123
    out <- list(
      labels = names(x),
      n1     = n1,
      n2     = n2,
      n3     = n3,
      n12    = n12,
      n13    = n13,
      n23    = n23,
      n123   = n123
    )
  } else if(length(x) == 4) {
    # check names
    if (is.null(names(x))) {
      names(x) <- LETTERS[1:4]
    }
    n1234 <- length(Reduce(intersect, x))
    n123  <- length(Reduce(intersect, x[c(1, 2, 3)])) - n1234
    n124  <- length(Reduce(intersect, x[c(1, 2, 4)])) - n1234
    n134  <- length(Reduce(intersect, x[c(1, 3, 4)])) - n1234
    n234  <- length(Reduce(intersect, x[c(2, 3, 4)])) - n1234
    n12   <- length(Reduce(intersect, x[c(1, 2)])) - n123 - n124 - n1234
    n13   <- length(Reduce(intersect, x[c(1, 3)])) - n123 - n134 - n1234
    n14   <- length(Reduce(intersect, x[c(1, 4)])) - n124 - n134 - n1234
    n23   <- length(Reduce(intersect, x[c(2, 3)])) - n123 - n234 - n1234
    n24   <- length(Reduce(intersect, x[c(2, 4)])) - n124 - n234 - n1234
    n34   <- length(Reduce(intersect, x[c(3, 4)])) - n134 - n234 - n1234
    n1    <- length(x[[1]]) - n12 - n13 - n14 - n123 - n124 - n134 - n1234
    n2    <- length(x[[2]]) - n12 - n23 - n24 - n123 - n124 - n234 - n1234
    n3    <- length(x[[3]]) - n13 - n23 - n34 - n123 - n134 - n234 - n1234
    n4    <- length(x[[4]]) - n12 - n23 - n24 - n124 - n134 - n234 - n1234
    out <- list(
      labels = names(x),
      n1     = n1,
      n2     = n2,
      n3     = n3,
      n4     = n4,
      n12    = n12,
      n13    = n13,
      n14    = n14,
      n23    = n23,
      n24    = n24,
      n34    = n34,
      n123   = n123,
      n124   = n124,
      n134   = n134,
      n234   = n234,
      n1234  = n1234
    )
  }
  return(out)
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

























