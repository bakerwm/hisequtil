#' overlap
#' generate overlap between groups


#' generate overlap between groups
#' `overlap` create venn plot for multiple groups
#'
#' @param x Input vector. could be list, data.frame or matrix, to calculate the
#'  overlap between groups
#' @param labels Input vector. The name of the groups
#' @param title Input vector. The title of the plot, default: Overlap
#' @param quantities character. could be `counts` or `percent` or both
#' @param png_file Input vector. NULL or path to the output png file, default
#'  : NULL
#' @param height Input vector. The height of output file in inches, default: 5
#' @param width Input vector. The width of output file in inches, default: 5
#' @param dpi Input vector. The resolution of output file, default: 150
#' @param colors character. The colors for each group,
#' @param ... A series of named character vectors. parameters for [base::plot()]
#'  function
#' @returns A plot
#'
#' @importFrom glue glue
#' @importFrom purrr list_modify
#' @importFrom eulerr euler
#'
#' @seealso [VennDiagram::venn.diagram] which doing the same job
#' @examples
#' x <- list(A = 1:10, B = 5:20)
#' overlap(x, labels = c("GroupA", "GroupB"))
#' y <- list(A = 1:10, B = 5:20, C = c(8:15, 18:25))
#' overlap(y)
#'
#' @export
overlap <- function(
    x, labels = c("A", "B"), title = "Overlap",
    quantities = "counts",
    png_file = NULL, height = 5, width = 5, dpi = 150,
    colors = c("#FF6D60", "#176B87", "#FF8400", "#435B66"), ...) {
  x <- check_overlap_input(x)
  if (is.null(x)) {
    return(NULL)
  }
  if (length(labels) < length(x)) {
    labels <- c(labels, names(x)[-seq_len(length(labels))])
  }
  labels <- labels[1:length(x)]
  # construct data for eulerr
  args <- list(
    fill   = "transparent",
    lwd    = 2,
    col    = colors,
    main   = title,
    labels = list(font = 1, cex = 0.8)
  )
  args <- purrr::list_modify(args, ...) # update from parameters
  args$labels <- c(args$labels, list(labels = labels)) # update names
  args$quantities <- list(type = quantities) # update quantities
  # run
  p <- tryCatch(
    {
      # args$x <- eulerr::euler(c("A" = n1, "B" = n2, "A&B" = n12))
      args$x <- eulerr::euler(x)
      do.call(base::plot, args)
    },
    error=function(cond) {
      message("Failed to run `eulerr::euler()`")
      message(cond)
      return(NULL)
    }
  )
  # save to file
  if (is.character(png_file)) {
    save_plot(p, png_file, width = width, height = height, dpi = dpi)
  }
  p
}


#' check the input for `overlap`
#'
#' @param x Input vector. could be list, data.frame or matrix, to calculate the
#'  overlap between groups
#' @param call vector. for error report, see [rlang::caller_env()]
#' @returns the input vector
#'
#' @importFrom glue glue
#' @importFrom purrr list_modify
#' @importFrom eulerr euler
#'
#' @examples
#' x <- list(A = 1:3, B = 2:4)
#' check_overlap_input(x)
#'
#' @export
check_overlap_input <- function(x, call = rlang::caller_env()) {
  if (is.list(x)) {
    tag <- sapply(x, length) == sapply(x, function(i) length(unique(i)))
    if (!isTRUE(tag)) {
      tag_label <- names(x)[!tag]
      message(glue::glue("duplicates found in {tag_label}"))
      x <- lapply(x, unique) # output
    }
  } else if (is.data.frame(x) | is.matrix(x)) {
    if (! all(apply(x, 2, is.logical))) {
      message(glue::glue("Expect only logical values in data.frame"))
      return(NULL)
    }
  } else {
    cli::cli_abort(
      "{.arg {x}} must be a list or data.frame, not {.obj_type_friendly {x}}.",
      call = call
    )
    return(NULL)
  }
  x
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


#' generate overlap between two groups
#' `overlap2` create venn plot for two groups
#'
#' @param n1 Input integer. The number of components only in the first group,
#'  must be positive integer.
#' @param n2 Input integer. The number of components only in the second group,
#'  must be positive integer.
#' @param n12 Input integer. The number of components overlapped between two
#'  groups, should be positive integer or zero.
#' @param labels Input vector. The name of the groups, length == 2
#' @param title Input vector. The title of the plot, default: Overlap
#' @param quantities character. could be `counts` or `percent` or both
#' @param png_file Input vector. NULL or path to the output png file, default
#'  : NULL
#' @param height Input vector. The height of output file in inches, default: 5
#' @param width Input vector. The width of output file in inches, default: 5
#' @param dpi Input vector. The resolution of output file, default: 150
#' @param colors character. The colors for each group,
#' @param ... A series of named character vectors. parameters for [base::plot()]
#'  function
#' @returns A plot
#'
#' @importFrom glue glue
#' @importFrom purrr list_modify
#' @importFrom eulerr euler
#'
#' @seealso [VennDiagram::venn.diagram] which doing the same job
#' @examples
#' overlap2(3, 8, 1)
#' overlap2(12, 8, 4, name1 = "group1", name2 = "group2")
#'
#' @export
overlap2 <- function(
    n1, n2, n12, labels = c("A", "B"), title = "Overlap",
    quantities = "counts",
    png_file = NULL, height = 5, width = 5, dpi = 150,
    colors = c("#FF6D60", "#176B87"), ...
) {
  # to-do: check input arguments
  if (!check_int(c(n1, n2, n12))) {
    message("overlap2() failed, invalid parameters")
    return(NULL)
  }
  # construct data for eulerr
  args <- list(
    fill   = "transparent",
    lwd    = 2,
    col    = colors,
    main   = title,
    labels = list(font = 1, cex = 0.8)
  )
  args <- purrr::list_modify(args, ...) # update from parameters
  args$labels <- c(args$labels, list(labels = labels)) # update names
  args$quantities <- list(type = quantities) # update quantities
  # run
  p <- tryCatch(
    {
      args$x <- eulerr::euler(c("A" = n1, "B" = n2, "A&B" = n12))
      do.call(base::plot, args)
    },
    error=function(cond) {
      message("Failed to run `eulerr::euler()`")
      message(cond)
      return(NULL)
    }
  )
  # save to file
  if (is.character(png_file)) {
    save_plot(p, png_file, width = width, height = height, dpi = dpi)
  }
  p
}


#' generate overlap between three groups
#' `overlap3` create venn plot for three groups
#'
#' @param n1 Input integer. The number of components only in the first group,
#'  must be positive integer.
#' @param n2 Input integer. The number of components only in the second group,
#'  must be positive integer.
#' @param n3 Input integer. The number of components only in the third group,
#'  must be positive integer.
#' @param n12 Input integer. The number of components overlapped between
#'  1 and 2, should be positive integer or zero.
#' @param n13 Input integer. The number of components overlapped between
#'  1 and 3, should be positive integer or zero.
#' @param n23 Input integer. The number of components overlapped between
#'  2 and 3, should be positive integer or zero.
#' @param n123 Input integer. The number of components overlapped between
#'  all three groups, should be positive integer or zero.
#' @param labels Input vector. The name of the groups, length == 3
#' @param title Input vector. The title of the plot, default: Overlap
#' @param quantities character. could be `counts` or `percent`
#' @param png_file Input vector. NULL or path to the output png file, default
#'  : NULL
#' @param height Input vector. The height of output file in inches, default: 5
#' @param width Input vector. The width of output file in inches, default: 5
#' @param dpi Input vector. The resolution of output file, default: 150
#' @param colors character. The colors for each group,
#' @param ... A series of named character vectors. parameters for [base::plot()]
#'  function
#' @returns A plot
#'
#' @importFrom glue glue
#' @importFrom purrr list_modify
#' @importFrom eulerr euler
#'
#' @seealso [VennDiagram::venn.diagram] which doing the same job
#' @examples
#' overlap2(3, 8, 1)
#' overlap2(12, 8, 4, name1 = "group1", name2 = "group2")
#'
#' @export
overlap3 <- function(
    n1, n2, n3, n12, n13, n23, n123, labels = c("A", "B", "C"),
    title = "Overlap", quantities = "counts",
    png_file = NULL, height = 5, width = 5, dpi = 150,
    colors = c("#FF6D60", "#176B87", "#FF8400"), ...
) {
  # to-do: check input arguments
  if (!check_int(c(n1, n2, n3, n12, n13, n23, n123))) {
    message("overlap3() failed, invalid parameters")
    return(NULL)
  }
  # construct data for eulerr
  args <- list(
    fill   = "transparent",
    lwd    = 2,
    col    = colors,
    main   = title,
    labels = list(font = 1, cex = 0.8),
    quantities = list(type = c("counts", "percent"))
  )
  args <- purrr::list_modify(args, ...) # update from parameters
  args$labels <- c(args$labels, list(labels = labels)) # update names
  args$quantities <- list(type = quantities) # update quantities
  # run
  p <- tryCatch(
    {
      args$x <- eulerr::euler(
        c(
          "A" = n1,
          "B" = n2,
          "C" = n3,
          "A&B" = n12,
          "A&C" = n13,
          "B&C" = n23,
          "A&B&C" = n123
        )
      )
      do.call(base::plot, args)
    },
    error=function(cond) {
      message("Failed to run `eulerr::euler()`")
      message(cond)
      return(NULL)
    }
  )
  # save to file
  if (is.character(png_file)) {
    save_plot(p, png_file, width = width, height = height, dpi = dpi)
  }
  p
}


#' recommend ggupset and UpSetR for more than 3 groups overlap plot
#' @param ... Input vector. arguments
overlap4 <- function(...) {
  message("recommend `ggupset` and `UpSetR` for overlap more than 3 groups")
}




