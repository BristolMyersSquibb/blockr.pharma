# Board-level scale map: per-variable discrete scales (level -> color/shape/
# linetype) carried as the "scale_map" board option and resolved at render
# time. Spec: blockr.design/open/cdex-attribute-map. The option id, value
# shape and hash assignment are a cross-package convention; consumers vendor
# resolve_scales() and read the option via get_board_option_or_null().

SCALE_MAP_CHANNELS <- c("color", "shape", "linetype")

#' Scale map: study-wide level aesthetics
#'
#' A scale map binds variable levels to aesthetics (colors, shapes, line
#' types) board-wide: every rendering block that consumes the map shows the
#' same level in the same color. Bindings are keyed by variable name. Each
#' channel is one vector: a *named* vector fixes values per level (stated in
#' display order), an *unnamed* vector is a pool from which unmatched levels
#' are assigned by a stable hash of the level name (consistent across views,
#' sessions and data refreshes). Levels of a `color` channel not covered by
#' either fall back to the palette supplied at resolution time (typically the
#' board theme's colors).
#'
#' `new_scale_map()` accepts bindings and whole maps in any mix and flattens
#' them with later-wins-by-variable semantics, so a study overrides a default
#' catalog by listing replacement bindings after it. Overriding replaces the
#' whole binding (no channel-level merge).
#'
#' @param ... For `new_scale_map()`: `scale_binding()` objects, `scale_map`
#'   objects or plain lists of the same shape (later entries win by variable
#'   name). For `new_scale_map_option()`: forwarded to
#'   [blockr.core::new_board_option()].
#' @param var Variable (column) name the binding applies to
#' @param color,shape,linetype Channel vectors: named = fixed values per
#'   level (names are always matched as character, also for numeric-looking
#'   levels such as AE grades `"1"`–`"5"`), unnamed = pool for stable-hash
#'   auto-assignment. `shape` is coerced to integer (R `pch` / symbol codes).
#'
#' @return `new_scale_map()` and `as_scale_map()` return a `scale_map` object
#'   (a named list of bindings); `scale_binding()` returns a `scale_binding`;
#'   `resolve_scales()` returns a list with entries `color`, `shape`,
#'   `linetype` (named vectors over the supplied levels; absent when nothing
#'   resolves) and `order` (character), or `NULL` for an unregistered
#'   variable; `new_scale_map_option()` returns a `board_option`.
#'
#' @export
new_scale_map <- function(...) {
  args <- Filter(Negate(is.null), list(...))

  res <- list()
  for (x in args) {
    if (inherits(x, "scale_binding")) {
      res[[attr(x, "var")]] <- unclass_binding(x)
    } else if (is.list(x)) {
      x <- as_scale_map(x)
      for (var in names(x)) {
        res[[var]] <- x[[var]]
      }
    } else {
      stop("`new_scale_map()` expects scale_binding or scale_map objects.",
           call. = FALSE)
    }
  }

  structure(res, class = c("scale_map", "list"))
}

#' @rdname new_scale_map
#' @export
scale_binding <- function(var, color = NULL, shape = NULL, linetype = NULL) {
  stopifnot(is.character(var), length(var) == 1L, nzchar(var))

  channels <- Filter(
    Negate(is.null),
    list(
      color = validate_channel(color, "color", var),
      shape = validate_channel(shape, "shape", var),
      linetype = validate_channel(linetype, "linetype", var)
    )
  )

  structure(channels, var = var, class = "scale_binding")
}

unclass_binding <- function(x) {
  attr(x, "var") <- NULL
  unclass(x)
}

validate_channel <- function(x, channel, var) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.list(x)) {
    nms <- names(x)
    x <- unlist(x, use.names = FALSE)
    names(x) <- nms
  }

  if (!is.atomic(x) || length(x) == 0L) {
    stop("Channel `", channel, "` of binding `", var,
         "` must be a non-empty vector.", call. = FALSE)
  }

  nms <- names(x)
  named <- !is.null(nms) & nzchar(nms %||% "")

  if (!is.null(nms) && any(named) && !all(named)) {
    stop("Channel `", channel, "` of binding `", var,
         "` must be fully named (fixed values) or fully unnamed (pool).",
         call. = FALSE)
  }

  if (!is.null(nms) && anyDuplicated(nms)) {
    stop("Channel `", channel, "` of binding `", var,
         "` has duplicated level names.", call. = FALSE)
  }

  if (identical(channel, "shape")) {
    nms <- names(x)
    x <- as.integer(x)
    names(x) <- nms
  } else {
    nms <- names(x)
    x <- as.character(x)
    names(x) <- nms
  }

  x
}

#' @param x Object to coerce / test
#' @rdname new_scale_map
#' @export
is_scale_map <- function(x) {
  inherits(x, "scale_map")
}

#' @rdname new_scale_map
#' @export
as_scale_map <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is_scale_map(x)) {
    return(x)
  }

  stopifnot(is.list(x))

  if (length(x) && (is.null(names(x)) || !all(nzchar(names(x))))) {
    stop("A scale map must be a fully named list (variable names).",
         call. = FALSE)
  }

  res <- lapply(names(x), function(var) {
    binding <- x[[var]]
    stopifnot(is.list(binding))
    extra <- setdiff(names(binding), SCALE_MAP_CHANNELS)
    if (length(extra)) {
      stop("Unknown channel(s) ", toString(extra), " in binding `", var, "`.",
           call. = FALSE)
    }
    chans <- lapply(
      SCALE_MAP_CHANNELS,
      function(ch) validate_channel(binding[[ch]], ch, var)
    )
    names(chans) <- SCALE_MAP_CHANNELS
    Filter(Negate(is.null), chans)
  })
  names(res) <- names(x)

  structure(res, class = c("scale_map", "list"))
}

#' @export
print.scale_map <- function(x, ...) {
  cat("<scale_map[", length(x), "]>\n", sep = "")
  for (var in names(x)) {
    chs <- names(x[[var]])
    desc <- if (length(chs)) {
      paste(
        vapply(chs, function(ch) {
          v <- x[[var]][[ch]]
          paste0(ch, "[", if (is.null(names(v))) "pool" else "fixed",
                 " ", length(v), "]")
        }, character(1L)),
        collapse = ", "
      )
    } else {
      "auto"
    }
    cat("  ", var, ": ", desc, "\n", sep = "")
  }
  invisible(x)
}

# The pinned hash assignment of the scale-map convention: a pure function of
# the level name, so assignment is independent of which other levels a view
# happens to show. Do not change without updating the convention (and every
# vendored copy).
scale_map_hash_pick <- function(level, pool) {
  idx <- strtoi(substr(rlang::hash(level), 1L, 7L), 16L) %% length(pool)
  pool[[idx + 1L]]
}

#' @param map A `scale_map` (or plain list of the same shape, or `NULL`)
#' @param levels Character vector of levels actually shown (pass
#'   `levels(col)` for factors, `unique(as.character(col))` otherwise)
#' @param palette Fallback pool for the `color` channel (typically the active
#'   board theme's colors); `shape`/`linetype` have no fallback pool
#' @rdname new_scale_map
#' @export
resolve_scales <- function(map, var, levels, palette = NULL) {
  map <- as_scale_map(map)

  if (is.null(map) || is.null(var) || !length(levels) ||
        !var %in% names(map)) {
    return(NULL)
  }

  levels <- unique(as.character(levels))
  binding <- map[[var]]

  resolve_channel <- function(channel, fallback_pool = NULL) {
    spec <- binding[[channel]]
    fixed <- if (!is.null(spec) && !is.null(names(spec))) spec
    pool <- if (!is.null(spec) && is.null(names(spec))) spec
    pool <- pool %||% fallback_pool

    vals <- lapply(levels, function(lv) {
      if (!is.null(fixed) && lv %in% names(fixed)) {
        fixed[[lv]]
      } else if (!is.null(pool) && length(pool)) {
        scale_map_hash_pick(lv, pool)
      } else {
        NULL
      }
    })

    keep <- !vapply(vals, is.null, logical(1L))
    if (!any(keep)) {
      return(NULL)
    }

    out <- unlist(vals[keep])
    names(out) <- levels[keep]
    out
  }

  fixed_names <- unique(unlist(lapply(
    binding[SCALE_MAP_CHANNELS],
    function(spec) names(spec)
  )))

  res <- Filter(
    Negate(is.null),
    list(
      color = resolve_channel("color", fallback_pool = palette),
      shape = resolve_channel("shape"),
      linetype = resolve_channel("linetype")
    )
  )

  res$order <- c(intersect(fixed_names, levels), setdiff(levels, fixed_names))

  res
}

#' @rdname new_scale_map
#' @export
board_scale_map <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "scale_map", blockr.core::get_session()
    )
    if (is.null(val) || !length(val)) NULL else as_scale_map(val)
  })
}

#' @param map Initial map value (e.g. `default_clinical_map()` amended with
#'   study bindings)
#' @param category Settings sidebar category
#' @rdname new_scale_map
#' @export
new_scale_map_option <- function(map = new_scale_map(), category = "Scales",
                                 ...) {
  blockr.core::new_board_option(
    id = "scale_map",
    default = as_scale_map(map),
    ui = scale_map_editor_ui,
    server = scale_map_editor_server,
    update_trigger = NULL,
    transform = function(x) as_scale_map(x),
    category = category,
    ...
  )
}

# The option value (a named list keyed by VARIABLE names) cannot go through
# blockr_ser.board_option() as-is: that method matches value names against
# constructor argument names. Wrap it under the `map` argument, mirroring
# blockr_ser.llm_model_option().
#' @exportS3Method blockr.core::blockr_ser
blockr_ser.scale_map_option <- function(x, option = NULL, ...) {
  val <- option %||% blockr.core::board_option_value(x)
  NextMethod(option = list(map = scale_map_to_plain(val)))
}

scale_map_to_plain <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  # jsonlite drops names on atomic vectors (only lists serialize as JSON
  # objects), so fixed channels must travel as named lists.
  lapply(unclass(x), function(binding) {
    lapply(binding, function(spec) {
      if (!is.null(names(spec))) as.list(spec) else spec
    })
  })
}
