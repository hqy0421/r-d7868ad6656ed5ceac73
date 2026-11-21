# Mergeable exact distinct-count states and reports
#
# Exact (not approximate) distinct counting over multiple grouping levels.
# Events arrive in shards: each shard produces a state via [distinct_state()],
# states are merged with [merge_distinct_state()], and a report with detail,
# subtotal and grand-total rows is materialized with [collect_distinct_report()].
#
# All operations are set-based, so merging is commutative, associative and
# idempotent: the order of shards and repeated merges of the same shard do not
# change the result.

new_distinct_state <- function(records, zero_groups, dim_ptype, id_ptype,
                               dimensions, id_name) {
  if (!is.na(id_name)) {
    stopifnot(
      is.data.frame(records),
      is.data.frame(zero_groups),
      identical(names(records), c(dimensions, id_name)),
      identical(names(zero_groups), dimensions)
    )
  }

  structure(
    list(
      # Unique (dimensions, id) rows, ids are guaranteed non-missing.
      records = records,
      # Unique dimension combinations only observed with missing ids.
      zero_groups = zero_groups,
      dim_ptype = dim_ptype,
      id_ptype = id_ptype,
      dimensions = dimensions,
      id_name = id_name
    ),
    class = "tbl_df_distinct_state"
  )
}

# Errors raised by the distinct-count machinery share a common condition class.
distinct_abort <- function(message, ..., call) {
  abort(
    message,
    class = tibble_error_class("distinct"),
    ...,
    call = call,
    use_cli_format = TRUE
  )
}

#' Test for a mergeable distinct-count state
#'
#' @param x An object.
#' @returns `is_distinct_state()` returns a single `TRUE` or `FALSE`.
#' @export
is_distinct_state <- function(x) {
  inherits(x, "tbl_df_distinct_state")
}

# Flatten plain lists one level, like rlang::list_flatten() (not available on
# the minimum supported rlang version). State objects are lists themselves but
# must be kept intact, so they are never treated as containers.
flatten_states <- function(x) {
  is_plain_list <- map_lgl(
    x,
    function(elt) is.list(elt) && !is_distinct_state(elt)
  )
  if (!any(is_plain_list)) {
    return(x)
  }
  c(x[!is_plain_list], do.call(c, unname(x[is_plain_list])))
}

# Recursively flatten lists of states while keeping non-list elements.
flatten_states_recursively <- function(x) {
  repeat {
    flattened <- flatten_states(x)
    if (identical(flattened, x)) {
      return(x)
    }
    x <- flattened
  }
}

# Resolve a column-selection argument against the columns of a data frame
# without taking a tidyselect dependency. Supported expressions:
# * bare column names, optionally combined with `c(...)`;
# * character vectors of column names (also via a variable);
# * integer column positions (also via a variable);
# * any expression that evaluates to such a character/integer vector.
select_columns <- function(quo, x, call) {
  env <- quo_get_env(quo)
  col_names <- names(x)

  resolve <- function(expr) {
    if (is.character(expr) || is.numeric(expr)) {
      return(vec_as_location(expr, n = length(x), names = col_names))
    }
    if (is.symbol(expr)) {
      nm <- as_string(expr)
      pos <- match(nm, col_names)
      if (!is.na(pos)) {
        return(pos)
      }
      # The symbol is not a column: look up a variable in the caller env.
      val <- tryCatch(
        get(nm, envir = env, inherits = TRUE),
        error = function(e) NULL
      )
      if (is.character(val) || is.numeric(val)) {
        return(vec_as_location(val, n = length(x), names = col_names))
      }
      distinct_abort(
        c(
          "Column not found.",
          x = paste0("Column ", encodeString(nm, quote = "\""), " doesn't exist.")
        ),
        call = call
      )
    }
    if (is.call(expr) && identical(expr[[1L]], quote(c))) {
      if (length(expr) == 1L) {
        return(integer())
      }
      return(unlist(map(as.list(expr[-1L]), resolve), use.names = FALSE))
    }
    # General expression: must evaluate to column names or positions.
    val <- eval_tidy(rlang::new_quosure(expr, env))
    vec_as_location(val, n = length(x), names = col_names)
  }

  locs <- vec_unique(resolve(quo_get_expr(quo)))
  col_names[locs]
}

# Build a zero-row data frame with the columns (and types) of a data-frame
# prototype.
empty_df_of <- function(ptype) {
  if (!is.data.frame(ptype)) {
    return(new_data_frame(list(), n = 0L))
  }
  out <- map(ptype, function(col) vec_rep(col, 0L))
  new_data_frame(out, n = 0L)
}

# Short, human-readable type description for error messages.
type_label <- function(x) {
  if (is.object(x)) {
    return(paste0("an object of class ", paste(class(x), collapse = "/")))
  }
  what <- typeof(x)
  article <- if (what %in% c("integer", "environment")) "an" else "a"
  paste(article, what)
}

#' Mergeable exact distinct-count state for a shard of events
#'
#' @description
#' `distinct_state()` summarizes a shard (a data frame of events) into a
#' mergeable state that supports exact distinct counting of a user id column
#' across one or several grouping dimensions.
#'
#' The state stores:
#'
#' * the unique `(dimensions, id)` combinations whose id is **not** missing;
#' * the unique dimension combinations whose rows all have missing ids, so that
#'   the corresponding zero-person groups are retained.
#'
#' States created from different shards are combined with
#' [merge_distinct_state()] and turned into a report with
#' [collect_distinct_report()]. Because states hold sets of already
#' deduplicated records, merging is independent of shard order and merging the
#' same shard twice does not change the result.
#'
#' @param x A data frame containing the dimension columns and the id column.
#' @param dimensions Dimension columns. Bare column names and `c(...)`
#'   combinations are supported, as are character vectors of column names and
#'   integer positions. Real `NA` values in these columns form their own groups
#'   and are never dropped.
#' @param id A single column holding user ids, selected with the same syntax as
#'   `dimensions`. Rows with a missing id do not contribute to any count, but
#'   their dimension combinations are retained as zero-person groups.
#'
#' @returns An object of class `"tbl_df_distinct_state"`. The state is a list
#'   with elements `records`, `zero_groups`, `dim_ptype`, `id_ptype`,
#'   `dimensions` and `id_name`.
#'
#'   `distinct_state()` is a generic so that other data sources can provide
#'   their own methods.
#' @export
#' @examples
#' shard1 <- tibble(
#'   region = c("a", "a", "b", "b", NA),
#'   channel = c("x", "x", "y", "y", "x"),
#'   user_id = c(1L, 1L, 2L, 3L, NA)
#' )
#' shard2 <- tibble(
#'   region = c("a", "b", "b"),
#'   channel = c("x", "y", "y"),
#'   user_id = c(4L, 2L, NA)
#' )
#'
#' st1 <- distinct_state(shard1, c(region, channel), user_id)
#' st2 <- distinct_state(shard2, c(region, channel), user_id)
#'
#' merged <- merge_distinct_state(st1, st2)
#' collect_distinct_report(merged)
distinct_state <- function(x, dimensions, id) {
  UseMethod("distinct_state")
}

#' @export
distinct_state.data.frame <- function(x, dimensions, id) {
  call <- rlang::caller_env()
  dim_names <- select_columns(rlang::enquo(dimensions), x, call = call)
  id_names <- select_columns(rlang::enquo(id), x, call = call)
  if (length(id_names) != 1L) {
    distinct_abort(
      c(
        "`id` must select exactly one column.",
        x = paste0(length(id_names), " columns were selected.")
      ),
      call = call
    )
  }
  id_name <- id_names[[1L]]

  if (anyDuplicated(c(dim_names, id_name)) > 0L) {
    distinct_abort(
      "The id column can't also be used as a dimension column.",
      call = call
    )
  }

  dim_data <- x[dim_names]
  id_vec <- x[[id_name]]

  dim_ptype <- vec_ptype(dim_data)
  id_ptype <- vec_ptype(id_vec)
  full_ptype <- new_data_frame(
    c(dim_ptype, stats::setNames(list(id_ptype), id_name))
  )

  if (vec_size(x) == 0L) {
    return(
      new_distinct_state(
        records = empty_df_of(full_ptype),
        zero_groups = empty_df_of(dim_ptype),
        dim_ptype = dim_ptype,
        id_ptype = id_ptype,
        dimensions = dim_names,
        id_name = id_name
      )
    )
  }

  id_missing <- vec_detect_missing(id_vec)

  full <- dim_data
  full[[id_name]] <- id_vec

  if (all(id_missing)) {
    records <- empty_df_of(full_ptype)
  } else {
    records <- vec_unique(vec_slice(full, !id_missing))
  }

  if (any(id_missing)) {
    zero_groups <- vec_unique(vec_slice(dim_data, id_missing))
  } else {
    zero_groups <- empty_df_of(dim_ptype)
  }

  new_distinct_state(
    records = records,
    zero_groups = zero_groups,
    dim_ptype = dim_ptype,
    id_ptype = id_ptype,
    dimensions = dim_names,
    id_name = id_name
  )
}

#' @export
distinct_state.default <- function(x, dimensions, id) {
  distinct_abort(
    c(
      "Can't build a distinct state from this object.",
      x = paste0("`x` must be a data frame, not ", type_label(x), ".")
    ),
    call = rlang::caller_env()
  )
}

#' Merge distinct-count states from several shards
#'
#' @description
#' Combines one or several states created by [distinct_state()] into a single
#' state. The underlying operation is set union followed by deduplication,
#' which makes the merge:
#'
#' * commutative (shards may be merged in any order);
#' * associative (shards may be merged pairwise or all at once);
#' * idempotent (merging the same shard again leaves the result unchanged).
#'
#' States must describe the same dimension columns (same names and
#' compatible types) and the same id column.
#'
#' @param ... States, or (possibly nested) lists of states. List arguments are
#'   flattened recursively, so both `merge_distinct_state(a, b, c)` and
#'   `merge_distinct_state(list(a, b, c))` work. `NULL` arguments are dropped.
#'
#' @returns A state of class `"tbl_df_distinct_state"`. Calling
#'   `merge_distinct_state()` with no arguments returns an empty state without
#'   a schema; merging it with a real state returns that state's schema.
#' @export
#' @examples
#' # See `distinct_state()` for the full example.
#' st1 <- distinct_state(
#'   tibble(region = "a", channel = "x", user_id = 1L),
#'   c(region, channel),
#'   user_id
#' )
#' st2 <- distinct_state(
#'   tibble(region = "a", channel = "x", user_id = 1L),
#'   c(region, channel),
#'   user_id
#' )
#' # Identical records across shards are counted once:
#' collect_distinct_report(merge_distinct_state(st1, st2))
merge_distinct_state <- function(...) {
  states <- compact(flatten_states_recursively(list(...)))

  if (length(states) == 0L) {
    return(
      new_distinct_state(
        records = new_data_frame(list(), n = 0L),
        zero_groups = new_data_frame(list(), n = 0L),
        dim_ptype = new_data_frame(list(), n = 0L),
        id_ptype = unspecified(0L),
        dimensions = character(),
        id_name = NA_character_
      )
    )
  }

  for (i in seq_along(states)) {
    if (!is_distinct_state(states[[i]])) {
      distinct_abort(
        c(
          "All arguments must be distinct states.",
          x = paste0("Argument ", i, " is ", type_label(states[[i]]), ".")
        ),
        call = rlang::caller_env()
      )
    }
  }

  # Schema-less empty states disappear when merged with real states.
  real_states <- compact(map(
    states,
    function(st) if (is.na(st$id_name)) NULL else st
  ))

  if (length(real_states) == 0L) {
    return(states[[1L]])
  }

  anchor <- real_states[[1L]]
  for (i in seq_along(real_states)[-1L]) {
    st <- real_states[[i]]
    if (!identical(st$dimensions, anchor$dimensions)) {
      distinct_abort(
        c(
          "All states must use the same dimension columns.",
          x = paste0(
            "State ", i, " uses ",
            paste0(encodeString(st$dimensions, quote = "\""), collapse = ", "),
            ", expected ",
            paste0(
              encodeString(anchor$dimensions, quote = "\""),
              collapse = ", "
            ),
            "."
          )
        ),
        call = rlang::caller_env()
      )
    }
    if (!identical(st$id_name, anchor$id_name)) {
      distinct_abort(
        c(
          "All states must use the same id column.",
          x = paste0(
            "State ", i, " uses id column ",
            encodeString(st$id_name, quote = "\""), ", expected ",
            encodeString(anchor$id_name, quote = "\""), "."
          )
        ),
        call = rlang::caller_env()
      )
    }
    # Compatible ptypes: vec_ptype2() raises a clear error otherwise.
    vec_ptype2(st$records, anchor$records, x_arg = "states", y_arg = "states")
    vec_ptype2(st$zero_groups, anchor$zero_groups)
  }

  records <- vec_unique(do.call(
    vec_rbind,
    map(real_states, function(st) st$records)
  ))
  zero_groups <- vec_unique(do.call(
    vec_rbind,
    map(real_states, function(st) st$zero_groups)
  ))

  # Use the prototypes of the actually combined data: when shards carry
  # coercible but different column types (e.g. integer + double), the report
  # must use the common type, not the anchor's original type.
  new_distinct_state(
    records = records,
    zero_groups = zero_groups,
    dim_ptype = vec_ptype(records[anchor$dimensions]),
    id_ptype = vec_ptype(records[[anchor$id_name]]),
    dimensions = anchor$dimensions,
    id_name = anchor$id_name
  )
}

# All grouping levels (subsets of the dimensions) for the full cube. Each
# element is a character vector of dimension names kept at that level.
cube_levels <- function(dimensions) {
  sets <- list(character())
  for (dim in dimensions) {
    sets <- c(sets, map(sets, function(s) c(s, dim)))
  }
  # Detail level (all dimensions) first, grand total (no dimensions) last.
  sets[rev(seq_along(sets))]
}

check_levels <- function(levels, dimensions, call) {
  if (!is.list(levels)) {
    distinct_abort(
      c(
        "`levels` must be a list of character vectors.",
        x = paste0("It is ", type_label(levels), ".")
      ),
      call = call
    )
  }

  for (i in seq_along(levels)) {
    lvl <- levels[[i]]
    if (!is.character(lvl) || anyNA(lvl) ||
      !all(lvl %in% dimensions) || anyDuplicated(lvl) > 0L) {
      distinct_abort(
        c(
          "Each element of `levels` must be a unique subset of the dimension columns.",
          x = paste0("Element ", i, " is not.")
        ),
        call = call
      )
    }
  }
  invisible(levels)
}

# Resolve unique output names for the count and grouping marker columns.
# Dimension columns always keep their names; the count/marker columns yield
# when they collide with a dimension name.
resolve_output_names <- function(dimensions, count_name, .name_repair) {
  if (.name_repair == "check_unique") {
    return(list(count = count_name, grouping = ".grouping"))
  }

  make_unique <- function(wanted, taken) {
    candidate <- wanted
    k <- 1L
    while (candidate %in% taken) {
      candidate <- paste0(wanted, "...", k)
      k <- k + 1L
    }
    candidate
  }

  count_final <- make_unique(count_name, dimensions)
  grouping_final <- make_unique(
    ".grouping",
    c(dimensions, count_final)
  )
  list(count = count_final, grouping = grouping_final)
}

# Materialize one grouping level: subset the dimensions, deduplicate at this
# level and count distinct non-missing ids.
render_level <- function(state, level_dims, count_name, grouping_name,
                         all_dimensions, zero_level_dims) {
  n_dims <- length(all_dimensions)
  omitted <- !(all_dimensions %in% level_dims)

  if (length(level_dims) == 0L) {
    # Grand-total level: a single (zero-column) key group, even on empty input.
    keys <- new_data_frame(list(), n = 1L)
    counts <- vec_size(vec_unique(state$records[[state$id_name]]))
  } else {
    # Groups observed at this level via non-missing ids.
    keys <- vec_unique(state$records[level_dims])

    # Zero-person groups belong only to the level where their full dimension
    # combination was observed: a zero group at a coarser level would be
    # indistinguishable from "no data" at that level and must not be invented.
    if (length(zero_level_dims) == length(level_dims) &&
      all(zero_level_dims %in% level_dims) &&
      vec_size(state$zero_groups) > 0L) {
      zero_keys <- vec_unique(state$zero_groups[level_dims])
      keys <- vec_unique(vec_rbind(keys, zero_keys))
    }

    if (vec_size(keys) == 0L) {
      counts <- integer()
    } else {
      # Distinct (level keys, id) pairs, then count ids per key. NA keys are
      # treated as their own group by vctrs. Every pair maps to exactly one
      # key, so the match result contains no missing values.
      pairs <- vec_unique(state$records[c(level_dims, state$id_name)])
      pair_keys <- pairs[level_dims]
      key_locs <- vec_match(pair_keys, keys)
      counts <- as.integer(
        tabulate(key_locs, nbins = vec_size(keys))
      )
    }
  }

  out <- as.list(keys)
  # Omitted dimensions are present in the output, filled with typed NA and
  # marked via the grouping bitmask. They keep their original column type.
  ptype_list <- as.list(state$dim_ptype)
  for (j in which(omitted)) {
    out[[all_dimensions[[j]]]] <- vec_init(ptype_list[[j]], vec_size(keys))
  }
  out <- out[all_dimensions]
  out[[count_name]] <- counts

  # Bit j is set when dimension j is omitted at this level. Integer supports
  # bit positions up to 30, i.e. at most 31 dimensions.
  bitmask <- if (n_dims == 0L) {
    0L
  } else {
    as.integer(sum(2^(which(omitted) - 1L)))
  }
  out[[grouping_name]] <- rep.int(bitmask, vec_size(keys))

  new_tibble(out, nrow = vec_size(keys))
}

#' Render a distinct-count report with detail, subtotal and total rows
#'
#' @description
#' Turns a merged state (see [distinct_state()] and
#' [merge_distinct_state()]) into a report tibble. Every requested grouping
#' level is deduplicated at that level's own dimensions, so a user spanning
#' several detail groups is counted once in each subtotal.
#'
#' By default the full "cube" is produced: the detail level (all dimensions),
#' every subtotal level, and the grand-total level. Custom levels can be
#' supplied via `levels`.
#'
#' Rows where a dimension is omitted by a subtotal carry `NA` in that column
#' regardless of whether `NA` is a real dimension value; such rows are marked
#' in the `.grouping` column. `.grouping` is an integer bitmask in which bit
#' `j` (counting from 0, following the order of the dimension columns) is set
#' when the `j`-th dimension is omitted. The grand total has all bits set,
#' detail rows have value `0`. Real `NA` dimension values remain their own
#' groups at the levels where the dimension is present.
#'
#' @param state A state produced by [distinct_state()] or
#'   [merge_distinct_state()].
#' @param levels An optional list of character vectors naming the dimensions
#'   kept at each level. `character(0)` is the grand-total level. The default
#'   (`NULL`) produces the full cube over all dimensions.
#' @param count_name Name of the output count column. Defaults to `"n"`.
#' @param .name_repair Name repair applied to the output tibble, see
#'   [tibble()].
#'
#' @returns A [tibble]. It contains the dimension columns (preserving their
#'   original types), the count column and the `.grouping` marker column.
#'   Zero-person groups and the grand total of an empty input are reported
#'   with a count of `0`.
#' @export
#' @examples
#' events <- tibble(
#'   region = c("a", "a", "b", "b", NA, "a"),
#'   channel = c("x", "x", "y", "y", "x", "x"),
#'   user_id = c(1L, 2L, 2L, 3L, NA, 1L)
#' )
#' st <- distinct_state(events, c(region, channel), user_id)
#'
#' # Full cube: detail + one-dimensional subtotals + grand total
#' collect_distinct_report(st)
#'
#' # Only the grand total and a region subtotal
#' collect_distinct_report(st, levels = list("region", character(0)))
collect_distinct_report <- function(state, levels = NULL, count_name = "n",
                                    .name_repair = c(
                                      "check_unique", "unique", "universal",
                                      "minimal"
                                    )) {
  call <- rlang::caller_env()

  if (!is_distinct_state(state)) {
    distinct_abort(
      c(
        "`state` must be a distinct state.",
        x = paste0("It is ", type_label(state), ".")
      ),
      call = call
    )
  }

  .name_repair <- match.arg(.name_repair)
  dimensions <- state$dimensions

  if (is.null(levels)) {
    levels <- cube_levels(dimensions)
  } else {
    check_levels(levels, dimensions, call = call)
  }

  if (!is_string(count_name) || nchar(count_name) == 0L) {
    distinct_abort(
      "`count_name` must be a single non-empty string.",
      call = call
    )
  }

  if (is.na(state$id_name)) {
    # Schema-less empty state: the report still carries a grand-total row
    # with a zero count.
    empty_names <- resolve_output_names(character(), count_name, .name_repair)
    return(
      new_tibble(
        stats::setNames(
          list(0L, 0L),
          c(empty_names$count, empty_names$grouping)
        ),
        nrow = 1L
      )
    )
  }

  if (length(dimensions) > 31L) {
    distinct_abort(
      "At most 31 dimension columns are supported (integer `.grouping` bitmask).",
      call = call
    )
  }

  reserved <- c(count_name, ".grouping")
  if (.name_repair == "check_unique" && any(reserved %in% dimensions)) {
    distinct_abort(
      c(
        "Column names must be unique.",
        x = paste0(
          "The count or marker column name collides with a dimension column: ",
          paste0(
            encodeString(intersect(reserved, dimensions), quote = "\""),
            collapse = ", "
          ),
          "."
        ),
        i = "Use `count_name` or `.name_repair` to resolve."
      ),
      call = call
    )
  }

  output_names <- resolve_output_names(
    dimensions,
    count_name,
    .name_repair
  )

  if (length(levels) == 0L) {
    out <- new_tibble(
      c(
        map(state$dim_ptype, function(col) vec_rep(col, 0L)),
        stats::setNames(
          list(integer(), integer()),
          c(output_names$count, output_names$grouping)
        )
      ),
      nrow = 0L
    )
    return(out)
  }
  pieces <- map(levels, function(level_dims) {
    render_level(
      state = state,
      level_dims = level_dims,
      count_name = output_names$count,
      grouping_name = output_names$grouping,
      all_dimensions = dimensions,
      zero_level_dims = dimensions
    )
  })

  # Names are already unique by construction; "check_unique" makes vec_rbind()
  # validate that without applying its own repair heuristics to names that
  # happen to contain "..." (e.g. a repaired ".grouping...1").
  out <- do.call(vec_rbind, c(pieces, list(.name_repair = "check_unique")))

  # Canonical order independent of shard merge order: group level first,
  # then dimension values within the level. Reference columns by position so
  # that name repair (a count column taking a dimension's name) cannot break
  # the lookup.
  grouping_pos <- match(output_names$grouping, names(out))
  dim_pos <- match(dimensions, names(out))
  dim_pos <- dim_pos[!is.na(dim_pos)]
  order_cols <- c(grouping_pos, dim_pos)
  out <- vec_slice(out, vec_order(out[order_cols]))

  out
}

#' @export
print.tbl_df_distinct_state <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.tbl_df_distinct_state <- function(x, ...) {
  dims <- paste0(encodeString(x$dimensions, quote = "\""), collapse = ", ")
  id_label <- if (is.na(x$id_name)) "id" else x$id_name
  c(
    paste0(
      "<distinct_state> ",
      vec_size(x$records), " deduplicated (",
      if (nzchar(dims)) dims else "no dimensions",
      ", ", id_label, ") record(s)"
    ),
    paste0(vec_size(x$zero_groups), " zero-person group(s)")
  )
}
