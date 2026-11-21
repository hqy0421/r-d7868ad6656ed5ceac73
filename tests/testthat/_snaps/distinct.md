# distinct_state() deduplicates and keeps zero-person groups

    <distinct_state> 3 deduplicated ("region", "channel", user_id) record(s)
    1 zero-person group(s)

# name repair keeps dimension names and yields count/marker names

    Column names must be unique.
    x The count or marker column name collides with a dimension column: "region".
    i Use `count_name` or `.name_repair` to resolve.

# errors get the tibble_error_distinct class

    `state` must be a distinct state.
    x It is an integer.

---

    Can't build a distinct state from this object.
    x `x` must be a data frame, not an integer.

---

    `id` must select exactly one column.
    x 2 columns were selected.

---

    Column not found.
    x Column "no_such_column" doesn't exist.

---

    All arguments must be distinct states.
    x Argument 1 is a character.

---

    All states must use the same dimension columns.
    x State 2 uses "channel", expected "region".

---

    Each element of `levels` must be a unique subset of the dimension columns.
    x Element 1 is not.

