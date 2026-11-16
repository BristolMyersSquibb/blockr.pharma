# Deprecating a block

Saved boards name their blocks' constructors, and blockr.core looks a
saved constructor up in the package namespace when it restores a board.
Board code and deploy scripts call the same constructors as
`pkg::new_x_block()`. So a block constructor is never removed or
unexported in one step. It goes through three stages.

## 1. Deprecated

The constructor stays exported and keeps working. It returns the
replacement block, so a board that restores it saves the replacement from
then on.

- **Board users get a toast.** When a saved board brings the block back in
  the app, show a warning toast that says the board has a block from an
  older version and should be saved.
- **R users get a warning.** When code calls the constructor, raise a
  warning of class `deprecatedWarning` that names the replacement.
- **A restore does not warn, and code does not toast.** blockr.core passes
  `ctor` to the constructor on restore, which tells the two apart.
- **Docs:** the help page title ends in "(deprecated)" and the first
  paragraph names the replacement. `@keywords internal` keeps it out of the
  pkgdown index; `@export` stays.
- **Tests:** a warning when code calls it, no warning on restore, a toast
  only on restore, and the restored block serialises like the replacement.

```r
new_x_block <- function(..., ctor = NULL) {
  restore <- !is.null(ctor)
  blk <- new_y_block(...)  # the replacement
  if (!restore) {
    warning(warningCondition(
      "new_x_block() is deprecated. Use new_y_block().",
      class = c("deprecatedWarning", "lifecycle_warning_deprecated"),
      call = NULL
    ))
  }
  if (restore && !is.null(blockr.core::get_session())) {
    blockr.core::notify(
      "This board has a block from an older version. Save the board to keep it.",
      type = "warning", duration = NULL
    )
  }
  blk
}
```

`new_ae_heatmap_block()` in `R/ae-heatmap-block.R` is the worked example.

## 2. Defunct

Only when nothing uses it any more: no call in the workspace, the deploy
and gallery repos or example boards (grep for the name), and the saved
boards in use have been opened and saved since the toast went out. The
constructor stays exported and stops with an error of class
`defunctError` that names the replacement. A restore shows the same text
as an error toast.

## 3. Removed

After a defunct release that nobody reported, the constructor and its
tests go.

## Not allowed

Unexporting a constructor that boards or code still name. That was done
to `new_ae_heatmap_block()` in 0.0.0.9154: `blockr.pharma::new_ae_heatmap_block()`
then failed with "not an exported object", which broke CI downstream with
no warning beforehand. 0.0.0.9164 exported it again as deprecated.
