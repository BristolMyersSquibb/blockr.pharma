# The metadata of a block with no registry entry: the old swimmer and timeline,
# which saved boards still restore. Without it blockr.core warns on every
# build and shows a default name and icon.
retired_block_metadata <- function(name, description) {
  list(
    id = NA_character_,
    name = name,
    description = description,
    details = NA_character_,
    link = NA_character_,
    guidance = NA_character_,
    keywords = character(),
    category = "table",
    icon = "bar-chart-steps",
    arguments = blockr.core::new_arg_specs(),
    package = "blockr.pharma",
    examples = list()
  )
}
