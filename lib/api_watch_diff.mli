type ('item, 'diff) change =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type diff =
  | Value of
      string
      * ( Types.value_description,
          Types.value_description * Types.value_description )
        change
  | Any

val diff_interface :
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  diff list
