- A database whose inputs are supplied by another loaded database now exports
  as EcoSpold 1. Each such input is written under its supplier's activity
  name, place and unit, so a reader holding that database beside the file
  links it there. The export refuses an input whose supplier states no place,
  makes another product under the same activity name and place, carries the
  name of a dataset of the file, or makes its product in a unit the input
  does not convert to. It warns when a supplier's activity is named apart from
  its product, since VoLCA links such an input by its product and does not
  find it there.
