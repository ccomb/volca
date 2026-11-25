- Brightway could not read a database exported as EcoSpold 1: it stopped on
  the first dataset, which lacked the technology, the period of validity, the
  dataset information and the person who entered the data. Each dataset now
  carries them. Its period of validity is the year of its date, or of the
  export for a dataset stating no date, and its text says which.
- Inside an exported EcoSpold 1 file, Brightway linked none of the inputs to
  the datasets of the same file: an input said the product's name, and
  Brightway looks it up by the dataset's name, which is its activity's.
  EcoSpold 1 gives a dataset one name, so the product, and every input buying
  it, is now written under the name of the activity making it, and the export
  warns how many products were renamed so.
