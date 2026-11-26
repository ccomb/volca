- An EcoSpold 1 dataset of several products is divided by its allocation
  keys. When every product takes one fraction of every line it shares, that
  fraction is the product's share, and a reading that keeps the block whole
  keeps it to price the products. When the fractions differ from line to
  line, each line is divided by its own fraction. A line no key gives a
  product counts 0 for it, and the load says so. A key that names no line,
  a line twice, or no product leaves the block whole, and the load says why.
- A dataset that names no reference product but lists allocated products
  takes the first of them that makes something as its reference, and the
  load says which.
- An input that buys a coproduct of a block kept whole, by the block's
  dataset number, is linked to that block.
