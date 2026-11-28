- A method collection can be built from a published report, one impact
  category at a time. An assistant creates an empty method collection of its
  own with the MCP tool `create_method_collection` (over HTTP,
  `POST /api/v1/method-collections/{newName}/create`), then imports each
  impact category with `edit_method_categories`, whose new op `import` reads
  one category and its factors from the method CSV text of the `csv` field. A
  text that cannot be read as written is refused, each defect named with the
  lines it is on, and nothing is recorded; a flow unit the engine's unit table does not
  know is such a defect, where scoring would have counted it as the base unit. The collection is kept in its journal alone:
  the list of method collections gives it the path `"journal"` and the format
  `"Journal"`. After a restart it is not loaded; the new MCP tool
  `load_method_collection` loads it again. pyvolca creates the collection with
  `create_method_collection()` and imports a category with
  `import_method_category()` (wire revision 130).
