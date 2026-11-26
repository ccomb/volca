- The MCP server now serves skills to assistants that read them (MCP
  revision 2026-07-28, skills extension): instructions an assistant loads
  when a question calls for them. Two are built in: `carbon-footprint`, the
  climate change score of a product with its fossil, biogenic and land use
  parts, and `pef-hotspots`, the most relevant impact categories, processes,
  and emissions and resources of a product under the PEF 80 % rule. A
  configuration adds its own with `[[skills]]`, or replaces a built-in one
  by giving its name.
