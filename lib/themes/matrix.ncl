let { ThemeSchema, .. } = import "../schemas/theme.ncl" in
{
  colorScheme = {
    slug = 'matrix,
    colors = {
      base00 = "001100",
      base01 = "003300",
      base02 = "005500",
      base03 = "006600",
      base04 = "007700",
      base05 = "009900",
      base06 = "00BB00",
      base07 = "00DD00",
      base08 = "003300",
      base09 = "005500",
      base0A = "007700",
      base0B = "008800",
      base0C = "009900",
      base0D = "00BB00",
      base0E = "00DD00",
      base0F = "00FF00"
    }
  }
} | ThemeSchema
