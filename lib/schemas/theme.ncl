let { Theme, .. } = import "../enums/theme.ncl" in
{
  ThemeSchema = {
    colorScheme
      | {
        slug | Theme,
        name | String | optional,
        author | String | optional,
        colors
          | {
            base00 | String,
            base01 | String,
            base02 | String,
            base03 | String,
            base04 | String,
            base05 | String,
            base06 | String,
            base07 | String,
            base08 | String,
            base09 | String,
            base0A | String,
            base0B | String,
            base0C | String,
            base0D | String,
            base0E | String,
            base0F | String
          }
      }
  }
}
