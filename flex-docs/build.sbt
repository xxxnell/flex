import microsites._

enablePlugins(MicrositesPlugin)

micrositeName := "Flex"
micrositeDescription := "Probabilistic deep learning library"
micrositeAuthor := "flex contributors"
micrositeFooterText := None
micrositeBaseUrl := "flex"
micrositeExtraMdFiles := Map(
  file("README.md") -> ExtraMdFileConfig(
    "index.md",
    "home",
    Map("title" -> "Home", "section" -> "home", "position" -> "0")
  ),
  file("CONTRIBUTING.md") -> ExtraMdFileConfig(
    "contributing.md",
    "home",
    Map("title" -> "Contributing", "section" -> "contributing", "position" -> "50")
  )
)
micrositeTwitterCreator := "@xxxnell"
micrositeGithubOwner := "xxxnell"
micrositeGithubRepo := "flex"
micrositeGitterChannel := false
micrositeHighlightTheme := "atom-one-light"
micrositePalette := Map(
  "brand-primary" -> "#FC4053",
  "brand-secondary" -> "#B92239",
  "brand-tertiary" -> "#8C192F",
  "gray-dark" -> "#464646",
  "gray" -> "#7E7E7E",
  "gray-light" -> "#E8E8E8",
  "gray-lighter" -> "#F6F6F6",
  "white-color" -> "#FFFFFF"
)
//micrositePushSiteWith := GitHub4s
//micrositeGithubToken := sys.env.get("GITHUB_ACCESS_TOKEN")
