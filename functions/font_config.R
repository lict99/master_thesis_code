# Configuring Chinese fonts
loadNamespace("sysfonts")

sysfonts::font_add(
  family = "heiti",
  regular = "data/fonts/NotoSansSC-Regular.ttf",
  bold = "data/fonts/NotoSansSC-Bold.ttf"
)

font_zh <- "heiti"
