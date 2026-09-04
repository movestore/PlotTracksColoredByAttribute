## Preview of the categorical palettes offered by the app.
## One horizontal bar per palette, split into one slot per colour.
## Sizes match what ShinyModule.R asks for: glasbey(32) and the full
## brewer.pal.info$maxcolors for each ColorBrewer palette.

library(pals)
library(RColorBrewer)

palette_colors <- function(name, n = NULL) {
  if (tolower(name) == "glasbey") {
    pals::glasbey(n = if (is.null(n)) 32 else n)
  } else {
    RColorBrewer::brewer.pal(
      n    = if (is.null(n)) RColorBrewer::brewer.pal.info[name, "maxcolors"] else n,
      name = name
    )
  }
}

plot_palettes <- function(names = c("Glasbey", "Set2", "Set3", "Dark2", "Paired", "Accent"),
                          border = "white",
                          label_slots = TRUE) {
  cols <- lapply(names, palette_colors)
  n_pal <- length(cols)

  op <- par(mar = c(0.5, 5.5, 0.5, 0.5), xpd = NA)
  on.exit(par(op), add = TRUE)

  plot(NULL,
       xlim = c(0, 1), ylim = c(0, n_pal),
       type = "n", axes = FALSE, xlab = "", ylab = "")

  for (i in seq_len(n_pal)) {
    cc <- cols[[i]]
    n  <- length(cc)
    x  <- seq(from = 0, to = 1, length.out = n + 1)
    # bars run top to bottom in the order given
    ytop <- n_pal - i + 1
    ybot <- ytop - 0.75

    rect(xleft   = x[-(n + 1)],
         ybottom = ybot,
         xright  = x[-1],
         ytop    = ytop,
         col     = cc,
         border  = border,
         lwd     = 0.5)

    text(x = -0.015, y = (ybot + ytop) / 2,
         labels = sprintf("%s (%d)", names[i], n),
         adj = c(1, 0.5), cex = 0.9)

    if (label_slots) {
      text(x = (x[-(n + 1)] + x[-1]) / 2,
           y = ybot - 0.12,
           labels = seq_len(n),
           cex = 0.5, col = "grey35", adj = c(0.5, 1))
    }
  }
  invisible(setNames(cols, names))
}

if (sys.nframe() == 0L) {
  png("palettes.png", width = 1400, height = 700, res = 150)
  plot_palettes()
  dev.off()
}
