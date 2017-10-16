#' Calculating and Visualizing a "Song Similarity" matrix
#'
#' This function will calculate the "song similarity" of a given song's lyrics
#' stored in a text file. A cell (i, j) is filled if word i in a song's lyrics
#' is the same as word j. This method was first suggested by Colin Morris
#' (see URLs)
#'
#' @param path Path to a txt file holding the song's lyrics
#' @param colorfulMode A boolean indicating whether to plot the matrix in color
#' (see @details), defaults to FALSE
#' @param singleColor What is the color to use for a full matrix cell (see @details)
#' @param interactiveMode A boolean indicating whether to plot the matrix in
#' interactive mode using the `heatmaply` package (see @details), defaults to FALSE
#' @param mainTitle plot's main title, defaults to an empty string
#' @param plotOptions Additional parameters for the base `plot` function
#' (non-interactive mode)
#' @param heatmaplyOptions Additional parameters for the `heatmaply` function
#' (non-interactive mode)
#' @param plotMatrix A boolean indicating whether to plot the matrix, defaults to TRUE
#'
#' @return a list containing:
#' \item{songMat}{a song similarity square matrix of dimensions no. of words x
#' no. of words, containing all 0 and 1}
#' \item{repetitiveness}{an experimental measure of repetitiveness which is the
#' mean of the upper triangular part of song matrix}
#'
#' @details
#' A cell (i, j) in a songsim square matrix is filled if word i in a song's lyrics
#' is the same as word j. This simple rule would create fascintaing visualizations
#' of songs lyrics as shown in the links below.
#'
#' If \code{colorfulMode} is set to TRUE, each word which appears more than once will be
#' colored with its own color (currently only available with \code{interactiveMode} set to FALSE).
#'
#' If \code{interactiveMode} is set to TRUE and the user has the \code{heatmaply} package
#' installed - the songsim matrix will be plotted using an interactive heatmap.
#'
#' The \code{singleColor} corresponds to the color a full cell is colored with.
#' If in \code{colorfulMode} it would be the color of words which appear only once.
#'
#' @note
#' This function was only tested on typical pop songs or poems, it was not tested
#' on large pieces of texts
#'
#' @references
#' A blog post describing the package with more examples:
#' \url{http://giorasimchoni.com/2017/08/08/2017-08-08-lambada-the-mocap-package/}
#'
#' A blog post by Colin Morris describing interesting "songsims":
#' \url{https://colinmorris.github.io/blog/weird-pop-songs}
#'
#' An interactive React (JS) demo by Colin Morris with more examples:
#' \url{https://colinmorris.github.io/SongSim/}
#'
#' @export
#' @examples
#' path <- system.file("extdata", "formation.txt", package = "songsim")
#' songsim(path)

songsim <- function(path = NULL, colorfulMode = FALSE,
                    singleColor = "black", interactiveMode = FALSE,
                    mainTitle = "", plotOptions = NULL,
                    heatmaplyOptions = NULL,
                    plotMatrix = TRUE) {
  songLyrics <- readLines(path)

  songWords <- strsplit(paste0(songLyrics, collapse = " "), " ")[[1]]

  songWords <- tolower(gsub("[[:punct:]]", "", songWords))

  songMat <- diag(1, length(songWords), length(songWords))

  grid <- as.data.frame(which(lower.tri(songMat, diag = FALSE), arr.ind = TRUE))

  repeatingWord <- function(i, j) {
    songMat[i, j] <<- ifelse(songWords[i] == songWords[j], 1, 0)
  }

  purrr::walk2(grid$row, grid$col, repeatingWord)

  songMat[upper.tri(songMat)] <- t(songMat)[upper.tri(songMat)]

  rownames(songMat) <- colnames(songMat) <- songWords

  if (plotMatrix) {
    if (interactiveMode) {
      if (!requireNamespace("heatmaply", quietly = TRUE)) {
        warning("interactiveMode = TRUE only possible if you have the heatmaply
                package installed, setting interactiveMode to FALSE")
        interactiveMode <- FALSE
      }
      if (colorfulMode) {
        warning("colorfulMode = TRUE only allowed with interactiveMode = TRUE,
                setting colorfulMode to FALSE")
        colorfulMode <- FALSE
      }
    }

    if (!is.color(singleColor)) {
      warning("singleColor could not be interpreted as a color, setting it to black")
      singleColor <- "black"
    }



    if (interactiveMode) {
      hm <- do.call(heatmaply::heatmaply, c(list(x = songMat, dendrogram = FALSE,
                                                 limits = c(0, 1),
                                                 showticklabels = FALSE,
                                                 colors = c("white", singleColor),
                                                 hide_colorbar = TRUE,
                                                 plot_method = "plotly",
                                                 main = mainTitle),
                                            heatmaplyOptions))
      print(hm)

    } else {
      songMatR <- t(apply(songMat, 2, rev))

      dimnames(songMatR) <- NULL

      songMatRLong <- reshape2::melt(songMatR)

      songMatRLong$color <- ifelse(songMatRLong$value == 1, singleColor, NA)

      if (colorfulMode) {

        dupWords <- unique(songWords[duplicated(songWords)])

        rbColors <- rainbow(length(dupWords))

        getRBColor <- function(Var1, Var2, value, color) {
          if (value == 1 && songWords[Var1] %in% dupWords) {
            rbColors[which(dupWords == songWords[Var1])]
          } else {
            color
          }
        }

        songMatRLong$color <- purrr::pmap_chr(songMatRLong, getRBColor)
      }
      .pardefault <- par(no.readonly = T)
      par(pty = "s")
      do.call(plot, c(list(x = songMatRLong$Var1, y = songMatRLong$Var2,
                           col = songMatRLong$color,
                           pch = 15, cex = 0.2 * 400/length(songWords),
                           xaxt = "n", yaxt = "n",
                           main = mainTitle, xlab = "",
                           ylab = ""),
                      plotOptions))
      par(.pardefault)
    }
  }

  invisible(list(songMat = songMat,
                 repetitiveness = mean(songMat[upper.tri(songMat)])))
}

is.color <- function(x) {
  tryCatch(is.matrix(col2rgb(x)),
             error = function(e) FALSE)
}
