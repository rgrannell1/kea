#' Usage - Installing Arrow
#'
#' A guide to installating Arrow.
#'
#' R libraries are fairly trivial to install from the CRAN repositories.
#'
#' \code{install.packages("arrow")}
#'
#'To check that everything is working you can run the following:
#'
#' \code{require(arrow)}
#'
#' \code{xDo(print('hi!'), 1)}
#'
#' Arrow is available under the GPL-3 license, a permissive open-source license. If you
#' want a more cutting-edge version of Arrow you an install Arrow directly from github.
#'
#' \code{install.packages('devtools')}
#'
#' \code{require(devtools)}
#'
#' \code{install_github('rgrannell1', 'arrow')}
#'
#' The github repository for arrow is available at \link{https://github.com/rgrannell1/arrow};
#' feel free to contribute with feedback, bug reports or feature requests.
#'
#' Note: if you are running Arrow from a terminal and aren't seeing coloured error messages
#' (and care), you might need to modify the TERM environmental variable.
#'
#' In R, run
#'
#' \code{Sys.getenv()["TERM"]}
#'
#' . If the output is 'xterm' but you're sure your terminal supports colour, do the following.
#' Run
#' sudo nano ~/.bashrc
#'
#' and add this line to the bottom (Ctrl + O to save).
#'
#'  \code{export TERM=term-color}
#'
#' Run
#'
#' \code{ ~/.bashenv}
#'
#' And if your TERM variable was misconfigured colour should now be displayed in your terminal.


#' @name help_arrow_install

NULL
