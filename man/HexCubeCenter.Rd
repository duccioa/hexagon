% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-HexCubeCenter.R
\name{HexCubeCenter}
\alias{HexCubeCenter}
\alias{hex_neighbour}
\alias{hex_diagonal_neighbour}
\title{Class object for hexagon center}
\usage{
hex_neighbour(object, direction)

hex_diagonal_neighbour(object, direction)
}
\description{
This is a class that contains the Cube coordinates of the center
of an Hexagon.
It inherits from \code{CubeCoords} but it is specific for \code{Hexagon}.
}
\section{Functions}{
\itemize{
\item \code{hex_neighbour}: Find the neighbour center along a direction.

\item \code{hex_diagonal_neighbour}: Find the neighbour center along a diagonal direction.
}}

\references{
\link{https://www.redblobgames.com/grids/hexagons}
}
\section{Super classes}{
\code{\link[hexagon:Coords]{hexagon::Coords}} -> \code{\link[hexagon:CubeCoords]{hexagon::CubeCoords}} -> \code{HexCubeCenter}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{HexCubeCenter$new()}}
\item \href{#method-clone}{\code{HexCubeCenter$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="Coords" data-id="dim">}\href{../../hexagon/html/Coords.html#method-dim}{\code{hexagon::Coords$dim()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="Coords" data-id="print">}\href{../../hexagon/html/Coords.html#method-print}{\code{hexagon::Coords$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="get">}\href{../../hexagon/html/CubeCoords.html#method-get}{\code{hexagon::CubeCoords$get()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="set">}\href{../../hexagon/html/CubeCoords.html#method-set}{\code{hexagon::CubeCoords$set()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="set_q">}\href{../../hexagon/html/CubeCoords.html#method-set_q}{\code{hexagon::CubeCoords$set_q()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="set_r">}\href{../../hexagon/html/CubeCoords.html#method-set_r}{\code{hexagon::CubeCoords$set_r()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="shift_along_q">}\href{../../hexagon/html/CubeCoords.html#method-shift_along_q}{\code{hexagon::CubeCoords$shift_along_q()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="shift_along_r">}\href{../../hexagon/html/CubeCoords.html#method-shift_along_r}{\code{hexagon::CubeCoords$shift_along_r()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hexagon" data-topic="CubeCoords" data-id="shift_along_s">}\href{../../hexagon/html/CubeCoords.html#method-shift_along_s}{\code{hexagon::CubeCoords$shift_along_s()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialise a new instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HexCubeCenter$new(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{A numeric vector of length 3 or 2, whose sum is 0.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An object of class \code{HexCubeCenter}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HexCubeCenter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
