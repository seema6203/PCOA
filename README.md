# PCOA

**Principal Coordinates Analysis (PCoA)** of SSR genotype data for *Nymphaea*
(water lily) populations, plus the distance matrices and NeighborNet input behind
the accompanying network figure.

The target figure is [this one from *Frontiers in Plant Science*](https://www.frontiersin.org/files/Articles/773572/fpls-13-773572-HTML-r1/image_m/fpls-13-773572-g002.jpg).

## What the script does

`pcoa.R`:

1. reads a GenAlEx-formatted CSV with `poppr::read.genalex()`
2. computes **Nei's genetic distance** (`poppr::nei.dist()`)
3. runs the ordination with `ade4::dudi.pco()`, keeping 3 axes
4. plots PCo1 against PCo2 with `ggplot2`, colouring points by cluster and
   labelling each individual
5. writes the figure to PDF

Axis labels carry the variance explained (PCo1 18.74%, PCo2 7.98%) as hard-coded
text. If you rerun on different data, read the real values off `pco$eig` and
update them.

## Files

| File | Description |
| --- | --- |
| `pcoa.R` | The analysis and plotting script. |
| `pcoa.csv` | GenAlEx-formatted SSR genotypes (the main input). |
| `primula_snapclust.csv` | A second genotype set, used with `adegenet`'s snapclust. |
| `DNA_structure-2.xlsx` | STRUCTURE-related worksheet. |
| `pcoa_dist`, `a.txt` | Pairwise genetic-distance matrices between species. |
| `net`, `netwrk_dist` | NEXUS files (taxa: Goa, Maharashtra, Kerala, Assam) for building a NeighborNet in SplitsTree. |
| `pcoa.pdf`, `pcoa1.pdf`, `nei_name.pdf`, `network.pdf`, `Rplot.pdf` | Rendered figures. |

## Usage

```r
# edit working_dir at the top of the script first
source("pcoa.R")
```

## Requirements

```r
install.packages(c("poppr", "adegenet", "ade4", "ggplot2"))
```

The script also calls `ggFunctions::s.class()`. If that package is unavailable,
`adegenet::s.class()` produces the equivalent base-graphics plot.

## Note

The plotting section reaches into `ggplot_build()` internals to override point
colours, sizes and labels after the fact. It works, but it is tied to the layer
order of this particular plot — adding or removing a geom will shift the
`q$data[[n]]` indices.

## License

MIT — see [LICENSE](LICENSE).
