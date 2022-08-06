# tt-weave

Experimental-ish tool for Knuth’s [WEB] language similar to [weave], which
outputs TeX code ready to be rendered into fancy HTML.

[WEB]: https://www.ctan.org/pkg/web
[weave]: https://www.ctan.org/pkg/weave

Current workflow:

1. Obtain `xetex.web` or `weave.web`, place in directory containing this file
2. `./weave.sh`
3. `./tectonic.sh` (requires unpublished branch and @pkgw's laptop!!)
4. `./yarn.sh serve` or `./yarn.sh build`
