# tt-weave

This repository contains an experimental-ish tool that converts programs in
Knuth’s [WEB] language into high-quality HTML. Traditionally, the program that
turns WEB into documentation is called [weave]. The TeX code emitted by
`tt-weave` can be processed into HTML by [Tectonic] and then bundled into an
interactive web application using [yarn] and [Parcel]. The only fully supported use case is
creating an HTML version of *XeTeX: The Program*.


[WEB]: https://www.ctan.org/pkg/web
[weave]: https://www.ctan.org/pkg/weave
[Tectonic]: https://tectonic-typesetting.github.io/
[yarn]: https://yarnpkg.com/
[Parcel]: https://parceljs.org/


## Quickstart

Prerequisites:

- [Rust]
- [Tectonic] >= 0.11.0
- [yarn]

[Rust]: https://rust-lang.org/

Steps:

1. Set up an input WEB file. There are two supported options:
   - Download [weave.web] and place it in the directory containing this file
   - Download `xetex.web` as described below.
2. `./yarn.sh install`
3. `./weave.sh $inputfilename`
4. `./tectonic.sh`
5. `./yarn.sh serve` or `./yarn.sh build`

[weave.web]: https://ctan.org/tex-archive/systems/knuth/dist/web/weave.web

The reference version of `xetex.web` used to test this tool is the one produced
by the [tectonic-staging] build process. This can be obtained from [the GitHub
releases associated with that repo][ts-releases], as an “asset” with a name of
the form `tectonic-book-tlYYYY.RR.web`.

[tectonic-staging]: https://github.com/tectonic-typesetting/tectonic-staging/
[ts-releases]: https://github.com/tectonic-typesetting/tectonic-staging/releases/
[xetex.web]: https://github.com/tectonic-typesetting/tectonic-staging/releases/


## Background

[WEB] is a language for literate programming invented by Donald E. Knuth. A WEB
program is distributed as a single text file that interleaves both source and
documentation. The source is essentially Pascal code, augmented with a
sophisticated preprocessor. The documentation is TeX code. A tool called
`tangle` converts the WEB source to a preprocessed Pascal file, while a tool
called `weave` converts it to a TeX file expressing the documented source code.

The TeX code emitted by the traditional `weave` program is plain TeX that can
be compiled into a large PDF book. The purpose of this project is to make it
possible to compile WEB code into high-quality HTML as well.

A further complication is that the traditonal `weave` representation of the WEB
source code is unusual by modern standards and, in this writer’s opinion, quite
hard to read. The `tt-weave` program therefore parses the WEB code and
pretty-prints it into a somewhat more familiar, C/Rust-like syntax. Because WEB
is heavily macro-based, however, it is not possible to do this in full
generality. While other inputs might be processed successfully, `tt-weave` only
officially supports `weave.web` and `xetex.web` as inputs.


## Repository Overview

Converting WEB code to an interactive web app is a multi-step process:

1. The `tt-weave` Rust program converts a original WEB file into a large, very
   specialized TeX file. This program is rooted in the top-level directory of
   this repository, with source in `src/`. The `weave.sh` script emits the TeX
   code into the file `template/src/index.tex`.
2. The TeX source then needs to be compiled into HTML using Tectonic. The script
   `./tectonic.sh` wraps this process. The subdirectory `template/` contains a
   Tectonic document with specialized macros and definitions in `template/src/`
   that are compiled into HTML outputs, which land in the directory
   `template/build/default/`. The wrapper script copies these outputs into
   `app/src/ttw/`.
3. Finally, the HTML code needs to be bundled with a web app implementation to
   create the final product. This is done using Yarn, wrapped with the `yarn.sh`
   script. The `app/` subdirectory of this repo contains the bulk of this
   app implementation, with a hierarchy of [Vue.js] components defined in the
   `app/src/` subdirectory.

[Vue.js]: https://vuejs.org/

This might seem complicated, but even though tt-weave’s HTML output is “just” a
document, the user interface surrounding that document quickly becomes complex
enough to benefit from tooling such as [Vue.js] and [TypeScript].

[TypeScript]: https://typescriptlang.org/


## Legalities

tt-weave is licensed under the MIT License.
