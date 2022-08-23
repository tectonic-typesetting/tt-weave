% Copyright 2021-2022 the Tectonic Project
% Licensed under the MIT License
%
% Tectonic Default (HTML) User Experience document class
%
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{tdux}[2021/10/19 Tectonic Default User Experience]
%
\ExecuteOptions{}
\ProcessOptions
%
\LoadClass{article}
%
\RequirePackage{fontspec}
\setmainfont{texgyrepagella}[%
  Ligatures = TeX,
  Extension = .otf,
  UprightFont = *-regular,
  BoldFont = *-bold,
  ItalicFont = *-italic,
  BoldItalicFont = *-bolditalic,
]
\setmonofont{SourceCodePro}[%
  Ligatures = TeX,
  Extension = .otf,
  UprightFont = *-Regular,
  BoldFont = *-Bold,
  ItalicFont = *-RegularIt,
  BoldItalicFont = *-BoldIt,
]
%
\RequirePackage{amsmath}
\RequirePackage{unicode-math}
\setmathfont{texgyrepagella-math.otf}
%
\RequirePackage{hyperref}
%
% \tduxAddTemplate{TMPL-FILENAME}
%   Register an HTML template to be included in the Tera templating session. Templates can
%   only be added before the first output file is emitted.
\newcommand\tduxAddTemplate[1]{%
  \special{tdux:addTemplate #1}
}
%
% \tduxSetupOutput{TMPL-FILENAME}{OUTPUT-PATH}
%   Set up the next output, by specifying the path of the file to create and the
%   name of the template to use. The template will be reread for each output
%   file that is created.
\newcommand\tduxSetupOutput[2]{%
  \special{tdux:setTemplate #1}
  \special{tdux:setOutputPath #2}
}
%
% \tduxEmit
%   Embed a \special in the output indicating that the current output file
%   should be emitted.
\newcommand\tduxEmit{%
  \special{tdux:emit}
}
%
% \tduxSetTemplateVariable{NAME}{VALUE}
%   Set the name of a variable in the output templating system. The value will
%   persist until it is changed. The variable name may not contain whitespace.
%   Whether the variable contents are HTML-escaped is decided in the template
%   (through the Tera `safe` directive).
\newcommand\tduxSetTemplateVariable[2]{%
  \special{tdux:setTemplateVariable #1 #2}
}
%
% \tduxProvideFile{SOURCE-PATH}{DEST-PATH}
%   Copy a file into the output tree. SOURCE-PATH is the "TeX path" of the
%   source file. DEST-PATH is the path of the file to be crated in the output
%   tree. SOURCE-PATH may not contain whitespace.
\newcommand\tduxProvideFile[2]{%
  \special{tdux:provideFile #1 #2}
}
%
\AtBeginDocument{%
  \pagestyle{empty}

  % Register main-body font family. Needs to happen before we exit the
  % "initialization" stage of the spx2html processing.
  \special{tdux:startDefineFontFamily}
  family-name:tduxMain
  \textbf{bold}
  \textit{italic \textbf{bold-italic}}
  \par
  \special{tdux:endDefineFontFamily}

  % Mono font family.
  \special{tdux:startDefineFontFamily}
  \texttt{
    family-name:tduxMono
    \textbf{bold}
    \textit{italic \textbf{bold-italic}}
  }
  \par
  \special{tdux:endDefineFontFamily}

  \special{tdux:startFontFamilyTagAssociations}
  \texttt{
    code
  }
  \par
  \special{tdux:endFontFamilyTagAssociations}
}
%
\AtEndDocument{%
  \special{tdux:contentFinished}
  \tduxSetupOutput{tdux-fonts.css}{tdux-fonts.css}
  \tduxEmit
}
%
\tduxAddTemplate{tdux-standalone.html}
\tduxSetTemplateVariable{tduxDocumentTitle}{Set tduxDocumentTitle}
