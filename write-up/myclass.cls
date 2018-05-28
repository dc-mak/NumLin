% Boilerplate
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{myclass}[05/02/2018 Dhruv Makwana Part III Project Class]
\LoadClass[12pt,a4paper,twoside,openright,notitlepage]{report}

% Standard
\RequirePackage[utf8x]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[dvipsnames]{xcolor}

% Hyperlinks
% ---
\RequirePackage[
  pdftitle={Applications of Linear Types},
  pdfauthor={Dhruv C.~Makwana},
  pdfkeywords={numerical, linear, algebra, types, OCaml}
]{hyperref}

% Remove ugly boxes around hyperlinks
\hypersetup{%
    colorlinks=true,
    linkcolor={black},
    citecolor={green!50!black},
    urlcolor={blue!80!black}
}

% Refer to Chapters and Sections by their Title
\RequirePackage{nameref}

% Referrable enumerated lists
% \RequirePackage{enumitem}

% Formatting
% ---

% Allow flexible pages
\raggedbottom{}
\sloppy%
% penalise orphan lines/paragraphs
\clubpenalty1000%
\widowpenalty1000%

% Paragraph formatting
\parindent0pt%
\parskip6pt%

% Full width footnote rule
\renewcommand{\footnoterule}{}%
%   \kern -3pt
%   \hrule width \textwidth height 1pt
%   \kern 2pt
% }

% Nice tables
\RequirePackage{booktabs}

% Nice captions
\RequirePackage[
  format=hang,
  font=small,
  labelfont=sf,
  labelsep=endash
]{caption}

% Fancy headers
\RequirePackage{fancyhdr}

% Margins
\RequirePackage[
  headheight=15pt, % 15pt for fancyhdr
  top=1.1in,
  bottom=1.25in,
  left=1.25in,
  right=1.25in
]{geometry}

% More readable; preserves hyphenation
% \RequirePackage[document]{ragged2e}

% Sans-serif section headers
\RequirePackage{sectsty}
  \allsectionsfont{\normalfont\sffamily\bfseries}% sans Sub*sections font
  \setcounter{secnumdepth}{3}% Number subsubsections
  \renewcommand{\thesubsubsection}{\Roman{subsubsection}}% with captial roman

% Control (format) table of contents, figures
\RequirePackage{tocloft}
  \setlength{\cftparskip}{6pt}% Spacing in ToC
  \renewcommand{\cfttoctitlefont}{\normalfont\Huge\sffamily\bfseries}% sans ToC Title
  \renewcommand{\cftchapfont}{\normalfont\sffamily\bfseries}% sans ToC chapter font
  \renewcommand{\cftsecfont}{\normalfont\sffamily}% sans ToC section font
  \renewcommand{\cftsubsecfont}{\normalfont\sffamily}% sans ToC subsection font
  \renewcommand{\cftloftitlefont}{\normalfont\Huge\sffamily\bfseries}% sans LoFig Title
  \renewcommand{\cftlottitlefont}{\normalfont\Huge\sffamily\bfseries}% sans LoTab Title

% Customise Chapter Titles
\RequirePackage{titlesec}
  % Fancy (and smaller) Chapter Heading Style
  \titleformat{\chapter}[hang]{\sffamily\Huge\bfseries}{%
  \thechapter\hspace{20pt}\textcolor{lightgray}{|}\hspace{20pt}}{0pt}{\sffamily\Huge\bfseries}

% Special features
% ---

% Maths symbols (explaining triangle for defs, square for proofs in Eval)
\RequirePackage{amsmath}
\RequirePackage{amssymb}

% Including stand-alone LaTeX documents as is
% \RequirePackage{docmute}

% Source code highlighting
\RequirePackage[outputdir=build]{minted}
  % Magic incantation to stop minted from putting red boxes around shit
  \RequirePackage{etoolbox}
  \makeatletter
  \AtBeginEnvironment{minted}{\dontdofcolorbox}
  \def\dontdofcolorbox{\renewcommand\fcolorbox[4][]{##4}}
  \makeatother

% Graphs
\RequirePackage{pgfplots}
\RequirePackage{rotating}

% % Evaluation table
% \RequirePackage{adjustbox}
\RequirePackage{array}
% 
%   % Rotated table headers
%   % http://tex.stackexchange.com/questions/32683/rotated-column-titles-in-tabular#32690
%   \newcolumntype{R}[2]{%
%       >{\adjustbox{angle=#1,lap=\width- (#2)}\bgroup}%
%       l%
%       <{\egroup}%
%   }%
%   \newcommand*\rot{\multicolumn{1}{R{90}{0.8em}}}
% 
%   % Fancy coloured boxes
%   \newcommand{\Y}{\tikz\fill [ForestGreen] (-0.3ex,0) rectangle (1.3ex,0.7em); }
%   \newcommand{\M}{\tikz\fill [YellowOrange]  (-0.3ex,0) rectangle (1.3ex,0.7em); }
%   \newcommand{\N}{\tikz\fill [Red]   (-0.3ex,0) rectangle (1.3ex,0.7em); }

% Per Chapter Mini Table of Contents (load after titlesec)
\RequirePackage{minitoc}

% Conveniences
% ---

% Guidance from template
\newif\ifguidance%
\guidancetrue%
\newenvironment{guidance}
    {\sffamily\color{red}}
    {\normalfont\color{black}}

% At the start of each chapter
\renewcommand{\mtctitle}{}
\newcommand*{\prechapter}[1]{%
\minitoc%
\bigskip%
\begin{center}%
  \begin{minipage}[h][][c]{0.8\linewidth}%
	#1
  \end{minipage}%
\end{center}

\newpage%
}

% End
\endinput
