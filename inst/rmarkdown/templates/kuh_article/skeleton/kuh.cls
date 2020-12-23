% Kuopio University Hospital house document style
% Juuso Tamminen (juuso.tamminen@kuh.fi)
% Version 1.0
% Date 2020-12-06

\LoadClass[12pt]{article}

\RequirePackage{babel}
\RequirePackage{t1enc}
\RequirePackage{graphicx}
%\RequirePackage{showframe}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{multirow}
\RequirePackage{titling}
\RequirePackage{booktabs}
\RequirePackage{xcolor}
\RequirePackage{helvet}
\RequirePackage{marginnote,calc,kantlipsum}
\RequirePackage[tabular,lining]{montserrat}
\RequirePackage{fancyhdr}

\newlength{\myoddoffset}
\setlength{\myoddoffset}{\marginparwidth + \marginparsep}

\renewcommand{\familydefault}{\sfdefault}

\def\@unit{Palveluyksikkö}

\newcommand{\unit}[1]{
  \def\@unit{#1}
}

\def\@kuhdoctype{Muistio}

\newcommand{\kuhdoctype}[1]{
  \def\@kuhdoctype{#1}
}

\def\@kuhemail{etunimi.sukunimi@kuh.fi}

\newcommand{\kuhemail}[1]{
  \def\@kuhemail{#1}
}

\def\@kuhtelephone{Vaihde (017) 173 311}

\newcommand{\kuhtelephone}[1]{
  \def\@kuhtelephone{#1}
}

\def\@kuhdate{\today}
\newcommand{\kuhdate}[1]{
  \def\@kuhdate{#1}
}

\def\@maketitle{%
	\newpage
	\newgeometry{right=6cm}
	\fontfamily{Montserrat-TOsF}\selectfont
	\pagestyle{frontpage}
	\fontsize{40}{45}\selectfont
	\raggedright{\textcolor{kuh-white}{\MakeUppercase{\vskip 2em\@title}}}
	\newpage
}


\definecolor{kuh-blue}{cmyk}{0.85,0.21,0,0}
\definecolor{kuh-white}{cmyk}{0,0,0,0}

\RequirePackage[%
    a4paper,
%   includeheadfoot,
    head=1cm, % \baselineskip,  % distance from bottom of header to block of text aka \headsep e.g. \baselineskip
    foot=1cm,  			% distance from top of footer to block of text aka \footskip
    headheight=2.8cm,    % height for the header block (no equivalent for footer)
%   heightrounded,       % ensure an integer number of lines
    marginparwidth=2cm,  % right marginal note width
    marginparsep=2mm,    % distance from text block to marginal note box
    height=\textheight,  % height of the text block
    width=\textwidth,    % width of the text block
    top=4.0cm,           % distance of the text block from the top of the page
    bottom=4.0cm,
    left=3cm,
    right=4cm,
%    showframe,           % show the main blocks
%    verbose,             % show the values of the parameters in the log file
]{geometry}



\pagestyle{fancy} % This must be here, because defaults are set and renewcommand for section marks will work.

\renewcommand{\sectionmark}[1]{\markright{#1}}
\renewcommand{\subsectionmark}[1]{}

\setlength{\parskip}{1em plus 4mm minus 3mm}
\setlength{\parindent}{0cm} % Default is 15pt.

\fancypagestyle{frontpage}{
	\fancyhf{} % clear all fields
	\pagecolor{kuh-blue}
	\renewcommand{\headrulewidth}{0pt}
	\lfoot{
		\begin{minipage}{\textwidth}
			\raggedright{\large\textcolor{white}{\theauthor\\ \@kuhdate}}
		\end{minipage}
	}
	\rfoot{
	\begin{minipage}{0.2\textwidth}
		\includegraphics[height=2cm]{KYS_val_fi_print_pdf}
	\end{minipage}
	}
}

\fancypagestyle{mainmatter}{%
   \fancyhf{} % clear all fields
   \pagecolor{white}
   \fontfamily{phv}\selectfont
   \fancyheadoffset{\myoddoffset}
   \fancyfootoffset{\myoddoffset}
   \renewcommand{\headrulewidth}{0pt}
   \lhead{
	\begin{minipage}{0.2\textwidth}
		\includegraphics[height=2cm]{KYS_sin_fi_print_pdf}
	\end{minipage}
	\begin{minipage}{0.6\textwidth}
		\sffamily\small{\textbf{Pohjois-Savon sairaanhoitopiiri}\\
		\@unit \\
		\theauthor}
	\end{minipage}
   }
   \rhead{\sffamily\small{\textbf{\@kuhdoctype} \\
   \@kuhdate}}
   \lfoot{\thepage}
   \cfoot{}
   \rfoot{\sffamily\footnotesize{Puhelin: \@kuhtelephone\\
   \@kuhemail\\
   Y-tunnus 0171495-3\\
   \setlength{\parskip}{1em}
   \large{\textcolor{kuh-blue}{www.psshp.fi}}}}
}%

