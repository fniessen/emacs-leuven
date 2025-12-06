# ---------------------------------------------------------
# Moteur LaTeX
# ---------------------------------------------------------

# Si tu veux utiliser pdflatex :
$pdflatex = 'pdflatex -interaction=nonstopmode -halt-on-error %O %S';

# Si tu préfères lualatex, remplace simplement la ligne ci-dessus :
# $lualatex = 'lualatex -interaction=nonstopmode -halt-on-error %O %S';
# $pdf_mode = 1;
# $pdf_previewer = 'start evince';

# ---------------------------------------------------------
# Indexation : utilisation de texindy
# ---------------------------------------------------------

$makeindex = 'texindy -L french -C utf8 %O -o %D %S';

# ---------------------------------------------------------
# Nettoyage : fichiers générés à supprimer
# ---------------------------------------------------------

@generated_exts = (
  'aux', 'auxlock', 'bbl', 'blg', 'fdb_latexmk', 'fls',
  'log', 'nav', 'out', 'snm', 'toc', 'vrb', 'xdv',
  'synctex.gz', 'ilg', 'ind',
);

# ---------------------------------------------------------
# Options générales
# ---------------------------------------------------------

$silent = 0;              # Mets 1 si tu veux une compilation silencieuse
$bibtex_use = 1;          # Active bibtex automatiquement
$clean_full_ext = '';     # Laisse latexmk gérer les extensions
$max_repeat = 5;          # Nombre max. de passes