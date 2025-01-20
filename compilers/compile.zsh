cd ..

latexmk -shell-escape -file-line-error -pdf template
latexmk -c template.tex