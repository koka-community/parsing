cd ../alex
# git checkout effects
stack build
stack run alex -- -k ../parsing/parsers/alex/json-alex.x -o ../parsing/parsers/alex/json-lex.kk
stack run alex -- -k ../parsing/parsers/alex/json-alex-eff.x -o ../parsing/parsers/alex/json-lex-eff.kk