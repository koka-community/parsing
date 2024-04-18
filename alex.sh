cd ../alex
stack build
stack run alex -- -k ../parsing/parsers/alex/json-alex.x -o ../parsing/parsers/alex/json-parse.kk