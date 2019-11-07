PACKAGE=${1:-aeson-injector}
ghcid -c "cabal new-repl --ghc-options='-Werror -Wall' $PACKAGE "
