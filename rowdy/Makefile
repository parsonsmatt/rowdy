ghcid:
	ghcid -c "stack ghci rowdy --ghci-options -fno-code" --restart package.yaml

ghcid-test:
	ghcid -c "stack ghci rowdy:lib rowdy:test:specs --ghci-options -fobject-code" --restart package.yaml --test "main"

.PHONY: ghcid ghcid-test
