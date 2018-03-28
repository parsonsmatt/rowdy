ghcid:
	ghcid -c "stack ghci" --restart package.yaml

ghcid-test:
	ghcid -c "stack ghci rowdy:lib rowdy:test:specs" --restart package.yaml --test "main"

.PHONY: ghcid ghcid-test
