build:
	stack build --fast

test:
	stack test --fast

test-watch:
	stack test --fast --file-watch

clean:
	stack clean
	rm -rf .stack-work

.PHONY: build test test-watch clean
