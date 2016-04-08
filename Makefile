CSS_OUT = ./target/app.css
CSS_FILES = ./node_modules/bulma/css/bulma.min.css

ELM_FILES = $(shell find . -name '*.elm')
JS_OUT = ./target/app.js

ELM_MAKE_ARGS = --yes src/Main.elm --output

all: $(JS_OUT) $(CSS_OUT)

$(JS_OUT): $(ELM_FILES)
	elm-make $(ELM_MAKE_ARGS) $(JS_OUT)

$(CSS_OUT): $(CSS_FILES)
	cat $(CSS_FILES) > $(CSS_OUT)

run: all
	./node_modules/.bin/elm-live -- $(ELM_MAKE_ARGS) $(JS_OUT)

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f $(JS_OUT)
	rm -f $(CSS_OUT)
	rm -rf elm-stuff/build-artifacts

.PHONY: all clean clean-deps run
