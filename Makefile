public_dir = $(abspath ./_site)

.PHONY: all clean

all: $(public_dir)/index.html $(public_dir)/app.js $(public_dir)/app.css $(public_dir)/favicon.ico

clean:
	rm -rf $(public_dir)
	mkdir -p $(public_dir)

$(public_dir)/index.html: src/index.html
	minify $< > $@

$(public_dir)/app.js: src/Umbra.elm
	elm make --optimize $< --output /tmp/app.js
	minify /tmp/app.js > $@

$(public_dir)/app.css: src/umbra.css
	minify $< > $@

$(public_dir)/favicon.ico: src/favicon.ico
	cp $< $@
