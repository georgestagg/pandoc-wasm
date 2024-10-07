OUTPUT = src/_generated

all: submodules build dist/pandoc-wasm.wasm.gz

PHONY: build
build: www/pandoc-data.data.gz www/pandoc-wasm.wasm.gz node_modules
	npm run build

PHONY: serve
serve: build
	npm run serve

dist/pandoc-data.data.gz www/pandoc-data.data.gz: pandoc/data
	mkdir -p www dist
	node pack.mjs
	gzip -9f dist/pandoc-data.data
	cp -f dist/pandoc-data.* www

dist/pandoc-wasm.wasm.gz www/pandoc-wasm.wasm.gz: $(OUTPUT)/pandoc-wasm.wasm
	gzip -9 -c $< > $@

node_modules: package.json package-lock.json
	npm ci

$(OUTPUT)/pandoc-wasm.wasm: src/Pandoc/Main.hs
	mkdir -p $(OUTPUT)
	docker run --rm -v ${PWD}:/workspace -w /workspace ghcr.io/georgestagg/pandoc-wasm:main \
	  ahc-cabal install --allow-older=base --builddir /root/_ahc/build \
	  --installdir $(OUTPUT) --install-method copy --overwrite-policy always
	docker run --rm -v ${PWD}:/workspace -w /workspace ghcr.io/georgestagg/pandoc-wasm:main \
	   ahc-dist --no-main --browser --input-exe $(OUTPUT)/pandoc-wasm \
	   --output-directory $(OUTPUT) --yolo
# Grow wasm memory heap as part of Haskell runtime initalisation
	mv -f $(OUTPUT)/rts.mjs $(OUTPUT)/rts.mjs.orig
	awk '/WebAssembly.instantiate/ {$$0=$$0" i.exports.memory.grow(1024);"} 1' \
	  $(OUTPUT)/rts.mjs.orig > $(OUTPUT)/rts.mjs
# BUG: Avoid infinite loop in Asterius error handling
	mv -f $(OUTPUT)/rts.scheduler.mjs $(OUTPUT)/rts.scheduler.mjs.orig
	awk '/if \(tso_info.ffiRetErr\)/ {$$0=$$0" throw tso_info.ffiRetErr;"} 1' \
	  $(OUTPUT)/rts.scheduler.mjs.orig > $(OUTPUT)/rts.scheduler.mjs

PHONY: docker-container
docker-container:
#	docker build -t pandoc-wasm .
	docker pull ghcr.io/georgestagg/pandoc-wasm:main

PHONY: lint
lint:
	npx eslint example/src/index.js $(shell find src -name '*.ts')

PHONY: clean
clean:
	rm -rf www dist $(OUTPUT)

.PHONY: submodules
submodules:
	@if git submodule status | egrep -q '^[-]' ; then \
		git submodule update --init; \
	fi
