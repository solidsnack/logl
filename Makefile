
servers = snap

$(servers): %: ./servers/%.hs tmp
	ghc --make -outputdir ./tmp $< -o $@

tmp:
	mkdir -p tmp

clean:
	rm -rf ./tmp $(servers)

