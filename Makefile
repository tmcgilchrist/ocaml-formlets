all:
	cd lib && make bcl
	cd pp && make
	cd examples && make

clean:
	@for d in lib pp examples; do (cd $$d && $(MAKE) clean); done

