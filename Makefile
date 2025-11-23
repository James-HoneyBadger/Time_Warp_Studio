# Convenience Makefile wrappers

.PHONY: win2000 win2000-test win2000-release clean-win2000

win2000:
	./scripts/build_win2000.sh

win2000-test: win2000
	./scripts/test_win2000_build.sh

win2000-release: win2000
	./scripts/generate_win2000_manifest.sh

clean-win2000:
	$(MAKE) -C platforms/win2000 clean || true
	rm -rf dist/win2000
