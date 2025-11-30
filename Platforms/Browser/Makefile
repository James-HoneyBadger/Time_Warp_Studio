PORT ?= 8000

.PHONY: serve open

serve:
	@echo "Serving Time Warp Web on http://127.0.0.1:$(PORT)"
	python3 -m http.server $(PORT)

open:
	@echo "Opening http://127.0.0.1:$(PORT)"
	xdg-open http://127.0.0.1:$(PORT) 2>/dev/null || \
	  open http://127.0.0.1:$(PORT) 2>/dev/null || true
