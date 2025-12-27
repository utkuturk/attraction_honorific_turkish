# =========================
# Quarto Paper Makefile
# =========================
# Usage:
#   make             # build html+pdf
#   make publish     # copy html->../index.html, commit+push pdf+index.html on main
#   make overleaf_branch  # update 'overleaf' branch with tex, figures, and latex files
#   make clean
#   OPEN=true make open

SHELL := /bin/bash
ROOT      := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BASE_DIR  := $(ROOT)paper/
QUARTO    := quarto
CP        := cp -f
OVERLEAF_REMOTE := overleaf
OVERLEAF_BRANCH := master
OVERLEAF_WORKTREE := $(ROOT).overleaf_worktree
OVERLEAF_SYNC_LOCAL := overleaf-sync
RSYNC := rsync -av
OVERLEAF_INCLUDE := \
  --exclude='.git' \
  --include='paper.tex' \
  --include='paper_files/***' \
  --include='figs/***' \
  --include='images/***' \
  --include='*.sty' \
  --include='*.cls' \
  --include='*.bst' \
  --include='*.bib' \
  --include='*.tex' \
  --exclude='*'
QMD       := $(BASE_DIR)paper.qmd
HTML      := $(BASE_DIR)paper.html
PDF       := $(BASE_DIR)paper.pdf
TEX       := $(BASE_DIR)paper.tex
PUBLISH_DIR := $(ROOT)
HTML_DST    := $(PUBLISH_DIR)/index.html
PDF_DST    := $(PUBLISH_DIR)/paper.pdf
REPO_ROOT := $(shell git -C "$(ROOT)" rev-parse --show-toplevel 2>/dev/null || echo "$(ROOT)")
.DEFAULT_GOAL := all

# --------- Build targets ---------
all: html pdf

.PHONY: html pdf
html: $(HTML)
pdf:  $(PDF)

$(HTML): $(QMD)
	$(QUARTO) render "$(QMD)" --to html

$(PDF): $(QMD)
	$(QUARTO) render "$(QMD)" --to pdf -M keep-tex=true

.PHONY: publish
publish: html pdf
	@echo "Copying $(HTML) -> $(HTML_DST)"
	$(CP) "$(HTML)" "$(HTML_DST)"
	$(CP) "$(PDF)" "$(PDF_DST)"
	@echo "Committing & pushing changes on main..."
	@if git -C "$(REPO_ROOT)" diff --quiet -- "$(HTML_DST)" "$(PDF_DST)"; then \
	  echo "No changes to commit."; \
	else \
	  git -C "$(REPO_ROOT)" add -- "$(HTML_DST)" "$(PDF_DST)"; \
	  git -C "$(REPO_ROOT)" commit -m "Publish HTML/PDF (auto)"; \
	  git -C "$(REPO_ROOT)" push; \
	fi
	@echo "Publish complete."

# --------- Overleaf branch via worktree ---------


# will always push to overleaf github.
# first add overleaf git as a remote
# git remote add overleaf <overleaf_git_link>
# then git fetch overleaf master:refs/remotes/overleaf/master
# or main
# Always pushes to Overleaf remote, branch master

.PHONY: overleaf_branch
overleaf_branch: pdf
	@echo "Publishing to Overleaf (artifacts only)..."

	@if ! git -C "$(REPO_ROOT)" rev-parse --verify --quiet "refs/remotes/$(OVERLEAF_REMOTE)/$(OVERLEAF_BRANCH)"; then \
	  echo "Missing ref $(OVERLEAF_REMOTE)/$(OVERLEAF_BRANCH). Run:"; \
	  echo "  git fetch $(OVERLEAF_REMOTE) $(OVERLEAF_BRANCH):refs/remotes/$(OVERLEAF_REMOTE)/$(OVERLEAF_BRANCH)"; \
	  exit 1; \
	fi

	@git -C "$(REPO_ROOT)" worktree remove -f "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@rm -rf "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@git -C "$(REPO_ROOT)" worktree prune 2>/dev/null || true

	@git -C "$(REPO_ROOT)" worktree add -B "$(OVERLEAF_SYNC_LOCAL)" "$(OVERLEAF_WORKTREE)" "$(OVERLEAF_REMOTE)/$(OVERLEAF_BRANCH)"

	@echo "Cleaning worktree (keeping .git)..."
	@cd "$(OVERLEAF_WORKTREE)" && \
	  find . -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +

	@echo "Syncing paper artifacts to Overleaf worktree..."
	@mkdir -p "$(OVERLEAF_WORKTREE)/paper_files/figure-pdf"
	@$(RSYNC) $(OVERLEAF_INCLUDE) \
	  "$(BASE_DIR)"/ "$(OVERLEAF_WORKTREE)/"

	@cd "$(OVERLEAF_WORKTREE)" && \
	  git add -A && \
	  (git commit -m "Update Overleaf artifacts" || true) && \
	  git push -u "$(OVERLEAF_REMOTE)" "$(OVERLEAF_SYNC_LOCAL):$(OVERLEAF_BRANCH)"

	@echo "Cleaning up Overleaf worktree..."
	@git -C "$(REPO_ROOT)" worktree remove -f "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@rm -rf "$(OVERLEAF_WORKTREE)" 2>/dev/null || true
	@git -C "$(REPO_ROOT)" worktree prune 2>/dev/null || true

	@echo "Overleaf updated."

.PHONY: open
open: pdf
	@if [ "$(OPEN)" = "true" ]; then \
	  echo "Opening $(PDF)"; \
	  open "$(PDF)"; \
	else \
	  echo "Set OPEN=true to auto-open the PDF."; \
	fi

.PHONY: clean
clean:
	@echo "Cleaning aux/latex scratch files..."
	@rm -f $(BASE_DIR)*.{lof,lot,bbl,gz,toc,log,glo,aux,ist} \
	       $(BASE_DIR)*-knitr.Rnw $(BASE_DIR)*-wordcount.tex \
	       $(BASE_DIR).aux.copy $(BASE_DIR).auxlock \
	       $(BASE_DIR).aux.for  $(BASE_DIR).for.tmp $(BASE_DIR).for
	@echo "Clean complete."
