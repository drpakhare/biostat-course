#!/bin/bash
# =============================================================
# Build script for Biostatistics for Clinicians
# Renders slides individually, then builds the book
# =============================================================

set -e

echo "=== Step 1: Rendering slide decks ==="
mkdir -p docs/slides

for slide in slides/*-slides.qmd; do
  echo "  Rendering $slide ..."
  quarto render "$slide"
done

echo "  Copying rendered slides to docs/slides/ ..."
cp slides/*-slides.html docs/slides/

echo "=== Step 2: Building the book ==="
quarto render

echo "  Ensuring slides are in docs/slides/ ..."
cp slides/*-slides.html docs/slides/

echo "=== Build complete ==="
echo "Book output:  docs/index.html"
echo "Slides:       docs/slides/"
