+++
title = 'CSS Grid Layout Fundamentals'
date = 2024-01-20T14:00:00-07:00
draft = false
tags = ['css', 'grid', 'layout']
+++

CSS Grid is a powerful layout system that allows you to create complex layouts with ease. Here's a quick example:

```css
.container {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: 20px;
  max-width: 1200px;
  margin: 0 auto;
}
```

This creates a three-column grid with equal width columns and 20px gaps between them.

<!--more-->

## Basic Grid Properties

The fundamental properties you need to know:

```css
.grid-container {
  display: grid;
  grid-template-columns: 200px 1fr 200px; /* Fixed-Flexible-Fixed */
  grid-template-rows: auto 1fr auto; /* Header-Content-Footer */
  grid-template-areas: 
    "header header header"
    "sidebar main aside"
    "footer footer footer";
  gap: 1rem;
  min-height: 100vh;
}
```

## Grid Item Placement

You can explicitly place items in the grid:

```css
.header {
  grid-area: header;
  background-color: #f0f0f0;
}

.sidebar {
  grid-area: sidebar;
  background-color: #e0e0e0;
}

.main-content {
  grid-area: main;
  background-color: #ffffff;
}
```

## Responsive Grids

Create responsive layouts without media queries:

```css
.responsive-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}
```

This automatically adjusts the number of columns based on available space while maintaining a minimum width of 250px per column.

## Grid vs Flexbox

- **Use Grid for**: Two-dimensional layouts, complex positioning
- **Use Flexbox for**: One-dimensional layouts, component alignment

Grid gives you precise control over both rows and columns simultaneously, making it perfect for page layouts and complex component structures.

## Testing Text Wrapping

Here's a very long URL to test wrapping: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout/Basic_Concepts_of_Grid_Layout#Grid_tracks_and_grid_lines_and_grid_areas_and_all_the_terminology

And here's some very long text without spaces to test word breaking: supercalifragilisticexpialidociousthisisaverylongwordthatshouldbreakatthecontaineredgewhenthereisnospaceavailable.

```css
/* This is a very long CSS rule with lots of properties to test horizontal scrolling in code blocks */
.very-long-selector-name-that-goes-on-and-on {
  background-color: rgba(255, 255, 255, 0.95);
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1), 0 8px 24px rgba(0, 0, 0, 0.05);
  border: 1px solid rgba(200, 200, 200, 0.8);
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  transform: translateX(-50%) translateY(-50%) scale(1.05) rotate(0.5deg);
}
```

This should demonstrate both text wrapping and code block horizontal scrolling working correctly within the paper container boundaries.