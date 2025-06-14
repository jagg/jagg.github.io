# Thumos - Paper-like Coding Blog Theme

A minimalist Hugo theme designed to look like reading code on paper. Perfect for technical blogs, programming notes, and developer documentation.

## Features

- **Paper-like design**: Clean white background with subtle shadows, mimicking a physical document
- **Coding-focused**: Optimized typography and styling for code snippets and technical content
- **Narrow fonts**: Uses Inter and JetBrains Mono for efficient screen space usage
- **Green link boxes**: All links are enclosed in distinctive green boxes for easy identification
- **Responsive**: Adapts seamlessly from mobile to desktop while maintaining the paper aesthetic
- **Minimal JavaScript**: Pure CSS styling with focus on readability and performance

## Visual Design

- Clean, paper-white content area with subtle border and shadow
- Monospace headers using JetBrains Mono for that "code editor" feel
- Text-justified paragraphs for a book-like reading experience
- Green-boxed links that stand out without being distracting
- Minimal color palette focusing on readability

## Setup

### 1. Configure your site

Update `hugo.toml` with your information:

```toml
baseURL = 'https://yourdomain.com/'
languageCode = 'en-US'
title = 'Your Blog Name'

[params]
  description = "A brief description of your coding blog."
  github = "https://github.com/yourusername"
  linkedin = "https://linkedin.com/in/yourprofile"
  profileImage = "/images/profile.jpg"
```

### 2. Add your profile image

Place a square profile image in `static/images/profile.jpg`. Recommended size: 400x400 pixels.

### 3. Create content

#### Homepage
Edit `content/_index.md` to customize your homepage intro text.

#### Blog posts
Create new posts in `content/posts/`:

```bash
hugo new posts/my-coding-post.md
```

Use this frontmatter structure:

```yaml
+++
title = 'Your Post Title'
date = 2024-01-15T09:00:00-07:00
draft = false
tags = ['python', 'javascript', 'tutorial']
+++

Your content here...
```

## Code Styling

The theme is optimized for code-heavy content:

- **Inline code**: `code` gets a light gray background with border
- **Code blocks**: Syntax highlighting with JetBrains Mono font
- **Blockquotes**: Green left border for important notes
- **Links**: All links get green boxes for easy identification

## Typography

- **Body text**: Inter font for excellent readability
- **Headers**: JetBrains Mono for a technical, code-like appearance
- **Code**: JetBrains Mono for consistent monospace rendering
- **Metadata**: Uppercase, spaced text for dates and tags

## Responsive Behavior

The theme adapts to different screen sizes while maintaining the paper metaphor:

- **Desktop**: Full paper-like experience with generous margins
- **Tablet**: Reduced margins, maintained paper appearance
- **Mobile**: Stacked navigation, smaller profile image, optimized touch targets

## Customization

Main styles are in `assets/css/main.css`. Key areas to customize:

- **Colors**: Change the green link boxes or background colors
- **Typography**: Modify font choices or sizing
- **Layout**: Adjust container width or spacing
- **Paper effect**: Modify shadows and borders for different paper styles

## File Structure

```
thumos/
├── assets/css/main.css          # Main stylesheet
├── layouts/
│   ├── baseof.html              # Base template
│   ├── home.html                # Homepage layout
│   ├── section.html             # Blog listing
│   └── posts/single.html        # Individual post layout
├── content/
│   ├── _index.md                # Homepage content
│   └── posts/                   # Blog posts
└── static/images/               # Profile image location
```

## Building and Running

```bash
# Development server
hugo server -D

# Build for production
hugo --minify
```

## Performance

This theme prioritizes performance:

- Minimal CSS with no unused styles
- No JavaScript dependencies
- Optimized fonts loaded from Google Fonts
- Print-friendly styles included

## Browser Support

Works in all modern browsers. Tested on:
- Chrome/Chromium
- Firefox  
- Safari
- Edge

## License

MIT License - feel free to use and modify for your projects.