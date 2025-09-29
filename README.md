# Markdown to HTML

Markdown to HTML is a parser and transpiler that converts Markdown syntax into valid HTML. It is implemented in Haskell using parser combinators and Algebraic Data Types (ADTs) to provide a modular and extensible architecture. A simple web interface is also included, allowing live preview of Markdown-to-HTML conversion.

## Features

- **Markdown Parsing**

  - Headings (\`#\` through \`######\`, plus underline styles using \`=\` and \`-\`)
  - Inline formatting: bold, italic, strikethrough
  - Links
  - Images with alt text and captions
  - Inline code and fenced code blocks (with optional language tag)
  - Blockquotes
  - Ordered lists (with nested lists via indentation)
  - Tables
  - Footnotes and references
  - Nested text modifiers

- **Web Interface**

  - Live Markdown editor with real-time HTML preview
  - Syntax highlighting in code blocks (via Highlight.js)
  - Editable page title
  - Save-to-file functionality

## Technologies Used

- **Haskell \& Scotty** – backend parsing, HTML rendering, lightweight API
- **TypeScript, RxJS, Vite** – reactive frontend with live editing and fast builds
- **Stack \& NPM** – build tools and package management

## Example

Markdown:

```markdown
# Example

This is **bold**, _italic_, and a [link](https://example.com).

1. First item
2. Second item
```

Generated HTML:

```html
<h1>Example</h1>
<p></p>
<p>This is <strong>bold</strong>, <em>italic</em>, and a <a href="https://example.com">link</a>.</p>
<p></p>
<ol>
  <li>First item</li>
  <li>Second item</li>
</ol>
```

## Installation and Setup

### Running Tests

```bash
stack test
```

This parses example Markdown files from `examples/input` and saves the HTML output to `examples/output`.

### Running the Interactive Webpage

1. Start the Haskell backend:

   ```bash
   cd haskell
   stack run
   ```

2. In another terminal, run the JavaScript frontend:

   ```bash
   cd javascript
   npm install
   npm run dev
   ```

You can then type Markdown in the left-hand side of the webpage and see the converted HTML on the right-hand side.

## Author

Developed by [@ryanhiizy](https://github.com/ryanhiizy). \
Skeleton code provided by [@adriankristanto](https://github.com/adriankristanto).
