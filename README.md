# Markdown to HTML

Markdown to HTML is a parser and transpiler that converts Markdown syntax into valid HTML. It is implemented in Haskell using parser combinators and Algebraic Data Types (ADTs) to provide a modular and extensible architecture. A simple web interface is also included, allowing live preview of Markdown-to-HTML conversion.

## Features

- **Markdown Parsing**

  - Headings (`#` through `######`, plus underline styles using `=` and `-`)
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

- **Haskell & Scotty** – parses Markdown, converts it into HTML, and serves it via a lightweight web API
- **TypeScript, RxJS, Vite** – reactive streams for live editing and real-time preview
- **Stack & NPM** – build tools and package managers for backend and frontend

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

## Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/ryanhiizy/MarkdownToHTML.git
   cd MarkdownToHTML
   ```

2. Start the Haskell backend:

   ```bash
   cd Haskell
   stack run
   ```

3. Start the JavaScript frontend:

   ```bash
   cd JS
   npm install
   npm run dev
   ```

   You can then type Markdown in the left-hand side of the webpage and see the converted HTML on the right-hand side.

4. (Optional) Run tests:
   ```bash
   stack test
   ```
   This parses example Markdown files from `examples/input` and saves the HTML output to `examples/output`.

## Authors

Developed by [@ryanhiizy](https://github.com/ryanhiizy). \
Scaffold provided by [@adriankristanto](https://github.com/adriankristanto).
