ox-s9y - Serendipity blog export for Org
========================================

This tries to be an [Org](http://orgmode) export backend to generate
HTML code suitable to be imported as an article into the
[Serendipity](https://docs.s9y.org) (s9y) blog system.  Code blocks
will be rendered via the [GeSHI](http://qbnz.com/highlighter/) plugin
in s9y.

todo
----

- fix handling of paragraphs (see ox-html)
- map Org languages to GeSHi languages
- export directly to X clipboard (export to buffer, select all, copy to
  clipboard, close buffer)

done
----

- export code blocks in GeSHi format
- add export commands to the export menu
- restrict export to the contents of the `<body>` tag
