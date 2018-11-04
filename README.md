ox-s9y - Serendipity blog export for Org
========================================

[![Build Status](https://travis-ci.org/mmitch/ox-s9y.svg?branch=master)](https://travis-ci.org/mmitch/ox-s9y)
[![GPL 3+](https://img.shields.io/badge/license-GPL%203%2B-blue.svg)](http://www.gnu.org/licenses/gpl-3.0-standalone.html)


This tries to be an [Org](http://orgmode) export backend to generate
HTML code suitable to be imported as an article into the
[Serendipity](https://docs.s9y.org) (s9y) blog system.  Code blocks
will be rendered via the [GeSHI](http://qbnz.com/highlighter/) plugin
in s9y.

todo
----

- fix handling of multiple paragraphs inside of list-items (see ox-html)
- map Org languages to GeSHi languages
- export directly to X clipboard (export to buffer, select all, copy to
  clipboard, close buffer)
- support offsets and jumps in ordered lists

done
----

- export code blocks in GeSHi format
- add export commands to the export menu
- restrict export to the contents of the `<body>` tag

resources
---------

Project homepage, git repository and bug tracker are available at
https://github.com/mmitch/ox-s9y

license
-------

Copyright (C) 2017,2018  Christian Garbs <mitch@cgarbs.de>  
Licensed under GNU GPL v3 or later.

ox-s9y is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ox-s9y is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ox-s9y.  If not, see <http://www.gnu.org/licenses/>.
