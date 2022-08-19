# Instructions for building slides

See targets in the Makefile. You make build all the slide sets using
the command:

```
$ make all
```

Upon completion, slides are available in the `lecture_*/` folders,
including a png-file (first page of slide, containing a
table-of-content) that can be used as a background in Zoom. The build
process assumes that the tool `pdftoppm` is available.

# Playing with the F# code in the `src` folder

Assuming mono is installed (e.g., with `brew install mono`), all
example source files can be compiled with the command:

```
$ make test
```

# MIT-LICENSE

Copyright 2022 Martin Elsman, DIKU

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
