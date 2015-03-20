# clj-predict

The *clj-predict* library consolidates functions for satellite ephemeris
propagation and other space maths. To use *clj-predict* with your
[Leiningen](http://leiningen.org/) project, include the following in the
`:dependencies` portion of your application's `project.clj` file:

    [clj-predict "0.0.1-SNAPSHOT"]

Documentation: http://david-rc-dayton.github.io/clj-predict/  
Repository: https://clojars.org/clj-predict

## Credits

*clj-predict* uses the
[predict4java](https://github.com/badgersoftdotcom/predict4java) library for
propagating satellite ephemeris from [Two-line Element Sets (TLEs)](http://en.wikipedia.org/wiki/Two-line_element_set). Current satellite TLEs can
be found on the [CelesTrak](https://celestrak.com/) website.

## License

The MIT License (MIT)

Copyright (c) 2015 David RC Dayton

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
