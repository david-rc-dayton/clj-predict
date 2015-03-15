# clj-predict
The *clj-predict* library consolidates functions for satellite ephemeris
propagation and other space maths. To use *clj-predict* with
[Leiningen](http://leiningen.org/) in your current
[Clojure](http://clojure.org/) project, include the following in the
`:dependencies` portion of your application's `project.clj` file:

    [clj-predict "0.0.1-SNAPSHOT"]
    
*clj-predict* uses the
[predict4java](https://github.com/badgersoftdotcom/predict4java) library for
propagating satellite ephemeris from [Two-line Element Sets (TLEs)](http://en.wikipedia.org/wiki/Two-line_element_set). Current satellite TLEs can
be found on the [CelesTrak](https://celestrak.com/) website.