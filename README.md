[![Build Status](https://travis-ci.org/oisdk/quickselect.svg?branch=master)](https://travis-ci.org/oisdk/quickselect)

# quickselect

Haskell implementation of introselect on vectors.

The implementation uses introselect, which begins as quickselect, but switches to the median-of-medians when certain cases are encountered. This guarantees O(n) worst-case time.

Optimal algorithms for finding the median of 3, 4, and 5 elements are also provided.
