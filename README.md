[![Hackage](https://img.shields.io/hackage/v/quickselect.svg)](http://hackage.haskell.org/package/quickselect) [![Build Status](https://travis-ci.org/oisdk/quickselect.svg?branch=master)](https://travis-ci.org/oisdk/quickselect)

# quickselect

Haskell implementation of introselect on vectors.

This package provides three selection algorithms on both boxed and unboxed vectors.

The first is quickselect: this is an O(n) selection algorithm, with very fast performance in practice, but an unfortunate O(n^2) worst-case time.

The second is median-of-medians: this has an O(n) worst-case time, but usually performs worse than quickselect in practice.

The final is introselect: this begins as quickselect, but switches to median-of-medians if it realizes it's in one of the pathalogical cases which causes O(n^2) time. It has O(n) worst-case time, and performance in practice very close to quickselect.

There are also machine-generate optimal selection and median-finding functions for inputs of size 3, 4, and 5.
