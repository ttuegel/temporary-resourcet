[![Build Status](https://travis-ci.org/ttuegel/temporary-resourcet.svg?branch=master)](https://travis-ci.org/ttuegel/temporary-resourcet)

# temporary-resourcet

The functions for creating temporary files and directories in the base library
are quite limited. The [Unixutils](http://hackage.haskell.org/package/Unixutils)
package contains some good ones, but they aren't portable to Windows.

This library repackages the Cabal implementations of its own temporary file
and folder functions so that you can use them without linking against Cabal
or depending on it being installed.

This library provides the same functionality as the
[temporary](http://hackage.haskell.org/package/temporary) package, but uses
[resourcet](http://hackage.haskell.org/package/resourcet) to provide automatic
deletion without nesting `withTempFile`.
