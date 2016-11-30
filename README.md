# Hexml [![Hackage version](https://img.shields.io/hackage/v/hexml.svg?label=Hackage)](https://hackage.haskell.org/package/hexml) [![Stackage version](https://www.stackage.org/package/hexml/badge/lts?label=Stackage)](https://www.stackage.org/package/hexml) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/hexml.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/hexml) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/hexml.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/hexml)

An XML DOM-style parser, that only parses a subset of XML, but is designed to be fast. In particular:

* Entities, e.g. `&amp;`, are not expanded.
* Not all the validity conditions are checked.
* No support for `<!DOCTYPE` related features.

The name "hexml" is a combination of "Hex" (a curse) and "XML". The "X" should not be capitalised because the parser is more curse and less XML.

Hexml may be suitable if you want to quickly parse XML, from known sources, and a full XML parser has been shown to be a bottleneck. As an alternative to hexml, which supports things like entities but is still pretty fast, see Pugixml.
