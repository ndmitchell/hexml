# Hexml [![Hackage version](https://img.shields.io/hackage/v/hexml.svg?label=Hackage)](https://hackage.haskell.org/package/hexml) [![Stackage version](https://www.stackage.org/package/hexml/badge/nightly?label=Stackage)](https://www.stackage.org/package/hexml) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/hexml/ci.yml?branch=master)](https://github.com/ndmitchell/hexml/actions)

An XML DOM-style parser, that only parses a subset of XML, but is designed to be fast. In particular:

* Entities, e.g. `&amp;`, are not expanded.
* Not all the validity conditions are checked.
* No support for `<!DOCTYPE` related features.

The name "hexml" is a combination of "Hex" (a curse) and "XML". The "X" should not be capitalised because the parser is more curse and less XML.

Hexml may be suitable if you want to quickly parse XML, from known sources, and a full XML parser has been shown to be a bottleneck. As an alternative to hexml, which supports things like entities but is still pretty fast, see [Pugixml](http://pugixml.org/) (with a [Haskell binding](https://hackage.haskell.org/package/pugixml) - but be aware the Haskell binding [can segfault](https://github.com/philopon/pugixml-hs/issues/5)).

Hexml is tested with [AFL](http://lcamtuf.coredump.cx/afl/).

If you want lenses for Hexml, see [hexml-lens](http://hackage.haskell.org/package/hexml-lens).

The optimisation work around Hexml spawned [Xeno](http://hackage.haskell.org/package/xeno), a Haskell-only alternative to Hexml. There is a [talk](https://ndmitchell.com/#hexml_12_oct_2017) covering the performance tricks of Hexml and Xeno.
