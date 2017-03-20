purescript-scores
=================

This module provides support for [VexTab](http://www.vexflow.com/vextab/) which is a language for creating and editing musical scores. Vextab itself is an open-source [javascript project](https://github.com/0xfe/vextab) and is still in a pre-alpha stage.  

VexTab renders a score by side-effect.  You must create a __canvas__ tag in your web page, assign it an __id__ and initialise the module with this id in the configuration.  You can then ask it to render some VexTab text - if it is valid then the score will appear in the canvas, otherwise an error will be returned. 

The module also allows you to generate a VexTab score from parsed ABC text.

Examples
--------

* __basic__ demonstrates production of a score from a basic VexTab script.
* __abc__ demonstrates production of a score from an ABC tune.