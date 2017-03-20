"use strict";

var wrapper = function() {

  var vextab = null;
  var artist = null;
  var renderer = null;

  return {

    initialise : function (config) {
      return function () {
        return wrapper.init(config);
      }
    },

    render : function (text) {
      return function () {
        try {
           vextab.reset();
           artist.reset();
           vextab.parse(text);
           artist.render(renderer);
           return true;
        } catch (e) {
           console.log (e.message);
           return false;
        }
      }
    },

    init: function (config) {
      console.log(config);

      // var VexTab = VexTabDiv;
      var Artist = VexTabDiv.Artist;
      var Renderer = Vex.Flow.Renderer;
      var vexDiv = $(config.canvasDivId)[0];

      Artist.DEBUG = true;
      VexTab.DEBUG = false;

      try {
         // Create VexFlow Renderer from canvas element with id vexDiv
         renderer = new Renderer(vexDiv, Renderer.Backends.CANVAS);

         // Initialize VexTab artist and parser.
         artist = new Artist(config.canvasX, config.canvasY, config.canvasWidth, {scale: config.scale});
         vextab = new VexTab(artist);
         return true;
       } catch (e) {
          console.log (e.message);
          return false;
       }
    }

  }

}();

exports.initialise = wrapper.initialise;
exports.render = wrapper.render;
