// Generated by psc-bundle 0.11.4
var PS = {};
(function(exports) {
    "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
  // Generated by purs version 0.11.4
  "use strict";
  var $foreign = PS["Data.Show"];     
  var Show = function (show) {
      this.show = show;
  };                                             
  var showBoolean = new Show(function (v) {
      if (v) {
          return "true";
      };
      if (!v) {
          return "false";
      };
      throw new Error("Failed pattern match at Data.Show line 13, column 3 - line 14, column 3: " + [ v.constructor.name ]);
  });
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showBoolean"] = showBoolean;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by purs version 0.11.4
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
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
        // console.log(config);

        // var VexTab = VexTabDiv;
        var Artist = VexTabDiv.Artist;
        var Renderer = Vex.Flow.Renderer;
        var vexDiv = $(config.canvasDivId)[0];

        Artist.DEBUG = false;
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
})(PS["VexTab.Score"] = PS["VexTab.Score"] || {});
(function(exports) {
  // Generated by purs version 0.11.4
  "use strict";
  var $foreign = PS["VexTab.Score"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["initialise"] = $foreign.initialise;
  exports["render"] = $foreign.render;
})(PS["VexTab.Score"] = PS["VexTab.Score"] || {});
(function(exports) {
  // Generated by purs version 0.11.4
  "use strict";
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Prelude = PS["Prelude"];
  var VexTab_Score = PS["VexTab.Score"];        
  var sampleText = "stave \x0a" + ("notation=true \x0a" + ("key=G time=3/4 \x0a" + "notes :q A/4 B/4 :8 C/5 D/5 |  E/5 $.top.$ $1\u2500\u2500\u2500$ F/5  :q A/4 D/4 =:| :8 E/5 $.top.$ $2\u2500\u2500\u2500$ F/5 :h A/4 |\x0a"));
  var config = {
      canvasDivId: "#vextab", 
      canvasX: 10, 
      canvasY: 10, 
      canvasWidth: 1200, 
      scale: 0.8
  };
  var main = function __do() {
      var v = VexTab_Score.initialise(config)();
      Control_Monad_Eff_Console.log("initialised?")();
      Control_Monad_Eff_Console.logShow(Data_Show.showBoolean)(v)();
      Control_Monad_Eff_Console.log("rendered?")();
      var v1 = VexTab_Score.render(sampleText)();
      return Control_Monad_Eff_Console.logShow(Data_Show.showBoolean)(v1)();
  };
  exports["config"] = config;
  exports["main"] = main;
  exports["sampleText"] = sampleText;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();
