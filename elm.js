var Elm = Elm || { Native: {} };
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 595, column 3 to 8");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 573, column 3 to 4");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 567, column 3 to 4");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Char",
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,
   high,
   $char) {
      return function () {
         var code = toCode($char);
         return _U.cmp(code,
         toCode(low)) > -1 && _U.cmp(code,
         toCode(high)) < 1;
      }();
   });
   var isUpper = A2(isBetween,
   _U.chr("A"),
   _U.chr("Z"));
   var isLower = A2(isBetween,
   _U.chr("a"),
   _U.chr("z"));
   var isDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("9"));
   var isOctDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,
      _U.chr("A"),
      _U.chr("F"),
      $char));
   };
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 150 and 152"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 114 and 118");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 108");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgb: rgb
                       ,rgba: rgba
                       ,hsl: hsl
                       ,hsla: hsla
                       ,greyscale: greyscale
                       ,grayscale: grayscale
                       ,complement: complement
                       ,linear: linear
                       ,radial: radial
                       ,toRgb: toRgb
                       ,toHsl: toHsl
                       ,red: red
                       ,orange: orange
                       ,yellow: yellow
                       ,green: green
                       ,blue: blue
                       ,purple: purple
                       ,brown: brown
                       ,lightRed: lightRed
                       ,lightOrange: lightOrange
                       ,lightYellow: lightYellow
                       ,lightGreen: lightGreen
                       ,lightBlue: lightBlue
                       ,lightPurple: lightPurple
                       ,lightBrown: lightBrown
                       ,darkRed: darkRed
                       ,darkOrange: darkOrange
                       ,darkYellow: darkYellow
                       ,darkGreen: darkGreen
                       ,darkBlue: darkBlue
                       ,darkPurple: darkPurple
                       ,darkBrown: darkBrown
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,black: black
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray};
   return _elm.Color.values;
};
Elm.DApprox = Elm.DApprox || {};
Elm.DApprox.make = function (_elm) {
   "use strict";
   _elm.DApprox = _elm.DApprox || {};
   if (_elm.DApprox.values)
   return _elm.DApprox.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "DApprox",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Dist = Elm.Dist.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var floatMod = F2(function (x,
   m) {
      return x - m * $Basics.toFloat($Basics.floor(x / m));
   });
   var dTheta = 3 * $Basics.pi / 1024;
   var advanceDApprox = F3(function (slopeDist,
   n,
   s) {
      return {_: {}
             ,muDist: A3($Dist.lift2,
             F2(function (slope,mu) {
                return A2(floatMod,
                mu + $Basics.toFloat(n) * slope,
                256);
             }),
             slopeDist,
             s.muDist)
             ,thetaDist: A2($Dist.map,
             function (theta) {
                return A2(floatMod,
                theta + $Basics.toFloat(n) * dTheta,
                2 * $Basics.pi);
             },
             s.thetaDist)};
   });
   var insideLowSlope = 40704 / 131072;
   var insideHighSlope = 48896 / 131072;
   var insideSlopeDist = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                      ,_0: insideLowSlope
                                                      ,_1: 3 / 4}
                                                     ,{ctor: "_Tuple2"
                                                      ,_0: insideHighSlope
                                                      ,_1: 1 / 4}]));
   var outsideLowSlope = -89856 / 131072;
   var outsideHighSlope = -81664 / 131072;
   var outsideSlopeDist = $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                       ,_0: outsideLowSlope
                                                       ,_1: 3 / 4}
                                                      ,{ctor: "_Tuple2"
                                                       ,_0: outsideHighSlope
                                                       ,_1: 1 / 4}]));
   var dsumApproximation = F2(function (mu,
   theta) {
      return A2($Basics._op["%"],
      $Basics.round(mu + 5.45 + 2.47 * $Basics.sin(theta)),
      256);
   });
   var dapproxDist = function (s) {
      return A3($Dist.lift2,
      dsumApproximation,
      s.muDist,
      s.thetaDist);
   };
   var filterMu = F2(function (f,
   thetaDist) {
      return $Dist.condition(function (mu) {
         return A2($Dist.probability,
         f,
         A2($Dist.map,
         dsumApproximation(mu),
         thetaDist));
      });
   });
   var filterTheta = F2(function (f,
   muDist) {
      return $Dist.condition(function (theta) {
         return A2($Dist.probability,
         f,
         A2($Dist.map,
         function (mu) {
            return A2(dsumApproximation,
            mu,
            theta);
         },
         muDist));
      });
   });
   var filterDApprox = F2(function (f,
   s) {
      return function () {
         var muDist$ = A3(filterMu,
         f,
         s.thetaDist,
         s.muDist);
         var thetaDist$ = A3(filterTheta,
         f,
         muDist$,
         s.thetaDist);
         return {_: {}
                ,muDist: muDist$
                ,thetaDist: thetaDist$};
      }();
   });
   var conditionMu = F2(function (f,
   thetaDist) {
      return $Dist.condition(function (mu) {
         return A2($Dist.weightedProbability,
         f,
         A2($Dist.map,
         dsumApproximation(mu),
         thetaDist));
      });
   });
   var conditionTheta = F2(function (f,
   muDist) {
      return $Dist.condition(function (theta) {
         return A2($Dist.weightedProbability,
         f,
         A2($Dist.map,
         function (mu) {
            return A2(dsumApproximation,
            mu,
            theta);
         },
         muDist));
      });
   });
   var conditionDApprox = F2(function (f,
   s) {
      return function () {
         var muDist$ = A3(conditionMu,
         f,
         s.thetaDist,
         s.muDist);
         var thetaDist$ = A3(conditionTheta,
         f,
         muDist$,
         s.thetaDist);
         return {_: {}
                ,muDist: muDist$
                ,thetaDist: thetaDist$};
      }();
   });
   var initialDApproxState = {_: {}
                             ,muDist: $Dist.uniform(_L.range(0,
                             255))
                             ,thetaDist: $Dist.uniform($List.map(function (x) {
                                return x * 2 * $Basics.pi / 16;
                             })(_L.range(0,15)))};
   var DApproxState = F2(function (a,
   b) {
      return {_: {}
             ,muDist: a
             ,thetaDist: b};
   });
   _elm.DApprox.values = {_op: _op
                         ,DApproxState: DApproxState
                         ,initialDApproxState: initialDApproxState
                         ,dsumApproximation: dsumApproximation
                         ,outsideHighSlope: outsideHighSlope
                         ,outsideLowSlope: outsideLowSlope
                         ,insideHighSlope: insideHighSlope
                         ,insideLowSlope: insideLowSlope
                         ,dTheta: dTheta
                         ,dapproxDist: dapproxDist
                         ,outsideSlopeDist: outsideSlopeDist
                         ,insideSlopeDist: insideSlopeDist
                         ,floatMod: floatMod
                         ,advanceDApprox: advanceDApprox
                         ,filterDApprox: filterDApprox
                         ,filterMu: filterMu
                         ,filterTheta: filterTheta
                         ,conditionDApprox: conditionDApprox
                         ,conditionMu: conditionMu
                         ,conditionTheta: conditionTheta};
   return _elm.DApprox.values;
};
Elm.DSum = Elm.DSum || {};
Elm.DSum.make = function (_elm) {
   "use strict";
   _elm.DSum = _elm.DSum || {};
   if (_elm.DSum.values)
   return _elm.DSum.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "DSum",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Dist = Elm.Dist.make(_elm),
   $Encounters = Elm.Encounters.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $RNG = Elm.RNG.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var randomizeBand = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple4":
            return $Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                ,_0: {ctor: "_Tuple4"
                                                     ,_0: _v0._0
                                                     ,_1: _v0._1 - A2($Basics._op["%"],
                                                     _v0._1,
                                                     16) + 4
                                                     ,_2: _v0._2
                                                     ,_3: _v0._3}
                                                ,_1: 3 / 4}
                                               ,{ctor: "_Tuple2"
                                                ,_0: {ctor: "_Tuple4"
                                                     ,_0: _v0._0
                                                     ,_1: _v0._1 - A2($Basics._op["%"],
                                                     _v0._1,
                                                     16)
                                                     ,_2: _v0._2
                                                     ,_3: _v0._3}
                                                ,_1: 1 / 4}]));}
         _U.badCase($moduleName,
         "between lines 40 and 43");
      }();
   };
   var dsumSlotDist = F2(function (rate,
   dsum) {
      return $Dist.map($Encounters.slotFromRand)($Dist.uniform($List.map(function (r1) {
         return A2($Basics._op["%"],
         dsum - r1,
         256);
      })(_L.range(0,rate - 1))));
   });
   var dsumStep = F2(function (carry,
   dist) {
      return A2($Dist.map,
      $RNG.rngStep$(carry),
      dist);
   });
   var dsumDist = $Dist.map($RNG.getDSum$);
   var conditionDSum = F2(function (f,
   dist) {
      return A2($Dist.condition,
      function ($) {
         return f($RNG.getDSum$($));
      },
      dist);
   });
   var filterDSum = F2(function (f,
   dist) {
      return A2($Dist.filter,
      function ($) {
         return f($RNG.getDSum$($));
      },
      dist);
   });
   var initialRDiv = 17;
   var initialRNGMix = $Dict.fromList($List.concat(_L.fromArray([A2($List.map,
                                                                function (dsum) {
                                                                   return {ctor: "_Tuple2"
                                                                          ,_0: {ctor: "_Tuple4"
                                                                               ,_0: initialRDiv
                                                                               ,_1: 4
                                                                               ,_2: dsum
                                                                               ,_3: 0}
                                                                          ,_1: 3 / 1024};
                                                                },
                                                                _L.range(0,255))
                                                                ,A2($List.map,
                                                                function (dsum) {
                                                                   return {ctor: "_Tuple2"
                                                                          ,_0: {ctor: "_Tuple4"
                                                                               ,_0: initialRDiv
                                                                               ,_1: 0
                                                                               ,_2: dsum
                                                                               ,_3: 0}
                                                                          ,_1: 1 / 1024};
                                                                },
                                                                _L.range(0,
                                                                255))])));
   _elm.DSum.values = {_op: _op
                      ,initialRDiv: initialRDiv
                      ,initialRNGMix: initialRNGMix
                      ,filterDSum: filterDSum
                      ,conditionDSum: conditionDSum
                      ,dsumDist: dsumDist
                      ,dsumStep: dsumStep
                      ,dsumSlotDist: dsumSlotDist
                      ,randomizeBand: randomizeBand};
   return _elm.DSum.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 417 and 421");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 406 and 410");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 182 and 185");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 129 and 132");
              }();}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 138 and 140");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 100 and 108");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 70 and 72");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var isEmpty = function (dict) {
      return _U.eq(dict,empty);
   };
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 394 and 399");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 56 and 60");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 154 and 162");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 339 and 341");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 378 and 380");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 244 and 248");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 253 and 257");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 262 and 264");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 386 and 388");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 323 and 328");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 282 and 286");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 280 and 309");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 194 and 198");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 201 and 206");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 215 and 220");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 208 and 213");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 199 and 220");
                    }();}
               _U.badCase($moduleName,
               "between lines 192 and 220");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 222 and 225");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      empty);
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 466, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 487 and 489");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,isEmpty: isEmpty
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.Dist = Elm.Dist || {};
Elm.Dist.make = function (_elm) {
   "use strict";
   _elm.Dist = _elm.Dist || {};
   if (_elm.Dist.values)
   return _elm.Dist.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dist",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var collapse$ = function (vals) {
      return $List.concat(A2($List.map,
      function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return A2($List.map,
                 function (_v4) {
                    return function () {
                       switch (_v4.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: _v4._0
                                 ,_1: _v0._1 * _v4._1};}
                       _U.badCase($moduleName,
                       "on line 85, column 74 to 80");
                    }();
                 },
                 _v0._0);}
            _U.badCase($moduleName,
            "on line 85, column 52 to 84");
         }();
      },
      vals));
   };
   var uniform = function (l) {
      return function () {
         var n = $List.length(l);
         return $Dict.fromList($List.map(function (x) {
            return {ctor: "_Tuple2"
                   ,_0: x
                   ,_1: 1 / $Basics.toFloat(n)};
         })(l));
      }();
   };
   var product$ = F2(function (l1,
   l2) {
      return function () {
         switch (l1.ctor)
         {case "::": switch (l1._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var probs = A2($List.map,
                      function (_v13) {
                         return function () {
                            switch (_v13.ctor)
                            {case "_Tuple2":
                               return {ctor: "_Tuple2"
                                      ,_0: {ctor: "_Tuple2"
                                           ,_0: l1._0._0
                                           ,_1: _v13._0}
                                      ,_1: l1._0._1 * _v13._1};}
                            _U.badCase($moduleName,
                            "on line 63, column 39 to 50");
                         }();
                      },
                      l2);
                      return A2($List.append,
                      probs,
                      A2(product$,l1._1,l2));
                   }();}
              break;
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 59 and 65");
      }();
   });
   var product = F2(function (d1,
   d2) {
      return function () {
         var probs2 = $Dict.toList(d2);
         var probs1 = $Dict.toList(d1);
         var productProbs = A2(product$,
         probs1,
         probs2);
         return $Dict.fromList(productProbs);
      }();
   });
   var normalize = function (probs) {
      return function () {
         var total = $List.sum(A2($List.map,
         $Basics.snd,
         probs));
         return $List.filter(function (_v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return !_U.eq(_v21._1,0);}
               _U.badCase($moduleName,
               "on line 42, column 32 to 38");
            }();
         })($List.map(function (_v17) {
            return function () {
               switch (_v17.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: _v17._0
                         ,_1: _v17._1 / total};}
               _U.badCase($moduleName,
               "on line 41, column 30 to 42");
            }();
         })(probs));
      }();
   };
   var filter = F2(function (f,
   dist) {
      return $Dict.fromList(normalize($List.filter(function (_v25) {
         return function () {
            switch (_v25.ctor)
            {case "_Tuple2":
               return f(_v25._0);}
            _U.badCase($moduleName,
            "on line 47, column 32 to 35");
         }();
      })($Dict.toList(dist))));
   });
   var condition = F2(function (f,
   dist) {
      return $Dict.fromList(normalize($List.map(function (_v29) {
         return function () {
            switch (_v29.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: _v29._0
                      ,_1: _v29._1 * f(_v29._0)};}
            _U.badCase($moduleName,
            "on line 54, column 30 to 40");
         }();
      })($Dict.toList(dist))));
   });
   var partitionPrefix = F2(function (f,
   l) {
      return function () {
         switch (l.ctor)
         {case "::":
            return f(l._0) ? function () {
                 var $ = A2(partitionPrefix,
                 f,
                 l._1),
                 matches = $._0,
                 rest = $._1;
                 return {ctor: "_Tuple2"
                        ,_0: A2($List._op["::"],
                        l._0,
                        matches)
                        ,_1: rest};
              }() : {ctor: "_Tuple2"
                    ,_0: _L.fromArray([])
                    ,_1: l};
            case "[]":
            return {ctor: "_Tuple2"
                   ,_0: _L.fromArray([])
                   ,_1: _L.fromArray([])};}
         _U.badCase($moduleName,
         "between lines 15 and 19");
      }();
   });
   var combineProbs = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            switch (list._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var $ = A2(partitionPrefix,
                      function ($) {
                         return F2(function (x,y) {
                            return _U.eq(x,y);
                         })(list._0._0)($Basics.fst($));
                      },
                      list),
                      matches = $._0,
                      rest = $._1;
                      return A2($List._op["::"],
                      {ctor: "_Tuple2"
                      ,_0: list._0._0
                      ,_1: $List.sum($List.map($Basics.snd)(matches))},
                      combineProbs(rest));
                   }();}
              break;
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 22 and 26");
      }();
   };
   var map = F2(function (f,dist) {
      return $Dict.fromList(combineProbs($List.sort($List.map(function (_v41) {
         return function () {
            switch (_v41.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: f(_v41._0)
                      ,_1: _v41._1};}
            _U.badCase($moduleName,
            "on line 31, column 30 to 36");
         }();
      })($Dict.toList(dist)))));
   });
   var lift2 = F3(function (f,
   d1,
   d2) {
      return A2(map,
      $Basics.uncurry(f),
      A2(product,d1,d2));
   });
   var collapseMap = F2(function (f,
   dist) {
      return function () {
         var probs = $Dict.toList(dist);
         var nestedProbs = A2($List.map,
         function (_v45) {
            return function () {
               switch (_v45.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: $Dict.toList(f(_v45._0))
                         ,_1: _v45._1};}
               _U.badCase($moduleName,
               "on line 91, column 41 to 61");
            }();
         },
         probs);
         var collapsedProbs = collapse$(nestedProbs);
         return $Dict.fromList(combineProbs($List.sort(collapsedProbs)));
      }();
   });
   var weightedProbability = F2(function (f,
   dist) {
      return A3($Dict.foldl,
      F3(function (x,p,s) {
         return s + f(x) * p;
      }),
      0,
      dist);
   });
   var probability = F2(function (f,
   dist) {
      return A3($Dict.foldl,
      F3(function (x,p,s) {
         return f(x) ? s + p : s;
      }),
      0,
      dist);
   });
   _elm.Dist.values = {_op: _op
                      ,probability: probability
                      ,weightedProbability: weightedProbability
                      ,partitionPrefix: partitionPrefix
                      ,combineProbs: combineProbs
                      ,map: map
                      ,normalize: normalize
                      ,filter: filter
                      ,condition: condition
                      ,product$: product$
                      ,product: product
                      ,lift2: lift2
                      ,uniform: uniform
                      ,collapse$: collapse$
                      ,collapseMap: collapseMap};
   return _elm.Dist.values;
};
Elm.Encounters = Elm.Encounters || {};
Elm.Encounters.make = function (_elm) {
   "use strict";
   _elm.Encounters = _elm.Encounters || {};
   if (_elm.Encounters.values)
   return _elm.Encounters.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Encounters",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Pokemon = Elm.Pokemon.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var noEncounter = {_: {}
                     ,level: 0
                     ,species: $Pokemon.noSpecies};
   var displayName = function (enc) {
      return A2($Basics._op["++"],
      "L",
      A2($Basics._op["++"],
      $Basics.toString(enc.level),
      A2($Basics._op["++"],
      " ",
      enc.species.name)));
   };
   var encounter = F2(function (name,
   level) {
      return {_: {}
             ,level: level
             ,species: $Maybe.withDefault($Pokemon.noSpecies)($Dict.get(name)($Pokemon.speciesByName))};
   });
   var route22table = {_: {}
                      ,name: "Route 22"
                      ,rate: 25
                      ,slot1: A2(encounter,
                      "Rattata",
                      3)
                      ,slot10: A2(encounter,
                      "Nidoran F",
                      4)
                      ,slot2: A2(encounter,
                      "Nidoran M",
                      3)
                      ,slot3: A2(encounter,
                      "Rattata",
                      4)
                      ,slot4: A2(encounter,
                      "Nidoran M",
                      4)
                      ,slot5: A2(encounter,
                      "Rattata",
                      2)
                      ,slot6: A2(encounter,
                      "Nidoran M",
                      2)
                      ,slot7: A2(encounter,
                      "Spearow",
                      3)
                      ,slot8: A2(encounter,
                      "Spearow",
                      5)
                      ,slot9: A2(encounter,
                      "Nidoran F",
                      3)};
   var EncounterTable = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return {_: {}
                                              ,name: a
                                              ,rate: b
                                              ,slot1: c
                                              ,slot10: l
                                              ,slot2: d
                                              ,slot3: e
                                              ,slot4: f
                                              ,slot5: g
                                              ,slot6: h
                                              ,slot7: i
                                              ,slot8: j
                                              ,slot9: k};
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var slotFromRand = function (x) {
      return function () {
         var x$ = A2($Basics._op["%"],
         x,
         256);
         return _U.cmp(x,
         51) < 0 ? 1 : _U.cmp(x,
         102) < 0 ? 2 : _U.cmp(x,
         141) < 0 ? 3 : _U.cmp(x,
         166) < 0 ? 4 : _U.cmp(x,
         191) < 0 ? 5 : _U.cmp(x,
         216) < 0 ? 6 : _U.cmp(x,
         229) < 0 ? 7 : _U.cmp(x,
         242) < 0 ? 8 : _U.cmp(x,
         253) < 0 ? 9 : 10;
      }();
   };
   var slots = _L.fromArray([{ctor: "_Tuple2"
                             ,_0: 0
                             ,_1: 51}
                            ,{ctor: "_Tuple2",_0: 51,_1: 51}
                            ,{ctor: "_Tuple2"
                             ,_0: 102
                             ,_1: 39}
                            ,{ctor: "_Tuple2"
                             ,_0: 141
                             ,_1: 25}
                            ,{ctor: "_Tuple2"
                             ,_0: 166
                             ,_1: 25}
                            ,{ctor: "_Tuple2"
                             ,_0: 191
                             ,_1: 25}
                            ,{ctor: "_Tuple2"
                             ,_0: 216
                             ,_1: 13}
                            ,{ctor: "_Tuple2"
                             ,_0: 229
                             ,_1: 13}
                            ,{ctor: "_Tuple2"
                             ,_0: 242
                             ,_1: 11}
                            ,{ctor: "_Tuple2"
                             ,_0: 253
                             ,_1: 3}]);
   var slotStarts = A2($List.map,
   $Basics.fst,
   slots);
   var slotWidths = A2($List.map,
   $Basics.snd,
   slots);
   var Encounter = F2(function (a,
   b) {
      return {_: {}
             ,level: b
             ,species: a};
   });
   var framesBeforeMove = 44;
   var baseBattleLength = 549;
   var battleLength = F2(function (you,
   enemy) {
      return baseBattleLength + you.cryDiff + enemy.cryDiff;
   });
   _elm.Encounters.values = {_op: _op
                            ,baseBattleLength: baseBattleLength
                            ,framesBeforeMove: framesBeforeMove
                            ,battleLength: battleLength
                            ,Encounter: Encounter
                            ,slots: slots
                            ,slotStarts: slotStarts
                            ,slotWidths: slotWidths
                            ,slotFromRand: slotFromRand
                            ,EncounterTable: EncounterTable
                            ,encounter: encounter
                            ,displayName: displayName
                            ,noEncounter: noEncounter
                            ,route22table: route22table};
   return _elm.Encounters.values;
};
Elm.Graph = Elm.Graph || {};
Elm.Graph.make = function (_elm) {
   "use strict";
   _elm.Graph = _elm.Graph || {};
   if (_elm.Graph.values)
   return _elm.Graph.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graph",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm);
   var roundTo = F2(function (gap,
   x) {
      return gap * $Basics.toFloat($Basics.round(x / gap));
   });
   var gapSize = function (x) {
      return function () {
         var powerOfTen = Math.pow(10,
         $Basics.toFloat($Basics.floor(A2($Basics.logBase,
         10,
         x))));
         return _U.cmp(5 * powerOfTen,
         x) < 0 ? 5 * powerOfTen : _U.cmp(2 * powerOfTen,
         x) < 0 ? 2 * powerOfTen : powerOfTen;
      }();
   };
   var labelPositions = F2(function (lo,
   hi) {
      return function () {
         var gap = gapSize((hi - lo) / 2);
         return A2($List._op["::"],
         lo,
         A2($List._op["::"],
         hi,
         A2($List.map,
         function ($) {
            return F2(function (x,y) {
               return x * y;
            })(gap)($Basics.toFloat($));
         },
         _L.range($Basics.ceiling(lo / gap + 0.2),
         $Basics.floor(hi / gap - 0.2)))));
      }();
   });
   var drawGraph = F3(function (w,
   h,
   g) {
      return function () {
         var axisStyle = _U.replace([["width"
                                     ,2]],
         $Graphics$Collage.defaultLine);
         var color_cycle = _L.fromArray([$Color.red
                                        ,$Color.darkGreen
                                        ,$Color.blue
                                        ,$Color.purple
                                        ,$Color.orange]);
         var colors = $List.concat(A2($List.repeat,
         ($List.length(g.points) / $List.length(color_cycle) | 0) + 1,
         color_cycle));
         var graphOffsetY = 20;
         var graphH = $Basics.toFloat(h) - 2 * graphOffsetY;
         var graphOffsetX = 40;
         var graphW = $Basics.toFloat(w) - 2 * graphOffsetX;
         var maxY = function () {
            var _v0 = g.yRange;
            switch (_v0.ctor)
            {case "Just":
               switch (_v0._0.ctor)
                 {case "_Tuple2":
                    return _v0._0._1;}
                 break;
               case "Nothing":
               return A2($Maybe.withDefault,
                 100,
                 $List.maximum(A2($List.map,
                 $Basics.snd,
                 $List.concat(g.points))));}
            _U.badCase($moduleName,
            "between lines 58 and 61");
         }();
         var minY = function () {
            var _v4 = g.yRange;
            switch (_v4.ctor)
            {case "Just":
               switch (_v4._0.ctor)
                 {case "_Tuple2":
                    return _v4._0._0;}
                 break;
               case "Nothing":
               return A2($Maybe.withDefault,
                 0,
                 $List.minimum(A2($List.map,
                 $Basics.snd,
                 $List.concat(g.points))));}
            _U.badCase($moduleName,
            "between lines 55 and 58");
         }();
         var pixelY = function (y) {
            return (y - minY) / (maxY - minY) * graphH - graphH / 2;
         };
         var yLabelPositions = A2(labelPositions,
         minY,
         maxY);
         var maxX = function () {
            var _v8 = g.xRange;
            switch (_v8.ctor)
            {case "Just":
               switch (_v8._0.ctor)
                 {case "_Tuple2":
                    return _v8._0._1;}
                 break;
               case "Nothing":
               return A2($Maybe.withDefault,
                 100,
                 $List.maximum(A2($List.map,
                 $Basics.fst,
                 $List.concat(g.points))));}
            _U.badCase($moduleName,
            "between lines 52 and 55");
         }();
         var minX = function () {
            var _v12 = g.xRange;
            switch (_v12.ctor)
            {case "Just":
               switch (_v12._0.ctor)
                 {case "_Tuple2":
                    return _v12._0._0;}
                 break;
               case "Nothing":
               return A2($Maybe.withDefault,
                 0,
                 $List.minimum(A2($List.map,
                 $Basics.fst,
                 $List.concat(g.points))));}
            _U.badCase($moduleName,
            "between lines 49 and 52");
         }();
         var pixelX = function (x) {
            return (x - minX) / (maxX - minX) * graphW - graphW / 2;
         };
         var pixelCoordinates = F2(function (x,
         y) {
            return {ctor: "_Tuple2"
                   ,_0: pixelX(x)
                   ,_1: pixelY(y)};
         });
         var yAxis = A2($Graphics$Collage.traced,
         axisStyle,
         A2($Graphics$Collage.segment,
         A2(pixelCoordinates,0,minY),
         A2(pixelCoordinates,0,maxY)));
         var xLabel = function (x) {
            return $Graphics$Collage.move({ctor: "_Tuple2"
                                          ,_0: 0
                                          ,_1: -10})($Graphics$Collage.move(A2(pixelCoordinates,
            x,
            0))($Graphics$Collage.text($Text.fromString($Basics.toString(x)))));
         };
         var xLabelLine = function (x) {
            return A2($Graphics$Collage.traced,
            $Graphics$Collage.dashed($Color.darkGray),
            A2($Graphics$Collage.segment,
            A2(pixelCoordinates,x,minY),
            A2(pixelCoordinates,x,maxY)));
         };
         var yLabel = function (y) {
            return $Graphics$Collage.move({ctor: "_Tuple2"
                                          ,_0: -20
                                          ,_1: 0})($Graphics$Collage.move(A2(pixelCoordinates,
            0,
            y))($Graphics$Collage.text($Text.fromString($Basics.toString(y)))));
         };
         var pointForm = F2(function (color,
         _v16) {
            return function () {
               switch (_v16.ctor)
               {case "_Tuple2":
                  return A2($Graphics$Collage.move,
                    {ctor: "_Tuple2"
                    ,_0: pixelX(_v16._0)
                    ,_1: pixelY(_v16._1)},
                    A2($Graphics$Collage.filled,
                    color,
                    A2($Graphics$Collage.oval,
                    1,
                    1)));}
               _U.badCase($moduleName,
               "on line 70, column 34 to 83");
            }();
         });
         var seriesForm = F2(function (color,
         points) {
            return A2($List.map,
            pointForm(color),
            points);
         });
         var xAxis = A2($Graphics$Collage.traced,
         axisStyle,
         A2($Graphics$Collage.segment,
         A2(pixelCoordinates,minX,0),
         A2(pixelCoordinates,maxX,0)));
         var yLabelLine = function (y) {
            return A2($Graphics$Collage.traced,
            $Graphics$Collage.dashed($Color.darkGray),
            A2($Graphics$Collage.segment,
            A2(pixelCoordinates,minX,y),
            A2(pixelCoordinates,maxX,y)));
         };
         var xLabelPositions = A2(labelPositions,
         minX,
         maxX);
         var forms = $List.concat(_L.fromArray([$List.concat(A3($List.map2,
                                               seriesForm,
                                               colors,
                                               g.points))
                                               ,A2($List.map,
                                               xLabel,
                                               xLabelPositions)
                                               ,A2($List.map,
                                               xLabelLine,
                                               xLabelPositions)
                                               ,A2($List.map,
                                               yLabel,
                                               yLabelPositions)
                                               ,A2($List.map,
                                               yLabelLine,
                                               yLabelPositions)
                                               ,_L.fromArray([xAxis,yAxis])]));
         return A3($Graphics$Collage.collage,
         w,
         h,
         forms);
      }();
   });
   var addSeries = F2(function (series,
   g) {
      return _U.replace([["points"
                         ,A2($List._op["::"],
                         series,
                         g.points)]],
      g);
   });
   var graph = F3(function (xRange,
   yRange,
   points) {
      return {_: {}
             ,points: points
             ,xRange: xRange
             ,yRange: yRange};
   });
   var Graph = F3(function (a,
   b,
   c) {
      return {_: {}
             ,points: c
             ,xRange: a
             ,yRange: b};
   });
   _elm.Graph.values = {_op: _op
                       ,Graph: Graph
                       ,graph: graph
                       ,addSeries: addSeries
                       ,gapSize: gapSize
                       ,roundTo: roundTo
                       ,labelPositions: labelPositions
                       ,drawGraph: drawGraph};
   return _elm.Graph.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 226, column 3 to 37");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FText = function (a) {
      return {ctor: "FText",_0: a};
   };
   var text = function (t) {
      return form(FText(t));
   };
   var FOutlinedText = F2(function (a,
   b) {
      return {ctor: "FOutlinedText"
             ,_0: a
             ,_1: b};
   });
   var outlinedText = F2(function (ls,
   t) {
      return form(A2(FOutlinedText,
      ls,
      t));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,collage: collage
                                  ,toForm: toForm
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,text: text
                                  ,outlinedText: outlinedText
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,rect: rect
                                  ,oval: oval
                                  ,square: square
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,polygon: polygon
                                  ,segment: segment
                                  ,path: path
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,defaultLine: defaultLine
                                  ,Form: Form
                                  ,LineStyle: LineStyle
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         A2($Maybe.withDefault,
         0,
         $List.maximum(ws)),
         A2($Maybe.withDefault,
         0,
         $List.maximum(hs)),
         A2(Flow,DOut,es));
      }();
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var maxOrZero = function (list) {
            return A2($Maybe.withDefault,
            0,
            $List.maximum(list));
         };
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DOut": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DUp": return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 362 and 368");
         }();
      }();
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   _elm.Graphics.Element.values = {_op: _op
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,leftAligned: leftAligned
                                  ,rightAligned: rightAligned
                                  ,centered: centered
                                  ,justified: justified
                                  ,show: show
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,color: color
                                  ,opacity: opacity
                                  ,link: link
                                  ,tag: tag
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,flow: flow
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,layers: layers
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,empty: empty
                                  ,spacer: spacer
                                  ,container: container
                                  ,middle: middle
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middleAt: middleAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   if (_elm.Graphics.Input.values)
   return _elm.Graphics.Input.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Input",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clickable = $Native$Graphics$Input.clickable;
   var hoverable = $Native$Graphics$Input.hoverable;
   var dropDown = $Native$Graphics$Input.dropDown;
   var checkbox = $Native$Graphics$Input.checkbox;
   var customButton = $Native$Graphics$Input.customButton;
   var button = $Native$Graphics$Input.button;
   _elm.Graphics.Input.values = {_op: _op
                                ,button: button
                                ,customButton: customButton
                                ,checkbox: checkbox
                                ,dropDown: dropDown
                                ,hoverable: hoverable
                                ,clickable: clickable};
   return _elm.Graphics.Input.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.Field = Elm.Graphics.Input.Field || {};
Elm.Graphics.Input.Field.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   _elm.Graphics.Input.Field = _elm.Graphics.Input.Field || {};
   if (_elm.Graphics.Input.Field.values)
   return _elm.Graphics.Input.Field.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Input.Field",
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm);
   var email = $Native$Graphics$Input.email;
   var password = $Native$Graphics$Input.password;
   var field = $Native$Graphics$Input.field;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var Selection = F3(function (a,
   b,
   c) {
      return {_: {}
             ,direction: c
             ,end: b
             ,start: a};
   });
   var Content = F2(function (a,
   b) {
      return {_: {}
             ,selection: b
             ,string: a};
   });
   var noContent = A2(Content,
   "",
   A3(Selection,0,0,Forward));
   var Style = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,highlight: c
             ,outline: b
             ,padding: a
             ,style: d};
   });
   var Highlight = F2(function (a,
   b) {
      return {_: {}
             ,color: a
             ,width: b};
   });
   var noHighlight = A2(Highlight,
   $Color.blue,
   0);
   var Outline = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: a
             ,radius: c
             ,width: b};
   });
   var Dimensions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: a
             ,right: b
             ,top: c};
   });
   var uniformly = function (n) {
      return A4(Dimensions,
      n,
      n,
      n,
      n);
   };
   var noOutline = A3(Outline,
   $Color.grey,
   uniformly(0),
   0);
   var defaultStyle = {_: {}
                      ,highlight: A2(Highlight,
                      $Color.blue,
                      1)
                      ,outline: A3(Outline,
                      $Color.grey,
                      uniformly(1),
                      2)
                      ,padding: uniformly(4)
                      ,style: $Text.defaultStyle};
   _elm.Graphics.Input.Field.values = {_op: _op
                                      ,field: field
                                      ,password: password
                                      ,email: email
                                      ,noContent: noContent
                                      ,defaultStyle: defaultStyle
                                      ,noOutline: noOutline
                                      ,noHighlight: noHighlight
                                      ,uniformly: uniformly
                                      ,Content: Content
                                      ,Selection: Selection
                                      ,Style: Style
                                      ,Outline: Outline
                                      ,Highlight: Highlight
                                      ,Dimensions: Dimensions
                                      ,Forward: Forward
                                      ,Backward: Backward};
   return _elm.Graphics.Input.Field.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,
      $Basics.identity,
      xs);
   };
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = $Native$List.any;
   var all = F2(function (pred,
   xs) {
      return $Basics.not(A2(any,
      function ($) {
         return $Basics.not(pred($));
      },
      xs));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_v0,i) {
         return function () {
            return i + 1;
         }();
      }),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var maximum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.max,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var minimum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.min,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var member = F2(function (x,
   xs) {
      return A2(any,
      function (a) {
         return _U.eq(a,x);
      },
      xs);
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._1);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 87 and 89");
      }();
   };
   var head = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 75 and 77");
      }();
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {
         return A2(_op["::"],
         f(x),
         acc);
      }),
      _L.fromArray([]),
      xs);
   });
   var filter = F2(function (pred,
   xs) {
      return function () {
         var conditionalCons = F2(function (x,
         xs$) {
            return pred(x) ? A2(_op["::"],
            x,
            xs$) : xs$;
         });
         return A3(foldr,
         conditionalCons,
         _L.fromArray([]),
         xs);
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v15 = f(mx);
         switch (_v15.ctor)
         {case "Just":
            return A2(_op["::"],_v15._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 179 and 181");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {
         return A2(_op["::"],x,y);
      }),
      _L.fromArray([]),
      list);
   };
   var scanl = F3(function (f,
   b,
   xs) {
      return function () {
         var scan1 = F2(function (x,
         accAcc) {
            return function () {
               switch (accAcc.ctor)
               {case "::": return A2(_op["::"],
                    A2(f,x,accAcc._0),
                    accAcc);
                  case "[]":
                  return _L.fromArray([]);}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         });
         return reverse(A3(foldl,
         scan1,
         _L.fromArray([b]),
         xs));
      }();
   });
   var append = F2(function (xs,
   ys) {
      return function () {
         switch (ys.ctor)
         {case "[]": return xs;}
         return A3(foldr,
         F2(function (x,y) {
            return A2(_op["::"],x,y);
         }),
         ys,
         xs);
      }();
   });
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v21._0)
                                   ,_1: _v21._1} : {ctor: "_Tuple2"
                                                   ,_0: _v21._0
                                                   ,_1: A2(_op["::"],
                                                   x,
                                                   _v21._1)};}
               _U.badCase($moduleName,
               "between lines 301 and 303");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v25,
         _v26) {
            return function () {
               switch (_v26.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v25.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],
                                 _v25._0,
                                 _v26._0)
                                 ,_1: A2(_op["::"],
                                 _v25._1,
                                 _v26._1)};}
                       _U.badCase($moduleName,
                       "on line 339, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 339, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 350 and 356");
      }();
   });
   _elm.List.values = {_op: _op
                      ,isEmpty: isEmpty
                      ,length: length
                      ,reverse: reverse
                      ,member: member
                      ,head: head
                      ,tail: tail
                      ,filter: filter
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,append: append
                      ,concat: concat
                      ,intersperse: intersperse
                      ,partition: partition
                      ,unzip: unzip
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,filterMap: filterMap
                      ,concatMap: concatMap
                      ,indexedMap: indexedMap
                      ,foldr: foldr
                      ,foldl: foldl
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,all: all
                      ,any: any
                      ,scanl: scanl
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main",
   $Basics = Elm.Basics.make(_elm),
   $DApprox = Elm.DApprox.make(_elm),
   $DSum = Elm.DSum.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Dist = Elm.Dist.make(_elm),
   $Encounters = Elm.Encounters.make(_elm),
   $Graph = Elm.Graph.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Graphics$Input$Field = Elm.Graphics.Input.Field.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Pokemon = Elm.Pokemon.make(_elm),
   $RNG = Elm.RNG.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Strategy = Elm.Strategy.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm),
   $Worker = Elm.Worker.make(_elm);
   var buildStrategy = function (threshold) {
      return function ($) {
         return $Strategy.simplify(15)($Strategy.frameStrategy($List.map(function (x) {
            return _U.cmp(x,
            threshold) > -1;
         })($)));
      };
   };
   var ChartRequest = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,desiredSlots: a
             ,encounterLength: d
             ,encounterRate: c
             ,encounteredSlots: b};
   });
   var combine = A2($List.foldr,
   $Signal.map2(F2(function (x,y) {
      return A2($List._op["::"],
      x,
      y);
   })),
   $Signal.constant(_L.fromArray([])));
   var dsums = F3(function (n,
   carry,
   state) {
      return _U.eq(n,
      0) ? {ctor: "_Tuple2"
           ,_0: state
           ,_1: _L.fromArray([])} : function () {
         var $ = A3(dsums,
         n - 1,
         carry,
         A2($RNG.rngStep,carry,state)),
         finalState = $._0,
         sums = $._1;
         return {ctor: "_Tuple2"
                ,_0: finalState
                ,_1: A2($List._op["::"],
                $RNG.getDSum(state),
                sums)};
      }();
   });
   var sampleEncounterDSums = function (state) {
      return function () {
         var $ = A3(dsums,
         594,
         1,
         state),
         state$ = $._0,
         sums = $._1;
         var $ = A3(dsums,44,0,state$),
         state$$ = $._0,
         sums$ = $._1;
         var _ = A3(dsums,
         1000,
         0,
         state$$);
         var sums$$ = function () {
            switch (_.ctor)
            {case "_Tuple2": return _._1;}
            _U.badCase($moduleName,
            "on line 362, column 23 to 43");
         }();
         return $List.concat(_L.fromArray([sums
                                          ,sums$
                                          ,sums$$]));
      }();
   };
   var toPath$ = F2(function (n,
   l) {
      return function () {
         switch (l.ctor)
         {case "::":
            return A2($List._op["::"],
              {ctor: "_Tuple2"
              ,_0: n
              ,_1: l._0},
              A2(toPath$,n + 1,l._1));
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 351 and 353");
      }();
   });
   var toPath = toPath$(0);
   var dsumPath = F3(function (n,
   carry,
   state) {
      return toPath($List.map($Basics.toFloat)($Basics.snd(A3(dsums,
      n,
      carry,
      state))));
   });
   var approxProbability = F3(function (rate,
   slots,
   state) {
      return $Dist.probability(function (s) {
         return A2($List.member,
         s,
         slots);
      })($Dist.collapseMap($DSum.dsumSlotDist(rate))($DApprox.dapproxDist(state)));
   });
   var successProbability = F3(function (rate,
   slots,
   state) {
      return $Dist.probability(function (s) {
         return A2($List.member,
         s,
         slots);
      })($Dist.collapseMap($DSum.dsumSlotDist(rate))($DSum.dsumDist(state)));
   });
   var successProbabilities = F5(function (rate,
   slots,
   n,
   carry,
   state) {
      return _U.cmp(n,
      0) < 1 ? _L.fromArray([]) : A2($List._op["::"],
      A3(successProbability,
      rate,
      slots,
      state),
      A5(successProbabilities,
      rate,
      slots,
      n - 1,
      carry,
      A2($DSum.dsumStep,
      carry,
      state)));
   });
   var iterate$ = F3(function (n,
   f,
   x) {
      return _U.eq(n,
      0) ? $Trampoline.Done(x) : $Trampoline.Continue(function (_v6) {
         return function () {
            switch (_v6.ctor)
            {case "_Tuple0":
               return A3(iterate$,
                 n - 1,
                 f,
                 f(x));}
            _U.badCase($moduleName,
            "on line 326, column 37 to 58");
         }();
      });
   });
   var iterate = F3(function (n,
   f,
   x) {
      return $Trampoline.trampoline(A3(iterate$,
      n,
      f,
      x));
   });
   var contentString = function (content) {
      return content.string;
   };
   var initialRNGStates = A2($Signal._op["~"],
   A2($Signal._op["<~"],
   F2(function (hRandomAdd,
   hRandomSub) {
      return A2($List.concatMap,
      function (rDiv) {
         return A2($List.map,
         function (cycle) {
            return A4($RNG.rngState,
            rDiv,
            cycle,
            hRandomAdd,
            hRandomSub);
         },
         _L.fromArray([0,4]));
      },
      _L.fromArray([17]));
   }),
   $Signal.constant(0)),
   $Signal.constant(0));
   var encounteredSlots = $Signal.constant(_L.fromArray([3]));
   var calculateBox = $Signal.mailbox({ctor: "_Tuple0"});
   var calculateButton = A2($Graphics$Input.button,
   A2($Signal.message,
   calculateBox.address,
   {ctor: "_Tuple0"}),
   "Calculate");
   var dsumGraph = A2($Signal._op["<~"],
   function ($) {
      return A2($Graph.graph,
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 1638}),
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 255}))($List.map(function ($) {
         return toPath($List.map($Basics.toFloat)($));
      })($List.map(sampleEncounterDSums)($)));
   },
   A2($Signal.sampleOn,
   calculateBox.signal,
   initialRNGStates));
   var thresholdBox = $Signal.mailbox($Graphics$Input$Field.noContent);
   var thresholdSignal = A2($Signal.map,
   function ($) {
      return $Maybe.withDefault(0.25)($Result.toMaybe($String.toFloat(function (_) {
         return _.string;
      }($))));
   },
   thresholdBox.signal);
   var thresholdInput = A2($Signal._op["<~"],
   A3($Graphics$Input$Field.field,
   $Graphics$Input$Field.defaultStyle,
   $Signal.message(thresholdBox.address),
   "Threshold (default 0.25)"),
   thresholdBox.signal);
   var partialRequestBox = $Signal.mailbox({_: {}
                                           ,desiredSlots: _L.fromArray([2
                                                                       ,4])
                                           ,encounterLength: 594
                                           ,encounterRate: 25
                                           ,encounteredSlots: _L.fromArray([1])});
   var buildRequestList = F3(function (table,
   slots,
   poke) {
      return _L.fromArray([{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot1)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot1.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([1])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot2)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot2.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([2])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot3)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot3.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([3])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot4)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot4.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([4])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot5)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot5.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([5])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot6)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot6.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([6])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot7)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot7.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([7])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot8)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot8.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([8])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot9)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot9.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([9])}}
                          ,{ctor: "_Tuple2"
                           ,_0: $Encounters.displayName(table.slot10)
                           ,_1: {_: {}
                                ,desiredSlots: slots
                                ,encounterLength: A2($Encounters.battleLength,
                                poke,
                                table.slot10.species)
                                ,encounterRate: table.rate
                                ,encounteredSlots: _L.fromArray([10])}}]);
   });
   var leadPokemon = $Signal.constant(A2($Maybe.withDefault,
   $Pokemon.noSpecies,
   A2($Dict.get,
   "Squirtle",
   $Pokemon.speciesByName)));
   var slot10DesiredBox = $Signal.mailbox(false);
   var slot10checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot10DesiredBox.address)),
   slot10DesiredBox.signal);
   var slot9DesiredBox = $Signal.mailbox(false);
   var slot9checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot9DesiredBox.address)),
   slot9DesiredBox.signal);
   var slot8DesiredBox = $Signal.mailbox(false);
   var slot8checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot8DesiredBox.address)),
   slot8DesiredBox.signal);
   var slot7DesiredBox = $Signal.mailbox(false);
   var slot7checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot7DesiredBox.address)),
   slot7DesiredBox.signal);
   var slot6DesiredBox = $Signal.mailbox(false);
   var slot6checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot6DesiredBox.address)),
   slot6DesiredBox.signal);
   var slot5DesiredBox = $Signal.mailbox(false);
   var slot5checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot5DesiredBox.address)),
   slot5DesiredBox.signal);
   var slot4DesiredBox = $Signal.mailbox(true);
   var slot4checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot4DesiredBox.address)),
   slot4DesiredBox.signal);
   var slot3DesiredBox = $Signal.mailbox(false);
   var slot3checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot3DesiredBox.address)),
   slot3DesiredBox.signal);
   var slot2DesiredBox = $Signal.mailbox(true);
   var slot2checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot2DesiredBox.address)),
   slot2DesiredBox.signal);
   var slot1DesiredBox = $Signal.mailbox(false);
   var slot1checkbox = A2($Signal._op["<~"],
   $Graphics$Input.checkbox($Signal.message(slot1DesiredBox.address)),
   slot1DesiredBox.signal);
   var desiredSlots = A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["<~"],
   function (slot1) {
      return function (slot2) {
         return function (slot3) {
            return function (slot4) {
               return function (slot5) {
                  return function (slot6) {
                     return function (slot7) {
                        return function (slot8) {
                           return function (slot9) {
                              return function (slot10) {
                                 return $List.map($Basics.fst)($List.filter($Basics.snd)(_L.fromArray([{ctor: "_Tuple2"
                                                                                                       ,_0: 1
                                                                                                       ,_1: slot1}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 2
                                                                                                       ,_1: slot2}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 3
                                                                                                       ,_1: slot3}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 4
                                                                                                       ,_1: slot4}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 5
                                                                                                       ,_1: slot5}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 6
                                                                                                       ,_1: slot6}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 7
                                                                                                       ,_1: slot7}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 8
                                                                                                       ,_1: slot8}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 9
                                                                                                       ,_1: slot9}
                                                                                                      ,{ctor: "_Tuple2"
                                                                                                       ,_0: 10
                                                                                                       ,_1: slot10}])));
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   },
   slot1DesiredBox.signal),
   slot2DesiredBox.signal),
   slot3DesiredBox.signal),
   slot4DesiredBox.signal),
   slot5DesiredBox.signal),
   slot6DesiredBox.signal),
   slot7DesiredBox.signal),
   slot8DesiredBox.signal),
   slot9DesiredBox.signal),
   slot10DesiredBox.signal);
   var requestSignal = A2($Signal._op["~"],
   A2($Signal._op["<~"],
   F2(function (partialRequest,
   slots) {
      return _U.replace([["desiredSlots"
                         ,slots]],
      partialRequest);
   }),
   partialRequestBox.signal),
   desiredSlots);
   var workerInputSignal = A2($Signal._op["<~"],
   function (req) {
      return function () {
         var initialState = $DSum.conditionDSum(function (x) {
            return $Dist.probability(function (s) {
               return A2($List.member,
               s,
               req.encounteredSlots);
            })(A2($DSum.dsumSlotDist,
            req.encounterRate,
            x));
         })($DSum.initialRNGMix);
         return {ctor: "_Tuple4"
                ,_0: req
                ,_1: 0
                ,_2: initialState
                ,_3: _L.fromArray([])};
      }();
   },
   A2($Signal.sampleOn,
   calculateBox.signal,
   requestSignal));
   var successProbabilitiesWorker = function () {
      var workerStep = function (_v8) {
         return function () {
            switch (_v8.ctor)
            {case "_Tuple4":
               return function () {
                    var acc$ = _U.cmp(_v8._1,
                    _v8._0.encounterLength + $Encounters.framesBeforeMove) < 0 ? _v8._3 : A2($List._op["::"],
                    A3(successProbability,
                    _v8._0.encounterRate,
                    _v8._0.desiredSlots,
                    _v8._2),
                    _v8._3);
                    var state$ = _U.cmp(_v8._1,
                    _v8._0.encounterLength) < 0 ? A2($DSum.dsumStep,
                    1,
                    _v8._2) : _U.eq(_v8._1,
                    _v8._0.encounterLength) ? $Dist.collapseMap($DSum.randomizeBand)(A2($DSum.dsumStep,
                    1,
                    _v8._2)) : A2($DSum.dsumStep,
                    0,
                    _v8._2);
                    return _U.cmp(_v8._1,
                    _v8._0.encounterLength + $Encounters.framesBeforeMove + 1000) < 0 ? $Worker.Working({ctor: "_Tuple4"
                                                                                                        ,_0: _v8._0
                                                                                                        ,_1: _v8._1 + 1
                                                                                                        ,_2: state$
                                                                                                        ,_3: acc$}) : $Worker.Done($List.reverse(_v8._3));
                 }();}
            _U.badCase($moduleName,
            "between lines 401 and 417");
         }();
      };
      return A2($Worker.createWorker,
      workerInputSignal,
      workerStep);
   }();
   var successProbabilitiesSignal = A2($Signal.map,
   function (state) {
      return function () {
         var _v14 = $Basics.snd(state);
         switch (_v14.ctor)
         {case "Done": return _v14._0;
            case "Unstarted":
            return _L.fromArray([]);
            case "Working":
            switch (_v14._0.ctor)
              {case "_Tuple4":
                 return $List.reverse(_v14._0._3);}
              break;}
         _U.badCase($moduleName,
         "between lines 423 and 427");
      }();
   },
   successProbabilitiesWorker.state);
   var successGraph = A2($Signal._op["<~"],
   function ($) {
      return A2($Graph.graph,
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 1000}),
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 1}))(function (x) {
         return _L.fromArray([x]);
      }(toPath($)));
   },
   successProbabilitiesSignal);
   var strategy = A2($Signal._op["~"],
   A2($Signal._op["<~"],
   buildStrategy,
   thresholdSignal),
   A2($Signal.map,
   $Maybe.withDefault(_L.fromArray([])),
   successProbabilitiesWorker.signal));
   var strategy2 = A2($Signal._op["<~"],
   $Strategy.roundStrategy(17),
   strategy);
   var stepStrategy = A2($Signal._op["<~"],
   $List.map(function (s) {
      return {ctor: "_Tuple2"
             ,_0: s.frames / 17 | 0
             ,_1: s.inGrass};
   }),
   strategy2);
   var approxInputSignal = A2($Signal._op["<~"],
   function (req) {
      return function () {
         var initialState = function (s) {
            return _U.replace([["muDist"
                               ,A2($Dist.map,
                               function ($) {
                                  return $Basics.toFloat($Basics.round($));
                               },
                               s.muDist)]],
            s);
         }(A2($DApprox.advanceDApprox,
         $DApprox.outsideSlopeDist,
         $Encounters.framesBeforeMove)(A2($DApprox.advanceDApprox,
         $DApprox.insideSlopeDist,
         req.encounterLength)($DApprox.conditionDApprox(function (x) {
            return $Dist.probability(function (s) {
               return A2($List.member,
               s,
               req.encounteredSlots);
            })(A2($DSum.dsumSlotDist,
            req.encounterRate,
            x));
         })($DApprox.initialDApproxState))));
         return {ctor: "_Tuple4"
                ,_0: req
                ,_1: 0
                ,_2: initialState
                ,_3: _L.fromArray([])};
      }();
   },
   A2($Signal.sampleOn,
   calculateBox.signal,
   requestSignal));
   var approxProbabilitiesWorker = function () {
      var workerStep = function (_v21) {
         return function () {
            switch (_v21.ctor)
            {case "_Tuple4":
               return function () {
                    var frameState = A3($DApprox.advanceDApprox,
                    $DApprox.outsideSlopeDist,
                    _v21._1,
                    _v21._2);
                    return _U.cmp(_v21._1,
                    1000) < 0 ? $Worker.Working({ctor: "_Tuple4"
                                                ,_0: _v21._0
                                                ,_1: _v21._1 + 1
                                                ,_2: _v21._2
                                                ,_3: A2($List._op["::"],
                                                A3(approxProbability,
                                                _v21._0.encounterRate,
                                                _v21._0.desiredSlots,
                                                frameState),
                                                _v21._3)}) : $Worker.Done($List.reverse(_v21._3));
                 }();}
            _U.badCase($moduleName,
            "between lines 452 and 456");
         }();
      };
      return A2($Worker.createWorker,
      approxInputSignal,
      workerStep);
   }();
   var approxProbabilitiesSignal = A2($Signal.map,
   function (state) {
      return function () {
         var _v27 = $Basics.snd(state);
         switch (_v27.ctor)
         {case "Done": return _v27._0;
            case "Unstarted":
            return _L.fromArray([]);
            case "Working":
            switch (_v27._0.ctor)
              {case "_Tuple4":
                 return $List.reverse(_v27._0._3);}
              break;}
         _U.badCase($moduleName,
         "between lines 462 and 466");
      }();
   },
   approxProbabilitiesWorker.state);
   var approxGraph = A2($Signal._op["<~"],
   function ($) {
      return A2($Graph.graph,
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 1000}),
      $Maybe.Just({ctor: "_Tuple2"
                  ,_0: 0
                  ,_1: 1}))(function (x) {
         return _L.fromArray([x]);
      }(toPath($)));
   },
   approxProbabilitiesSignal);
   var encounterTable = $Signal.constant($Encounters.route22table);
   var desiredSlotsInputs = A2($Signal._op["<~"],
   $Graphics$Element.flow($Graphics$Element.right),
   combine(_L.fromArray([slot1checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot1;
                           }($))));
                        },
                        encounterTable)
                        ,slot2checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot2;
                           }($))));
                        },
                        encounterTable)
                        ,slot3checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot3;
                           }($))));
                        },
                        encounterTable)
                        ,slot4checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot4;
                           }($))));
                        },
                        encounterTable)
                        ,slot5checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot5;
                           }($))));
                        },
                        encounterTable)
                        ,slot6checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot6;
                           }($))));
                        },
                        encounterTable)
                        ,slot7checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot7;
                           }($))));
                        },
                        encounterTable)
                        ,slot8checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot8;
                           }($))));
                        },
                        encounterTable)
                        ,slot9checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot9;
                           }($))));
                        },
                        encounterTable)
                        ,slot10checkbox
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.centered($Text.fromString($Encounters.displayName(function (_) {
                              return _.slot10;
                           }($))));
                        },
                        encounterTable)])));
   var requestDropDown = A2($Signal._op["<~"],
   $Graphics$Input.dropDown($Signal.message(partialRequestBox.address)),
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["<~"],
   buildRequestList,
   encounterTable),
   desiredSlots),
   leadPokemon));
   var main = A2($Signal._op["<~"],
   $Graphics$Element.flow($Graphics$Element.down),
   combine(_L.fromArray([requestDropDown
                        ,desiredSlotsInputs
                        ,$Signal.constant(calculateButton)
                        ,thresholdInput
                        ,A2($Signal._op["<~"],
                        function ($) {
                           return $Graphics$Element.show($DApprox.dapproxDist(function (_v34) {
                              return function () {
                                 switch (_v34.ctor)
                                 {case "_Tuple4":
                                    return _v34._2;}
                                 _U.badCase($moduleName,
                                 "on line 490, column 48 to 49");
                              }();
                           }($)));
                        },
                        approxInputSignal)
                        ,A2($Signal._op["<~"],
                        A2($Graph.drawGraph,700,400),
                        successGraph)
                        ,A2($Signal._op["<~"],
                        A2($Graph.drawGraph,700,400),
                        approxGraph)
                        ,A2($Signal.map,
                        $Graphics$Element.show,
                        strategy)
                        ,A2($Signal.map,
                        $Graphics$Element.show,
                        strategy2)
                        ,A2($Signal.map,
                        $Graphics$Element.show,
                        stepStrategy)])));
   _elm.Main.values = {_op: _op
                      ,encounterTable: encounterTable
                      ,slot1DesiredBox: slot1DesiredBox
                      ,slot2DesiredBox: slot2DesiredBox
                      ,slot3DesiredBox: slot3DesiredBox
                      ,slot4DesiredBox: slot4DesiredBox
                      ,slot5DesiredBox: slot5DesiredBox
                      ,slot6DesiredBox: slot6DesiredBox
                      ,slot7DesiredBox: slot7DesiredBox
                      ,slot8DesiredBox: slot8DesiredBox
                      ,slot9DesiredBox: slot9DesiredBox
                      ,slot10DesiredBox: slot10DesiredBox
                      ,slot1checkbox: slot1checkbox
                      ,slot2checkbox: slot2checkbox
                      ,slot3checkbox: slot3checkbox
                      ,slot4checkbox: slot4checkbox
                      ,slot5checkbox: slot5checkbox
                      ,slot6checkbox: slot6checkbox
                      ,slot7checkbox: slot7checkbox
                      ,slot8checkbox: slot8checkbox
                      ,slot9checkbox: slot9checkbox
                      ,slot10checkbox: slot10checkbox
                      ,desiredSlotsInputs: desiredSlotsInputs
                      ,desiredSlots: desiredSlots
                      ,leadPokemon: leadPokemon
                      ,buildRequestList: buildRequestList
                      ,partialRequestBox: partialRequestBox
                      ,requestSignal: requestSignal
                      ,requestDropDown: requestDropDown
                      ,thresholdBox: thresholdBox
                      ,thresholdSignal: thresholdSignal
                      ,thresholdInput: thresholdInput
                      ,calculateBox: calculateBox
                      ,encounteredSlots: encounteredSlots
                      ,calculateButton: calculateButton
                      ,initialRNGStates: initialRNGStates
                      ,contentString: contentString
                      ,iterate: iterate
                      ,iterate$: iterate$
                      ,successProbability: successProbability
                      ,approxProbability: approxProbability
                      ,successProbabilities: successProbabilities
                      ,toPath: toPath
                      ,toPath$: toPath$
                      ,dsumPath: dsumPath
                      ,sampleEncounterDSums: sampleEncounterDSums
                      ,dsums: dsums
                      ,combine: combine
                      ,dsumGraph: dsumGraph
                      ,ChartRequest: ChartRequest
                      ,workerInputSignal: workerInputSignal
                      ,successProbabilitiesWorker: successProbabilitiesWorker
                      ,successProbabilitiesSignal: successProbabilitiesSignal
                      ,successGraph: successGraph
                      ,approxInputSignal: approxInputSignal
                      ,approxProbabilitiesWorker: approxProbabilitiesWorker
                      ,approxProbabilitiesSignal: approxProbabilitiesSignal
                      ,approxGraph: approxGraph
                      ,buildStrategy: buildStrategy
                      ,strategy: strategy
                      ,strategy2: strategy2
                      ,stepStrategy: stepStrategy
                      ,main: main};
   return _elm.Main.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 47");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 66");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 66");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 78");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a/b)|0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error("Cannot perform mod 0. Division by zero error.");
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a,b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a,b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity
	}

	function truncate(n)
	{
		return n|0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees:  degrees,
		turns:  turns,
		fromPolar:  fromPolar,
		toPolar:  toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode : function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode   : function(c) { return c.charCodeAt(0); },
		toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
		toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Show.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary:F3(watchSummary),
	};
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx,style,path)
	{
		(style.dashing.ctor === '[]')
			? trace(ctx, path)
			: customLineHelp(ctx, style, path);
		ctx.scale(1,-1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0,y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(ctx, style);
		ctx.scale(1,-1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0,-2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w/2,
			destY = -h/2,
			destW = w,
			destH = h;

		ctx.scale(1,-1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta);
		}
		if (scale !== 1)
		{
			ctx.scale(scale,scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i].form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i];
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w,h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = "block";
		canvas.style.position = "absolute";
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w,h,div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w,h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w,h,div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w,h,forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w:w, h:h, forms:forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};

};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	function createNode(elementType)
	{
		var node = document.createElement(elementType);
		node.style.padding = "0";
		node.style.margin = "0";
		return node;
	}


	function newElement(width, height, elementPrim)
	{
		return {
			_: {},
			element: elementPrim,
			props: {
				_: {},
				id: Utils.guid(),
				width: width,
				height: height,
				opacity: 1,
				color: Maybe.Nothing,
				href: "",
				tag: "",
				hover: Utils.Tuple0,
				click: Utils.Tuple0
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width |0) + 'px';
		node.style.height = (height|0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img,props.width,props.height,img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = "block";
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = "hidden";

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw)|0) + 'px';
			img.style.height = ((this.height * sh)|0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
			img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp    : goDown,
		DDown  : goDown,
		DLeft  : goRight,
		DRight : goRight,
		DIn    : goOut,
		DOut   : goOut
	};
	function needsReversal(dir)
	{
		return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
	}

	function flow(dir,elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir == goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === "Absolute"
			? pos._0 + "px"
			: (pos._0 * 100) + "%";
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos,elem,e)
	{
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w/2)|0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h/2)|0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos,elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var guid = elem.guid;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = "hidden";
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(elem)
	{
		return setProps(elem, makeElement(elem));
	}
	function makeElement(e)
	{
		var elem = e.element;
		switch(elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, curr, next)
	{
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(next);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch(nextE.ctor)
		{
			case "Spacer":
				updateProps(node, curr, next);
				return rootNode;

			case "RawHtml":
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Image":
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE,currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(next);
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Flow":
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i].element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(next);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(next);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i-- ;)
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Container":
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case "Custom":
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(next);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width|0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height|0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor :'RawHtml',
				html : Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		}
	}

	function markdown(text)
	{
		var raw = {
			ctor:'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	function htmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + "px";
		}
		temp.style.visibility = "hidden";
		temp.style.styleFloat = "left";
		temp.style.cssFloat   = "left";

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
		var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w,h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};

};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Input = Elm.Native.Graphics.Input || {};

// definition
Elm.Native.Graphics.Input.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	if ('values' in Elm.Native.Graphics.Input) {
		return Elm.Native.Graphics.Input.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Signal = Elm.Native.Signal.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var Element = Elm.Native.Graphics.Element.make(localRuntime);


	function renderDropDown(model)
	{
		var drop = Element.createNode('select');
		drop.style.border = '0 solid';
		drop.style.pointerEvents = 'auto';
		drop.style.display = 'block';

		drop.elm_values = List.toArray(model.values);
		drop.elm_handler = model.handler;
		var values = drop.elm_values;

		for (var i = 0; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
			drop.appendChild(option);
		}
		drop.addEventListener('change', function() {
			Signal.sendMessage(drop.elm_handler(drop.elm_values[drop.selectedIndex]._1));
		});

		return drop;
	}

	function updateDropDown(node, oldModel, newModel)
	{
		node.elm_values = List.toArray(newModel.values);
		node.elm_handler = newModel.handler;

		var values = node.elm_values;
		var kids = node.childNodes;
		var kidsLength = kids.length;

		var i = 0;
		for (; i < kidsLength && i < values.length; ++i)
		{
			var option = kids[i];
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
		}
		for (; i < kidsLength; ++i)
		{
			node.removeChild(node.lastChild);
		}
		for (; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
			node.appendChild(option);
		}
		return node;
	}

	function dropDown(handler, values)
	{
		return A3(Element.newElement, 100, 24, {
			ctor: 'Custom',
			type: 'DropDown',
			render: renderDropDown,
			update: updateDropDown,
			model: {
				values: values,
				handler: handler
			}
		});
	}

	function renderButton(model)
	{
		var node = Element.createNode('button');
		node.style.display = 'block';
		node.style.pointerEvents = 'auto';
		node.elm_message = model.message;
		function click()
		{
			Signal.sendMessage(node.elm_message);
		}
		node.addEventListener('click', click);
		node.innerHTML = model.text;
		return node;
	}

	function updateButton(node, oldModel, newModel)
	{
		node.elm_message = newModel.message;
		var txt = newModel.text;
		if (oldModel.text !== txt)
		{
			node.innerHTML = txt;
		}
		return node;
	}

	function button(message, text)
	{
		return A3(Element.newElement, 100, 40, {
			ctor: 'Custom',
			type: 'Button',
			render: renderButton,
			update: updateButton,
			model: {
				message: message,
				text:text
			}
		});
	}

	function renderCustomButton(model)
	{
		var btn = Element.createNode('div');
		btn.style.pointerEvents = 'auto';
		btn.elm_message = model.message;

		btn.elm_up    = Element.render(model.up);
		btn.elm_hover = Element.render(model.hover);
		btn.elm_down  = Element.render(model.down);

		btn.elm_up.style.display = 'block';
		btn.elm_hover.style.display = 'none';
		btn.elm_down.style.display = 'none';

		btn.appendChild(btn.elm_up);
		btn.appendChild(btn.elm_hover);
		btn.appendChild(btn.elm_down);

		function swap(visibleNode, hiddenNode1, hiddenNode2)
		{
			visibleNode.style.display = 'block';
			hiddenNode1.style.display = 'none';
			hiddenNode2.style.display = 'none';
		}

		var overCount = 0;
		function over(e)
		{
			if (overCount++ > 0) return;
			swap(btn.elm_hover, btn.elm_down, btn.elm_up);
		}
		function out(e)
		{
			if (btn.contains(e.toElement || e.relatedTarget)) return;
			overCount = 0;
			swap(btn.elm_up, btn.elm_down, btn.elm_hover);
		}
		function up()
		{
			swap(btn.elm_hover, btn.elm_down, btn.elm_up);
			Signal.sendMessage(btn.elm_message);
		}
		function down()
		{
			swap(btn.elm_down, btn.elm_hover, btn.elm_up);
		}

		btn.addEventListener('mouseover', over);
		btn.addEventListener('mouseout' , out);
		btn.addEventListener('mousedown', down);
		btn.addEventListener('mouseup'  , up);

		return btn;
	}

	function updateCustomButton(node, oldModel, newModel)
	{
		node.elm_message = newModel.message;

		var kids = node.childNodes;
		var styleUp    = kids[0].style.display;
		var styleHover = kids[1].style.display;
		var styleDown  = kids[2].style.display;

		Element.updateAndReplace(kids[0], oldModel.up, newModel.up);
		Element.updateAndReplace(kids[1], oldModel.hover, newModel.hover);
		Element.updateAndReplace(kids[2], oldModel.down, newModel.down);

		var kids = node.childNodes;
		kids[0].style.display = styleUp;
		kids[1].style.display = styleHover;
		kids[2].style.display = styleDown;

		return node;
	}

	function max3(a,b,c)
	{
		var ab = a > b ? a : b;
		return ab > c ? ab : c;
	}

	function customButton(message, up, hover, down)
	{
		return A3(Element.newElement,
				  max3(up.props.width, hover.props.width, down.props.width),
				  max3(up.props.height, hover.props.height, down.props.height),
				  { ctor: 'Custom',
					type: 'CustomButton',
					render: renderCustomButton,
					update: updateCustomButton,
					model: {
						message: message,
						up: up,
						hover: hover,
						down: down
					}
				  });
	}

	function renderCheckbox(model)
	{
		var node = Element.createNode('input');
		node.type = 'checkbox';
		node.checked = model.checked;
		node.style.display = 'block';
		node.style.pointerEvents = 'auto';
		node.elm_handler = model.handler;
		function change()
		{
			Signal.sendMessage(node.elm_handler(node.checked));
		}
		node.addEventListener('change', change);
		return node;
	}

	function updateCheckbox(node, oldModel, newModel)
	{
		node.elm_handler = newModel.handler;
		node.checked = newModel.checked;
		return node;
	}

	function checkbox(handler, checked)
	{
		return A3(Element.newElement, 13, 13, {
			ctor: 'Custom',
			type: 'CheckBox',
			render: renderCheckbox,
			update: updateCheckbox,
			model: { handler:handler, checked:checked }
		});
	}

	function setRange(node, start, end, dir)
	{
		if (node.parentNode)
		{
			node.setSelectionRange(start, end, dir);
		}
		else
		{
			setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
		}
	}

	function updateIfNeeded(css, attribute, latestAttribute)
	{
		if (css[attribute] !== latestAttribute)
		{
			css[attribute] = latestAttribute;
		}
	}
	function cssDimensions(dimensions)
	{
		return dimensions.top    + 'px ' +
			   dimensions.right  + 'px ' +
			   dimensions.bottom + 'px ' +
			   dimensions.left   + 'px';
	}
	function updateFieldStyle(css, style)
	{
		updateIfNeeded(css, 'padding', cssDimensions(style.padding));

		var outline = style.outline;
		updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
		updateIfNeeded(css, 'border-color', Color.toCss(outline.color));
		updateIfNeeded(css, 'border-radius', outline.radius + 'px');

		var highlight = style.highlight;
		if (highlight.width === 0)
		{
			css.outline = 'none';
		}
		else
		{
			updateIfNeeded(css, 'outline-width', highlight.width + 'px');
			updateIfNeeded(css, 'outline-color', Color.toCss(highlight.color));
		}

		var textStyle = style.style;
		updateIfNeeded(css, 'color', Color.toCss(textStyle.color));
		if (textStyle.typeface.ctor !== '[]')
		{
			updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
		}
		if (textStyle.height.ctor !== "Nothing")
		{
			updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
		}
		updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
		updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
		if (textStyle.line.ctor !== 'Nothing')
		{
			updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
		}
	}

	function renderField(model)
	{
		var field = Element.createNode('input');
		updateFieldStyle(field.style, model.style);
		field.style.borderStyle = 'solid';
		field.style.pointerEvents = 'auto';

		field.type = model.type;
		field.placeholder = model.placeHolder;
		field.value = model.content.string;

		field.elm_handler = model.handler;
		field.elm_old_value = field.value;

		function inputUpdate(event)
		{
			var curr = field.elm_old_value;
			var next = field.value;
			if (curr === next)
			{
				return;
			}

			var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
			var start = field.selectionStart;
			var end = field.selectionEnd;
			field.value = field.elm_old_value;

			Signal.sendMessage(field.elm_handler({
				_:{},
				string: next,
				selection: {
					_:{},
					start: start,
					end: end,
					direction: { ctor: direction }
				}
			}));
		}

		field.addEventListener('input', inputUpdate);
		field.addEventListener('focus', function() {
			field.elm_hasFocus = true;
		});
		field.addEventListener('blur', function() {
			field.elm_hasFocus = false;
		});

		return field;
	}

	function updateField(field, oldModel, newModel)
	{
		if (oldModel.style !== newModel.style)
		{
			updateFieldStyle(field.style, newModel.style);
		}
		field.elm_handler = newModel.handler;

		field.type = newModel.type;
		field.placeholder = newModel.placeHolder;
		var value = newModel.content.string;
		field.value = value;
		field.elm_old_value = value;
		if (field.elm_hasFocus)
		{
			var selection = newModel.content.selection;
			var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
			setRange(field, selection.start, selection.end, direction);
		}
		return field;
	}

	function mkField(type)
	{
		function field(style, handler, placeHolder, content)
		{
			var padding = style.padding;
			var outline = style.outline.width;
			var adjustWidth = padding.left + padding.right + outline.left + outline.right;
			var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
			return A3(Element.newElement, 200, 30, {
				ctor: 'Custom',
				type: type + 'Field',
				adjustWidth: adjustWidth,
				adjustHeight: adjustHeight,
				render: renderField,
				update: updateField,
				model: {
					handler:handler,
					placeHolder:placeHolder,
					content:content,
					style:style,
					type:type
				}
			});
		}
		return F4(field);
	}

	function hoverable(handler, elem)
	{
		function onHover(bool)
		{
			Signal.sendMessage(handler(bool));
		}
		var props = Utils.replace([['hover',onHover]], elem.props);
		return {
			props: props,
			element: elem.element
		};
	}

	function clickable(message, elem)
	{
		function onClick()
		{
			Signal.sendMessage(message);
		}
		var props = Utils.replace([['click',onClick]], elem.props);
		return {
			props: props,
			element: elem.element
		};
	}

	return Elm.Native.Graphics.Input.values = {
		button: F2(button),
		customButton: F4(customButton),
		checkbox: F2(checkbox),
		dropDown: F2(dropDown),
		field: mkField('text'),
		email: mkField('email'),
		password: mkField('password'),
		hoverable: F2(hoverable),
		clickable: F2(clickable)
	};

};

Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo,hi)
	{
		var lst = Nil;
		if (lo <= hi)
		{
			do { lst = Cons(hi,lst) } while (hi-->lo);
		}
		return lst
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function any(pred, xs)
	{
		while (xs.ctor !== '[]')
		{
			if (pred(xs._0))
			{
				return true;
			}
			xs = xs._1;
		}
		return false;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}

	function drop(n, xs)
	{
		while (xs.ctor !== '[]' && n > 0)
		{
			xs = xs._1;
			--n;
		}
		return xs;
	}

	function repeat(n, x)
	{
		var arr = [];
		var pattern = [x];
		while (n > 0)
		{
			if (n & 1)
			{
				arr = arr.concat(pattern);
			}
			n >>= 1, pattern = pattern.concat(pattern);
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil:Nil,
		Cons:Cons,
		cons:F2(Cons),
		toArray:toArray,
		fromArray:fromArray,
		range:range,

		foldl:F3(foldl),
		foldr:F3(foldr),

		any:F2(any),
		map2:F3(map2),
		map3:F4(map3),
		map4:F5(map4),
		map5:F6(map5),
		sortBy:F2(sortBy),
		sortWith:F2(sortWith),
		take:F2(take),
		drop:F2(drop),
		repeat:F2(repeat)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;

};

Elm.Native.Port = {};
Elm.Native.Port.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				"Port Error:\n" +
				"No argument was given for the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You need to provide an initial value!\n\n" +
				"Find out more about ports here <http://elm-lang.org/learn/Ports.elm>"
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				"Port Error:\n" +
				"Regarding the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You just sent the value:\n\n" +
				"    " + JSON.stringify(value) + "\n\n" +
				"but it cannot be converted to the necessary type.\n" +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};


if (!Elm.fullscreen) {

	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];
				// elm.swap = newElm.swap;
				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild == 'undefined')
				{
					console.log(error.message);
				}
				else
				{
					container.appendChild(errorNode(error.message));
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		};

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode("Open the developer console for more details."));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.props)
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000/60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							"Unexpected draw callback.\n" +
							"Please report this to <https://github.com/elm-lang/core/issues>."
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld,nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld,nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b) } }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a,b,c) }}
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a,b,c,d) }}}
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a,b,c,d,e,f) }}}}}
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a,b,c,d,e,f,g,h)}}}}}}}
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun,a,b)
	{
		return fun.arity === 2
			? fun.func(a,b)
			: fun(a)(b);
	}
	function A3(fun,a,b,c)
	{
		return fun.arity === 3
			? fun.func(a,b,c)
			: fun(a)(b)(c);
	}
	function A4(fun,a,b,c,d)
	{
		return fun.arity === 4
			? fun.func(a,b,c,d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun,a,b,c,d,e)
	{
		return fun.arity === 5
			? fun.func(a,b,c,d,e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun,a,b,c,d,e,f)
	{
		return fun.arity === 6
			? fun.func(a,b,c,d,e,f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun,a,b,c,d,e,f,g)
	{
		return fun.arity === 7
			? fun.func(a,b,c,d,e,f,g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun,a,b,c,d,e,f,g,h)
	{
		return fun.arity === 8
			? fun.func(a,b,c,d,e,f,g,h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun,a,b,c,d,e,f,g,h,i)
	{
		return fun.arity === 9
			? fun.func(a,b,c,d,e,f,g,h,i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native.Show = {};
Elm.Native.Show.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Show = localRuntime.Native.Show || {};
	if (localRuntime.Native.Show.values)
	{
		return localRuntime.Native.Show.values;
	}

	var _Array;
	var Dict;
	var List;
	var Utils = Elm.Native.Utils.make(localRuntime);

	var toString = function(v)
	{
		var type = typeof v;
		if (type === "function")
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === "boolean")
		{
			return v ? "True" : "False";
		}
		else if (type === "number")
		{
			return v + "";
		}
		else if ((v instanceof String) && v.isChar)
		{
			return "'" + addSlashes(v, true) + "'";
		}
		else if (type === "string")
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === "object" && '_' in v && probablyPublic(v))
		{
			var output = [];
			for (var k in v._)
			{
				for (var i = v._[k].length; i--; )
				{
					output.push(k + " = " + toString(v._[k][i]));
				}
			}
			for (var k in v)
			{
				if (k === '_') continue;
				output.push(k + " = " + toString(v[k]));
			}
			if (output.length === 0)
			{
				return "{}";
			}
			return "{ " + output.join(", ") + " }";
		}
		else if (type === "object" && 'ctor' in v)
		{
			if (v.ctor.substring(0,6) === "_Tuple")
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return "(" + output.join(",") + ")";
			}
			else if (v.ctor === "_Array")
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return "Array.fromList " + toString(list);
			}
			else if (v.ctor === "::")
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === "::")
				{
					output += "," + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === "[]")
			{
				return "[]";
			}
			else if (v.ctor === "RBNode" || v.ctor === "RBEmpty")
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				if (!List)
				{
					List = Elm.List.make(localRuntime);
				}
				var list = Dict.toList(v);
				var name = "Dict";
				if (list.ctor === "::" && list._0._1.ctor === "_Tuple0")
				{
					name = "Set";
					list = A2(List.map, function(x){return x._0}, list);
				}
				return name + ".fromList " + toString(list);
			}
			else if (v.ctor.slice(0,5) === "Text:")
			{
				return '<text>'
			}
			else
			{
				var output = "";
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<Signal>';
		}
		return "<internal structure>";
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, "\\'")
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}

	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v)
		{
			return false;
		}
		else if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v)
		{
			return false;
		}
		else if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v)
		{
			return false;
		}
		return true;
	}

	return localRuntime.Native.Show.values = {
		toString: toString
	};
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			_: {},
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a,b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a,b,c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a,b,c,d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a,b,c,d,e]);
	}



	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.String = {};
Elm.Native.String.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr,str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd;
		return (hd = str[0])
			? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
			: Maybe.Nothing;
	}
	function append(a,b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f,str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred,str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f,b,str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f,b,str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}

	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}

	function slice(start, end, str)
	{
		return str.slice(start,end);
	}
	function left(n, str)
	{
		return n < 1 ? "" : str.slice(0,n);
	}
	function right(n, str)
	{
		return n < 1 ? "" : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0,-n);
	}

	function pad(n,chr,str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
	}
	function padRight(n,chr,str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n,chr,str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.trimLeft();
	}
	function trimRight(str)
	{
		return str.trimRight();
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Task = {};
Elm.Native.Task.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			setTimeout(function() {
				if (workQueue.length > 0)
				{
					runTask(workQueue[0], onComplete);
				}
			}, 0);
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== "Nothing")
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);;
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas['href'];
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length == 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  "&#38;")
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  "&#39;")
			.replace(/</g,  "&#60;")
			.replace(/>/g,  "&#62;");
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length == 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] == ' ')
		{
			arr[0] = "&nbsp;"
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] == ' ' && arr[i-1] == ' ')
			{
				arr[i-1] = arr[i-1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] == ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length-1] === " ")
		{
			return arr.slice(0,-1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(localRuntime)
{

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input,state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value,initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2( NS.map, fst, NS.timestamp(ticker) );
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new window.Date(t); },
		read: read
	};

};

Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Trampoline = localRuntime.Native.Trampoline || {};
	if (localRuntime.Native.Trampoline.values)
	{
		return localRuntime.Native.Trampoline.values;
	}

	// trampoline : Trampoline a -> a
	function trampoline(t)
	{
		var tramp = t;
		while(true)
		{
			switch(tramp.ctor)
			{
				case "Done":
					return tramp._0;
				case "Continue":
					tramp = tramp._0({ ctor: "_Tuple0" });
					continue;
			}
		}
	}

	return localRuntime.Native.Trampoline.values = {
		trampoline: trampoline
	};
};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1,0,0,0,1,0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t,m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
					  m21*c + m22*s, -m21*s + m22*c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity:identity,
		matrix:F6(matrix),
		rotation:rotation,
		multiply:F2(multiply)
		/*
		transform:F7(transform),
		rotate:F2(rotate),
		move:F2(move),
		scale:F2(scale),
		scaleX:F2(scaleX),
		scaleY:F2(scaleY),
		reflectX:reflectX,
		reflectY:reflectY
		*/
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}

	function eq(l,r)
	{
		var stack = [{'x': l, 'y': r}]
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === "object")
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];

	function compare(x,y)
	{
		return {
			ctor: ord[cmp(x,y)+1]
		};
	}

	function cmp(x,y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === "::" || x.ctor === "[]")
		{
			while (true)
			{
				if (x.ctor === "[]" && y.ctor === "[]")
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0,6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	var Tuple0 = {
		ctor: "_Tuple0"
	};

	function Tuple2(x,y)
	{
		return {
			ctor: "_Tuple2",
			_0: x,
			_1: y
		};
	}

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}

	var count = 0;
	function guid(_)
	{
		return count++
	}

	function copy(oldRecord)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = key === '_'
				? copy(oldRecord._)
				: oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}

	function remove(key, oldRecord)
	{
		var record = copy(oldRecord);
		if (key in record._)
		{
			record[key] = record._[key][0];
			record._[key] = record._[key].slice(1);
			if (record._[key].length === 0)
			{
				delete record._[key];
			}
		}
		else
		{
			delete record[key];
		}
		return record;
	}

	function replace(keyValuePairs, oldRecord)
	{
		var record = copy(oldRecord);
		for (var i = keyValuePairs.length; i--; )
		{
			var pair = keyValuePairs[i];
			record[pair[0]] = pair[1];
		}
		return record;
	}

	function insert(key, value, oldRecord)
	{
		var newRecord = copy(oldRecord);
		if (key in newRecord)
		{
			var values = newRecord._[key];
			var copiedValues = values ? values.slice(0) : [];
			newRecord._[key] = [newRecord[key]].concat(copiedValues);
		}
		newRecord[key] = value;
		return newRecord;
	}

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor:'[]' };

	function Cons(hd,tl)
	{
		return {
			ctor: "::",
			_0: hd,
			_1: tl
		};
	}

	function append(xs,ys)
	{
		// append Strings
		if (typeof xs === "string")
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0,5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}



		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}

	//// RUNTIME ERRORS ////

	function indent(lines)
	{
		return '\n' + lines.join('\n');
	}

	function badCase(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in case-expression.',
			'Make sure your patterns cover every case!'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}

	function badIf(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in multi-way-if expression.',
			'It is best to use \'otherwise\' as the last branch of multi-way-if.'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}


	function badPort(expected, received)
	{
		var msg = indent([
			'Expecting ' + expected + ' but was given ',
			JSON.stringify(received)
		]);
		throw new Error('Runtime error when sending values through a port.' + msg);
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		copy: copy,
		remove: remove,
		replace: replace,
		insert: insert,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		append: F2(append),

		badCase: badCase,
		badIf: badIf,
		badPort: badPort
	};
};

Elm.Pokemon = Elm.Pokemon || {};
Elm.Pokemon.make = function (_elm) {
   "use strict";
   _elm.Pokemon = _elm.Pokemon || {};
   if (_elm.Pokemon.values)
   return _elm.Pokemon.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Pokemon",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var speciesList = _L.fromArray([{_: {}
                                   ,cryDiff: 49
                                   ,indexNumber: 1
                                   ,name: "Rhydon"
                                   ,pokedexNumber: 112}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 2
                                   ,name: "Kangaskhan"
                                   ,pokedexNumber: 115}
                                  ,{_: {}
                                   ,cryDiff: 16
                                   ,indexNumber: 3
                                   ,name: "Nidoran M"
                                   ,pokedexNumber: 32}
                                  ,{_: {}
                                   ,cryDiff: 6
                                   ,indexNumber: 4
                                   ,name: "Clefairy"
                                   ,pokedexNumber: 35}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 5
                                   ,name: "Spearow"
                                   ,pokedexNumber: 21}
                                  ,{_: {}
                                   ,cryDiff: 57
                                   ,indexNumber: 6
                                   ,name: "Voltorb"
                                   ,pokedexNumber: 100}
                                  ,{_: {}
                                   ,cryDiff: 59
                                   ,indexNumber: 7
                                   ,name: "Nidoking"
                                   ,pokedexNumber: 34}
                                  ,{_: {}
                                   ,cryDiff: 28
                                   ,indexNumber: 8
                                   ,name: "Slowbro"
                                   ,pokedexNumber: 80}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 9
                                   ,name: "Ivysaur"
                                   ,pokedexNumber: 2}
                                  ,{_: {}
                                   ,cryDiff: 78
                                   ,indexNumber: 10
                                   ,name: "Exeggutor"
                                   ,pokedexNumber: 103}
                                  ,{_: {}
                                   ,cryDiff: 34
                                   ,indexNumber: 11
                                   ,name: "Lickitung"
                                   ,pokedexNumber: 108}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 12
                                   ,name: "Exeggcute"
                                   ,pokedexNumber: 102}
                                  ,{_: {}
                                   ,cryDiff: 18
                                   ,indexNumber: 13
                                   ,name: "Grimer"
                                   ,pokedexNumber: 88}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 14
                                   ,name: "Gengar"
                                   ,pokedexNumber: 94}
                                  ,{_: {}
                                   ,cryDiff: 15
                                   ,indexNumber: 15
                                   ,name: "Nidoran F"
                                   ,pokedexNumber: 29}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 16
                                   ,name: "Nidoqueen"
                                   ,pokedexNumber: 31}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 17
                                   ,name: "Cubone"
                                   ,pokedexNumber: 104}
                                  ,{_: {}
                                   ,cryDiff: 44
                                   ,indexNumber: 18
                                   ,name: "Rhyhorn"
                                   ,pokedexNumber: 111}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 19
                                   ,name: "Lapras"
                                   ,pokedexNumber: 131}
                                  ,{_: {}
                                   ,cryDiff: 36
                                   ,indexNumber: 20
                                   ,name: "Arcanine"
                                   ,pokedexNumber: 59}
                                  ,{_: {}
                                   ,cryDiff: 79
                                   ,indexNumber: 21
                                   ,name: "Mew"
                                   ,pokedexNumber: 151}
                                  ,{_: {}
                                   ,cryDiff: 44
                                   ,indexNumber: 22
                                   ,name: "Gyarados"
                                   ,pokedexNumber: 130}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 23
                                   ,name: "Shellder"
                                   ,pokedexNumber: 90}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 24
                                   ,name: "Tentacool"
                                   ,pokedexNumber: 72}
                                  ,{_: {}
                                   ,cryDiff: 52
                                   ,indexNumber: 25
                                   ,name: "Gastly"
                                   ,pokedexNumber: 92}
                                  ,{_: {}
                                   ,cryDiff: 24
                                   ,indexNumber: 26
                                   ,name: "Scyther"
                                   ,pokedexNumber: 123}
                                  ,{_: {}
                                   ,cryDiff: 44
                                   ,indexNumber: 27
                                   ,name: "Staryu"
                                   ,pokedexNumber: 120}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 28
                                   ,name: "Blastoise"
                                   ,pokedexNumber: 9}
                                  ,{_: {}
                                   ,cryDiff: 22
                                   ,indexNumber: 29
                                   ,name: "Pinsir"
                                   ,pokedexNumber: 127}
                                  ,{_: {}
                                   ,cryDiff: 29
                                   ,indexNumber: 30
                                   ,name: "Tangela"
                                   ,pokedexNumber: 114}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 33
                                   ,name: "Growlithe"
                                   ,pokedexNumber: 58}
                                  ,{_: {}
                                   ,cryDiff: 59
                                   ,indexNumber: 34
                                   ,name: "Onix"
                                   ,pokedexNumber: 95}
                                  ,{_: {}
                                   ,cryDiff: 35
                                   ,indexNumber: 35
                                   ,name: "Fearow"
                                   ,pokedexNumber: 22}
                                  ,{_: {}
                                   ,cryDiff: 2
                                   ,indexNumber: 36
                                   ,name: "Pidgey"
                                   ,pokedexNumber: 16}
                                  ,{_: {}
                                   ,cryDiff: 9
                                   ,indexNumber: 37
                                   ,name: "Slowpoke"
                                   ,pokedexNumber: 79}
                                  ,{_: {}
                                   ,cryDiff: 67
                                   ,indexNumber: 38
                                   ,name: "Kadabra"
                                   ,pokedexNumber: 64}
                                  ,{_: {}
                                   ,cryDiff: 66
                                   ,indexNumber: 39
                                   ,name: "Graveler"
                                   ,pokedexNumber: 75}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 40
                                   ,name: "Chansey"
                                   ,pokedexNumber: 113}
                                  ,{_: {}
                                   ,cryDiff: 27
                                   ,indexNumber: 41
                                   ,name: "Machoke"
                                   ,pokedexNumber: 67}
                                  ,{_: {}
                                   ,cryDiff: 39
                                   ,indexNumber: 42
                                   ,name: "Mr. Mime"
                                   ,pokedexNumber: 122}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 43
                                   ,name: "Hitmonlee"
                                   ,pokedexNumber: 106}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 44
                                   ,name: "Hitmonchan"
                                   ,pokedexNumber: 107}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 45
                                   ,name: "Arbok"
                                   ,pokedexNumber: 24}
                                  ,{_: {}
                                   ,cryDiff: 81
                                   ,indexNumber: 46
                                   ,name: "Parasect"
                                   ,pokedexNumber: 47}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 47
                                   ,name: "Psyduck"
                                   ,pokedexNumber: 54}
                                  ,{_: {}
                                   ,cryDiff: 70
                                   ,indexNumber: 48
                                   ,name: "Drowzee"
                                   ,pokedexNumber: 96}
                                  ,{_: {}
                                   ,cryDiff: 28
                                   ,indexNumber: 49
                                   ,name: "Golem"
                                   ,pokedexNumber: 76}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 51
                                   ,name: "Magmar"
                                   ,pokedexNumber: 126}
                                  ,{_: {}
                                   ,cryDiff: 93
                                   ,indexNumber: 53
                                   ,name: "Electabuzz"
                                   ,pokedexNumber: 125}
                                  ,{_: {}
                                   ,cryDiff: 65
                                   ,indexNumber: 54
                                   ,name: "Magneton"
                                   ,pokedexNumber: 82}
                                  ,{_: {}
                                   ,cryDiff: 47
                                   ,indexNumber: 55
                                   ,name: "Koffing"
                                   ,pokedexNumber: 109}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 57
                                   ,name: "Mankey"
                                   ,pokedexNumber: 56}
                                  ,{_: {}
                                   ,cryDiff: 39
                                   ,indexNumber: 58
                                   ,name: "Seel"
                                   ,pokedexNumber: 86}
                                  ,{_: {}
                                   ,cryDiff: 42
                                   ,indexNumber: 59
                                   ,name: "Diglett"
                                   ,pokedexNumber: 50}
                                  ,{_: {}
                                   ,cryDiff: 38
                                   ,indexNumber: 60
                                   ,name: "Tauros"
                                   ,pokedexNumber: 128}
                                  ,{_: {}
                                   ,cryDiff: 18
                                   ,indexNumber: 64
                                   ,name: "Farfetch\'d"
                                   ,pokedexNumber: 83}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 65
                                   ,name: "Venonat"
                                   ,pokedexNumber: 48}
                                  ,{_: {}
                                   ,cryDiff: 44
                                   ,indexNumber: 66
                                   ,name: "Dragonite"
                                   ,pokedexNumber: 149}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 70
                                   ,name: "Doduo"
                                   ,pokedexNumber: 84}
                                  ,{_: {}
                                   ,cryDiff: 18
                                   ,indexNumber: 71
                                   ,name: "Poliwag"
                                   ,pokedexNumber: 60}
                                  ,{_: {}
                                   ,cryDiff: 121
                                   ,indexNumber: 72
                                   ,name: "Jynx"
                                   ,pokedexNumber: 124}
                                  ,{_: {}
                                   ,cryDiff: 54
                                   ,indexNumber: 73
                                   ,name: "Moltres"
                                   ,pokedexNumber: 146}
                                  ,{_: {}
                                   ,cryDiff: 53
                                   ,indexNumber: 74
                                   ,name: "Articuno"
                                   ,pokedexNumber: 144}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 75
                                   ,name: "Zapdos"
                                   ,pokedexNumber: 145}
                                  ,{_: {}
                                   ,cryDiff: 20
                                   ,indexNumber: 76
                                   ,name: "Ditto"
                                   ,pokedexNumber: 132}
                                  ,{_: {}
                                   ,cryDiff: 8
                                   ,indexNumber: 77
                                   ,name: "Meowth"
                                   ,pokedexNumber: 52}
                                  ,{_: {}
                                   ,cryDiff: 56
                                   ,indexNumber: 78
                                   ,name: "Krabby"
                                   ,pokedexNumber: 98}
                                  ,{_: {}
                                   ,cryDiff: 54
                                   ,indexNumber: 82
                                   ,name: "Vulpix"
                                   ,pokedexNumber: 37}
                                  ,{_: {}
                                   ,cryDiff: 56
                                   ,indexNumber: 83
                                   ,name: "Ninetales"
                                   ,pokedexNumber: 38}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 84
                                   ,name: "Pikachu"
                                   ,pokedexNumber: 25}
                                  ,{_: {}
                                   ,cryDiff: 55
                                   ,indexNumber: 85
                                   ,name: "Raichu"
                                   ,pokedexNumber: 26}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 88
                                   ,name: "Dratini"
                                   ,pokedexNumber: 147}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 89
                                   ,name: "Dragonair"
                                   ,pokedexNumber: 148}
                                  ,{_: {}
                                   ,cryDiff: 21
                                   ,indexNumber: 90
                                   ,name: "Kabuto"
                                   ,pokedexNumber: 140}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 91
                                   ,name: "Kabutops"
                                   ,pokedexNumber: 141}
                                  ,{_: {}
                                   ,cryDiff: 7
                                   ,indexNumber: 92
                                   ,name: "Horsea"
                                   ,pokedexNumber: 116}
                                  ,{_: {}
                                   ,cryDiff: 6
                                   ,indexNumber: 93
                                   ,name: "Seadra"
                                   ,pokedexNumber: 117}
                                  ,{_: {}
                                   ,cryDiff: 16
                                   ,indexNumber: 96
                                   ,name: "Sandshrew"
                                   ,pokedexNumber: 27}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 97
                                   ,name: "Sandslash"
                                   ,pokedexNumber: 28}
                                  ,{_: {}
                                   ,cryDiff: 24
                                   ,indexNumber: 98
                                   ,name: "Omanyte"
                                   ,pokedexNumber: 138}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 99
                                   ,name: "Omastar"
                                   ,pokedexNumber: 139}
                                  ,{_: {}
                                   ,cryDiff: 1
                                   ,indexNumber: 100
                                   ,name: "Jigglypuff"
                                   ,pokedexNumber: 39}
                                  ,{_: {}
                                   ,cryDiff: 8
                                   ,indexNumber: 101
                                   ,name: "Wigglytuff"
                                   ,pokedexNumber: 40}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 102
                                   ,name: "Eevee"
                                   ,pokedexNumber: 133}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 103
                                   ,name: "Flareon"
                                   ,pokedexNumber: 136}
                                  ,{_: {}
                                   ,cryDiff: 34
                                   ,indexNumber: 104
                                   ,name: "Jolteon"
                                   ,pokedexNumber: 135}
                                  ,{_: {}
                                   ,cryDiff: 55
                                   ,indexNumber: 105
                                   ,name: "Vaporeon"
                                   ,pokedexNumber: 134}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 106
                                   ,name: "Machop"
                                   ,pokedexNumber: 66}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 107
                                   ,name: "Zubat"
                                   ,pokedexNumber: 41}
                                  ,{_: {}
                                   ,cryDiff: 39
                                   ,indexNumber: 108
                                   ,name: "Ekans"
                                   ,pokedexNumber: 23}
                                  ,{_: {}
                                   ,cryDiff: 71
                                   ,indexNumber: 109
                                   ,name: "Paras"
                                   ,pokedexNumber: 46}
                                  ,{_: {}
                                   ,cryDiff: 6
                                   ,indexNumber: 110
                                   ,name: "Poliwhirl"
                                   ,pokedexNumber: 61}
                                  ,{_: {}
                                   ,cryDiff: 19
                                   ,indexNumber: 111
                                   ,name: "Poliwrath"
                                   ,pokedexNumber: 62}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 112
                                   ,name: "Weedle"
                                   ,pokedexNumber: 13}
                                  ,{_: {}
                                   ,cryDiff: 39
                                   ,indexNumber: 113
                                   ,name: "Kakuna"
                                   ,pokedexNumber: 14}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 114
                                   ,name: "Beedrill"
                                   ,pokedexNumber: 15}
                                  ,{_: {}
                                   ,cryDiff: 42
                                   ,indexNumber: 116
                                   ,name: "Dodrio"
                                   ,pokedexNumber: 85}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 117
                                   ,name: "Primeape"
                                   ,pokedexNumber: 57}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 118
                                   ,name: "Dugtrio"
                                   ,pokedexNumber: 51}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 119
                                   ,name: "Venomoth"
                                   ,pokedexNumber: 49}
                                  ,{_: {}
                                   ,cryDiff: 52
                                   ,indexNumber: 120
                                   ,name: "Dewgong"
                                   ,pokedexNumber: 87}
                                  ,{_: {}
                                   ,cryDiff: 20
                                   ,indexNumber: 123
                                   ,name: "Caterpie"
                                   ,pokedexNumber: 10}
                                  ,{_: {}
                                   ,cryDiff: 51
                                   ,indexNumber: 124
                                   ,name: "Metapod"
                                   ,pokedexNumber: 11}
                                  ,{_: {}
                                   ,cryDiff: 21
                                   ,indexNumber: 125
                                   ,name: "Butterfree"
                                   ,pokedexNumber: 12}
                                  ,{_: {}
                                   ,cryDiff: 34
                                   ,indexNumber: 126
                                   ,name: "Machamp"
                                   ,pokedexNumber: 68}
                                  ,{_: {}
                                   ,cryDiff: 20
                                   ,indexNumber: 128
                                   ,name: "Golduck"
                                   ,pokedexNumber: 55}
                                  ,{_: {}
                                   ,cryDiff: 68
                                   ,indexNumber: 129
                                   ,name: "Hypno"
                                   ,pokedexNumber: 97}
                                  ,{_: {}
                                   ,cryDiff: 42
                                   ,indexNumber: 130
                                   ,name: "Golbat"
                                   ,pokedexNumber: 42}
                                  ,{_: {}
                                   ,cryDiff: 80
                                   ,indexNumber: 131
                                   ,name: "Mewtwo"
                                   ,pokedexNumber: 150}
                                  ,{_: {}
                                   ,cryDiff: 0
                                   ,indexNumber: 132
                                   ,name: "Snorlax"
                                   ,pokedexNumber: 143}
                                  ,{_: {}
                                   ,cryDiff: 44
                                   ,indexNumber: 133
                                   ,name: "Magikarp"
                                   ,pokedexNumber: 129}
                                  ,{_: {}
                                   ,cryDiff: 34
                                   ,indexNumber: 136
                                   ,name: "Muk"
                                   ,pokedexNumber: 89}
                                  ,{_: {}
                                   ,cryDiff: 59
                                   ,indexNumber: 138
                                   ,name: "Kingler"
                                   ,pokedexNumber: 99}
                                  ,{_: {}
                                   ,cryDiff: 50
                                   ,indexNumber: 139
                                   ,name: "Cloyster"
                                   ,pokedexNumber: 91}
                                  ,{_: {}
                                   ,cryDiff: 60
                                   ,indexNumber: 141
                                   ,name: "Electrode"
                                   ,pokedexNumber: 101}
                                  ,{_: {}
                                   ,cryDiff: 12
                                   ,indexNumber: 142
                                   ,name: "Clefable"
                                   ,pokedexNumber: 36}
                                  ,{_: {}
                                   ,cryDiff: 52
                                   ,indexNumber: 143
                                   ,name: "Weezing"
                                   ,pokedexNumber: 110}
                                  ,{_: {}
                                   ,cryDiff: 47
                                   ,indexNumber: 144
                                   ,name: "Persian"
                                   ,pokedexNumber: 53}
                                  ,{_: {}
                                   ,cryDiff: 31
                                   ,indexNumber: 145
                                   ,name: "Marowak"
                                   ,pokedexNumber: 105}
                                  ,{_: {}
                                   ,cryDiff: 53
                                   ,indexNumber: 147
                                   ,name: "Haunter"
                                   ,pokedexNumber: 93}
                                  ,{_: {}
                                   ,cryDiff: 55
                                   ,indexNumber: 148
                                   ,name: "Abra"
                                   ,pokedexNumber: 63}
                                  ,{_: {}
                                   ,cryDiff: 82
                                   ,indexNumber: 149
                                   ,name: "Alakazam"
                                   ,pokedexNumber: 65}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 150
                                   ,name: "Pidgeotto"
                                   ,pokedexNumber: 17}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 151
                                   ,name: "Pidgeot"
                                   ,pokedexNumber: 18}
                                  ,{_: {}
                                   ,cryDiff: 49
                                   ,indexNumber: 152
                                   ,name: "Starmie"
                                   ,pokedexNumber: 121}
                                  ,{_: {}
                                   ,cryDiff: 33
                                   ,indexNumber: 153
                                   ,name: "Bulbasaur"
                                   ,pokedexNumber: 1}
                                  ,{_: {}
                                   ,cryDiff: 46
                                   ,indexNumber: 154
                                   ,name: "Venusaur"
                                   ,pokedexNumber: 3}
                                  ,{_: {}
                                   ,cryDiff: 54
                                   ,indexNumber: 155
                                   ,name: "Tentacruel"
                                   ,pokedexNumber: 73}
                                  ,{_: {}
                                   ,cryDiff: 20
                                   ,indexNumber: 157
                                   ,name: "Goldeen"
                                   ,pokedexNumber: 118}
                                  ,{_: {}
                                   ,cryDiff: 43
                                   ,indexNumber: 158
                                   ,name: "Seaking"
                                   ,pokedexNumber: 119}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 163
                                   ,name: "Ponyta"
                                   ,pokedexNumber: 77}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 164
                                   ,name: "Rapidash"
                                   ,pokedexNumber: 78}
                                  ,{_: {}
                                   ,cryDiff: 8
                                   ,indexNumber: 165
                                   ,name: "Rattata"
                                   ,pokedexNumber: 19}
                                  ,{_: {}
                                   ,cryDiff: 21
                                   ,indexNumber: 166
                                   ,name: "Raticate"
                                   ,pokedexNumber: 20}
                                  ,{_: {}
                                   ,cryDiff: 26
                                   ,indexNumber: 167
                                   ,name: "Nidorino"
                                   ,pokedexNumber: 33}
                                  ,{_: {}
                                   ,cryDiff: 24
                                   ,indexNumber: 168
                                   ,name: "Nidorina"
                                   ,pokedexNumber: 30}
                                  ,{_: {}
                                   ,cryDiff: 54
                                   ,indexNumber: 169
                                   ,name: "Geodude"
                                   ,pokedexNumber: 74}
                                  ,{_: {}
                                   ,cryDiff: 53
                                   ,indexNumber: 170
                                   ,name: "Porygon"
                                   ,pokedexNumber: 77}
                                  ,{_: {}
                                   ,cryDiff: 61
                                   ,indexNumber: 171
                                   ,name: "Aerodactyl"
                                   ,pokedexNumber: 142}
                                  ,{_: {}
                                   ,cryDiff: 53
                                   ,indexNumber: 173
                                   ,name: "Magnemite"
                                   ,pokedexNumber: 81}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 176
                                   ,name: "Charmander"
                                   ,pokedexNumber: 4}
                                  ,{_: {}
                                   ,cryDiff: 38
                                   ,indexNumber: 177
                                   ,name: "Squirtle"
                                   ,pokedexNumber: 7}
                                  ,{_: {}
                                   ,cryDiff: 32
                                   ,indexNumber: 178
                                   ,name: "Charmeleon"
                                   ,pokedexNumber: 5}
                                  ,{_: {}
                                   ,cryDiff: 40
                                   ,indexNumber: 179
                                   ,name: "Wartortle"
                                   ,pokedexNumber: 8}
                                  ,{_: {}
                                   ,cryDiff: 41
                                   ,indexNumber: 180
                                   ,name: "Charizard"
                                   ,pokedexNumber: 6}
                                  ,{_: {}
                                   ,cryDiff: 27
                                   ,indexNumber: 185
                                   ,name: "Oddish"
                                   ,pokedexNumber: 43}
                                  ,{_: {}
                                   ,cryDiff: 29
                                   ,indexNumber: 186
                                   ,name: "Gloom"
                                   ,pokedexNumber: 44}
                                  ,{_: {}
                                   ,cryDiff: 63
                                   ,indexNumber: 187
                                   ,name: "Vileplume"
                                   ,pokedexNumber: 45}
                                  ,{_: {}
                                   ,cryDiff: 7
                                   ,indexNumber: 188
                                   ,name: "Bellsprout"
                                   ,pokedexNumber: 69}
                                  ,{_: {}
                                   ,cryDiff: 30
                                   ,indexNumber: 189
                                   ,name: "Weepinbell"
                                   ,pokedexNumber: 70}
                                  ,{_: {}
                                   ,cryDiff: 46
                                   ,indexNumber: 190
                                   ,name: "Victreebel"
                                   ,pokedexNumber: 71}]);
   var noSpecies = {_: {}
                   ,cryDiff: 0
                   ,indexNumber: 0
                   ,name: ""
                   ,pokedexNumber: 0};
   var speciesByName = $Dict.fromList($List.map(function (species) {
      return {ctor: "_Tuple2"
             ,_0: species.name
             ,_1: species};
   })(speciesList));
   var speciesByPokedex = $Dict.fromList($List.map(function (species) {
      return {ctor: "_Tuple2"
             ,_0: species.pokedexNumber
             ,_1: species};
   })(speciesList));
   var speciesByIndex = $Dict.fromList($List.map(function (species) {
      return {ctor: "_Tuple2"
             ,_0: species.indexNumber
             ,_1: species};
   })(speciesList));
   var Species = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,cryDiff: d
             ,indexNumber: a
             ,name: c
             ,pokedexNumber: b};
   });
   _elm.Pokemon.values = {_op: _op
                         ,Species: Species
                         ,speciesByIndex: speciesByIndex
                         ,speciesByPokedex: speciesByPokedex
                         ,speciesByName: speciesByName
                         ,noSpecies: noSpecies
                         ,speciesList: speciesList};
   return _elm.Pokemon.values;
};
Elm.RNG = Elm.RNG || {};
Elm.RNG.make = function (_elm) {
   "use strict";
   _elm.RNG = _elm.RNG || {};
   if (_elm.RNG.values)
   return _elm.RNG.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "RNG",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var getDSum$ = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple4":
            return A2($Basics._op["%"],
              _v0._2 + _v0._3,
              256);}
         _U.badCase($moduleName,
         "on line 48, column 28 to 42");
      }();
   };
   var getDSum = function (s) {
      return A2($Basics._op["%"],
      s.hRandomAdd + s.hRandomSub,
      256);
   };
   var rngStep = F2(function (carry,
   s) {
      return function () {
         var cycle$$ = A2($Basics._op["%"],
         s.cycle + 70224,
         256);
         var rDiv$$ = A2($Basics._op["%"],
         s.rDiv + ((s.cycle + 70224) / 256 | 0),
         256);
         var cycle$ = s.cycle + 44;
         var rDiv$ = _U.cmp(cycle$,
         256) > -1 ? A2($Basics._op["%"],
         s.rDiv + 1,
         256) : s.rDiv;
         var hRandomAdd$ = s.hRandomAdd + s.rDiv + carry;
         var carry$ = _U.cmp(hRandomAdd$,
         256) > -1 ? 1 : 0;
         var hRandomSub$ = s.hRandomSub - rDiv$ - carry$;
         return {_: {}
                ,cycle: cycle$$
                ,hRandomAdd: A2($Basics._op["%"],
                hRandomAdd$,
                256)
                ,hRandomSub: A2($Basics._op["%"],
                hRandomSub$,
                256)
                ,rDiv: rDiv$$};
      }();
   });
   var rngState = F4(function (r,
   c,
   a,
   s) {
      return {_: {}
             ,cycle: c
             ,hRandomAdd: a
             ,hRandomSub: s
             ,rDiv: r};
   });
   var fromComparable = function (_v6) {
      return function () {
         switch (_v6.ctor)
         {case "_Tuple4":
            return A4(rngState,
              _v6._0,
              _v6._1,
              _v6._2,
              _v6._3);}
         _U.badCase($moduleName,
         "on line 16, column 56 to 97");
      }();
   };
   var toComparable = function (state) {
      return {ctor: "_Tuple4"
             ,_0: state.rDiv
             ,_1: state.cycle
             ,_2: state.hRandomAdd
             ,_3: state.hRandomSub};
   };
   var rngStep$ = F2(function (carry,
   s) {
      return toComparable(rngStep(carry)(fromComparable(s)));
   });
   var RNGState = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,cycle: b
             ,hRandomAdd: c
             ,hRandomSub: d
             ,rDiv: a};
   });
   _elm.RNG.values = {_op: _op
                     ,RNGState: RNGState
                     ,toComparable: toComparable
                     ,fromComparable: fromComparable
                     ,rngState: rngState
                     ,rngStep: rngStep
                     ,rngStep$: rngStep$
                     ,getDSum: getDSum
                     ,getDSum$: getDSum$};
   return _elm.RNG.values;
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 164 and 166");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 126 and 128");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 41 and 43");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 55 and 58");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 63 and 67");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 72 and 77");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 82 and 88");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 148 and 150");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 180 and 182");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe
                        ,formatError: formatError
                        ,Ok: Ok
                        ,Err: Err};
   return _elm.Result.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var send = F2(function (_v0,
   value) {
      return function () {
         switch (_v0.ctor)
         {case "Address":
            return A2($Task.onError,
              _v0._0(value),
              function (_v3) {
                 return function () {
                    return $Task.succeed({ctor: "_Tuple0"});
                 }();
              });}
         _U.badCase($moduleName,
         "between lines 370 and 371");
      }();
   });
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var message = F2(function (_v5,
   value) {
      return function () {
         switch (_v5.ctor)
         {case "Address":
            return Message(_v5._0(value));}
         _U.badCase($moduleName,
         "on line 352, column 5 to 24");
      }();
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address"
             ,_0: a};
   };
   var forwardTo = F2(function (_v8,
   f) {
      return function () {
         switch (_v8.ctor)
         {case "Address":
            return Address(function (x) {
                 return _v8._0(f(x));
              });}
         _U.badCase($moduleName,
         "on line 339, column 5 to 29");
      }();
   });
   var Mailbox = F2(function (a,
   b) {
      return {_: {}
             ,address: a
             ,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,
   base,
   signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,
   right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      return function () {
         var _v11 = $List.reverse(signalList);
         switch (_v11.ctor)
         {case "::":
            return A3($List.foldl,
              merge,
              _v11._0,
              _v11._1);
            case "[]":
            return $Debug.crash("mergeMany was given an empty list!");}
         _U.badCase($moduleName,
         "between lines 177 and 182");
      }();
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   _op["~"] = F2(function (funcs,
   args) {
      return A3(map2,
      F2(function (f,v) {
         return f(v);
      }),
      funcs,
      args);
   });
   var map = $Native$Signal.map;
   _op["<~"] = map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,constant: constant
                        ,dropRepeats: dropRepeats
                        ,filter: filter
                        ,filterMap: filterMap
                        ,sampleOn: sampleOn
                        ,foldp: foldp
                        ,mailbox: mailbox
                        ,send: send
                        ,message: message
                        ,forwardTo: forwardTo
                        ,Mailbox: Mailbox};
   return _elm.Signal.values;
};
Elm.Strategy = Elm.Strategy || {};
Elm.Strategy.make = function (_elm) {
   "use strict";
   _elm.Strategy = _elm.Strategy || {};
   if (_elm.Strategy.values)
   return _elm.Strategy.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Strategy",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var roundStrategy$ = F3(function (rounding,
   extraFrames,
   strat) {
      return function () {
         switch (strat.ctor)
         {case "::": return function () {
                 var idealFrameCount = strat._0.frames + extraFrames;
                 var roundedFrameCount = rounding * $Basics.round($Basics.toFloat(idealFrameCount) / $Basics.toFloat(rounding));
                 var extras = idealFrameCount - roundedFrameCount;
                 return A2($List._op["::"],
                 _U.replace([["frames"
                             ,roundedFrameCount]],
                 strat._0),
                 A3(roundStrategy$,
                 rounding,
                 extras,
                 strat._1));
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 46 and 54");
      }();
   });
   var removeSmallSteps = F2(function (frameThreshold,
   strat) {
      return function () {
         switch (strat.ctor)
         {case "::":
            switch (strat._1.ctor)
              {case "::":
                 return _U.cmp(strat._0.frames,
                   frameThreshold) < 0 ? A2(removeSmallSteps,
                   frameThreshold,
                   A2($List._op["::"],
                   {_: {}
                   ,frames: strat._0.frames + strat._1._0.frames
                   ,inGrass: strat._1._0.inGrass},
                   strat._1._1)) : A2($List._op["::"],
                   strat._0,
                   A2(removeSmallSteps,
                   frameThreshold,
                   A2($List._op["::"],
                   strat._1._0,
                   strat._1._1)));
                 case "[]":
                 return A2($List._op["::"],
                   strat._0,
                   _L.fromArray([]));}
              break;
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 21 and 26");
      }();
   });
   var combineSteps = function (l) {
      return function () {
         switch (l.ctor)
         {case "::": switch (l._1.ctor)
              {case "::":
                 return _U.eq(l._0.inGrass,
                   l._1._0.inGrass) ? combineSteps(A2($List._op["::"],
                   {_: {}
                   ,frames: l._0.frames + l._1._0.frames
                   ,inGrass: l._0.inGrass},
                   l._1._1)) : A2($List._op["::"],
                   l._0,
                   combineSteps(A2($List._op["::"],
                   l._1._0,
                   l._1._1)));
                 case "[]":
                 return A2($List._op["::"],
                   l._0,
                   _L.fromArray([]));}
              break;
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 13 and 18");
      }();
   };
   var simplify = F2(function (frameThreshold,
   strat) {
      return combineSteps(removeSmallSteps(frameThreshold)(combineSteps(strat)));
   });
   var frameStrategy = function (l) {
      return combineSteps($List.map(function (b) {
         return {_: {}
                ,frames: 1
                ,inGrass: b};
      })(l));
   };
   var roundStrategy = F2(function (rounding,
   strat) {
      return combineSteps(A3(roundStrategy$,
      rounding,
      0,
      strat));
   });
   var StrategyStep = F2(function (a,
   b) {
      return {_: {}
             ,frames: b
             ,inGrass: a};
   });
   _elm.Strategy.values = {_op: _op
                          ,StrategyStep: StrategyStep
                          ,combineSteps: combineSteps
                          ,removeSmallSteps: removeSmallSteps
                          ,simplify: simplify
                          ,frameStrategy: frameStrategy
                          ,roundStrategy: roundStrategy
                          ,roundStrategy$: roundStrategy$};
   return _elm.Strategy.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,length: length
                        ,reverse: reverse
                        ,repeat: repeat
                        ,cons: cons
                        ,uncons: uncons
                        ,fromChar: fromChar
                        ,append: append
                        ,concat: concat
                        ,split: split
                        ,join: join
                        ,words: words
                        ,lines: lines
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,map: map
                        ,filter: filter
                        ,foldl: foldl
                        ,foldr: foldr
                        ,any: any
                        ,all: all};
   return _elm.String.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values)
   return _elm.Task.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID"
             ,_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,
   promise) {
      return A2(onError,
      promise,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,
   promiseA) {
      return A2(andThen,
      promiseA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,
   promiseA,
   promiseB) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,
   promiseA,
   promiseB,
   promiseC) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return succeed(A3(func,
               a,
               b,
               c));
            });
         });
      });
   });
   var map4 = F5(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return succeed(A4(func,
                  a,
                  b,
                  c,
                  d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD,
   promiseE) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return A2(andThen,
                  promiseE,
                  function (e) {
                     return succeed(A5(func,
                     a,
                     b,
                     c,
                     d,
                     e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (promiseFunc,
   promiseValue) {
      return A2(andThen,
      promiseFunc,
      function (func) {
         return A2(andThen,
         promiseValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (promises) {
      return function () {
         switch (promises.ctor)
         {case "::": return A3(map2,
              F2(function (x,y) {
                 return A2($List._op["::"],
                 x,
                 y);
              }),
              promises._0,
              sequence(promises._1));
            case "[]":
            return succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 101 and 106");
      }();
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_v3) {
         return function () {
            return succeed($Maybe.Nothing);
         }();
      });
   };
   var fromMaybe = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return succeed(maybe._0);
            case "Nothing":
            return fail($default);}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return fail(result._0);
            case "Ok":
            return succeed(result._0);}
         _U.badCase($moduleName,
         "between lines 151 and 153");
      }();
   };
   var Task = {ctor: "Task"};
   _elm.Task.values = {_op: _op
                      ,succeed: succeed
                      ,fail: fail
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,andMap: andMap
                      ,sequence: sequence
                      ,andThen: andThen
                      ,onError: onError
                      ,mapError: mapError
                      ,toMaybe: toMaybe
                      ,fromMaybe: fromMaybe
                      ,toResult: toResult
                      ,fromResult: fromResult
                      ,spawn: spawn
                      ,sleep: sleep};
   return _elm.Task.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,link: link
                      ,style: style
                      ,defaultStyle: defaultStyle
                      ,typeface: typeface
                      ,monospace: monospace
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,Style: Style
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Signal.delay;
   var since = F2(function (time,
   signal) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,time,signal));
         var start = A2($Signal.map,
         $Basics.always(1),
         signal);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,
      targetFrames,
      $Signal.constant(true));
   };
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,timestamp: timestamp
                      ,delay: delay
                      ,since: since};
   return _elm.Time.values;
};
Elm.Trampoline = Elm.Trampoline || {};
Elm.Trampoline.make = function (_elm) {
   "use strict";
   _elm.Trampoline = _elm.Trampoline || {};
   if (_elm.Trampoline.values)
   return _elm.Trampoline.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Trampoline",
   $Native$Trampoline = Elm.Native.Trampoline.make(_elm);
   var trampoline = $Native$Trampoline.trampoline;
   var Continue = function (a) {
      return {ctor: "Continue"
             ,_0: a};
   };
   var Done = function (a) {
      return {ctor: "Done",_0: a};
   };
   _elm.Trampoline.values = {_op: _op
                            ,trampoline: trampoline
                            ,Done: Done
                            ,Continue: Continue};
   return _elm.Trampoline.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
Elm.Worker = Elm.Worker || {};
Elm.Worker.make = function (_elm) {
   "use strict";
   _elm.Worker = _elm.Worker || {};
   if (_elm.Worker.values)
   return _elm.Worker.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Worker",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm);
   var getResult = function (state) {
      return function () {
         switch (state.ctor)
         {case "Done":
            return $Maybe.Just(state._0);
            case "Unstarted":
            return $Maybe.Nothing;
            case "Working":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 19 and 22");
      }();
   };
   var isWorking = function (state) {
      return function () {
         switch (state.ctor)
         {case "Done": return false;
            case "Unstarted": return false;
            case "Working": return true;}
         _U.badCase($moduleName,
         "between lines 13 and 16");
      }();
   };
   var Worker = F2(function (a,b) {
      return {_: {}
             ,signal: b
             ,state: a};
   });
   var Unstarted = {ctor: "Unstarted"};
   var Done = function (a) {
      return {ctor: "Done",_0: a};
   };
   var Working = function (a) {
      return {ctor: "Working"
             ,_0: a};
   };
   var createWorker = F2(function (inputSignal,
   step) {
      return function () {
         var state = A3($Signal.foldp,
         F2(function (inp,_v6) {
            return function () {
               switch (_v6.ctor)
               {case "_Tuple2":
                  return _U.eq($Maybe.Just(inp),
                    _v6._0) ? function () {
                       switch (_v6._1.ctor)
                       {case "Done":
                          return {ctor: "_Tuple2"
                                 ,_0: _v6._0
                                 ,_1: Done(_v6._1._0)};
                          case "Working":
                          return {ctor: "_Tuple2"
                                 ,_0: _v6._0
                                 ,_1: step(_v6._1._0)};}
                       _U.badCase($moduleName,
                       "between lines 29 and 32");
                    }() : {ctor: "_Tuple2"
                          ,_0: $Maybe.Just(inp)
                          ,_1: Working(inp)};}
               _U.badCase($moduleName,
               "between lines 28 and 32");
            }();
         }),
         {ctor: "_Tuple2"
         ,_0: $Maybe.Nothing
         ,_1: Unstarted},
         $Signal.sampleOn($Time.fps(60))(inputSignal));
         return {_: {}
                ,signal: $Signal.dropRepeats($Signal.map(function ($) {
                   return getResult($Basics.snd($));
                })(state))
                ,state: state};
      }();
   });
   _elm.Worker.values = {_op: _op
                        ,Working: Working
                        ,Done: Done
                        ,Unstarted: Unstarted
                        ,Worker: Worker
                        ,isWorking: isWorking
                        ,getResult: getResult
                        ,createWorker: createWorker};
   return _elm.Worker.values;
};
