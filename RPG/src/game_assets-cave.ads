with GESTE;
with GESTE.Grid;
pragma Style_Checks (Off);
package Game_Assets.cave is

   --  cave
   Width       : constant := 20;
   Height      : constant := 15;
   Tile_Width  : constant := 16;
   Tile_Height : constant := 16;

   --  Ground
   package Ground is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 339, 340, 341, 341, 341, 341, 341, 341, 341, 341, 341, 341, 341, 341, 342),
         ( 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 348, 349, 350, 354),
         ( 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 360, 361, 362, 366),
         ( 343, 344, 345, 346, 347, 367, 368, 369, 370, 371, 372, 367, 368, 369, 354),
         ( 355, 356, 357, 358, 359, 373, 374, 375, 376, 377, 378, 373, 374, 375, 366),
         ( 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 348, 349, 350, 354),
         ( 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 360, 361, 362, 366),
         ( 343, 344, 345, 346, 347, 367, 368, 369, 370, 371, 372, 367, 368, 369, 354),
         ( 355, 356, 357, 358, 359, 373, 374, 375, 376, 377, 378, 373, 374, 375, 366),
         ( 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 348, 349, 350, 354),
         ( 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 360, 361, 362, 366),
         ( 343, 344, 345, 346, 347, 367, 368, 369, 370, 371, 372, 367, 368, 369, 354),
         ( 355, 356, 357, 358, 359, 373, 374, 375, 376, 377, 378, 373, 374, 375, 366),
         ( 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 348, 349, 350, 354),
         ( 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 360, 361, 362, 366),
         ( 343, 344, 345, 346, 347, 367, 368, 369, 370, 371, 372, 367, 368, 369, 354),
         ( 355, 356, 357, 358, 359, 373, 374, 375, 376, 377, 378, 373, 374, 375, 366),
         ( 343, 344, 345, 346, 347, 367, 368, 369, 370, 371, 372, 367, 0, 0, 354),
         ( 355, 356, 357, 358, 359, 373, 374, 375, 376, 377, 378, 0, 0, 0, 366),
         ( 379, 380, 381, 381, 381, 381, 381, 381, 381, 381, 381, 381, 381, 381, 382))      ;
   end Ground;

   --  Over
   package Over is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 383, 384, 385, 386, 387, 388, 389, 390, 0, 0, 0),
         ( 0, 0, 0, 0, 391, 392, 393, 394, 395, 396, 397, 398, 0, 0, 0),
         ( 0, 167, 168, 0, 399, 400, 401, 402, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 173, 174, 0, 403, 404, 405, 406, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 407, 408, 409, 410, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 411, 412, 413, 414, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 415, 416, 417, 418, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 419, 420, 421, 422, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 167, 168, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 173, 174, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 423, 424, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 425, 426, 0, 0, 0, 0, 0),
         ( 0, 427, 428, 429, 430, 431, 0, 0, 432, 433, 0, 0, 0, 0, 0),
         ( 0, 434, 435, 436, 437, 438, 0, 0, 439, 440, 0, 0, 0, 0, 0),
         ( 0, 441, 442, 443, 444, 445, 0, 0, 0, 0, 446, 447, 448, 449, 0),
         ( 0, 450, 451, 452, 453, 454, 0, 0, 0, 0, 455, 456, 457, 458, 0),
         ( 0, 459, 460, 461, 462, 463, 0, 0, 0, 0, 464, 465, 466, 467, 0),
         ( 0, 468, 469, 470, 471, 472, 0, 0, 0, 0, 473, 474, 475, 476, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Over;

   --  collisions
   package collisions is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 234, 234, 234, 234, 234, 234, 234, 234, 234, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 234, 234, 234, 234, 234, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 234, 241, 477, 241, 477, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 478, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 477, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 0, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 478, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 0, 234, 234, 234, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 234, 240, 0, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 0, 0, 0, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 0, 0, 0, 0, 0, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 0, 0, 0, 234, 234, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 0, 0, 0, 234, 234, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 238, 234, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 234, 234, 0, 0, 0, 0, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 0, 0, 0, 242, 234, 234, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 0, 0, 0, 234, 234, 234, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 0, 0, 242, 234, 234, 234, 234),
         ( 0, 0, 0, 0, 234, 235, 0, 0, 0, 0, 234, 234, 234, 234, 234),
         ( 0, 0, 0, 0, 234, 234, 234, 234, 234, 234, 234, 234, 234, 234, 234))      ;
   end collisions;

   package gates is
      Objects : Object_Array :=
        (
           0 => (
            Kind => POINT_OBJ,
            Id   =>  1,
            Name => new String'("From_House"),
            X    =>  2.00000E+02,
            Y    =>  2.08000E+02,
            Width =>  0.00000E+00,
            Height =>  0.00000E+00,
            Tile_Id =>  0,
            Str => null
          ),
           1 => (
            Kind => RECTANGLE_OBJ,
            Id   =>  2,
            Name => new String'("To_House"),
            X    =>  2.24000E+02,
            Y    =>  1.88000E+02,
            Width =>  1.60000E+01,
            Height =>  3.60000E+01,
            Tile_Id =>  0,
            Str => null
          )
        );
      From_House : aliased constant Object := (
        Kind => POINT_OBJ,
        Id   =>  1,
        Name => new String'("From_House"),
        X    =>  2.00000E+02,
        Y    =>  2.08000E+02,
        Width =>  0.00000E+00,
        Height =>  0.00000E+00,
        Tile_Id =>  0,
        Str => null
        );
      To_House : aliased constant Object := (
        Kind => RECTANGLE_OBJ,
        Id   =>  2,
        Name => new String'("To_House"),
        X    =>  2.24000E+02,
        Y    =>  1.88000E+02,
        Width =>  1.60000E+01,
        Height =>  3.60000E+01,
        Tile_Id =>  0,
        Str => null
        );
   end gates;
end Game_Assets.cave;
