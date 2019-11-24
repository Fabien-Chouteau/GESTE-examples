with SDL;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes; use SDL.Video.Palettes;
with SDL.Video.Pixel_Formats; use SDL.Video.Pixel_Formats;
with SDL.Video.Textures; use SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

use SDL.Video;
with Interfaces.C; use Interfaces.C;
with SDL.Video.Pixels;
with Ada.Unchecked_Conversion;
with System;

package body SDL_Display is

   W          : SDL.Video.Windows.Window;
   Renderer   : SDL.Video.Renderers.Renderer;
   Texture    : SDL.Video.Textures.Texture;
   SDL_Pixels : System.Address;

   Screen_Offset : GESTE.Pix_Point := (0, 0);

   XS, XE, YS, YE : Natural := 0;
   X, Y : Natural := 0;

   type Texture_2D_Array is array (Natural range <>,
                                   Natural range <>)
     of aliased SDL_Pixel;

   type Texture_1D_Array is array (Natural range <>)
     of aliased SDL_Pixel;

   package Texture_2D is new SDL.Video.Pixels.Texture_Data
     (Index              => Natural,
      Element            => SDL_Pixel,
      Element_Array_1D   => Texture_1D_Array,
      Element_Array_2D   => Texture_2D_Array,
      Default_Terminator => 0);

   procedure Lock is new SDL.Video.Textures.Lock
     (Pixel_Pointer_Type => System.Address);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         raise Program_Error with "SDL Video init failed";
      end if;

      SDL.Video.Windows.Makers.Create
        (W, "GESTE Example",
         0,
         0,
         800 * Pixel_Scale,
         600 * Pixel_Scale,
         Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create
        (Tex      => Texture,
         Renderer => Renderer,
         Format   => SDL.Video.Pixel_Formats.Pixel_Format_RGB_565,
         Kind     => SDL.Video.Textures.Streaming,
         Size     => (800 * Pixel_Scale,
                      600 * Pixel_Scale));
   end Initialize;

   ----------------------
   -- Set_Drawing_Area --
   ----------------------

   procedure Set_Drawing_Area (Area : GESTE.Pix_Rect) is
   begin
      XS := Area.TL.X - Screen_Offset.X;
      YS := Area.TL.Y - Screen_Offset.Y;
      XE := Area.BR.X - Screen_Offset.X;
      YE := Area.BR.Y - Screen_Offset.Y;
      X := XS;
      Y := YS;
      if XS < 0 then
         raise Program_Error;
      end if;
      if YS < 0 then
         raise Program_Error;
      end if;
      if XE >= Width then
         raise Program_Error;
      end if;
      if YE >= Height then
         raise Program_Error;
      end if;
   end Set_Drawing_Area;

   -----------------------
   -- Set_Screen_Offset --
   -----------------------

   procedure Set_Screen_Offset (Pt : GESTE.Pix_Point) is
   begin
      Screen_Offset := Pt;
   end Set_Screen_Offset;

   ------------
   -- Update --
   ------------

   procedure Update is
      Width  : constant Natural := Texture.Get_Size.Width;
      Height : constant Natural := Texture.Get_Size.Height;
   begin

      Renderer.Clear;
      Renderer.Copy (Texture, To => (0,
                                     0,
                                     int (Width * Pixel_Scale),
                                     int (Height * Pixel_Scale)));
      Renderer.Present;
   end Update;

   ------------------
   -- To_SDL_Color --
   ------------------

   function To_SDL_Color (R, G, B : Unsigned_8) return SDL_Pixel is
      RB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (R), 3) and 16#1F#;
      GB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (G), 2) and 16#3F#;
      BB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (B), 3) and 16#1F#;
   begin
      return (Shift_Left (RB, 11) or Shift_Left (GB, 5) or BB);
   end To_SDL_Color;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels (Pixels : GESTE.Output_Buffer) is

      function To_Address is
        new Ada.Unchecked_Conversion
          (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
           Target => System.Address);

      Width  : constant Natural := Texture.Get_Size.Width;
      Height : constant Natural := Texture.Get_Size.Height;
   begin
      Lock (Texture, SDL_Pixels);

      declare
         Actual_Pixels : Texture_1D_Array (0 .. Natural (Width * Height - 1))
           with
             Address => SDL_Pixels;

      begin

         for Pix of Pixels loop
            Actual_Pixels (X + Y * Width) := Pix;

            if X = XE then
               X := XS;
               if Y = YE then
                  Y := YS;
               else
                  Y := Y + 1;
               end if;
            else
               X := X + 1;
            end if;
         end loop;
      end;


      Texture.Unlock;
   end Push_Pixels;

   ----------
   -- Kill --
   ----------

   procedure Kill is
   begin
      W.Finalize;
      SDL.Finalise;
   end Kill;

begin
   Initialize;
end SDL_Display;
