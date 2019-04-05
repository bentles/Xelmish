﻿module Xelmish.Viewables

open Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Viewable =
| Position of x: int * y: int * width: int * height: int * Viewable
| Window of x: int * y: int * width: int * height: int * Viewable list
| Row of Viewable list
| Text of textStyle: TextStyle * text: string
| Button of buttonStyle: ButtonStyle * text: string * onClick: (unit -> unit)
and TextStyle = {
    font: string
    colour: Colour
} and ButtonStyle = {
    font: string
    colour: Colour
    backColour: Colour
}

//let private buttonBack: Texture2D = null

let private vector2 x y = Vector2(float32 x, float32 y)
//let private rectangle x y w h = Rectangle(x, y, w, h)
let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let rec internal renderViewable (spriteBatch: SpriteBatch) gameTime gameState (px, py, pw, ph) viewable =
    match viewable with
    | Position (x, y, w, h, sv) ->
        renderViewable spriteBatch gameTime gameState (px + x, py + y, w, h) sv
    | Window (x, y, w, h, svl) ->
        let div = h / svl.Length
        svl 
        |> List.iteri (fun i -> 
            renderViewable spriteBatch gameTime gameState (px + x, py + y + (i * div), w, div))
    | Row svl ->
        let div = pw / svl.Length
        svl 
        |> List.iteri (fun i -> 
            renderViewable spriteBatch gameTime gameState (px + (i * div), py, div, ph))
    | Text (ts, s) ->
        let font = Map.find ts.font gameState.fonts
        spriteBatch.DrawString(font, s, vector2 px py, xnaColor ts.colour)
    | Button (bs, s, evt) ->
        spriteBatch.Draw(gameState.whiteTexture, xnaRect px py pw ph, xnaColor bs.backColour)

        let font = Map.find bs.font gameState.fonts
        spriteBatch.DrawString(font, s, vector2 px py, xnaColor bs.colour)

        if (gameState.mouseState.X, gameState.mouseState.Y) ||> isInside px py pw ph then
            if gameState.mouseState.LeftButton = ButtonState.Pressed 
            && gameState.lastMouseState.LeftButton <> ButtonState.Pressed then
                evt ()