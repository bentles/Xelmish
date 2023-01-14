open Elmish
open Xelmish.Model
open Xelmish.Viewables

type Model =
    { Count: int
      StepSize: int
      FrameTimeSample: float list
      FpsSample: float
      LastSampleTime: float }

let init () =
    { Count = 0
      StepSize = 1
      FrameTimeSample = []
      FpsSample = 0
      LastSampleTime = 0 }

let sampleFpsEvery = 200.0 //ms

type Msg =
    | Increment
    | Decrement
    | SetStepSize of int
    | Reset
    | Tick of float * float

let update msg m =
    match msg with
    | Increment -> { m with Count = m.Count + m.StepSize }
    | Decrement -> { m with Count = m.Count - m.StepSize }
    | SetStepSize x -> { m with StepSize = x }
    | Reset -> init ()
    | Tick(dt, totalGameTime) ->
        if totalGameTime - m.LastSampleTime > sampleFpsEvery then
            { m with
                LastSampleTime = totalGameTime
                FrameTimeSample = []
                FpsSample = List.average m.FrameTimeSample }

        else
            { m with FrameTimeSample = (1000.0 / dt) :: m.FrameTimeSample }


// all of the above is straight from the Elmish.WPF sample (with only some indentation changes)
// the view function below, replacing Elmish.WPF's bindings function, is Xelmish specific,
// though note it still follows the dispatch model common to Elmish implementations.

let view model dispatch =
    // because all text uses the same font, it is shadowed here with this param already applied
    let text = text "defaultFont"
    // a button is defined here as a background colour which some text and a click event.
    // Xelmish doesn't provide a button viewable by default - too many possible variables.
    // it just provides the core building blocks
    let button s event (x, y) =
        let width, height = 120, 36

        [ colour Colour.Blue (width, height) (x, y)
          text 20. Colour.White (-0.5, -0.5) s (x + width / 2, y + height / 2)
          onclick event (width, height) (x, y) ]

    // the yield pattern is useful, as single elements like the text with no background can be yielded alongside
    // multi-element elements like buttons with yield!
    [
      // by placing the below code in a viewable function, it will get evaluated on every game draw
      // This can be more effective than using an Elmish subscription, especially if smoothness is needed
      yield
          onupdate (fun inputs ->
              dispatch (
                  Tick(
                      inputs.gameTime.ElapsedGameTime.TotalMilliseconds,
                      inputs.gameTime.TotalGameTime.TotalMilliseconds
                  )
              ))

      yield text 30. Colour.Black (0., 0.) (sprintf "fps: %f" model.FpsSample) (80, 10)
      yield text 30. Colour.Black (0., 0.) (sprintf "Counter value: %i" model.Count) (80, 40)
      yield! button "- counter" (fun () -> dispatch Decrement) (80, 80)
      yield! button "+ counter" (fun () -> dispatch Increment) (220, 80)
      yield text 20. Colour.Black (0., 0.) (sprintf "Step size: %i" model.StepSize) (80, 140)
      yield! button "- step size" (fun () -> dispatch (SetStepSize(model.StepSize - 1))) (80, 170)
      yield! button "+ step size" (fun () -> dispatch (SetStepSize(model.StepSize + 1))) (220, 170)
      yield! button "reset" (fun () -> dispatch Reset) (80, 215)

      yield onkeydown Keys.Escape exit ] // this is added in all the samples, to make it simple and intuitive to exit the app.

[<EntryPoint>]
let main _ =
    let config =
        { resolution = Windowed(420, 300)
          clearColour = Some Colour.White // if set to None, then each draw will layer over the previous. which looks weird.
          mouseVisible = true
          assetsToLoad = [ PipelineFont("defaultFont", "./content/SourceCodePro") ] // the font used in the game needs to be loaded. there is no built-in font.
          isFixedTimeStepWithVsync = false //play with this value to see fps change
          }

    Program.mkSimple init update view // standard, out of the box Elmish initialisation
    |> Program.withConsoleTrace // add and remove to see effect on fps
    |> Xelmish.Program.runGameLoop config // Xelmish specific run function

    0
