(* file: main.ml of stopNgo app
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   A basic animation function stopping a ball when the touchpad is released.
*)
type state = Stop | Go

(* toggle : state -> state *)
let toggle state =
  match state with
  | Stop -> Go
  | Go -> Stop

type model = { state : state
             ; x : int
             }

let displayHeight = 800
let displayWidth = displayHeight
let delta = 2
let radius = 100
let circle = Image.circle radius Color.red
let courseName = Image.text "CSCI 1103" ~size:38. Color.white
let circle = Image.placeImage courseName (5, 85) circle
let y = displayHeight / 2 - radius

let backGround =
  Image.rectangle displayWidth displayHeight Color.dodgerBlue

let view model =
  Image.placeImage circle (model.x, y) backGround

let update model =
  match model.state with
  | Go ->
    let x = (model.x + delta) mod displayWidth
    in
    { model with x = x }
  | Stop -> model

let initialModel = {state = Go; x = - radius}

let handleMouse model x y event =
  match event = "button_up" with
  | true  -> { model with state = toggle model.state }
  | false -> model

let go () =
  Animate.start initialModel
    ~name: "stopNgo"
    ~width: displayWidth
    ~height: displayHeight
    ~rate: 0.01
    ~view: view
    ~onTick: update
    ~onMouse: handleMouse

let s = go ()
