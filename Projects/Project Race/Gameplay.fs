namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // process initialization
            let initializing = FQueue.contains Select selectionResults
            let world =
                if initializing
                then World.mapRenderer3dConfig (fun config -> { config with SsrEnabled = true }) world
                else world

            // begin scene declaration
            let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

            // declare player car
            let world = World.doEntityFromFile "PlayerCar" "Assets/Gameplay/Cars/Sedan/Sedan.nuentity" [] world
            let playerCar = world.DeclaredEntity
            let playerCarBodyId = playerCar.GetBodyId world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Up world then World.setBodyVehicleForwardInput 10.0f playerCarBodyId world
                elif World.isKeyboardKeyDown KeyboardKey.Down world then World.setBodyVehicleForwardInput -1.0f playerCarBodyId world
                else World.setBodyVehicleForwardInput 0.0f playerCarBodyId world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Left world then World.setBodyVehicleRightInput -0.25f playerCarBodyId world
                elif World.isKeyboardKeyDown KeyboardKey.Right world then World.setBodyVehicleRightInput 0.25f playerCarBodyId world
                else World.setBodyVehicleRightInput 0.0f playerCarBodyId world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.Space world
                then World.setBodyVehicleBrakeInput 5.0f playerCarBodyId world
                else World.setBodyVehicleBrakeInput 0.0f playerCarBodyId world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.LCtrl world || World.isKeyboardKeyDown KeyboardKey.RCtrl world
                then World.setBodyVehicleHandBrakeInput 5.0f playerCarBodyId world
                else World.setBodyVehicleHandBrakeInput 0.0f playerCarBodyId world
            let world =
                if World.isKeyboardKeyDown KeyboardKey.R world
                then World.setBodyRotation quatIdentity playerCarBodyId world
                else world

            // declare speed text
            let speed = (playerCar.GetLinearVelocity world).Magnitude
            let world = World.doText "Speed" [Entity.Position .= v3 -232.0f -144.0f 0.0f; Entity.Text @= string (int (speed * 60.0f / 10.0f)) + " KPH"] world

            // update sun to shine over player car as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let mutable shadowViewInverse = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion (sun.GetRotation world)
            shadowViewInverse.Translation <- sun.GetPosition world
            let shadowView = shadowViewInverse.Inverted
            let shadowWidth = max (sun.GetLightCutoff world * 2.0f) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
            let shadowTexelSize = shadowWidth / single world.GeometryViewport.ShadowTextureResolution.X // assuming square shadow texture, of course
            let position = Simulants.GameplayPlayerCar.GetPosition world + (Simulants.GameplayPlayerCar.GetRotation world).Forward * 112.0f
            let positionShadow = position.Transform shadowView
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowViewInverse
            let world = sun.SetPositionLocal position world

            // update eye to look at player while game is advancing
            let world =
                if world.Advancing then
                    let position = Simulants.GameplayPlayerCar.GetPosition world
                    let rotation = Simulants.GameplayPlayerCar.GetRotation world * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
                    let world = World.setEye3dCenter (position + v3Up * 1.75f - rotation.Forward * 3.0f) world
                    let world = World.setEye3dRotation rotation world
                    world
                else world

            // declare quit button
            let (clicked, world) = World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world
            let world = if clicked then screen.SetGameplayState Quit world else world

            // end scene declaration
            World.endGroup world

        // otherwise, no processing
        else world