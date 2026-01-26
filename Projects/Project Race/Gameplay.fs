namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

// this represents the state of gameplay simulation.
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

    static let createWheelSettingsWV front position =
        let settings = new JoltPhysicsSharp.WheelSettingsWV ()
        settings.Position <- position
        settings.WheelForward <- v3Forward
        settings.SuspensionSpring <- JoltPhysicsSharp.SpringSettings (JoltPhysicsSharp.SpringMode.FrequencyAndDamping, 3.0f, 0.5f)
        if front then
            settings.MaxBrakeTorque <- 0.0f
            settings.MaxHandBrakeTorque <- 4000.0f
        else
            settings.MaxBrakeTorque <- 4000.0f
            settings.MaxHandBrakeTorque <- 4000.0f
            settings.MaxSteerAngle <- 0.0f
        settings

    static let makeVehicleProperties () =

        // vehicle controller config
        let mutable differential = JoltPhysicsSharp.VehicleDifferentialSettings (LeftWheel = 0, RightWheel = 1)
        let wheeledVehicleControllerSettings = new JoltPhysicsSharp.WheeledVehicleControllerSettings ()
        wheeledVehicleControllerSettings.DifferentialsCount <- 1
        wheeledVehicleControllerSettings.SetDifferential (0, differential)

        // vehicle wheels config
        let wheelSettings =
            [|for i in 0 .. dec 4 do
                let position =
                    match i with
                    | 0 -> v3 -0.8f 0.72f -3.0f // front left
                    | 1 -> v3 0.8f 0.72f -3.0f // front right
                    | 2 -> v3 -0.8f 0.72f 1.5f // back left
                    | 3 -> v3 0.8f 0.72f 1.5f // back right
                    | _ -> failwithumf ()
                createWheelSettingsWV (i < 2) position :> JoltPhysicsSharp.WheelSettings|]

        // vehicle constraint config
        let vehicleConstraintSettings = new JoltPhysicsSharp.VehicleConstraintSettings ()
        vehicleConstraintSettings.Forward <- v3Forward
        vehicleConstraintSettings.Wheels <- wheelSettings
        vehicleConstraintSettings.Controller <- wheeledVehicleControllerSettings

        // fin
        VehiclePropertiesJolt vehicleConstraintSettings

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // process initialization
            let initializing = FQueue.contains Select selectionResults
            if initializing then
                let rendererConfig = World.getRenderer3dConfig world
                World.configureRenderer3d { rendererConfig with FxaaEnabled = true } world

            // begin scene declaration
            World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

            // configure player car manually due to: https://github.com/bryanedds/Nu/issues/1266
            World.doEntity Simulants.GameplayPlayerCar.Name [Entity.VehicleProperties |= makeVehicleProperties ()] world
            let playerCar = world.DeclaredEntity
            let playerCarBodyId = playerCar.GetBodyId world

            // process player input
            if World.isKeyboardKeyDown KeyboardKey.Up world then World.setBodyVehicleForwardInput 10.0f playerCarBodyId world
            elif World.isKeyboardKeyDown KeyboardKey.Down world then World.setBodyVehicleForwardInput -1.0f playerCarBodyId world
            else World.setBodyVehicleForwardInput 0.0f playerCarBodyId world
            if World.isKeyboardKeyDown KeyboardKey.Left world then World.setBodyVehicleRightInput -0.25f playerCarBodyId world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then World.setBodyVehicleRightInput 0.25f playerCarBodyId world
            else World.setBodyVehicleRightInput 0.0f playerCarBodyId world
            if World.isKeyboardKeyDown KeyboardKey.Space world
            then World.setBodyVehicleBrakeInput 5.0f playerCarBodyId world
            else World.setBodyVehicleBrakeInput 0.0f playerCarBodyId world
            if World.isKeyboardKeyDown KeyboardKey.LCtrl world || World.isKeyboardKeyDown KeyboardKey.RCtrl world
            then World.setBodyVehicleHandBrakeInput 5.0f playerCarBodyId world
            else World.setBodyVehicleHandBrakeInput 0.0f playerCarBodyId world
            if World.isKeyboardKeyDown KeyboardKey.R world
            then World.setBodyRotation quatIdentity playerCarBodyId world

            // declare speed text
            let speed = (playerCar.GetLinearVelocity world).Magnitude
            World.doText "Speed" [Entity.Position .= v3 -232.0f -144.0f 0.0f; Entity.Text @= string (int (speed * 60.0f / 10.0f)) + " KPH"] world

            // update eye to look at player while game is advancing
            if world.Advancing then
                let position = Simulants.GameplayPlayerCar.GetPosition world
                let rotation = Simulants.GameplayPlayerCar.GetRotation world * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
                World.setEye3dCenter (position + v3Up * 1.75f - rotation.Forward * 3.0f) world
                World.setEye3dRotation rotation world

            // declare quit button
            if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
                screen.SetGameplayState Quit world

            // end scene declaration
            World.endGroup world