data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Car : (fule : Nat) -> Vehicle Petrol
     Bus : (fule : Nat) -> Vehicle Petrol
     MotorCycle : (fule: Nat) -> Vehicle Petrol


wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels (MotorCycle fule) = 2
wheels Bicycle = 2
wheels (Car fule) = 4
wheels (Bus fule) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (MotorCycle fule) = MotorCycle 50
refuel (Car fule) = Car 100
refuel (Bus fule) = Bus 200
