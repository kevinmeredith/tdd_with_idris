module VendingMachine

Pounds : Type
Pounds = Nat

Choc : Type
Choc = Nat

data Machine = Mach Pounds Choc

fill : Machine -> Nat -> Machine
fill (Mach ps bars) ns = Mach ps (bars + ns)

init : Machine
init = Mach 0 0

insertCoin : Machine -> Machine
insertCoin (Mach ps bars) = (Mach (ps + 1) bars)

vend : Machine -> Machine
vend (Mach Z bars)          = Mach Z bars
vend (Mach (S ps) (S bars)) = Mach ps bars

getChange : Machine -> Machine
getChange (Mach Z bars) = Mach Z bars
getChange (Mach n bars) = Mach 0 bars

display : Machine -> IO Machine
display = ?disp
