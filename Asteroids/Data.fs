namespace Asteroids

module Data = 
    
    open Common.Vector
    open System
    
    type Jet = JetSmall|JetMedium|JetBig
    type Ship = Ship of (int * Vector * float * Jet option)         // id, position, velocity angle, jet status
    type DestroyedShip = DestroyedShip of (Vector * float * float)  // position, angle, scale (explosion)
    type Bullet = Bullet of (int * Vector)                          // id, position
     
    type MeteorSize = Big|Medium|Small with
        static member size = function
            |Big    -> 2.0/20.0
            |Medium -> 2.0/30.0
            |Small  -> 2.0/40.0
        static member speedFactor = function
            |Big    -> 1.0
            |Medium -> 1.5
            |Small  -> 2.0
        static member smaller = function
            |Big    -> Medium
            |Medium -> Small
            |Small  -> Small
        static member score = function
            |Big    -> 100
            |Medium -> 200
            |Small  -> 400
                                
    type Meteor = Meteor of (int * Vector * MeteorSize)  // id, pos, size