# FsReactive

A pull implementation of a Functional Reactive Programming (FRP) Library

FsReactive is a simple implementation based on residual Behaviors.

You can find (partial) explanation of design choices [here](http://call-with-cc-en.blogspot.com/2008/12/functional-reactive-programming-in-f-1.html)

It includes:

Similar abstraction of Behaviors and Events. Events are Behaviors of type <code>option<'a></code>

Recursive definition of Behaviors.

Behavior integration over time.

Dynamic collection of Behaviors.

Handling of collisions.

Handling of discontinuities in Behaviors.

#Demos

The library comes a set of working graphical demos using XNA:

Rectangles: 
- Left click to create a rectangle.
- Move the mouse to change its size.
- Right click to save it.

Spring:

Simulates the motion of a mass connected with a spring to the mouse pointer.
The mass is only allowed to move within the Window (it bounces against the window sides).

Paddle:

A simple paddle game. Move the move to hit the ball.

DynCol:

Demonstrates the dynamic collection of Behaviors of the same type and the handling of collisions.
Left click to create bouncing balls. When two balls hit each other, they are destroyed and disappear. 
Otherwise, balls keep on accelerating.

Bricks:

A very simple Brick-like game demonstrating collision between Behaviors of different types.
Hit enter to start. You have 3 lives.

Asteroids:

The classical Asteroid game. Demonstrates most of FsReactive's features in a not so trivial game.

Hit enter to start a game.

Right and left arrows turn your ship.

Space accelerates your ship.

Hit 'o' to fire a missile.

Hit 'z' to create a temporary shield around your ship.

Note: watch for bonus ships.
