# Henriette the Witch

##Authors
Andrew Gabriel

Patrick Lehane

##Overview
Henriette the Witch is a 2D side-scroller which involves a number of mechanics such as running, jumping, shooting spells, and more!

##Screenshot
Here's the title screen to our game where you can either start a new one or continue from where you last left off:
![titlescreenshot](v8eHm27.png)

Here's the game in action:
![gamescreenshot](3wv7dHN.png)

##Concepts Demonstrated
* **Higher Order Procedures** are used in order to add/remove sprites from the sprite list, to check for collisions between different sprites.
* We used **abstraction** when creating our sprites, tiles, and maps and the procedures needed to access elements from those “classes”.
* We also made heavy use of local bindings such as let, let*, etc. when dealing with our sprite updates.

##External Technology and Libraries
[2htdp/image](http://docs.racket-lang.org/teachpack/2htdpuniverse.html) was used to draw the game background, map, sprites, and just about everything else you see when you play the game.
[2htdp/universe](http://docs.racket-lang.org/teachpack/2htdpuniverse.html) allowed us to manipulate our sprites on every frame so that we can have a fully functional game.
[2htdp/batch-io](http://docs.racket-lang.org/teachpack/2htdpbatch-io.html) was also used when the user wanted to save their current progress in the game by grabbing the current state of the game and saving it into a text file; when the user wanted to continue we loaded the same data from the text file in order to create our world from the user’s last session.


##Favorite Lines of Code
####Andrew 

This is the function that draws all of the sprites. It uses the higher order procedure accumulate to place all of the sprites at the proper x and y coordinates, using the draw function each sprite has. It also takes into account the offset from the far left boundry of the level. This consideration ended up making implementing side-scrolling much easier than anticipated, and because of that, it's my favorite line.

```scheme
(define (draw-sprites sprite-list img offset)
  (foldr (lambda (x y) (place-image/align ((sprite-draw x) x) 
                                          (- (sprite-realX x) offset) 
                                          (sprite-realY x) 
                                          'left 'top y)) 
         img
         sprite-list))
```
####Patrick
This was used inside of the "slim-goo" enemy update procedure, these lines of code basically handled all of the slim-goo's basic movements across the map which made it nice. 
```
 (if (equal? (move-left sprite 2) sprite)
     (change-sprite-state sprite "walk-right")
     (move-left sprite 2)))
(else
     (if (equal? (move-right sprite 2) sprite)
         (change-sprite-state sprite "walk-left")
         (move-right sprite 2)))))

```


#How to Download and Run
In order to play the game, open final-project.rkt in DrRacket and run it.
To select start or continue on the title screen use the arrow keys to move and z to select.
The controls for the rest of the game are:
* z button - jump, also used to select when on title screen or in pause menu
* x button - attack
* arrow keys - move player to left and right
* left shift - open pause menu

The player's level is the number in the star in the top left corner of the screen. The higher level you are, the more powerful your spells. Experience points, represented by the yellow bar, can be collected by killing enemies to gain levels. 

Latest Release : ([x](https://github.com/oplS15projects/Henriette-the-Witch/releases/tag/v2.0))
