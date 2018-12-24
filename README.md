### FPS Scene

You can walk around the scene with simple FPS controls.
Open `scene.html` with an http server, you can use the following;

```python3 -m http.server 8000```

Unfortunately, elm-reactor has issues with loading obj files so that's why the
python server is needed.

##### Controls

Mouse - Look Rotation
WASD - Player movement
◀ ▼ ▲ ▶ - Move the Robot
N and M - Rotate robot left and right
Y and H - Rotate robot arm up and down
U and J - Rotate robot hand up and down

### PQTorusknot

Either open up `torus.html`, or use the command `elm-reactor`, if you want to be able to modify the source file and compile;

https://guide.elm-lang.org/install.html

Alternatively, here's the Ellie link, so you can easily modify and compile;

https://ellie-app.com/vVTgpBj77ra1

##### Constants you can modify

// Total verts in the line
totalLinePoints = 100
// Radius of the torus
ringRadius = 0.15
// Amount of verts in each ring
ringVerts = 18

// The model.p and model.q increment speeds
{ model | time = model.time + dt * 0.001 , p = model.p + 0.08 , q = model.q + 0.04}
